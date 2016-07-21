{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
module Data.BDD.Tape
    ( Tape
    , TapeM
    , at
    , insert
    , run
    , run'
    , restrict
    , apply
    , neg
    , anySat
    , satCount
    , isLeaf
    , build
    , BinOp(..)
    , pretty
    , toString
    ) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Control.Monad.Reader
import Data.BDD.Types
import qualified Data.HashMap.Strict as M
import qualified Data.HashTable.ST.Cuckoo as H
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as V
import Data.Function
import Data.STRef
import Data.Profunctor.Unsafe
import Text.PrettyPrint

data Tape s = Tape
    { tapeVector :: {-# UNPACK #-} !(STRef s (V.MVector s Node))
    , tapeCurrId :: {-# UNPACK #-} !(STRef s Int)
    , tapeMap :: {-# UNPACK #-} !(H.HashTable s Node NodeId)
    , tapeAnd :: {-# UNPACK #-} !(H.HashTable s (NodeId, NodeId) NodeId)
    , tapeOr :: {-# UNPACK #-} !(H.HashTable s (NodeId, NodeId) NodeId)
    , tapeRestrict :: {-# UNPACK #-} !(H.HashTable s (NodeId, Index, Bool) NodeId)
    , tapeNot :: {-# UNPACK #-} !(H.HashTable s NodeId NodeId)
    }

newtype TapeM s a = TapeM (ReaderT (Tape s) (ST s) a)
    deriving (Functor, Applicative, Monad, MonadReader (Tape s))

data BinOp = BAnd | BOr
    deriving (Eq, Show)

liftST :: ST s a -> TapeM s a
liftST = TapeM #. lift
{-# INLINE liftST #-}

instance PrimMonad (TapeM s) where
    type PrimState (TapeM s) = PrimState (ST s)

    primitive = TapeM #. primitive
    {-# INLINE primitive #-}

readRef :: STRef s a -> TapeM s a
readRef = liftST . readSTRef
{-# INLINE readRef #-}

writeRef :: STRef s a -> a -> TapeM s ()
writeRef r = liftST . writeSTRef r
{-# INLINE writeRef #-}

modifyRef :: STRef s a -> (a -> a) -> TapeM s ()
modifyRef r = liftST . modifySTRef' r
{-# INLINE modifyRef #-}

extend :: TapeM s ()
extend = do
    ref <- asks tapeVector
    vec <- readRef ref
    vec' <- V.unsafeGrow vec (V.length vec + 1)
    writeRef ref vec'
{-# INLINE extend #-}

at :: NodeId -> TapeM s Node
at (NodeId n) = do
    tv <- asks tapeVector
    vec <- readRef tv
    V.unsafeRead vec n
{-# INLINE at #-}

insert :: Node -> TapeM s NodeId
insert Node{..} | nodeLeft == nodeRight = pure nodeLeft
insert n@Node{..} = {-# SCC "insert" #-} do
    tm <- asks tapeMap
    liftST (H.lookup tm n) >>= \case
        Just ni -> pure ni
        Nothing -> doInsert n 
{-# INLINE insert #-}

doInsert :: Node -> TapeM s NodeId
doInsert n@Node{..} = {-# SCC "doInsert" #-} do
    i <- asks tapeCurrId >>= readRef
    vec <- asks tapeVector >>= readRef
    if i == V.length vec
    then extend >> doInsert n
    else do
        Tape{..} <- ask
        modifyRef tapeCurrId (+1)
        liftST $ H.insert tapeMap n (NodeId i)
        V.unsafeWrite vec i n
        pure (NodeId i)
{-# INLINEABLE doInsert #-}

newTape :: ST s (Tape s)
newTape = do
    vec <- V.unsafeNew (2 ^ (25 :: Int))
    V.write vec 0 (Node (Index $ -1) (NodeId 0) (NodeId 0))
    V.write vec 1 (Node (Index $ -1) (NodeId 1) (NodeId 1))
    tapeVector <- newSTRef vec
    tapeCurrId <- newSTRef 2
    tapeMap <- create
    tapeAnd <- create
    tapeOr <- create
    tapeNot <- create
    tapeRestrict <- create
    pure Tape{..}
  where create = H.newSized 1000000
{-# INLINE newTape #-}

run :: (forall s. TapeM s a) -> a
run x = runST $ case x of TapeM f -> newTape >>= runReaderT f
{-# INLINE run #-}

run' :: (forall s. TapeM s a) -> (a, U.Vector Node)
run' f = run $ do
    a <- f
    v <- U.take <$> (asks tapeCurrId >>= readRef) <*> (asks tapeVector >>= readRef >>= U.freeze)
    pure (a, v)
{-# INLINE run' #-}

restrict :: Index -> Bool -> NodeId -> TapeM s NodeId
restrict i b ni = {-# SCC "restrict" #-} do
    hm <- asks tapeRestrict
    liftST (H.lookup hm (ni, i, b)) >>= \case
        Just ret -> pure ret
        Nothing -> do
            ret <- go
            liftST $ H.insert hm (ni, i, b) ret
            pure ret
  where
    go = {-# SCC "restrict_go" #-} do
        Node{..} <- at ni
        case compare nodeIndex i of
            EQ -> pure $ if b then nodeLeft else nodeRight
            LT -> pure ni
            GT -> do
                left <- restrict i b nodeLeft
                right <- restrict i b nodeRight
                insert $ Node nodeIndex left right
{-# INLINEABLE restrict #-}

anySat :: NodeId -> TapeM s (Maybe Subst)
anySat NodeFalse = pure Nothing
anySat NodeTrue = pure $ Just M.empty
anySat ni = do
    Node{..} <- at ni
    ls <- anySat nodeLeft
    case ls of
        Just s -> pure . Just $ M.insert nodeIndex False s
        Nothing -> fmap (M.insert nodeIndex True) <$> anySat nodeRight
{-# INLINEABLE anySat #-}

satCount :: Num a => Int -> NodeId -> TapeM s a
satCount _ NodeFalse = pure 0
satCount varCount NodeTrue = pure $ 2 ^ varCount
satCount varCount ni = do
     Node{..} <- at ni
     let ix = getIndex nodeIndex
     left <- satCount ix nodeLeft
     right <- satCount ix nodeRight
     pure $ 2 ^ (varCount - ix - 1) * (left + right)
{-# INLINEABLE satCount #-}

isLeaf :: NodeId -> Bool
isLeaf = (<2) . getNodeId
{-# INLINE isLeaf #-}

apply :: BinOp -> NodeId -> NodeId -> TapeM s NodeId
apply BAnd NodeTrue x = pure x
apply BAnd NodeFalse _ = pure NodeFalse
apply BAnd x NodeTrue = pure x
apply BAnd _ NodeFalse = pure NodeFalse
apply BOr NodeTrue _ = pure NodeTrue
apply BOr NodeFalse x = pure x
apply BOr _ NodeTrue = pure NodeTrue
apply BOr x NodeFalse = pure x
apply b li ri = {-# SCC "apply" #-} do
    hm <- asks $ case b of
                    BAnd -> tapeAnd
                    BOr -> tapeOr
    liftST (H.lookup hm (li, ri)) >>= \case
        Just n -> pure n
        Nothing -> do
            ret <- join $ go <$> at li <*> at ri
            liftST $ H.insert hm (li, ri) ret
            pure ret
  where
    go :: Node -> Node -> TapeM s NodeId
    go l r = {-# SCC "go" #-} case (compare `on` nodeIndex) l r of
        EQ -> do
            left <- apply b (nodeLeft l) (nodeLeft r)
            right <- apply b (nodeRight l) (nodeRight r)
            insert Node
                { nodeIndex = nodeIndex l
                , nodeLeft = left
                , nodeRight = right
                }
        GT -> do
            left <- apply b (nodeLeft l) ri
            right <- apply b (nodeRight l) ri
            insert Node
                { nodeIndex = nodeIndex l
                , nodeLeft = left
                , nodeRight = right
                }
        LT -> do
            left <- apply b li (nodeLeft r)
            right <- apply b li (nodeRight r)
            insert Node
                { nodeIndex = nodeIndex r
                , nodeLeft = left
                , nodeRight = right
                }
{-# INLINEABLE apply #-}

neg :: NodeId -> TapeM s NodeId
neg NodeTrue = pure NodeFalse
neg NodeFalse = pure NodeTrue
neg ni = {-# SCC "neg" #-} do
    hm <- asks tapeNot
    liftST (H.lookup hm ni) >>= \case
        Just ret -> pure ret
        Nothing -> {-# SCC "neg_inner" #-} do
            Node{..} <- at ni
            left <- neg nodeLeft
            right <- neg nodeRight
            ret <- insert Node
                { nodeIndex = nodeIndex
                , nodeLeft = left
                , nodeRight = right
                }
            liftST $ H.insert hm ni ret
            pure ret
{-# INLINEABLe neg #-}

build :: BExp -> TapeM s NodeId
build (EPrim True) = pure NodeTrue
build (EPrim False) = pure NodeFalse
build (EIdRef ix) = insert $ Node ix NodeTrue NodeFalse
build (ENot x) = build x >>= neg
build (EAnd e e') = {-# SCC "build_EAnd" #-} (join $ apply BAnd <$> build e <*> build e')
build (EOr e e') = {-# SCC "build_EOr" #-} (join $ apply BOr <$> build e <*> build e')
{-# INLINEABLE build #-}

pretty :: NodeId -> TapeM s Doc
pretty NodeFalse = pure $ text "False"
pretty NodeTrue = pure $ text "True"
pretty ni = do
    Node{..} <- at ni
    left <- pretty nodeLeft
    right <- pretty nodeRight
    pure . hang (text "if" <+> int (getIndex nodeIndex)) 2 $
        vcat [left, right]

toString :: NodeId -> TapeM s String
toString = fmap render . pretty
