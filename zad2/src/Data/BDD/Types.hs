{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
module Data.BDD.Types
    ( Index(..)
    , NodeId(..)
    , Node(..)
    , BExp(..)
    , Subst
    , pattern NodeTrue
    , pattern NodeFalse
    , pattern (:&:)
    , pattern (:|:)
    ) where

import Data.Hashable
import qualified Data.HashMap.Strict as M
import Data.Vector.Unboxed as VU
import Data.Vector.Generic as VG
import Data.Vector.Generic.Mutable as VGM
import Data.Vector.Primitive as VP

newtype Index = Index { getIndex :: Int }
    deriving (Eq, Show, Hashable, Ord)

newtype NodeId = NodeId { getNodeId :: Int }
    deriving (Eq, Show, Hashable)

data Node = Node
    { nodeIndex :: {-# UNPACK #-} !Index
    , nodeLeft :: {-# UNPACK #-} !NodeId
    , nodeRight :: {-# UNPACK #-} !NodeId
    }
    deriving (Eq, Show)

instance Hashable Node where
    hashWithSalt s Node{..} = s `hashWithSalt` nodeIndex `hashWithSalt` nodeLeft `hashWithSalt` nodeRight
    {-# INLINE hashWithSalt #-}

newtype instance VU.MVector s Node = MV_Node (VP.MVector s Int)
newtype instance VU.Vector Node = V_Node (VP.Vector Int)
instance VU.Unbox Node

instance VGM.MVector VU.MVector Node where
    basicLength (MV_Node v) = VGM.basicLength v `div` 3
    {-# INLINE basicLength #-}

    basicUnsafeSlice i n (MV_Node v) = MV_Node $ VGM.basicUnsafeSlice (3 * i) (3 * n) v
    {-# INLINE basicUnsafeSlice #-}

    basicOverlaps (MV_Node v) (MV_Node v') = basicOverlaps v v'
    {-# INLINE basicOverlaps #-}

    basicUnsafeNew n = MV_Node <$> VGM.basicUnsafeNew (3 * n)
    {-# INLINE basicUnsafeNew #-}

    basicInitialize (MV_Node v) = VGM.basicInitialize v
    {-# INLINE basicInitialize #-}

    basicUnsafeRead (MV_Node v) ix = do
        nodeIndex <- Index <$> basicUnsafeRead v (3 * ix)
        nodeLeft <- NodeId <$> basicUnsafeRead v (3 * ix + 1)
        nodeRight <- NodeId <$> basicUnsafeRead v (3 * ix + 2)
        pure Node{..}
    {-# INLINE basicUnsafeRead #-}

    basicUnsafeWrite (MV_Node v) ix Node{..} = do
        basicUnsafeWrite v (3 * ix) (getIndex nodeIndex)
        basicUnsafeWrite v (3 * ix + 1) (getNodeId nodeLeft)
        basicUnsafeWrite v (3 * ix + 2) (getNodeId nodeRight)
    {-# INLINE basicUnsafeWrite #-}

instance VG.Vector VU.Vector Node where
    basicUnsafeFreeze (MV_Node v) = V_Node <$> VG.basicUnsafeFreeze v
    {-# INLINE basicUnsafeFreeze #-}

    basicUnsafeThaw (V_Node v) = MV_Node <$> VG.basicUnsafeThaw v
    {-# INLINE basicUnsafeThaw #-}

    basicLength (V_Node v) = VG.basicLength v `div` 3
    {-# INLINE basicLength #-}

    basicUnsafeSlice i n (V_Node v) = V_Node $ VG.basicUnsafeSlice (3 * i) (3 * n) v
    {-# INLINE basicUnsafeSlice #-}

    basicUnsafeIndexM (V_Node v) ix = do
        nodeIndex <- Index <$> basicUnsafeIndexM v (3 * ix)
        nodeLeft <- NodeId <$> basicUnsafeIndexM v (3 * ix + 1)
        nodeRight <- NodeId <$> basicUnsafeIndexM v (3 * ix + 2)
        pure Node{..}
    {-# INLINE basicUnsafeIndexM #-}

data BExp = EPrim !Bool
          | EIdRef !Index
          | ENot !BExp
          | EAnd !BExp !BExp
          | EOr !BExp !BExp
          deriving (Eq, Ord, Show)

pattern (:&:) x y = EAnd x y
infixr 7 :&:
pattern (:|:) x y = EOr x y
infixr 5 :|:

type Subst = M.HashMap Index Bool

pattern NodeFalse = NodeId 0
pattern NodeTrue = NodeId 1
