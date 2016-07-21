#!/bin/sh

wget -O stack.tar.gz https://www.stackage.org/stack/linux-x86_64
tar xvvf stack.tar.gz

if [ `hostname` = "students" ]
then
    export SYSTEM_CERTIFICATE_PATH=/etc/openssl/certs
fi

./stack-1.1.0-linux-x86_64/stack setup
./stack-1.1.0-linux-x86_64/stack install --local-bin-path=`pwd`
