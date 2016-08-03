#!/bin/bash

VERSION=`cat marathon-baton.cabal| grep "^version:" | awk '{print $2}'`

echo "Publishing as version $VERSION"

stack image container && \
docker login && \
docker tag vipo/marathon-baton:latest vipo/marathon-baton:$VERSION && \
docker push vipo/marathon-baton:$VERSION && \
docker logout

echo "Done"