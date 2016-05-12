FROM haskell
# FROM zsol/haskell-platform-2013.2.0.0
# FROM darinmorrison/haskell

MAINTAINER lemmih@gmail.com

RUN apt-get update && apt-get install -y libpq-dev git
RUN cabal update && cabal install http-client happstack-server postgresql-simple resource-pool file-embed influxdb mongoDB mwc-random optparse-applicative mwc-random xhtml yaml conduit

