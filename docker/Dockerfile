# FROM haskell:8.6

# MAINTAINER Hamilton

# RUN apt-get update

# RUN apt-get -y install libblas-dev libatlas-dev liblapack-dev libgsl0-dev libghc-postgresql-libpq-dev libicu-dev libssl-dev openssl wget

FROM haskell:8.6.5

ADD main.sh /

ENTRYPOINT ["/bin/bash", "-c", "/main.sh ${*}", "--"]
