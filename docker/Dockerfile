# FROM haskell:8.6

# MAINTAINER Hamilton

# RUN apt-get update

# RUN apt-get -y install libblas-dev libatlas-dev liblapack-dev libgsl0-dev libghc-postgresql-libpq-dev libicu-dev libssl-dev openssl wget

FROM rcs/haskell:8.6.5

ADD main.sh /

ADD etradejanitor-1.2 /

ENTRYPOINT ["/bin/bash", "-c", "/main.sh ${*}", "--"]
