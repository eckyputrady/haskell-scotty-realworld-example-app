FROM ubuntu:16.04

ENV DIRPATH /usr/local/etc

WORKDIR $DIRPATH

RUN apt-get update -y
RUN apt-get install -y postgresql-client

COPY bin/realworld-exe /usr/local/bin/realworld-exe
COPY postgresql $DIRPATH/postgresql
COPY secrets $DIRPATH/secrets

ENTRYPOINT [ "realworld-exe" ]
