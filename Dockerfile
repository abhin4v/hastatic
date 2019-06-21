FROM haskell:8.6.5 as builder
WORKDIR /opt/hastatic
ADD . .
RUN stack install && strip /root/.local/bin/hastatic

FROM abhin4v/haskell-scratch:integer-gmp
COPY --from=builder /root/.local/bin/hastatic /usr/bin/hastatic
