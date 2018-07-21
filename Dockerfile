FROM haskell:8.4.3 as builder
WORKDIR /opt/hastatic
ADD . .
RUN stack install && strip /root/.local/bin/hastatic

FROM fpco/haskell-scratch:integer-gmp
COPY --from=builder /root/.local/bin/hastatic /usr/bin/hastatic