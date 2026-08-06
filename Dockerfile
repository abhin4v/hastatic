FROM fossa/haskell-static-alpine:ghc-9.8.2 AS builder
WORKDIR /opt/hastatic

RUN apk add --no-cache upx \
    && cabal update

COPY hastatic.cabal cabal.project ./
RUN cabal configure -O2 --enable-executable-static \
        --ghc-option=-fPIC \
        --ghc-option=-split-sections \
        --ghc-option=-optl=-static \
        --ghc-option=-optl=-pthread \
        --ld-option=-Wl,--gc-sections,--build-id,--icf=all \
    && cabal build --only-dependencies

COPY . .
RUN cabal build exe:hastatic \
    && cp "$(cabal -v0 list-bin exe:hastatic)" /usr/local/bin/hastatic \
    && strip /usr/local/bin/hastatic \
    && upx --best --lzma /usr/local/bin/hastatic

FROM scratch
COPY --from=builder /usr/local/bin/hastatic /usr/bin/hastatic