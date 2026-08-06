FROM fossa/haskell-static-alpine:ghc-9.8.2 AS builder
WORKDIR /opt/hastatic

RUN apk add --no-cache upx \
    && cabal update

COPY hastatic.cabal cabal.project ./
ARG TARGETARCH
RUN arch="${TARGETARCH:-$(uname -m)}"; \
    icf=",--icf=all"; \
    case "$arch" in \
      amd64|x86_64) icf="" ;; \
    esac; \
    cabal configure -O2 --enable-executable-static \
        --ghc-option=-fPIC \
        --ghc-option=-split-sections \
        --ghc-option=-optl=-static \
        --ghc-option=-optl=-pthread \
        --ld-option="-Wl,--gc-sections,--build-id$icf" \
    && cabal build --only-dependencies

COPY . .
RUN cabal build exe:hastatic \
    && cp "$(cabal -v0 list-bin exe:hastatic)" /usr/local/bin/hastatic \
    && strip /usr/local/bin/hastatic \
    && upx --best --lzma /usr/local/bin/hastatic \
    && printf 'hastatic:x:1000:1000::/srv:/sbin/nologin\n' > /etc/passwd \
    && printf 'hastatic:x:1000:\n' > /etc/group

FROM scratch
COPY --from=builder /etc/passwd /etc/passwd
COPY --from=builder /etc/group /etc/group
COPY --from=builder /usr/local/bin/hastatic /usr/bin/hastatic

USER 1000:1000
WORKDIR /srv
CMD ["/usr/bin/hastatic"]