#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

version="$(awk '$1 == "version:" { print $2; exit }' hastatic.cabal)"
repo="$(sed -n 's/.*github.com\/\([^ ]*\)#readme.*/\1/p' hastatic.cabal)"
registry="${REGISTRY:-docker.io}"
image="${IMAGE:-${repo}}"
platforms="${PLATFORMS:-linux/amd64,linux/arm64}"
enable_tls="${ENABLE_TLS:-1}"

suffix=""
if [[ "$enable_tls" != "1" ]]; then
  suffix="notls-"
fi

ref="${registry}/${image}:${suffix}${version}"
latest="${registry}/${image}:${suffix}latest"

PLATFORMS="$platforms" IMAGE="$image" ENABLE_TLS="$enable_tls" ./scripts/build.sh

echo "Pushing ${ref}"
podman manifest push --all "${image}:${suffix}${version}" "${ref}"

echo "Pushing ${latest}"
podman manifest push --all "${image}:${suffix}${version}" "${latest}"

echo "Done: ${ref}"
podman manifest inspect "${ref}" | \
  sed -n 's/.*"architecture": "\(.*\)".*/\1/p' | sort -u | \
  while read -r arch; do echo "  - ${arch}"; done