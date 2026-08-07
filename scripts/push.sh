#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

version="$(awk '$1 == "version:" { print $2; exit }' hastatic.cabal)"
repo="$(sed -n 's/.*github.com\/\([^ ]*\)#readme.*/\1/p' hastatic.cabal)"
registry="${REGISTRY:-docker.io}"
image="${IMAGE:-${repo}}"
platforms="${PLATFORMS:-linux/amd64,linux/arm64}"

ref="${registry}/${image}:${version}"
latest="${registry}/${image}:latest"

PLATFORMS="$platforms" IMAGE="$image" ./scripts/build.sh

echo "Pushing manifest list ${ref}"
podman manifest push --all "${image}:${version}" "${ref}"

echo "Pushing ${latest} manifest list"
podman manifest push --all "${image}:${version}" "${latest}"

echo "Done: ${ref}"
podman manifest inspect "${registry}/${image}:${version}" | \
  sed -n 's/.*"architecture": "\(.*\)".*/\1/p' | sort -u | \
  while read -r arch; do echo "  - ${arch}"; done