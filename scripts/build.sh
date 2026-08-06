#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

version="$(awk '$1 == "version:" { print $2; exit }' hastatic.cabal)"
image="${IMAGE:-hastatic}"
platforms="${PLATFORMS:-linux/amd64,linux/arm64}"

echo "Building ${image}:${version} for: ${platforms}"

podman build \
  --platform "$platforms" \
  --manifest "${image}:${version}" \
  .

podman tag "${image}:${version}" "${image}:latest"

echo "Built manifest list ${image}:${version} (also tagged ${image}:latest)"
podman manifest inspect "${image}:${version}" | \
  sed -n 's/.*"architecture": "\(.*\)".*/\1/p' | sort -u | \
  while read -r arch; do echo "  - ${arch}"; done
