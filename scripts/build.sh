#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

version="$(awk '$1 == "version:" { print $2; exit }' hastatic.cabal)"
image="${IMAGE:-hastatic}"
platforms="${PLATFORMS:-linux/amd64,linux/arm64}"
enable_tls="${ENABLE_TLS:-1}"

suffix=""
if [[ "$enable_tls" != "1" ]]; then
  suffix="notls-"
fi

tag="${image}:${suffix}${version}"
latest_tag="${image}:${suffix}latest"

echo "Building ${tag} for: ${platforms} (ENABLE_TLS=${enable_tls})"

podman build \
  --platform "$platforms" \
  --build-arg "ENABLE_TLS=${enable_tls}" \
  --manifest "${tag}" \
  .

podman tag "${tag}" "${latest_tag}"

echo "Built manifest list ${tag} (also tagged ${latest_tag})"
podman manifest inspect "${tag}" | \
  sed -n 's/.*"architecture": "\(.*\)".*/\1/p' | sort -u | \
  while read -r arch; do echo "  - ${arch}"; done
