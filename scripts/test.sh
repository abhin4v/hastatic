#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

PORT="${TEST_PORT:-8080}"
BIN="$(cabal -v0 list-bin exe:hastatic)"

work="$(mktemp -d)"
cleanup() {
  if [[ -n "${server_pid:-}" ]]; then
    kill "$server_pid" 2>/dev/null || true
    wait "$server_pid" 2>/dev/null || true
  fi
  rm -rf "$work"
}
trap cleanup EXIT INT TERM

mk_build_tree() {
  mkdir -p "$work"/{sub,sub2,a/b/c,foo/.hidden,.git,.well-known/acme-challenge,diridx,empty}
  mk_file() { mkdir -p "$1"; echo "$3" > "$1/$2"; }
  mk_file . index.html "ROOT INDEX"
  mk_file . 404.html "404 PAGE"
  mk_file sub index.html "nested index"
  mk_file sub noext "noext in sub"
  mk_file sub2 foo.html "sub2 content"
  mk_file a/b/c index.html "deep nested"
  mk_file a/b deep.txt "deep file"
  mk_file .well-known/acme-challenge token1 "token1"
  mk_file .well-known foo "wellk"
  mk_file .well-known robots.txt "robots"
  mk_file foo/.hidden data.txt "hidden"
  mk_file .git config "gitcfg"
  mk_file diridx index.html "diridx"
  mk_file empty .dirfile "emptydf"
  echo "noext root" > noext_root
}

to_status() {
  local url="$1"
  curl -s -o /dev/null -w "%{http_code}" "http://localhost:${PORT}${url}"
}

# run_two <url> <expected> [name]
run_two() {
  local url="$1" expect="$2" name="${3:-$1}"
  local got
  got="$(to_status "$url")"
  if [[ "$got" == "$expect" ]]; then
    echo "PASS  ${name} -> ${got}"
  else
    echo "FAIL  ${name} -> ${got} (expected ${expect})"
    status=1
  fi
}

status=0

cd "$work"
mk_build_tree

"$BIN" . &
server_pid=$!

# start the server, wait until it is accepting connections
for _ in $(seq 1 50); do
  if curl -s -o /dev/null "http://localhost:${PORT}/"; then
    break
  fi
  sleep 0.1
done

echo "=== directories (with index.html) ==="
run_two "/" 200
run_two "/sub" 200
run_two "/sub/" 200
run_two "/a/b/c" 200
run_two "/a/b/c/" 200
run_two "/diridx" 200
run_two "/diridx/" 200

echo "=== directories without index.html ==="
run_two "/a" 404
run_two "/a/b" 404
run_two "/empty" 404
run_two "/empty/" 404

echo "=== extensionless files ==="
run_two "/noext_root" 200
run_two "/sub/noext" 200
run_two "/sub/noext/" 200

echo "=== regular files ==="
run_two "/sub2/foo.html" 200
run_two "/404.html" 200
run_two "/a/b/deep.txt" 200

echo "=== missing paths ==="
run_two "/nope" 404
run_two "/nope/" 404
run_two "/a/nope" 404
run_two "/a/b/nope/" 404
run_two "/sub2/nope.html" 404

echo "=== dotfile blocking ==="
run_two "/.git/config" 404
run_two "/foo/.hidden/data.txt" 404
run_two "/.well-known/acme-challenge/../.git/config" 404
run_two "/foo/%2Ehidden/data.txt" 404
run_two "/sub/../.git/config" 404

echo "=== .well-known allowed ==="
run_two "/.well-known/robots.txt" 200
run_two "/.well-known/" 404 # directory, no index
run_two "/.well-known/acme-challenge/token1" 200

echo "=== query strings ==="
run_two "/sub?x=1" 200
run_two "/sub2/foo.html?x=1" 200

echo "=== case sensitivity (MacOS: case-insensitive fs) ==="
run_two "/SUB" 200
run_two "/SUB2/FOO.HTML" 200

echo "=== trailing dot missing ==="
run_two "/noext_root." 404

echo "=== custom 404 file content-type ==="
got="$(curl -s -D - -o /dev/null "http://localhost:${PORT}/nope" | tr -d '\r' | awk 'tolower($1)=="content-type:" {print $2}')"
if [[ "$got" == *"text/html"* ]]; then
  echo "PASS  404 content-type -> ${got}"
else
  echo "FAIL  404 content-type -> ${got} (expected text/html)"
  status=1
fi

echo "=== concurrency (200 parallel on /sub) ==="
got="$(seq 1 200 | xargs -P 20 -I{} curl -s -o /dev/null -w "%{http_code}\n" "http://localhost:${PORT}/sub" | sort | uniq -c | tr '\n' ' ')"
echo "      results: ${got}"
if grep -q "200" <<< "${got}"; then
  echo "PASS  concurrency"
else
  echo "FAIL  concurrency"
  status=1
fi

echo
if [[ "${status}" == "0" ]]; then
  echo "All tests passed."
else
  echo "Some tests FAILED."
fi