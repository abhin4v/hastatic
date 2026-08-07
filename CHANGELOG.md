# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Paths under `.well-known/` are now served, so ACME challenges and other
  well-known resources work without a custom config.

### Changed

- The container now defaults to a `WORKDIR` of `/var/www` and uses
  `ENTRYPOINT ["/usr/bin/hastatic"]`, so it can run without a custom
  Dockerfile by mounting content at `/var/www`.
- The default server port changed from `3000` to `8080`. The port is still
  configurable via the `PORT` environment variable.
- The `Content-Type` of the 404 page is now derived from the `NF_FILE`
  extension (e.g. `404.txt` serves as `text/plain`), matching the type the
  file would get when served directly.
- Filesystem type lookups (directory vs. file vs. missing) are now cached in
  memory after the first request for a path, so repeated requests do not
  re-stat the filesystem. This is safe for the bundled-content use case.

### Fixed

- Missing paths now actually return `HTTP 404`. Previously Warp's file
  serving path ignored the requested `404` status and served the 404 body
  with a `200` status.
- Extensionless files (e.g. `/test`) are served directly instead of being
  rewritten to `index.html`, so they no longer return 404.
- Trailing slashes (e.g. `/sub/`) now resolve to `sub/index.html` instead of
  the doubled `sub//index.html` path.

### Security

- Dotfiles and dot-directories anywhere in a path (e.g. `foo/.git/...`,
  `.env`) are now blocked, closing a previous disclosure where they could
  be served from subdirectories. Only the top-level `/.well-known/` prefix
  remains accessible.

## [0.9.0] - 2026-08-07

- First release.
