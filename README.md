# hastatic

[![Docker Build Status](https://img.shields.io/docker/build/abhin4v/hastatic.svg?style=flat-square)](https://hub.docker.com/r/abhin4v/hastatic/) [![Docker Pulls](https://img.shields.io/docker/pulls/abhin4v/hastatic.svg?style=flat-square)](https://hub.docker.com/r/abhin4v/hastatic/) [![MicroBadger Size](https://img.shields.io/microbadger/image-size/abhin4v/hastatic.svg?style=flat-square)](https://hub.docker.com/r/abhin4v/hastatic/)

_hastatic_ is a very small web server for serving static files from a Docker container.

## Features

- A lightweight web server, just 5 MB in size.
- Statically compiled binary with no dependencies.
- [Built for Docker](https://hub.docker.com/r/abhin4v/hastatic/).
- Supports HTTPS.
- Supports custom 404 file.
- Supports custom index files for URLs ending with "/".
- Takes care to not serve hidden files.
- Adds caching headers automatically.
- Adds security headers automatically.
- Caches file descriptors and info for better performance.

## Usage

Create a Dockerfile for your website, deriving from `abhin4v/hastatic`:

```dockerfile
FROM abhin4v/hastatic:latest

COPY mywebsite /opt/mywebsite
WORKDIR /opt/mywebsite
CMD ["/usr/bin/hastatic"]
```

Build and run:

```bash
$ docker build -t mywebsite .
$ # run with default configs
$ docker run -p 8080:3000 mywebsite
$ # run with custom configs
$ docker run -e PORT=2000 -e NF_FILE=404.html -e IDX_FILE=index.html -p 8080:2000 mywebsite
$ # run with HTTPS support
$ docker run -e TLS_CERT_FILE=certificate.pem -e TLS_KEY_FILE=key.pem -p 443:3000 mywebsite
```

## Configuration

The Docker image supports these environment variable for configuration:

- PORT: the port to run the web server on, default: 3000
- NF_FILE: name of the custom 404 file, default: `404.html`
- IDX_FILE: name of the custom index files, default: `index.html`
- TLS_CERT_FILE: TLS certification file, optional, required for HTTPS support
- TLS_KEY_FILE: TLS key file, optional, required for HTTPS support

## Internals

_hastatic_ is written in Haskell, just 60 lines of it. It uses the excellent [Warp](https://hackage.haskell.org/package/warp) server underneath with the [warp-tls](https://hackage.haskell.org/package/warp-tls) package for HTTPS support.
