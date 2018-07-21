# hastatic

_hastatic_ is a tiny static web server for Docker.

## Features

- A tiny web server, just 3MB in size.
- Statically compiled binary with no dependencies.
- Built for Docker.
- Supports custom 404 file.
- Supports custom index files for URLs ending with "/".
- Adds caching headers automatically.
- Does not support HTTPS. It is expected that you run it behind a reverse-proxy server with HTTPS support, like nginx.

## Usage

Create a Dockerfile for your website, deriving from `abhin4v/hastatic`:

```dockerfile
FROM abhin4v/hastatic:latest

COPY mywebsite /opt/mywebsite
WORKDIR /opt/mywebsite
CMD ["/usr/bin/hastatic"]
```

Build and run:

```
$ docker build -t mywebsite .
$ docker run -p 8080:3000 mywebsite # run with default configs
$ docker run -e PORT=2000 -e NF_FILE=404.html -e IDX_FILE=index.html -p 8080:2000 mywebsite
```

## Configuration

The Docker image supports these environment variable for configuration:

- PORT: the port to run the web server on, default: 3000
- NF_FILE: name of the custom 404 file, default: `404.html`
- IDX_FILE: name of the custom index files, default: `index.html`

## Internals

_hastatic_ is written in Haskell, just 30 lines of it. It uses the excellent [Warp](https://hackage.haskell.org/package/warp) server underneath.
