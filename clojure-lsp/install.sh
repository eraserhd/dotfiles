#!/bin/bash

release="20180913T061535"

set -eo pipefail

curl -L https://github.com/snoe/clojure-lsp/releases/download/release-${release}/clojure-lsp >/usr/local/bin/clojure-lsp
chmod +x /usr/local/bin/clojure-lsp
