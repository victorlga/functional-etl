FROM ocaml/opam:latest

RUN sudo apt-get update && sudo apt-get install -y --no-install-recommends \
    m4 \
    pkg-config \
    libssl-dev \
    build-essential \
    curl \
    git \
    libsqlite3-dev

USER opam

RUN opam switch 5.3 --yes && \
    eval $(opam env) && \
    opam install -y \
    dune \
    utop \
    merlin \
    ocaml-lsp-server \
    earlybird \
    odoc \
    ocamlformat \
    sqlite3.5.1.0 && \
    dune init proj etl

COPY . /home/opam/workspace/
WORKDIR /home/opam/workspace/