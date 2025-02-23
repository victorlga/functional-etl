FROM ocaml/opam:latest

RUN sudo apt-get update && sudo apt-get install -y --no-install-recommends \
    m4 \
    pkg-config \
    libssl-dev \
    build-essential \
    curl \
    git

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
    ocamlformat && \
    dune init proj etl

COPY . /home/opam/workspace/
WORKDIR /home/opam/workspace/