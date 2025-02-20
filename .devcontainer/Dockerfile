FROM ocaml/opam:latest

# Install system dependencies
RUN sudo apt-get update && sudo apt-get install -y --no-install-recommends \
    m4 \
    pkg-config \
    libssl-dev \
    build-essential \
    curl \
    git

USER opam

RUN opam switch create 4.14.0 --yes && eval $(opam env)
RUN opam install -y dune utop merlin ocaml-lsp-server earlybird

COPY . /home/opam/
WORKDIR /home/opam/workspace