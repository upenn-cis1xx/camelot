FROM alpine:3.20

RUN apk add --no-cache \
    bash \
    sudo \
    build-base \
    opam \
    m4 \
    pkgconfig \
    openssl-dev \
    libev-dev \
    linux-headers \
    git \
    curl

ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=1000

RUN addgroup -g $USER_GID $USERNAME \
    && adduser -u $USER_UID -G $USERNAME -D -s /bin/bash $USERNAME \
    && echo "$USERNAME ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

USER $USERNAME
ENV HOME=/home/$USERNAME

RUN opam init --disable-sandboxing --shell-setup -y \
    && opam switch create 5.4.1 ocaml-base-compiler.5.4.1 \
    && eval $(opam env)

WORKDIR /workspaces/app
COPY --chown=$USERNAME:$USERNAME *.opam dune-project* ./

# Install project deps and tools
# TODO: this might not work properly
RUN opam exec -- opam install . --deps-only -y \
    && opam exec -- opam install ocaml-lsp-server dune utop -y

RUN echo 'eval $(opam env)' >> ~/.bashrc