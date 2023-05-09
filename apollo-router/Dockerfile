FROM --platform=linux/amd64 registry.access.redhat.com/ubi7/ubi


RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

COPY . .

RUN source "$HOME/.cargo/env" && \
    cargo build --release
