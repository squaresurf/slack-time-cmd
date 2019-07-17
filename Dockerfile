FROM haskell:8.6.5 as builder

RUN mkdir -p /app/user
WORKDIR /app/user
COPY stack.yaml *.cabal ./

RUN export PATH=$(stack path --local-bin):$PATH
RUN stack build --dependencies-only

COPY . /app/user
RUN stack install

FROM haskell:8.6.5

COPY --from=builder /root/.local/bin/slack-time-cmd /usr/local/bin/slack-time-cmd
CMD slack-time-cmd
