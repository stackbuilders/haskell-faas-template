# Compiling function dependencies
FROM ghcr.io/stackbuilders/openfaas-haskell-container:v1.0 as dependencies
COPY function/haskell-template-function.cabal /app/
WORKDIR /app
RUN cabal build --dependencies-only --enable-tests

# Copy compiled deps to coniner
FROM ghcr.io/stackbuilders/openfaas-haskell-container:v1.0 as builder
COPY cabal.project haskell-template.cabal /app/
COPY function /app/function
COPY app /app/app
COPY src /app/src

COPY --from=dependencies /app/dist-newstyle /app/dist-newstyle
COPY --from=dependencies /root/.cabal /root/.cabal

WORKDIR /app

ARG TEST_ENABLED=true
RUN if [ "$TEST_ENABLED" = "true" ]; then \
    cabal run spec;\
    else \
    echo 'Skipped tests'; \
    fi

RUN rm -f ~/.ssh/id_rsa ~/.ssh/id_rsa.pub

RUN mkdir /app/bin && mv $(cabal list-bin haskell-template-server) /app/bin/

# ------------------------------------------------------------------------------
FROM openfaas/of-watchdog:0.8.2 as watchdog

FROM ubuntu:20.04
WORKDIR /app
COPY --from=builder /app/bin/haskell-template-server .

COPY --from=watchdog /fwatchdog /usr/bin/fwatchdog
RUN chmod +x /usr/bin/fwatchdog

RUN addgroup --system app \
    && adduser --system --ingroup app app
RUN chown app /app

USER app

ENV fprocess="./haskell-template-server"
ENV cgi_headers="true"
ENV mode="http"
ENV upstream_url="http://127.0.0.1:5000"
EXPOSE 8080

HEALTHCHECK --interval=5s CMD [ -e /tmp/.lock ] || exit 1

CMD [ "fwatchdog" ]
