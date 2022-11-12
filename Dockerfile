# Build backend
FROM fpco/stack-build:lts-16.20 as build-hs
RUN mkdir /opt/build
COPY backend /opt/build
RUN mkdir -p ~/.stack
RUN touch ~/.stack/config.yaml
RUN echo 'allow-newer: true' > ~/.stack/config.yaml
RUN cd /opt/build && stack install --system-ghc

# Build frontend
FROM node:16 as build-elm
WORKDIR /app
COPY frontend/package.json package.json
COPY frontend/package-lock.json package-lock.json
RUN npm ci
COPY frontend/src src
COPY frontend/public public
COPY frontend/elm-stuff elm-stuff
COPY frontend/elm.json elm.json
RUN npm run elm-app -- build

# Runner
FROM ubuntu:18.04
ENV PORT 80
EXPOSE $PORT
RUN mkdir -p /opt/app
WORKDIR /opt/app
RUN apt-get update && apt-get install -y ca-certificates libgmp-dev
COPY --from=build-hs /root/.local/bin .
COPY --from=build-elm /app/build ./frontend/build
CMD ["/opt/app/sixty-three-exe"]

