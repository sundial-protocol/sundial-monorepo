FROM node:22 AS base
RUN corepack enable

FROM base AS build
WORKDIR /workspace
COPY package.json pnpm-workspace.yaml pnpm-lock.yaml .npmrc ./
COPY midgard-sdk/package.json ./midgard-sdk/package.json
COPY midgard-node/package.json ./midgard-node/package.json
COPY midgard-ts/package.json ./midgard-ts/package.json
ENV CI=true
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store \
  pnpm install --frozen-lockfile
COPY midgard-sdk ./midgard-sdk/
COPY midgard-node ./midgard-node/
COPY midgard-ts ./midgard-ts/
RUN pnpm --filter @al-ft/midgard-sdk run build
RUN pnpm --filter sundial-node run build
RUN pnpm --filter sundial-node deploy --prod --legacy /deploy

FROM base AS final
RUN mkdir -p /app && chown node:node /app
WORKDIR /app
COPY --chown=node:node --from=build /deploy .
COPY --chown=node:node --from=build /workspace/midgard-node/dist ./dist
ENV NODE_ENV=production
# Build provenance surfaced as the midgard_node_build_info metric. Pass with
# `docker build --build-arg MIDGARD_NODE_COMMIT=$(git rev-parse --short HEAD)`.
ARG MIDGARD_NODE_COMMIT=unknown
ENV MIDGARD_NODE_COMMIT=${MIDGARD_NODE_COMMIT}
USER node
CMD ["node", "./dist/index.cjs", "listen"]
