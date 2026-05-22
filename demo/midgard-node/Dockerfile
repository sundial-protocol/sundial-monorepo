FROM node:22 AS base
RUN corepack enable

FROM base AS build
WORKDIR /workspace
COPY package.json pnpm-workspace.yaml pnpm-lock.yaml ./
COPY midgard-sdk ./midgard-sdk/
COPY midgard-node ./midgard-node/
COPY midgard-ts ./midgard-ts/
ENV CI=true
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store \
  pnpm install --frozen-lockfile
RUN pnpm --filter @al-ft/midgard-sdk run build
RUN pnpm --filter sundial-node run build
RUN pnpm --filter sundial-node deploy --prod --legacy /deploy

FROM base AS final
RUN mkdir -p /app && chown node:node /app
WORKDIR /app
COPY --chown=node:node --from=build /deploy .
COPY --chown=node:node --from=build /workspace/midgard-node/dist ./dist
ENV NODE_ENV=production
USER node
CMD ["node", "./dist/index.js", "listen"]
