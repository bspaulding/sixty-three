import { defineConfig } from "vite";
import elm from "vite-plugin-elm";

export default defineConfig({
  plugins: [elm()],
  // The Haskell backend serves the production bundle from frontend/build.
  build: { outDir: "build" },
  // src/index.js connects to the backend websocket on :3000 when served from :3001.
  server: { port: 3001, strictPort: true }
});
