import { defineConfig } from 'vite'
import elm from 'vite-plugin-elm-watch'

export default defineConfig({
    plugins: [elm({ mode: "optimize" })],
    base: "/sepsis-watch-web-ui-prototype"
})