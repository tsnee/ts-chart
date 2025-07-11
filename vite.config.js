import { defineConfig } from 'vite'
import { cloudflare } from "@cloudflare/vite-plugin"
import path from 'path'

export default defineConfig({
  root: '.',                 // project root
  publicDir: 'public',       // optional: static assets
  server: {
    port: 3000,
    open: true               // open browser on start
  },
  resolve: {
    alias: {
      // optional: if you want clean imports for src files
      '@src': path.resolve(__dirname, './src')
    }
  },
  build: {
    outDir: 'dist',          // output folder for production build
    emptyOutDir: true,
    rollupOptions: {
      input: {
        area11: path.resolve(__dirname, 'area11.html'),
        area12: path.resolve(__dirname, 'area12.html'),
        area13: path.resolve(__dirname, 'area13.html'),
        area14: path.resolve(__dirname, 'area14.html')
      }
    }
  },
  optimizeDeps: {
    // This tells Vite not to try to pre-bundle PureScript output modules
    exclude: ['output']
  },
  plugins: [cloudflare()]
})
