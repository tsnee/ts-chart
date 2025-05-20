import { defineConfig } from 'vite'
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
    emptyOutDir: true
  },
  optimizeDeps: {
    // This tells Vite not to try to pre-bundle PureScript output modules
    exclude: ['output']
  }
})
