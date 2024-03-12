import {fileURLToPath, URL} from 'node:url'

import {resolve} from 'path'
import {defineConfig} from 'vite'
import vue from '@vitejs/plugin-vue'

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [vue()],
/* Trying to use in library mode left some nodejs environment variable testing against undefined so I gave up.
  build: {
    lib: {
      // Could also be a dictionary or array of multiple entry points
      entry: resolve(__dirname, 'src/main.js'),
      name: 'skyve-vue',
      // the proper extensions will be added
      fileName: 'skyve-vue'
    }
  },
*/
  rollupOptions: {
    // make sure to externalize deps that shouldn't be bundled
    // into your library
    external: ['vue'],
    output: {
      // Provide global variables to use in the UMD build
      // for externalized deps
      globals: {
        vue: 'Vue',
      }
    }
  },
  resolve: {
    alias: {
      '@': fileURLToPath(new URL('./src', import.meta.url))
    }
  },
  base: './',
  baseAssets: './',
  server: {
    proxy: {
      '/smartlist': {
        target: 'http://127.0.0.1:8080/skyve/',
        changeOrigin: true
      },
    }
  }
})
