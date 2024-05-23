import { fileURLToPath, URL } from 'node:url'

import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'

// https://vitejs.dev/config/
export default defineConfig({
    plugins: [vue()],
    build: {
        rollupOptions: {
            output: {
                entryFileNames: 'assets/index.js',
                assetFileNames: 'assets/[name].[ext]'
            }
        },
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
            '/smartsnap': {
                target: 'http://127.0.0.1:8080/skyve/',
                changeOrigin: true
            },
            '/content': {
                target: 'http://127.0.0.1:8080/skyve/',
                changeOrigin: true
            },
        }
    },
    define: {
        __VUE_PROD_HYDRATION_MISMATCH_DETAILS__: false
    }
})
