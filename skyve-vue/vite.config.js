import { fileURLToPath, URL } from 'node:url'

import { resolve } from 'path'
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
        }
    }
})
