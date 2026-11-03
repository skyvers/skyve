import { fileURLToPath, URL } from 'node:url'

import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'

// https://vitejs.dev/config/
export default defineConfig({
    plugins: [vue()],
    build: {
        // skyve.js is loaded by Faces as a classic <script>, not a module.
        // Wrap it in an IIFE so its minified top-level names (eg L, $) stay private
        // and other page scripts can't overwrite them (or vice versa).
        modulePreload: { polyfill: false },
        // Keep the CSS in its own skyve.css (an IIFE build would otherwise inline it into skyve.js)
        cssCodeSplit: false,
        rollupOptions: {
            output: {
                format: 'iife',
                entryFileNames: 'assets/skyve.js',
                // the single CSS file comes out as style.css; build.xml expects skyve.css
                assetFileNames: (assetInfo) => {
                    const name = assetInfo.names?.[0] ?? assetInfo.name;
                    return (name === 'style.css') ? 'assets/skyve.css' : 'assets/[name].[ext]';
                }
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
