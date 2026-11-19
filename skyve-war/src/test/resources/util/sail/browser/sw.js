// Passes /sw/* requests to the network with its own fetch, so they show what reaches a service worker, and answers
// /sw/whoami itself with the time it started, to show when Chrome starts it again. Anything else goes to the network
// from the page, as the worker doesn't answer it.
const started = Date.now() + '-' + Math.random();
self.addEventListener('install', () => self.skipWaiting());
self.addEventListener('activate', (e) => e.waitUntil(self.clients.claim()));
self.addEventListener('fetch', (e) => {
	const path = new URL(e.request.url).pathname;
	if (path === '/sw/whoami') {
		e.respondWith(new Response(started));
	}
	else if (path.startsWith('/sw/')) {
		e.respondWith(fetch(e.request));
	}
});
