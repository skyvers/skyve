// Caches both pages as it installs and answers them from the cache, so that a page can load while the browser is
// offline: the document that must see navigator.onLine === false.
self.addEventListener('install', (e) => {
	self.skipWaiting();
	e.waitUntil(caches.open('screencast').then((c) => c.addAll(['/screencast/a.html', '/screencast/b.html'])));
});
self.addEventListener('activate', (e) => e.waitUntil(self.clients.claim()));
self.addEventListener('fetch', (e) => e.respondWith(caches.match(e.request).then((r) => r || fetch(e.request))));
