/* The list of runs' filters: only failed runs, or the runs whose text (or file or workflow heading) has what is
   typed; a heading, or a session, with none of its runs shown is hidden too. SailMediaHtml inlines it into index.html. */
(function () {
var failed = document.getElementById('failedOnly'), find = document.getElementById('find');
function all(selector, root) { return Array.prototype.slice.call((root || document).querySelectorAll(selector)); }
var rows = all('tr[data-run]'), headers = all('tr[data-header]'), sessions = all('section.session'), count = document.getElementById('shown');
var headerText = {};
headers.forEach(function (h) { headerText[h.dataset.header] = h.textContent.toLowerCase(); });
function apply() {
	var text = find.value.trim().toLowerCase(), n = 0, shown = {};
	rows.forEach(function (r) {
		var haystack = r.textContent.toLowerCase() + ' ' + (headerText[r.dataset.g] || '') + ' ' + (headerText[r.dataset.u] || '');
		r.hidden = (failed.checked && r.dataset.failed !== 'true') || (text !== '' && haystack.indexOf(text) < 0);
		if (!r.hidden) { n++; if (r.dataset.g) shown[r.dataset.g] = true; if (r.dataset.u) shown[r.dataset.u] = true; }
	});
	headers.forEach(function (h) { h.hidden = !shown[h.dataset.header]; });
	sessions.forEach(function (s) { s.hidden = all('tr[data-run]', s).every(function (r) { return r.hidden; }); });
	count.textContent = (n === rows.length) ? '' : n + ' shown';
}
failed.onchange = apply; find.oninput = apply;
})();
