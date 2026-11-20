/* A run page's previous and next run buttons, and its place in the list of runs: from ../runs-order.js
   (window.SAIL_RUNS, the runs in the list's order), which the harness rewrites with the list. A page of a run the list
   no longer has, or one without that file, shows no buttons. SailMediaHtml inlines it into the film strip and player. */
(function () {
var R = window.SAIL_RUNS, nav = document.querySelector('nav[data-run]');
if (!R || !nav) return;
var me = nav.dataset.run, video = nav.dataset.page === 'video', i = -1;
for (var k = 0; k < R.length; k++) { if (R[k].n === me) { i = k; break; } }
if (i < 0) return;
/* the neighbour's page of the same kind (steps or video) when it has one, else its other page */
function link(id, k, label) {
	var a = document.getElementById(id), r = R[k];
	if (!a || !r) return;
	var file = (video ? r.v : !r.s) ? 'player.html' : 'index.html';
	a.href = '../' + encodeURIComponent(r.n) + '/' + file;
	a.title = label + ': ' + r.t + (r.f ? ' (failed)' : '');
	a.hidden = false;
}
link('prevRun', i - 1, 'Previous run');
link('nextRun', i + 1, 'Next run');
var pos = document.getElementById('runPos');
if (pos) pos.textContent = 'Run ' + (i + 1) + ' of ' + R.length;
})();
