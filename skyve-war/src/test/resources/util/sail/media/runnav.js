/* A run page's previous and next run buttons, and its place in its session: from ../runs-order.js
   (window.SAIL_RUNS, the runs in the list's order, which the buttons follow across sessions), which the harness
   rewrites with the list. A page of a run the list no longer has, or one without that file, shows no buttons.
   SailMediaHtml inlines it into the film strip and player. */
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
/* the run's place in its session, as the list's # column numbers it */
var pos = document.getElementById('runPos'), first = i, last = i;
while (first > 0 && R[first - 1].e === R[i].e) first--;
while (last + 1 < R.length && R[last + 1].e === R[i].e) last++;
if (pos) { pos.textContent = 'Run ' + (i - first + 1) + ' of ' + (last - first + 1); pos.hidden = false; }
})();
