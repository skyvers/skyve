/* The video player: plays video.js's frames (window.SAIL_VIDEO) at their real timing, captioned with the step,
   with the steps by interaction to jump to. SailMediaHtml inlines it into player.html. */
(function () {
var V = window.SAIL_VIDEO, F = V.frames, S = V.steps, i = 0, playing = false, timer = null, pin = -1, shown = -2;
function $(id) { return document.getElementById(id); }
function el(tag, cls, text) { var e = document.createElement(tag); if (cls) e.className = cls; if (text != null) e.textContent = text; return e; }
function secs(ms) { return (ms / 1000).toFixed(1) + ' s'; }
function dur(ms) { return ms < 1000 ? ms + ' ms' : secs(ms); }
var img = $('frame'), cap = $('caption'), seek = $('seek'), play = $('play'), speed = $('speed'), clock = $('clock'), list = $('steps');
var t0 = F.length ? F[0][1] : 0, end = F.length ? F[F.length - 1][1] - t0 : 0, items = [], group = null, ol = null;
seek.max = Math.max(F.length - 1, 0);
S.forEach(function (s, k) {
	if (s.p !== group) {
		group = s.p; var section = el('section'); section.append(el('h3', null, s.p));
		if (s.pd) { var desc = el('div', 'desc'); s.pd.split('\n\n').forEach(function (p) { desc.append(el('p', null, p)); }); section.append(desc); }
		ol = el('ol'); section.append(ol); list.append(section);
	}
	var li = el('li', s.f ? 'fail' : null); li.value = s.n; li.title = s.x;
	li.append(el('div', 'summary', s.d || s.x));
	if (s.f) { li.append(el('div', 'failure', s.f)); }
	li.onclick = function () { jump(k); };
	ol.append(li); items.push(li);
});
/* the last frame drawn at or before t */
function frameAt(t) { var lo = 0, hi = F.length - 1; if (hi < 0) return 0; while (lo < hi) { var m = (lo + hi + 1) >> 1; if (F[m][1] <= t) lo = m; else hi = m - 1; } return lo; }
function stepAt(t) { var r = -1; for (var k = 0; k < S.length; k++) { if (S[k].s <= t) r = k; else break; } return r; }
function caption(k) {
	var cur = list.querySelector('li.current'); if (cur) cur.classList.remove('current');
	cap.className = 'card caption';
	if (k < 0) { cap.replaceChildren(el('div', 'cap-meta', 'Before the first step')); return; }
	var s = S[k], li = items[k]; li.classList.add('current'); li.scrollIntoView({block: 'nearest'});
	if (s.f) cap.classList.add('fail');
	var meta = el('div', 'cap-meta');
	meta.append(el('span', s.f ? 'badge fail' : 'badge pass', s.f ? 'Failed' : 'Passed'), el('span', null, 'Step ' + s.n + ' of ' + S.length + ' \u00b7 ' + dur(s.e - s.s)));
	var parts = [meta, el('div', 'cap-group', s.p), el('div', 'cap-summary', s.d || s.x)];
	if (s.f) { parts.push(el('div', 'failure', s.f)); }
	parts.push(el('code', 'xml', s.x));
	cap.replaceChildren.apply(cap, parts);
}
function show() {
	if (!F.length) { cap.replaceChildren(el('div', 'cap-meta', 'No frames were recorded.')); return; }
	var t = F[i][1]; img.src = F[i][0]; seek.value = i; clock.textContent = secs(t - t0) + ' / ' + secs(end);
	if (i + 1 < F.length) { new Image().src = F[i + 1][0]; }
	var k = pin >= 0 ? pin : stepAt(t);
	if (k !== shown) { shown = k; caption(k); }
}
function go(k) { i = Math.max(0, Math.min(F.length - 1, k)); show(); if (playing) schedule(); }
/* shows a step as it finished: the last frame drawn by its end (the screen as it was during it when nothing was
   drawn then), as the film strip's screenshot shows it; Play goes on from there (from the start, at the video's end) */
function jump(k) {
	pin = k; go(frameAt(S[k].e));
}
function schedule() {
	clearTimeout(timer);
	if (i + 1 >= F.length) { pause(); return; }
	timer = setTimeout(function () { i++; pin = -1; show(); schedule(); }, Math.max(0, (F[i+1][1]-F[i][1]) / parseFloat(speed.value)));
}
function pause() { playing = false; clearTimeout(timer); play.textContent = 'Play'; }
play.onclick = function () { if (playing) { pause(); } else { if (i + 1 >= F.length) { i = 0; pin = -1; } playing = true; play.textContent = 'Pause'; schedule(); } };
/* from before the first step (shown -1), or with no frames (-2), Step goes to the first step */
$('prev').onclick = function () { if (S.length) jump(Math.max(0, shown - 1)); };
$('next').onclick = function () { if (S.length) jump(Math.min(S.length - 1, Math.max(0, shown + 1))); };
speed.onchange = function () { if (playing) schedule(); };
seek.oninput = function () { pin = -1; go(parseInt(seek.value, 10)); };
document.addEventListener('keydown', function (e) {
	var tag = e.target.tagName;
	if (tag === 'INPUT' || tag === 'SELECT' || tag === 'TEXTAREA' || e.metaKey || e.ctrlKey || e.altKey) return;
	if (e.key === ' ' && tag !== 'BUTTON' && tag !== 'SUMMARY') { e.preventDefault(); play.click(); }
	else if (e.key === 'ArrowLeft') { e.preventDefault(); $('prev').click(); }
	else if (e.key === 'ArrowRight') { e.preventDefault(); $('next').click(); }
});
show();
})();
