/*
	Highlights the active item in the editorial left menu by matching each menu link's
	module/query/document parameters against the current location. List views link with
	?a=l&m=<module>&q=<query> and edit views open as ?a=e&m=<module>&d=<document>, so a
	document edit also highlights the list menu item it was opened from (default queries
	share the document name).

	Menu items are not necessarily unique by target - e.g. a list and a tree over the same
	query render identical URLs - so the last clicked menu item is remembered (per tab) and
	used as a tiebreaker, and only ever one item is highlighted.
*/
(function() {
	'use strict';
	var STORAGE_KEY = 'skyve.editorial.menu.clicked';

	var linkQuery = function(link) {
		var href = link.getAttribute('href') || '';
		var queryIndex = href.indexOf('?');
		if (queryIndex < 0) {
			return null;
		}
		// menu links can be javascript: wrapped - location('?a=l&m=...&q=...') - so
		// trim anything from the first quote or closing bracket
		return href.substring(queryIndex + 1).replace(/['")].*$/, '');
	};

	var linkIdentity = function(link) {
		return link.textContent.trim() + '|' + (linkQuery(link) || '');
	};

	var initialise = function() {
		var links = document.querySelectorAll('#leftMenu a.ui-menuitem-link');

		// remember the last clicked menu item to disambiguate menu items sharing a target
		links.forEach(function(link) {
			link.addEventListener('click', function() {
				try {
					sessionStorage.setItem(STORAGE_KEY, linkIdentity(link));
				}
				catch (e) {
					// storage unavailable - fall back to first-match highlighting
				}
			});
		});

		var current = new URLSearchParams(window.location.search);
		var m = current.get('m');
		if (! m) {
			return;
		}
		var q = current.get('q');
		var d = current.get('d');

		// score 2 - direct target match; score 1 - edit view of a list's document (or vice versa)
		var candidates = [];
		links.forEach(function(link) {
			var query = linkQuery(link);
			if (! query) {
				return;
			}
			var linkParams = new URLSearchParams(query);
			if (linkParams.get('m') !== m) {
				return;
			}
			var lq = linkParams.get('q');
			var ld = linkParams.get('d');
			var score = 0;
			if (((q !== null) && (lq !== null) && (lq === q)) ||
					((d !== null) && (ld !== null) && (ld === d))) {
				score = 2;
			}
			else if (((q !== null) && (ld !== null) && (ld === q)) ||
						((d !== null) && (lq !== null) && (lq === d))) {
				score = 1;
			}
			if (score > 0) {
				candidates.push({link: link, score: score});
			}
		});
		if (candidates.length === 0) {
			return;
		}

		var bestScore = candidates.reduce(function(max, c) {
			return Math.max(max, c.score);
		}, 0);
		candidates = candidates.filter(function(c) {
			return c.score === bestScore;
		});

		var chosen = candidates[0].link;
		if (candidates.length > 1) {
			var remembered = null;
			try {
				remembered = sessionStorage.getItem(STORAGE_KEY);
			}
			catch (e) {
				// storage unavailable
			}
			if (remembered !== null) {
				candidates.some(function(c) {
					if (linkIdentity(c.link) === remembered) {
						chosen = c.link;
						return true;
					}
					return false;
				});
			}
		}

		var item = chosen.closest('.ui-menuitem');
		if (item) {
			item.classList.add('skyve-menu-active');
		}
	};

	if (document.readyState === 'loading') {
		document.addEventListener('DOMContentLoaded', initialise);
	}
	else {
		initialise();
	}
})();
