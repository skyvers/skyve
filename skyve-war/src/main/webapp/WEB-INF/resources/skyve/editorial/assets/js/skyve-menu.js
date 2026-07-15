/*
	Highlights the active item in the editorial left menu by matching each menu link's
	module/query/document parameters against the current location. List views link with
	?a=l&m=<module>&q=<query> and edit views open as ?a=e&m=<module>&d=<document>, so a
	document edit also highlights the list menu item it was opened from (default queries
	share the document name).

	Menu items are not necessarily unique by target - e.g. a list and a tree over the same
	query render identical URLs - so the last clicked menu item is remembered (per tab) and
	used as a tiebreaker, and only ever one item is highlighted. Menu selections also collapse
	inactive modules and groups through the PrimeFaces widget so its persisted state stays in
	sync with the menu shown to the user.
*/
(function() {
	'use strict';
	var STORAGE_KEY = 'skyve.editorial.menu.clicked';
	// Match PrimeFaces' animated root-panel transition
	var MENU_ANIMATION_DURATION = 'normal';
	var MENU_ANIMATION_EASING = 'easeInOutCirc';
	var INITIALISE_RETRY_INTERVAL = 50;
	var INITIALISE_RETRY_LIMIT = 100;
	var initialiseAttempts = 0;
	var initialised = false;

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

	var isExpanded = function(widget, item) {
		return widget.expandedNodes.indexOf(item.attr('id')) >= 0;
	};

	var animateTreeItem = function(item, expanded) {
		var children = item.children('.ui-menu-list');
		children.stop(true, true);
		if (expanded) {
			children.hide().slideDown(MENU_ANIMATION_DURATION, MENU_ANIMATION_EASING);
		}
		else {
			children.show().slideUp(MENU_ANIMATION_DURATION, MENU_ANIMATION_EASING);
		}
	};

	var collapseTreeItems = function(widget, items, animate) {
		items.get().reverse().forEach(function(element) {
			var item = $(element);
			if (isExpanded(widget, item)) {
				var children = item.children('.ui-menu-list');
				children.stop(true, true);
				widget.collapseTreeItem(item);
				if (animate) {
					children.show().slideUp(MENU_ANIMATION_DURATION, MENU_ANIMATION_EASING);
				}
			}
		});
	};

	var collapseTreeBranches = function(widget, branches, animate) {
		branches.each(function() {
			var branch = $(this);
			collapseTreeItems(widget, branch.find('.ui-menu-parent').addBack('.ui-menu-parent'), animate);
		});
	};

	var collapseRootPanel = function(widget, panel) {
		var header = panel.children('.ui-panelmenu-header');
		var content = panel.children('.ui-panelmenu-content');
		var collapseNestedGroups = function() {
			collapseTreeItems(widget, panel.find('.ui-menu-parent'));
		};

		if (header.hasClass('ui-state-active') || isExpanded(widget, content)) {
			widget.collapseRootSubmenu(header);
			content.promise().done(collapseNestedGroups);
		}
		else if (content.is(':animated')) {
			// multiple=false has already started closing this module. Let its slide finish
			// before hiding nested groups so the content height does not jump mid-animation.
			content.promise().done(collapseNestedGroups);
		}
		else {
			collapseNestedGroups();
		}
	};

	var collapseInactiveRootPanels = function(widget, activePanel) {
		widget.jq.children('.ui-panelmenu-panel').each(function() {
			if (this !== activePanel) {
				collapseRootPanel(widget, $(this));
			}
		});
	};

	var collapseInactiveForSelection = function(widget, link) {
		var item = $(link).closest('.ui-menuitem');
		var activeGroups = item.parents('.ui-menu-parent').get();
		var activePanel = item.closest('.ui-panelmenu-panel')[0];

		collapseInactiveRootPanels(widget, activePanel);
		widget.jq.find('.ui-menu-parent').each(function() {
			if (activeGroups.indexOf(this) < 0) {
				collapseTreeItems(widget, $(this));
			}
		});
	};

	var makeMenuExclusive = function(widget) {
		widget.headers.on('click.skyveEditorialMenu', function() {
			var header = $(this);
			var panel = header.closest('.ui-panelmenu-panel');
			var activePanel = header.hasClass('ui-state-active') ? panel[0] : null;
			collapseInactiveRootPanels(widget, activePanel);
		});

		widget.treeLinks.on('click.skyveEditorialMenu', function() {
			var item = $(this).parent('.ui-menu-parent');
			var panel = item.closest('.ui-panelmenu-panel');
			collapseInactiveRootPanels(widget, panel[0]);

			if (isExpanded(widget, item)) {
				animateTreeItem(item, true);
				collapseTreeBranches(widget, item.siblings('.ui-menu-parent'), true);
			}
			else {
				animateTreeItem(item, false);
				collapseTreeItems(widget, item.find('.ui-menu-parent'));
			}
		});
	};

	var initialise = function() {
		if (initialised) {
			return;
		}

		// PrimeFaces creates widgets from a document-ready callback. This script can receive
		// DOMContentLoaded first, so wait until the panel menu has actually been registered.
		var widget = window.PrimeFaces && PrimeFaces.widgets && PrimeFaces.widgets.leftMenu;
		if (! widget) {
			initialiseAttempts++;
			if (initialiseAttempts < INITIALISE_RETRY_LIMIT) {
				window.setTimeout(initialise, INITIALISE_RETRY_INTERVAL);
			}
			return;
		}
		initialised = true;
		makeMenuExclusive(widget);

		var links = document.querySelectorAll('#leftMenu a.ui-menuitem-link');

		// Remember the last clicked leaf to disambiguate menu items sharing a target, then
		// retain only the expanded branch leading to that selection.
		widget.menuitemLinks.not(widget.treeLinks).each(function() {
			var link = this;
			link.addEventListener('click', function() {
				try {
					sessionStorage.setItem(STORAGE_KEY, linkIdentity(link));
				}
				catch (e) {
					// storage unavailable - fall back to first-match highlighting
				}
				collapseInactiveForSelection(widget, link);
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
			collapseInactiveForSelection(widget, chosen);
		}
	};

	if (document.readyState === 'loading') {
		document.addEventListener('DOMContentLoaded', initialise);
	}
	else {
		initialise();
	}
})();
