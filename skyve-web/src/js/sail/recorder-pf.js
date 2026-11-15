/*
 * SAIL recorder for PrimeFaces: the PrimeFaces adapter of the recorder core (recorder-core.js), with which
 * skyve-web/build.xml concatenates it into skyve/sail/recorder-pf-min.js. See docs/sail.md, "Recording".
 *
 * Only a session that is recording loads this script: each PrimeFaces theme's head-resources.xhtml adds it when the
 * page bean's isSailRecording() is true. PrimeFaces loads a page per view, so the recording and the panel come back
 * with each page: the page renders the recording's state (token, counts, last entries) in #sailRecorderState, and the
 * panel's own state is in sessionStorage.
 *
 * PrimeFaces client ids are generated, so the adapter names what the user acted on from the page's client-id map
 * (org.skyve.impl.sail.recording.PrimeFacesRecordingMap), rendered in #sailRecorderMap inside the form, so that an
 * ajax update of the form brings the map of what it now shows. Skyve's client code has no recorder checks in it; the
 * adapter attaches itself to the page when recording starts:
 * - DOM click listeners in the capture phase map a click to its step: a menu item (navigateMenu), a tab header
 *   (tabSelect), a mapped button (ok, save, cancel, delete, zoomOut, remove, action), a Vue list grid's New or View
 *   Detail (listGridNew, listGridZoom with rowContaining) and a PrimeFaces data table's New and zoom controls;
 * - data entry is read from the mapped fields: what changed since the page loaded or the last response becomes
 *   dataEnter steps (combos and radios by code) before the next step and before each ajax request (PrimeFaces'
 *   pfAjaxSend event), as the final value;
 * - PrimeFaces' pfAjaxComplete event marks a response: the adapter notes the messages, growls and field errors it
 *   showed and the values it changed, and reads the map again;
 * - a Vue list grid's filters and sort are read from the grid's state each time it fetches its rows; a PrimeFaces data
 *   table list grid's from its filter and sort requests (its column filters, and the sorted column's header, which the
 *   map's columns name): listGridFilter, listGridClearFilter and listGridSort;
 * - a click on the global confirm dialog's No declines the action that asked (confirm="false");
 * - a page load from a typed URL, a link or a reload is recorded as navigateEdit or navigateList; a load that a recorded
 *   gesture caused (a menu item, a list's New or zoom, OK) is not, as the gesture says it already.
 */
(function () {
	"use strict";

	window.SKYVE = window.SKYVE || {};
	if (SKYVE.SailRecorder) {
		return;
	}

	/** Marks, for the next page, that a recorded gesture is loading it: the load is not recorded again. */
	var NAVIGATING_KEY = "skyve.sail.recorder.navigating";
	/** The Control Panel's recording actions, which are not recorded. */
	var CONTROL_ACTIONS = { StartSAILRecording: true, StopSAILRecording: true, DownloadSAILRecording: true };
	var STOP_ACTION = "StopSAILRecording";
	/** Steps that close the view, so that a page load may follow. */
	var CLOSING_STEPS = { ok: true, cancel: true, delete: true, zoomOut: true, remove: true };
	/** Widget kinds whose value the adapter reads; any other bound widget's change is an unmapped comment for now. */
	var TEXT_KINDS = { textField: true, textArea: true, password: true, colourPicker: true, spinner: true, slider: true };
	var SHOWN_LENGTH = 60;
	/** The messages a page shows: growls, the global messages, and a field's message (an icon whose title says it). */
	var MESSAGES = ".ui-growl-item, .ui-messages li, .ui-message";
	/** The messages a response shows for the whole page. */
	var PAGE_MESSAGES = ".ui-growl-item, .ui-messages li";

	var core = null;
	/** The page's client-id map. */
	var map = { ids: {}, grids: {} };
	/** What each mapped field showed after the page loaded or the last response, by client id. */
	var shown = {};
	/** The step of a button that asked for confirmation, held until the user answers. */
	var confirming = null;
	/** Bindings whose widget the adapter cannot read yet, changed since the last flush. */
	var unreadable = [];

	// ---------------------------------------------------------------- the page

	function text(element) {
		return String((element && element.textContent) || "").replace(/\s+/g, " ").trim();
	}

	function shorten(value) {
		return (value.length > SHOWN_LENGTH) ? value.substring(0, SHOWN_LENGTH) + "..." : value;
	}

	function readJson(id, attribute) {
		var element = document.getElementById(id);
		var value = element ? element.getAttribute(attribute) : null;
		try {
			return value ? JSON.parse(value) : null;
		} catch (e) {
			return null;
		}
	}

	function readMap() {
		map = readJson("sailRecorderMap", "data-map") || {};
		map.ids = map.ids || {};
		map.grids = map.grids || {};
	}

	function uxui() {
		return map.uxui || "";
	}

	/** "module.Document" of an edit page, "list" for a list page, "" otherwise. */
	function describeView() {
		if (map.module) {
			return map.module + "." + map.document;
		}
		return map.list ? "list" : "";
	}

	function screen() {
		if (map.module) {
			return map.module + "." + map.document + ", edit view, PrimeFaces " + uxui();
		}
		if (map.list) {
			var l = map.list;
			return "list " + l.module + "." + (l.query || l.model || l.document) + ", PrimeFaces " + uxui();
		}
		return "";
	}

	/** The map's id for a page id: a data table's row segment (s60:3:s70) is left out of the map (s60:s70). */
	function mapKey(id) {
		return String(id).replace(/:\d+:/g, ":");
	}

	/** The mapped element an element is, or is inside: { id, entry, element, row } or null. */
	function mapped(element) {
		for (var e = element; e && (e !== document.body) && e.getAttribute; e = e.parentElement) {
			var id = e.id;
			if (id) {
				var key = mapKey(id);
				var entry = map.ids[key] || map.ids[key.replace(/_(input|focus|hinput|editor|label|panel|items|list)$/, "")];
				if (entry) {
					var row = /:(\d+):/.exec(id);
					return { id: id, entry: entry, element: e, row: row ? Number(row[1]) : -1 };
				}
			}
		}
		return null;
	}

	function disabled(element) {
		return !!element && (element.disabled || element.classList.contains("ui-state-disabled") ||
								(element.getAttribute("aria-disabled") === "true"));
	}

	// ---------------------------------------------------------------- data entry

	function label(id) {
		var l = document.querySelector("label[for=\"" + id + "\"]") || document.querySelector("label[for=\"" + id + "_input\"]");
		// without the required marker and the colon ("description *:" is "description"), as the builder adds its own
		return l ? text(l).replace(/\s*:$/, "").replace(/\s*\*$/, "") : "";
	}

	/** The input of a field's component. */
	function input(element, selector) {
		return element.matches(selector) ? element : element.querySelector(selector);
	}

	/**
	 * What a bound field shows, as dataEnter enters it: { value[, match] }; { unreadable } for a widget the adapter
	 * cannot read yet; null if the field is not on the page.
	 */
	function fieldValue(id, kind) {
		var element = document.getElementById(id);
		if (!element) {
			return null;
		}
		if (kind === "checkBox") {
			// a tri-state check box (a boolean that can be empty) keeps 0 (empty), 1 (true) or 2 (false) in its input
			var box = document.getElementById(id + "_input") || input(element, "input");
			if (!box) {
				return { unreadable: true };
			}
			if (box.type === "checkbox") {
				return { value: String(box.checked) };
			}
			return { value: (box.value === "1") ? "true" : ((box.value === "2") ? "false" : "") };
		}
		if (kind === "combo") {
			var select = input(element, "select");
			return select ? { value: select.value, match: "code" } : { unreadable: true };
		}
		if (kind === "radio") {
			var radio = element.querySelector("input[type=radio]:checked");
			return { value: radio ? radio.value : "", match: "code" };
		}
		if (TEXT_KINDS[kind] || /^date|time/i.test(kind)) {
			var field = input(element, "input:not([type=hidden]), textarea") || input(element, "input");
			return field ? { value: field.value } : { unreadable: true };
		}
		return { unreadable: true };
	}

	/** The bound fields of the map that are on the page, in page order: [{ id, binding, kind, element }]. */
	function fields() {
		var result = [];
		Object.keys(map.ids).forEach(function (id) {
			var entry = map.ids[id];
			var element = entry.binding && entry.widget && !entry.step ? document.getElementById(id) : null;
			if (element) {
				result.push({ id: id, binding: entry.binding, kind: entry.widget, element: element });
			}
		});
		result.sort(function (a, b) {
			return (a.element.compareDocumentPosition(b.element) & Node.DOCUMENT_POSITION_FOLLOWING) ? -1 : 1;
		});
		return result;
	}

	/** What each readable field shows now, by client id. */
	function snapshot() {
		var result = {};
		fields().forEach(function (f) {
			var value = fieldValue(f.id, f.kind);
			if (value && !value.unreadable) {
				result[f.id] = value.value;
			}
		});
		return result;
	}

	/** Writes the user's changes since the last snapshot as dataEnter steps, in page order, as final values. */
	function flushDataEntry() {
		if (!core) {
			return;
		}
		var view = describeView();
		var where = screen();
		unreadable.forEach(function (binding) {
			core.push({ op: "unmapped", text: "changed " + binding + " (a widget SAIL cannot enter, or the recorder cannot read)", view: view, screen: where });
		});
		unreadable = [];
		fields().forEach(function (f) {
			var value = fieldValue(f.id, f.kind);
			if (!value || value.unreadable || (value.value === shown[f.id])) {
				return;
			}
			var before = shown[f.id];
			shown[f.id] = value.value;
			if (value.value === "") {
				core.push({ op: "unmapped", text: "cleared " + f.binding + " (SAIL cannot enter an empty value)", view: view, screen: where });
				return;
			}
			var step = { op: "dataEnter", binding: f.binding, value: value.value, view: view, screen: where, label: label(f.id) };
			if (value.match) {
				step.match = value.match;
			}
			if (before !== undefined) {
				step.before = before;
			}
			core.push(step);
		});
	}

	/** A change to a widget the adapter cannot read: noted, so that the gesture is not dropped. */
	function onChange(event) {
		var m = mapped(event.target);
		if (m && m.entry.binding && !m.entry.step && (m.entry.widget !== "lookupDescription")) {
			var value = fieldValue(m.id, m.entry.widget);
			if (value && value.unreadable && (unreadable.indexOf(m.entry.binding) < 0)) {
				unreadable.push(m.entry.binding);
			}
		}
	}

	// ---------------------------------------------------------------- responses

	/** What a message says: a growl's message (not its hidden severity), or an icon-only message's title. */
	function messageText(m) {
		var t = text(m.querySelector(".ui-growl-message") || m);
		if (!t) {
			var icon = m.querySelector("[title]");
			t = icon ? icon.getAttribute("title").trim() : "";
		}
		return t;
	}

	/** A message's severity, from the element around it that has it in its class (as ui-growl-error does). */
	function severityOf(m) {
		var c = (m.closest(".ui-growl-item-container, .ui-messages > div, .ui-message") || m).className;
		return /error|fatal/.test(c) ? "error" : (/warn/.test(c) ? "warn" : "info");
	}

	/** Notes, as context, the messages a response showed that were not shown before. */
	function recordMessages() {
		var texts = [];
		var errors = false;
		document.querySelectorAll(PAGE_MESSAGES).forEach(function (m) {
			var t = messageText(m);
			if (m.sailSeen === t || !t || !m.getClientRects().length) {
				return;
			}
			m.sailSeen = t;
			var severity = severityOf(m);
			if (severity === "error") {
				errors = true;
			}
			texts.push(severity + " " + (m.classList.contains("ui-growl-item") ? "growl" : "message") + " \"" + shorten(t) + "\"");
		});
		if (texts.length) {
			var step = { op: "context", text: "shown: " + texts.join("; ") };
			if (errors) {
				step.errors = "true";
			}
			core.push(step);
		}
	}

	/** After a response: the values it changed on screen, the messages it showed, and the map of what it shows. */
	function onAjaxComplete() {
		if (!core || !core.active()) {
			return;
		}
		readMap();
		var now = snapshot();
		var changes = [];
		Object.keys(now).forEach(function (id) {
			if (Object.prototype.hasOwnProperty.call(shown, id) && (shown[id] !== now[id])) {
				var entry = map.ids[id];
				changes.push(entry.binding + " \"" + shorten(shown[id]) + "\" -> \"" + shorten(now[id]) + "\"");
			}
		});
		if (changes.length) {
			var changedValues = {};
			Object.keys(now).forEach(function (id) {
				if (Object.prototype.hasOwnProperty.call(shown, id) && (shown[id] !== now[id]) && now[id]) {
					changedValues[map.ids[id].binding] = now[id];
				}
			});
			core.push({ op: "context", text: "changed on screen: " + changes.join("; "), shown: JSON.stringify(changedValues) });
		}
		shown = now;
		tableBaseline();
		recordMessages();
		release();
	}

	// ---------------------------------------------------------------- grids

	function rowsOf(table) {
		var body = table ? (table.querySelector("tbody.ui-datatable-data") || table.querySelector("tbody.p-datatable-tbody")) : null;
		return body ? Array.prototype.filter.call(body.children, function (r) {
			return (r.tagName === "TR") && !r.classList.contains("ui-datatable-empty-message") &&
					!r.classList.contains("p-datatable-emptymessage");
		}) : [];
	}

	/** The text each cell of a row shows, as rowContaining compares it (a cell's text contains the text). */
	function cellTexts(row) {
		return Array.prototype.map.call(row.querySelectorAll("td"), function (td) {
			// a card layout repeats the column's title in each cell, an inline cell's select lists its options, and a
			// button's label (View Detail, a date picker's) is not what the row shows
			var cell = td.cloneNode(true);
			cell.querySelectorAll(".ui-column-title, .skyve-card-cell-label, select, script, button").forEach(function (e) {
				e.remove();
			});
			return String(cell.textContent || "").replace(/\s+/g, " ").trim();
		});
	}

	/**
	 * How a list grid row step names the row: { rowContaining } with the cell text that the fewest rows show (the
	 * leftmost on a tie) such that the first row showing it is this row; { row } only if no text does that.
	 */
	function rowLocator(rows, rowNum) {
		var texts = rows.map(cellTexts);
		var mine = texts[rowNum] || [];
		var best = null;
		var bestCount = 0;
		mine.forEach(function (t) {
			if (!t) {
				return;
			}
			var first = -1;
			var count = 0;
			texts.forEach(function (cells, i) {
				if (cells.some(function (c) {
					return c.indexOf(t) >= 0;
				})) {
					if (first < 0) {
						first = i;
					}
					count++;
				}
			});
			if ((first === rowNum) && ((best == null) || (count < bestCount))) {
				best = t;
				bestCount = count;
			}
		});
		return (best == null) ? { row: String(rowNum) } : { rowContaining: best };
	}

	/** The list grid steps' attributes for a grid's map entry: none for the page's own list. */
	function gridAttributes(entry) {
		var result = {};
		["module", "document", "query", "model"].forEach(function (key) {
			if (entry && entry[key]) {
				result[key] = entry[key];
			}
		});
		return result;
	}

	/** The mapped list grid (a Vue list grid's container) an element is in: { id, entry, element }, or null. */
	function vueGrid(element) {
		var m = mapped(element);
		return (m && m.entry.listGrid && m.element.querySelector(".skyve-vue-list-grid")) ? m : null;
	}

	function navigating() {
		try {
			window.sessionStorage.setItem(NAVIGATING_KEY, "gesture");
		} catch (e) {
			// the next page records its load as a navigation
		}
	}

	/** A new gesture on this page: an earlier one that may have loaded a page did not. */
	function notNavigating() {
		try {
			window.sessionStorage.removeItem(NAVIGATING_KEY);
		} catch (e) {
			// nothing to forget
		}
	}

	/** Records a Vue list grid's New or View Detail, as the page's harness presses them. Returns true if it did. */
	function vueGesture(target, grid) {
		var vue = grid.element;
		var step = null;
		if (target.closest("button.skyve-vue-list-grid-new") || target.closest(".skyve-vue-list-grid-fab button")) {
			step = gridAttributes(grid.entry);
			step.op = "listGridNew";
		} else {
			var tr = target.closest("tr");
			var cell = target.closest("td");
			var zoom = target.closest("td.skyve-vue-list-grid-action button") ||
						(cell && !cell.classList.contains("skyve-vue-list-grid-action") && vue.querySelector(".skyve-vue-list-grid-zoomable"));
			var rows = rowsOf(vue);
			var rowNum = tr ? rows.indexOf(tr) : -1;
			var component = vueComponent(grid.id);
			if (!zoom && (rowNum >= 0) && cell && component && component.selectable && !target.closest("a, button")) {
				step = withRow(gridAttributes(grid.entry), rows, rowNum);
				step.op = "listGridSelect";
				core.record(step);
				return true;
			}
			if (zoom && (rowNum >= 0)) {
				step = gridAttributes(grid.entry);
				step.op = "listGridZoom";
				var located = rowLocator(rows, rowNum);
				if (located.rowContaining != null) {
					step.rowContaining = located.rowContaining;
				} else {
					step.row = located.row;
				}
				step.rowTexts = cellTexts(tr).filter(function (t) {
					return !!t;
				}).join(" | ");
			}
		}
		if (!step) {
			return false;
		}
		navigating();
		core.record(step);
		return true;
	}

	// ---------------------------------------------------------------- data grids, lookups and list grids (phase 5)

	/** What the user last typed into each lookup, by the lookup's client id. */
	var lookupTyped = {};
	/** The state (filters and sort) each Vue list grid showed, by its container id. */
	var vueStates = {};
	/** SmartClient-style filter operators (the Vue list grid's match modes) and the listGridFilter operators they are. */
	var OPERATORS = { iContains: "contains", contains: "contains", iStartsWith: "startsWith", startsWith: "startsWith",
						equals: "equals", iEquals: "equals", notEqual: "notEquals", iNotEqual: "notEquals",
						lessThan: "lessThan", greaterThan: "greaterThan" };
	var MONTHS = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

	/** Adds a row to a step: rowContaining (or row) and the row's texts, for the checks. */
	function withRow(step, rows, rowNum) {
		var located = rowLocator(rows, rowNum);
		if (located.rowContaining != null) {
			step.rowContaining = located.rowContaining;
		} else {
			step.row = located.row;
		}
		step.rowTexts = cellTexts(rows[rowNum]).filter(function (t) {
			return !!t;
		}).join(" | ");
		return step;
	}

	/** The mapped data grid (a data table) an element is in: { id, entry, element } or null. */
	function dataGridAt(element) {
		for (var e = element; e && (e !== document.body); e = e.parentElement) {
			if (e.id && e.classList && e.classList.contains("ui-datatable")) {
				var entry = map.ids[mapKey(e.id)];
				if (entry && (entry.dataGrid || entry.listGrid)) {
					return { id: e.id, entry: entry, element: e };
				}
			}
		}
		return null;
	}

	/** A lookup's typing: kept for the autocomplete step its pick becomes. */
	function onInput(event) {
		if (!core || !core.active()) {
			return;
		}
		var target = event.target;
		var m = (target && target.id && /_input$/.test(target.id)) ? mapped(target) : null;
		if (m && (m.entry.widget === "lookupDescription")) {
			lookupTyped[target.id.replace(/_input$/, "")] = target.value;
		}
	}

	/**
	 * A pick from a lookup's drop-down: lookupDescriptionAutoComplete when the user typed into it first (search what was
	 * typed, pick the description chosen), otherwise lookupDescriptionPick of the row containing the description. Returns
	 * true if the click was on a lookup's item.
	 */
	function lookupPick(target) {
		var item = target.closest(".ui-autocomplete-item");
		var panel = item ? item.closest("[id$='_panel']") : null;
		if (!panel) {
			return false;
		}
		var id = panel.id.substring(0, panel.id.length - "_panel".length);
		var entry = map.ids[mapKey(id)];
		if (!entry || (entry.widget !== "lookupDescription")) {
			return false;
		}
		var description = text(item);
		var typed = lookupTyped[id];
		delete lookupTyped[id];
		var step = (typed && typed.trim()) ? { op: "lookupDescriptionAutoComplete", binding: entry.binding, search: typed, pick: description } :
												{ op: "lookupDescriptionPick", binding: entry.binding, rowContaining: description };
		step.label = label(id);
		core.record(step);
		return true;
	}

	/** The step for a link in a data grid's row (dataGridLink), or null. */
	function rowLinkStep(target) {
		var link = target.closest("a");
		var grid = link ? dataGridAt(link) : null;
		if (!grid || !grid.entry.dataGrid) {
			return null;
		}
		var rows = rowsOf(grid.element);
		var tr = link.closest("tr");
		var rowNum = tr ? rows.indexOf(tr) : -1;
		if (rowNum < 0) {
			return null;
		}
		var step = withRow({ op: "dataGridLink", binding: grid.entry.dataGrid }, rows, rowNum);
		step.text = String(link.innerText || link.textContent || "").replace(/\s+/g, " ").trim();
		return step.text ? { step: step, link: link } : null;
	}

	/** The parameters of a request's form data (a jQuery ajax settings.data string). */
	function formParams(data) {
		var result = {};
		if (typeof data !== "string") {
			return result;
		}
		data.split("&").forEach(function (pair) {
			var equals = pair.indexOf("=");
			if (equals > 0) {
				try {
					result[decodeURIComponent(pair.substring(0, equals))] = decodeURIComponent(pair.substring(equals + 1).replace(/\+/g, " "));
				} catch (e) {
					// not form data
				}
			}
		});
		return result;
	}

	/** "filter" or "sort" if a PrimeFaces ajax request is a data table list grid's filter or sort request, else null. */
	function tableRequest(settings) {
		var params = formParams(settings && settings.data);
		var source = params["jakarta.faces.source"] || params["javax.faces.source"];
		var behaviour = params["jakarta.faces.behavior.event"] || params["javax.faces.behavior.event"];
		var table = source ? document.getElementById(source) : null;
		var grid = table ? dataGridAt(table) : null;
		if (!grid || grid.entry.dataGrid) {
			return null;
		}
		if ((behaviour === "filter") || (params[source + "_filtering"] === "true")) {
			return "filter";
		}
		return ((behaviour === "sort") || (params[source + "_sorting"] === "true")) ? "sort" : null;
	}

	/**
	 * A PrimeFaces ajax request about to go: a data table's rowReorder (a drag) is dataGridMove; its rowSelect is
	 * dataGridSelect or listGridSelect.
	 */
	function onAjaxRequest(settings) {
		var params = formParams(settings && settings.data);
		var source = params["jakarta.faces.source"] || params["javax.faces.source"];
		var behaviour = params["jakarta.faces.behavior.event"] || params["javax.faces.behavior.event"];
		var table = source ? document.getElementById(source) : null;
		var grid = table ? dataGridAt(table) : null;
		if (!grid) {
			return;
		}
		var rows = rowsOf(grid.element);
		if ((behaviour === "rowReorder") && grid.entry.dataGrid) {
			var to = Number(params[source + "_toIndex"]);
			if ((to >= 0) && rows[to]) {
				var move = withRow({ op: "dataGridMove", binding: grid.entry.dataGrid }, rows, to);
				move.to = String(to);
				core.record(move);
			}
		} else if (tableRequest(settings) === "filter") {
			tableFiltering(grid);
		} else if (tableRequest(settings) === "sort") {
			tableSorting(grid, params[source + "_sortKey"], params[source + "_sortDir"]);
		} else if (behaviour === "rowSelect") {
			var selected = grid.element.querySelector("tbody tr.ui-state-highlight");
			var rowNum = selected ? rows.indexOf(selected) : -1;
			if (rowNum >= 0) {
				var select = grid.entry.dataGrid ? { op: "dataGridSelect", binding: grid.entry.dataGrid } : gridAttributes(grid.entry);
				if (!grid.entry.dataGrid) {
					select.op = "listGridSelect";
				}
				core.record(withRow(select, rows, rowNum));
			}
		}
	}

	// ---------------------------------------------------------------- data table list grids' filters and sorts

	/**
	 * What each PrimeFaces data table list grid's filters showed when last seen, by the table's client id:
	 * { element, state } (see tableFilterState()).
	 */
	var tableFilters = {};
	/**
	 * Data table filters the user changed whose steps are not yet written, by the table's client id: { grid, before }
	 * with the filters shown before the first change. A typed filter sends a request each time the user pauses, and a
	 * check box filter one for each state it passes through, so the steps are the final filters, written before the
	 * next other step or request.
	 */
	var pendingFilters = {};
	/**
	 * Data table sorts not yet written, by the table's client id: { grid, keys, orders } of the last sort request. A
	 * column's sort turns round with each click on its header, so the step is the final sort, written before the next
	 * other step or request.
	 */
	var pendingSorts = {};

	/** A data table list grid's columns from the map that have a header (their client id): { id, column, operator }. */
	function tableColumns(entry) {
		var grid = map.grids[entry.listGrid] || {};
		return (grid.columns || []).filter(function (c) {
			return !!c.id;
		}).map(function (c) {
			// the table's column is named by its field: the column's name, or else its binding
			return { id: c.id, column: c.name || c.binding, operator: c.operator };
		});
	}

	/**
	 * What a data table list grid's column filters show, by column: { operator, value } for each that has a value. A
	 * typed filter's value is its text; a constant domain's combo its option's label; a boolean's check box true or
	 * false.
	 */
	function tableFilterState(entry) {
		var result = {};
		tableColumns(entry).forEach(function (c) {
			var th = c.operator ? document.getElementById(c.id) : null;
			if (!th) {
				return;
			}
			var value = null;
			var select = th.querySelector(".ui-column-customfilter select");
			var check = th.querySelector(".ui-column-customfilter .ui-chkbox input");
			var input = th.querySelector("input.ui-column-filter");
			if (select) {
				var option = select.options[select.selectedIndex];
				value = ((select.value === "") || !option) ? null : text(option);
			} else if (check) {
				value = (check.value === "1") ? "true" : ((check.value === "2") ? "false" : null);
			} else if (input) {
				value = input.value.trim() || null;
			}
			if (value) {
				result[c.column] = { operator: c.operator, value: value };
			}
		});
		return result;
	}

	/** The filters a data table showed when last seen, taken now if the table is new to the page (a load or an update). */
	function tableSeen(grid) {
		var seen = tableFilters[grid.id];
		if (!seen || (seen.element !== grid.element)) {
			seen = { element: grid.element, state: tableFilterState(grid.entry) };
			tableFilters[grid.id] = seen;
		}
		return seen;
	}

	/** Notes the data tables the page shows, with their filters, so that only the user's changes are recorded. */
	function tableBaseline() {
		Object.keys(map.ids).forEach(function (id) {
			var element = map.ids[id].listGrid ? document.getElementById(id) : null;
			var table = element ? (element.classList.contains("ui-datatable") ? element : element.querySelector(".ui-datatable")) : null;
			var grid = table ? dataGridAt(table) : null;
			if (grid && !grid.entry.dataGrid) {
				tableSeen(grid);
			}
		});
	}

	/** A data table list grid sends its filters: notes them, to be written as the final filters (see flushTables()). */
	function tableFiltering(grid) {
		if (pendingSorts[grid.id]) {
			flushTables(); // a sort, then a filter: each is written in turn
		}
		var seen = tableSeen(grid);
		if (!pendingFilters[grid.id]) {
			pendingFilters[grid.id] = { grid: grid, before: seen.state };
		}
		seen.state = tableFilterState(grid.entry);
	}

	/** A data table list grid sends its sort: notes it, to be written as the final sort (see flushTables()). */
	function tableSorting(grid, keys, orders) {
		if (pendingFilters[grid.id]) {
			flushTables(); // a filter, then a sort: each is written in turn
		}
		pendingSorts[grid.id] = { grid: grid, keys: keys, orders: orders };
	}

	/** Writes the data table filters and sorts the user changed (see writeFilters() and writeSort()). */
	function flushTables() {
		var filters = pendingFilters;
		var sorts = pendingSorts;
		pendingFilters = {};
		pendingSorts = {};
		writeFilters(filters);
		Object.keys(sorts).forEach(function (id) {
			writeSort(sorts[id].grid, sorts[id].keys, sorts[id].orders);
		});
	}

	/**
	 * Writes the data table filters the user changed: listGridFilter for each column whose filter changed (no value for
	 * a column cleared), or listGridClearFilter when the user cleared them all. The operator is written unless it is
	 * contains.
	 */
	function writeFilters(pending) {
		Object.keys(pending).forEach(function (id) {
			var grid = pending[id].grid;
			var before = pending[id].before;
			var now = tableFilters[id] ? tableFilters[id].state : {};
			var list = function (op) {
				var step = gridAttributes(grid.entry);
				step.op = op;
				return step;
			};
			var names = Object.keys(now);
			var old = Object.keys(before);
			if (!names.length && old.length) {
				core.record(list("listGridClearFilter"));
				return;
			}
			names.forEach(function (name) {
				var was = before[name];
				var is = now[name];
				if (!was || (was.value !== is.value)) {
					var step = list("listGridFilter");
					step.column = name;
					step.value = is.value;
					if (is.operator !== "contains") {
						step.operator = is.operator;
					}
					core.record(step);
				}
			});
			old.forEach(function (name) {
				if (!now[name]) {
					var step = list("listGridFilter");
					step.column = name;
					core.record(step);
				}
			});
		});
	}

	/**
	 * Writes a data table list grid's sort, from the columns' header ids and their orders (1, -1 or 0) that its sort
	 * request sent: listGridSort for a sort by one column.
	 */
	function writeSort(grid, keys, orders) {
		var ids = String(keys || "").split(",").filter(function (k) {
			return !!k;
		});
		var order = String(orders || "").split(",")[0];
		var column = (ids.length === 1) ? tableColumns(grid.entry).filter(function (c) {
			return c.id === ids[0];
		})[0] : null;
		if (ids.length > 1) {
			core.unmapped("sorted a list by more than one column");
		} else if (!column || (order === "0")) {
			core.unmapped(column ? "cleared a list's sort" : "sorted a list by a column SAIL cannot name");
		} else {
			var sort = gridAttributes(grid.entry);
			sort.op = "listGridSort";
			sort.column = column.column;
			if (order === "-1") {
				sort.descending = "true";
			}
			core.record(sort);
		}
	}

	/** Writes everything the user did that is not yet a step: the data table filters and sorts, then the data entry. */
	function flushAll() {
		if (core) {
			flushTables();
		}
		flushDataEntry();
	}

	/** The Vue list grid component in a container, or null. */
	function vueComponent(containerId) {
		// the app's root component is the one rendered into its container (app._instance is only in development builds)
		var app = window[containerId];
		var root = app && app._container && app._container._vnode && app._container._vnode.component;
		var component = root && root.subTree && root.subTree.component;
		return component ? component.proxy : null;
	}

	function pad(n) {
		return (n < 10) ? "0" + n : String(n);
	}

	/** A filter value as listGridFilter writes it: an enum's label, a boolean, a date as Skyve's default converter. */
	function filterText(column, value) {
		if ((value === true) || (value === false)) {
			return String(value);
		}
		if (column && column.enumValues) {
			var option = column.enumValues.filter(function (o) {
				return o.value === value;
			})[0];
			if (option) {
				return String(option.label);
			}
		}
		if (value instanceof Date) {
			var date = pad(value.getDate()) + "-" + MONTHS[value.getMonth()] + "-" + value.getFullYear();
			if (column && (column.type === "dateTime")) {
				return date + " " + pad(value.getHours()) + ":" + pad(value.getMinutes());
			}
			if (column && (column.type === "timestamp")) {
				return date + " " + pad(value.getHours()) + ":" + pad(value.getMinutes()) + ":" + pad(value.getSeconds());
			}
			if (column && (column.type === "time")) {
				return pad(value.getHours()) + ":" + pad(value.getMinutes());
			}
			return date;
		}
		return String(value);
	}

	/** The column binding of a Vue list grid's field, from the map's columns, or the field unsanitised. */
	function columnBinding(entry, field) {
		var grid = map.grids[entry.listGrid] || {};
		var found = (grid.columns || []).filter(function (c) {
			return (c.name === field) || (c.binding === field) || (c.binding && (c.binding.replace(/\./g, "_") === field));
		})[0];
		return found ? (found.binding || found.name) : String(field).replace(/_(\d*)_/g, "[$1]").replace(/_/g, ".");
	}

	/** A Vue list grid's filters and sort: { ok, any, columns: { field: { operator, value } }, sort: [{ field, order }] }. */
	function vueState(component) {
		var topLevel = component.selectedTopLevelOperator;
		topLevel = (topLevel && topLevel.value) ? topLevel.value : topLevel;
		var result = { ok: topLevel !== "or", columns: {}, sort: [] };
		var columns = {};
		(component.columns || []).forEach(function (c) {
			columns[c.field] = c;
		});
		Object.keys(component.filters || {}).forEach(function (field) {
			var filter = component.filters[field] || {};
			var constraints = (filter.constraints || [filter]).filter(function (c) {
				return c && (c.value != null) && (c.value !== "");
			});
			if (constraints.length > 1) {
				result.ok = false;
			} else if (constraints.length === 1) {
				var operator = OPERATORS[constraints[0].matchMode];
				if (!operator) {
					result.ok = false;
				} else {
					result.columns[field] = { operator: operator, value: filterText(columns[field], constraints[0].value) };
				}
			}
		});
		(component.multiSortMeta || []).forEach(function (s) {
			result.sort.push({ field: s.field, order: s.order });
		});
		return result;
	}

	/**
	 * A Vue list grid is about to fetch its rows (window.fetch of smartlist): records how the user changed each grid's
	 * filters and sort since it last fetched.
	 */
	function vueFetching() {
		Object.keys(map.ids).forEach(function (id) {
			var entry = map.ids[id];
			var component = entry.listGrid ? vueComponent(id) : null;
			if (!component) {
				return;
			}
			var now = vueState(component);
			var before = vueStates[id];
			vueStates[id] = now;
			if (!before) {
				return; // the state the grid had when the page loaded (perhaps kept from before)
			}
			var list = function (op) {
				var step = gridAttributes(entry);
				step.op = op;
				return step;
			};
			if (!now.ok) {
				if (JSON.stringify(now) !== JSON.stringify(before)) {
					core.unmapped("filtered a list in a way SAIL cannot write as column filters ANDed together");
				}
				return;
			}
			var names = Object.keys(now.columns);
			var old = Object.keys(before.columns);
			if (!names.length && old.length) {
				core.record(list("listGridClearFilter"));
			} else {
				names.forEach(function (name) {
					var was = before.columns[name];
					var is = now.columns[name];
					if (!was || (was.operator !== is.operator) || (was.value !== is.value)) {
						var step = list("listGridFilter");
						step.column = columnBinding(entry, name);
						step.value = is.value;
						if (is.operator !== "contains") {
							step.operator = is.operator;
						}
						core.record(step);
					}
				});
				old.forEach(function (name) {
					if (!now.columns[name]) {
						var step = list("listGridFilter");
						step.column = columnBinding(entry, name);
						core.record(step);
					}
				});
			}
			if (JSON.stringify(now.sort) !== JSON.stringify(before.sort)) {
				if (now.sort.length === 1) {
					var sort = list("listGridSort");
					sort.column = columnBinding(entry, now.sort[0].field);
					if (now.sort[0].order === -1) {
						sort.descending = "true";
					}
					core.record(sort);
				} else {
					core.unmapped(now.sort.length ? "sorted a list by more than one column" : "cleared a list's sort");
				}
			}
		});
	}

	/** Keeps each Vue list grid's state as the page shows it, so that only the user's changes are recorded. */
	function vueBaseline() {
		Object.keys(map.ids).forEach(function (id) {
			var component = map.ids[id].listGrid ? vueComponent(id) : null;
			if (component && !vueStates[id]) {
				vueStates[id] = vueState(component);
			}
		});
	}

	/** The question a server-side question dialog asks, and whether the button declines it (No, Cancel or the abort). */
	function questionAnswer(target) {
		var button = target.closest(".ui-dialog-buttonpane button");
		var dialog = button ? button.closest("[role=alertdialog][aria-labelledby=skyveQuestionTitle]") : null;
		if (!dialog) {
			return null;
		}
		var buttons = Array.prototype.slice.call(dialog.querySelectorAll(".ui-dialog-buttonpane button"));
		var close = dialog.querySelector(".ui-dialog-titlebar-close");
		var abortLabel = close ? close.getAttribute("aria-label") : null;
		var negative = buttons.filter(function (b) {
			return text(b) === abortLabel;
		})[0] || buttons[buttons.length - 1];
		var title = document.getElementById("skyveQuestionTitle");
		return { declined: button === negative, answer: text(button), question: text(title) };
	}

	/** The title the page shows for its view, as captureTitle reads it. */
	function viewTitle() {
		var title = Array.prototype.filter.call(document.querySelectorAll(".skyve-title-text"), function (t) {
			return t.getClientRects().length > 0;
		})[0];
		return title ? String(title.innerText || "").trim() : "";
	}

	// ---------------------------------------------------------------- clicks

	/** Records a menu item: its module from its link, and its shown labels with its groups. */
	function recordMenu(link) {
		// an item's link is the URL, or javascript that opens it (SKYVE.PF.startHistory('<url>')); a group's has none
		var href = link.getAttribute("href");
		if (!href) {
			return false;
		}
		var quoted = /^javascript:.*?'([^']+)'/.exec(href);
		var url = new URL(quoted ? quoted[1] : href, location.href);
		var module = url.searchParams.get("m");
		if (!module) {
			return false;
		}
		var labels = [text(link)];
		for (var li = link.closest("li"); li; li = li.parentElement ? li.parentElement.closest("li.ui-menu-parent") : null) {
			if (li.classList.contains("ui-menu-parent")) {
				var a = li.querySelector(":scope > a");
				if (a) {
					labels.unshift(text(a));
				}
			}
		}
		var mapNavigation = mapStep(url.searchParams);
		if (mapNavigation) {
			// a map item opens the map page: navigateMap, as the SmartClient recorder records it
			navigating();
			core.record(mapNavigation);
			return true;
		}
		var step = { op: "navigateMenu", module: module, item: labels.join("/") };
		if ((url.searchParams.get("a") === "e") && url.searchParams.get("d")) {
			step.target = module + "." + url.searchParams.get("d");
		}
		navigating();
		core.record(step);
		return true;
	}

	/**
	 * The navigateMap step for a map page's URL parameters (a=m, m, q or d and q for a model, b the geometry binding), or
	 * null for any other page. q names a query, or a document (its default query) when it starts with a capital.
	 */
	function mapStep(params) {
		if ((params.get("a") !== "m") || !params.get("m") || !params.get("q")) {
			return null;
		}
		var step = { op: "navigateMap", module: params.get("m") };
		if (params.get("d")) {
			step.document = params.get("d");
			step.model = params.get("q");
		} else if (/^[A-Z]/.test(params.get("q"))) {
			step.document = params.get("q");
		} else {
			step.query = params.get("q");
		}
		if (params.get("b")) {
			step.geometryBinding = params.get("b");
		}
		return step;
	}

	/** Lets a held step go: its confirmation was answered yes, or no confirmation came. */
	function release() {
		if (confirming) {
			delete confirming.hold;
			confirming = null;
			core.send();
		}
	}

	/** The step for a mapped button, or null for one that is not recorded. */
	function buttonStep(m) {
		var entry = m.entry;
		var step = null;
		if (CLOSING_STEPS[entry.step] || (entry.step === "save")) {
			step = { op: entry.step };
		} else if (entry.step === "action") {
			step = CONTROL_ACTIONS[entry.name] ? null : { op: "action", name: entry.name };
		} else if ((entry.step === "listGridNew") || (entry.step === "listGridZoom")) {
			step = gridAttributes(entry);
			step.op = entry.step;
			if (entry.step === "listGridZoom") {
				var table = document.getElementById(m.id.split(":")[0]);
				var rows = rowsOf(table);
				var tr = m.element.closest("tr");
				var rowNum = tr ? rows.indexOf(tr) : m.row;
				var located = rowLocator(rows, rowNum);
				if (located.rowContaining != null) {
					step.rowContaining = located.rowContaining;
				} else {
					step.row = located.row;
				}
			}
		} else if (entry.step === "tabSelect") {
			step = { op: "tabSelect", path: entry.path };
		} else if (/^dataGrid(New|Zoom|Remove|Select)$/.test(entry.step)) {
			step = { op: entry.step, binding: entry.binding };
			if (entry.step !== "dataGridNew") {
				var grid = dataGridAt(m.element);
				var gridRows = grid ? rowsOf(grid.element) : [];
				var row = m.element.closest("tr");
				var at = row ? gridRows.indexOf(row) : m.row;
				if (at < 0) {
					return null;
				}
				withRow(step, gridRows, at);
			}
		} else if (entry.step === "upload") {
			// the core records the upload when the upload page sends the file
			core.handled();
			return null;
		} else {
			step = { op: "unmapped", text: "pressed " + entry.step + " " + (entry.binding || entry.action || entry.name || "") +
											" (not recorded)" };
		}
		return step;
	}

	function onClick(event) {
		if (!core || !core.active() || core.isPanelEvent(event)) {
			return;
		}
		var target = event.target;
		// the global confirm dialog answers the button that asked
		if (target.closest(".ui-confirmdialog-no")) {
			if (confirming) {
				if (CLOSING_STEPS[confirming.op] || (confirming.op === "save") || (confirming.op === "action") || (confirming.op === "dataGridLink")) {
					confirming.confirm = "false";
				}
				release();
			}
			return;
		}
		if (target.closest(".ui-confirmdialog-yes")) {
			release();
			return;
		}
		var question = questionAnswer(target);
		if (question) {
			// a server-side question: No (or the button that aborts) declines the step that raised it
			if (question.declined) {
				core.push({ op: "context", text: "answered \"" + question.answer + "\" to \"" + question.question + "\"", declined: "true" });
			}
			return;
		}
		release();
		notNavigating();
		if (lookupPick(target)) {
			return;
		}
		var link = target.closest("a.ui-menuitem-link");
		if (link && link.closest(".ui-panelmenu")) {
			recordMenu(link); // opening a module or a group is not a step
			return;
		}
		var tab = target.closest(".ui-tabs-nav a[href^=\"#\"]");
		if (tab) {
			var li = tab.closest("li");
			var tabEntry = map.ids[mapKey(tab.getAttribute("href").substring(1))];
			if (tabEntry && (tabEntry.step === "tabSelect") && li && !li.classList.contains("ui-state-active") && !disabled(li)) {
				core.record({ op: "tabSelect", path: tabEntry.path });
			}
			return;
		}
		var grid = vueGrid(target);
		if (grid && vueGesture(target, grid)) {
			return;
		}
		var m = mapped(target);
		if (m && (m.entry.step === "listGridSelect")) {
			// a data table list grid's own element: a row's selection, a filter and a sort are recorded from the
			// requests they send (see onAjaxRequest)
			return;
		}
		if (m && m.entry.widget && /^content/.test(m.entry.widget)) {
			core.handled(); // a content widget's upload: the core records it from the upload page
			return;
		}
		if (!m || !m.entry.step) {
			var rowLink = rowLinkStep(target);
			if (rowLink && !disabled(rowLink.link)) {
				// an action link may ask in the confirm dialog: the step waits for the answer, or for its request to go
				rowLink.step.hold = true;
				confirming = rowLink.step;
				core.record(rowLink.step);
			}
			return;
		}
		if (disabled(m.element.closest("button, a") || m.element)) {
			return;
		}
		if ((m.entry.step === "action") && (m.entry.name === STOP_ACTION)) {
			// the server builds the script when this action reaches it: send every step first, then press it
			if (!m.element.sailStopping) {
				event.preventDefault();
				event.stopImmediatePropagation();
				var element = m.element;
				flushAll();
				core.whenSent(function () {
					core.stop(true);
					element.sailStopping = true;
					element.click();
				});
			}
			return;
		}
		var step = buttonStep(m);
		if (!step) {
			return;
		}
		if (CLOSING_STEPS[step.op] || (step.op === "listGridNew") || (step.op === "listGridZoom") ||
				(step.op === "dataGridNew") || (step.op === "dataGridZoom")) {
			navigating();
		}
		// a button may ask for confirmation in the global confirm dialog: its step waits for the answer
		var onclick = String((m.element.closest("button, a") || m.element).getAttribute("onclick") || "");
		if (onclick.indexOf("confirm") >= 0) {
			step.hold = true;
			confirming = step;
		}
		core.record(step);
	}

	// ---------------------------------------------------------------- Check mode

	function rectOf(element) {
		var r = element.getBoundingClientRect();
		return { left: r.left, top: r.top, width: r.width, height: r.height };
	}

	/** A field's validation error shown next to it, if any. */
	function fieldError(element) {
		var id = element.id;
		var message = document.querySelector(".ui-message-error[id^=\"" + id + "\"]") ||
						(element.parentElement ? element.parentElement.querySelector(".ui-message-error") : null);
		return message ? messageText(message) : "";
	}

	function checkTarget(element) {
		var message = element.closest(MESSAGES);
		if (message) {
			var t = messageText(message);
			return t ? { rect: rectOf(message), label: "testMessage \"" + shorten(t) + "\"", step: { op: "testMessage", contains: t } } : null;
		}
		var vue = element.closest(".skyve-vue-list-grid");
		var table = vue || element.closest(".ui-datatable");
		if (table) {
			var m = mapped(table);
			var step = m && m.entry.dataGrid ? { binding: m.entry.dataGrid } : gridAttributes(m ? m.entry : null);
			step.op = "testGridRows";
			var rows = rowsOf(table);
			var tr = element.closest("tr");
			var rowNum = tr ? rows.indexOf(tr) : -1;
			if (rowNum >= 0) {
				var located = rowLocator(rows, rowNum);
				if (located.rowContaining == null) {
					return null;
				}
				step.rowContaining = located.rowContaining;
				step.minRows = "1";
				return { rect: rectOf(tr), label: "testGridRows containing \"" + located.rowContaining + "\"", step: step };
			}
			step.minRows = String(rows.length);
			step.maxRows = String(rows.length);
			return { rect: rectOf(table), label: "testGridRows " + rows.length + " rows", step: step };
		}
		var f = mapped(element);
		if (f && f.entry.binding && !f.entry.step) {
			var container = document.getElementById(mapKey(f.id)) || f.element;
			var error = fieldError(container);
			if (error) {
				return { rect: rectOf(container), label: "testMessage \"" + shorten(error) + "\"", step: { op: "testMessage", contains: error } };
			}
			var value = fieldValue(container.id, f.entry.widget);
			if (!value || value.unreadable || (value.value === "")) {
				return null;
			}
			return { rect: rectOf(container), label: "testValue " + f.entry.binding,
						step: { op: "testValue", binding: f.entry.binding, value: value.value, label: label(container.id) } };
		}
		return null;
	}

	// ---------------------------------------------------------------- unmapped clicks

	function control(element) {
		if (element.closest(".p-column-filter-overlay, .p-column-filter-menu-button, .p-sortable-column, .p-column-header-content, " +
								".ui-dialog-titlebar, .ui-overlaypanel-close, .ui-dialog-titlebar-close")) {
			return null; // a list's filter and sort are recorded when it fetches; a dialog's or overlay's close
		}
		if (element.closest(".ui-confirmdialog, .ui-confirm-dialog, .ui-panelmenu, .ui-tabs-nav, .ui-growl, #skyve-sail-recorder")) {
			return null;
		}
		var toggle = element.closest("[onclick*='toggleFilters']");
		if (toggle && toggle.closest(".ui-datatable thead")) {
			return null; // a data table's filter toggle shows its filters, which are recorded when the table filters
		}
		var target = element.closest("a[href], button, .ui-button, [role=button]");
		if (!target || disabled(target)) {
			return null;
		}
		var t = shorten(text(target) || target.getAttribute("title") || target.getAttribute("aria-label") || "");
		if (target.tagName === "A") {
			return "link \"" + t + "\" (" + target.getAttribute("href") + ")";
		}
		return (target.id ? "#" + target.id + " " : "") + "\"" + t + "\"";
	}

	// ---------------------------------------------------------------- the adapter

	/** Sends a request with jQuery, which the page and its tests count as a running request (jQuery.active). */
	function transport(method, url, body, callback) {
		jQuery.ajax({
			url: url,
			type: method,
			data: (body == null) ? undefined : body,
			contentType: (body == null) ? undefined : "application/json",
			dataType: "text",
			processData: false,
			cache: false,
			complete: function (xhr) {
				callback(xhr.status, xhr.responseText);
			}
		});
	}

	function attach(recorder) {
		core = recorder;
		core.listen(document, "click", onClick, true);
		core.listen(document, "change", onChange, true);
		core.listen(document, "input", onInput, true);
		// PrimeFaces' ajax events, which it triggers through jQuery
		var send = function (event, xhr, settings) {
			if (core.active()) {
				// a data table's filters and sort send a request for each change: their steps wait for the final ones
				if (!tableRequest(settings)) {
					flushTables();
				}
				flushDataEntry();
				release();
				onAjaxRequest(settings);
			}
		};
		// a Vue list grid fetches its rows with window.fetch (smartlist): its filters and sort are the user's then
		core.wrap(window, "fetch", function (original) {
			return function (request) {
				var url = (request && request.url) ? request.url : String(request || "");
				if (core.active() && /smartlist(\?|$)/.test(url)) {
					try {
						vueFetching();
					} catch (e) {
						// never stop the grid's fetch
					}
				}
				return original.apply(window, arguments);
			};
		});
		jQuery(document).on("pfAjaxSend", send);
		jQuery(document).on("pfAjaxComplete", onAjaxComplete);
		core.onDetach(function () {
			jQuery(document).off("pfAjaxSend", send);
			jQuery(document).off("pfAjaxComplete", onAjaxComplete);
		});
	}

	/** Records the page load as a navigation, unless a recorded gesture caused it. */
	function recordPageLoad() {
		var cause = null;
		try {
			cause = window.sessionStorage.getItem(NAVIGATING_KEY);
			window.sessionStorage.removeItem(NAVIGATING_KEY);
		} catch (e) {
			cause = null;
		}
		var params = new URL(location.href).searchParams;
		var module = params.get("m");
		if (cause || !module) {
			return;
		}
		var a = params.get("a");
		if ((a === "e") && params.get("d")) {
			var edit = { op: "navigateEdit", module: module, document: params.get("d"), target: module + "." + params.get("d"), view: "" };
			if (params.get("i")) {
				edit.bizId = params.get("i");
			}
			core.push(edit);
		} else if ((a === "l") && params.get("q")) {
			var list = { op: "navigateList", module: module, view: "" };
			if (params.get("d")) {
				list.document = params.get("d");
				list.model = params.get("q");
			} else if (map.list && !map.list.query) {
				list.document = params.get("q");
			} else {
				list.query = params.get("q");
			}
			core.push(list);
		} else if (mapStep(params)) {
			var mapNavigation = mapStep(params);
			mapNavigation.view = "";
			core.push(mapNavigation);
		} else if (a) {
			core.push({ op: "unmapped", text: "opened " + location.search + " (no SAIL step opens this page)", view: "" });
		}
	}

	var recorder = SKYVE.SailRecorderCore.create({
		// bottom-right, clear of the sidebar's last items, and left of a scrolled list's floating New button (a 48px
		// button 15px from the window's right edge), which is the only control PrimeFaces keeps at the bottom right:
		// the action bar and the growls are at the top
		corner: "bottom-right",
		cornerInset: 76,
		transport: transport,
		attach: attach,
		flush: flushAll,
		describe: function () {
			return { view: describeView(), screen: screen(), title: viewTitle() };
		},
		connected: function () {
			// nothing to resolve: the map names the page's list
		},
		control: control,
		checkTarget: checkTarget,
		isSignOut: function (element) {
			var link = element && element.closest ? element.closest("a[href]") : null;
			return !!link && /(^|\/)loggedOut$/.test(new URL(link.href, location.href).pathname);
		},
		signOut: function (target) {
			var link = target && target.closest ? target.closest("a[href]") : null;
			window.location = link ? link.href : "loggedOut";
		},
		idle: function () {
			return !confirming;
		},
		reset: function () {
			confirming = null;
			unreadable = [];
			lookupTyped = {};
			vueStates = {};
			tableFilters = {};
			pendingFilters = {};
			pendingSorts = {};
		}
	});
	SKYVE.SailRecorder = recorder;

	function begin() {
		if (window.top !== window) {
			// a page in a frame (the upload page, in a renderer's window or overlay): the page around it records
			return;
		}
		readMap();
		shown = snapshot();
		tableBaseline();
		recorder.start(readJson("sailRecorderState", "data-state"));
		recordPageLoad();
		recordMessages();
		// the values the page shows, which a later step may reuse (a capture; see SailRecordingCaptures)
		var values = {};
		Object.keys(shown).forEach(function (id) {
			if (shown[id] && map.ids[id]) {
				values[map.ids[id].binding] = shown[id];
			}
		});
		if (core && recorder.active() && Object.keys(values).length) {
			core.push({ op: "context", shown: JSON.stringify(values) });
		}
	}
	if (document.readyState === "loading") {
		document.addEventListener("DOMContentLoaded", begin);
	} else {
		begin();
	}
})();
