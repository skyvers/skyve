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
			core.push({ op: "unmapped", text: "changed " + binding + " (a widget recorded in a later phase)", view: view, screen: where });
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
		if (m && m.entry.binding && !m.entry.step) {
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
			core.push({ op: "context", text: "changed on screen: " + changes.join("; ") });
		}
		shown = now;
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
			return String(td.textContent || "").trim();
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
		var step = { op: "navigateMenu", module: module, item: labels.join("/") };
		if ((url.searchParams.get("a") === "e") && url.searchParams.get("d")) {
			step.target = module + "." + url.searchParams.get("d");
		}
		navigating();
		core.record(step);
		return true;
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
		} else {
			step = { op: "unmapped", text: "pressed " + entry.step + " " + (entry.binding || entry.action || entry.name || "") +
											" (recorded in a later phase)" };
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
				if (CLOSING_STEPS[confirming.op] || (confirming.op === "save") || (confirming.op === "action")) {
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
		release();
		notNavigating();
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
		if (!m || !m.entry.step || disabled(m.element.closest("button, a") || m.element)) {
			return;
		}
		if ((m.entry.step === "action") && (m.entry.name === STOP_ACTION)) {
			// the server builds the script when this action reaches it: send every step first, then press it
			if (!m.element.sailStopping) {
				event.preventDefault();
				event.stopImmediatePropagation();
				var element = m.element;
				flushDataEntry();
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
		if (CLOSING_STEPS[step.op] || (step.op === "listGridNew") || (step.op === "listGridZoom")) {
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
		if (element.closest(".ui-confirmdialog, .ui-confirm-dialog, .ui-panelmenu, .ui-tabs-nav, .ui-growl, #skyve-sail-recorder")) {
			return null;
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
		// PrimeFaces' ajax events, which it triggers through jQuery
		var send = function () {
			if (core.active()) {
				flushDataEntry();
				release();
			}
		};
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
		} else if (a) {
			core.push({ op: "unmapped", text: "opened " + location.search + " (not recorded yet)", view: "" });
		}
	}

	var recorder = SKYVE.SailRecorderCore.create({
		corner: "bottom-left",
		transport: transport,
		attach: attach,
		flush: flushDataEntry,
		describe: function () {
			return { view: describeView(), screen: screen() };
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
		}
	});
	SKYVE.SailRecorder = recorder;

	function begin() {
		readMap();
		shown = snapshot();
		recorder.start(readJson("sailRecorderState", "data-state"));
		recordPageLoad();
		recordMessages();
	}
	if (document.readyState === "loading") {
		document.addEventListener("DOMContentLoaded", begin);
	} else {
		begin();
	}
})();
