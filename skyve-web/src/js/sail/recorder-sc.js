/*
 * SAIL recorder for SmartClient: the SmartClient adapter of the recorder core (recorder-core.js), with which
 * skyve-web/build.xml concatenates it into skyve/sail/recorder-sc-min.js. See docs/sail.md, "Recording".
 *
 * Only a session that is recording loads this script: desktop/home.xhtml adds it when DesktopView.isSailRecording()
 * is true, and the Control Panel loads it when recording starts. The core sends the steps to the sailrecording
 * endpoint (org.skyve.impl.sail.recording.SailRecordingServlet), shows the panel and runs Check mode; this adapter
 * maps SmartClient to SAIL.
 *
 * Skyve's client code has no recorder checks in it. The adapter attaches itself when recording starts, and detaches
 * when it stops:
 * - it wraps these methods of Skyve's SmartClient classes, calling the original and recording what it did (each is
 *   marked in skyve-web/src/js/desktop with a comment pointing here):
 *     isc.BizUtil.handleMenuItemClick   navigateMenu
 *     isc.BizButton.initWidget          wraps each button's action: action, save, ok, cancel, delete, zoomOut, remove
 *     isc.BizTabPane.tabSelected        tabSelect, when the user selected the tab
 *     isc.BizListGrid.zoom              listGridZoom (rowContaining), listGridNew
 *     isc.EditView.scatter              writes the pending dataEnter steps before a response replaces the values, then
 *                                       notes what the response changed on screen and what each field shows
 *     isc.EditView.gather               notes the errors when validation stops a request in the browser
 *     isc.EditView._displayGrowlsAndMessages, _displayFailure   note the messages and errors a response shows
 *     isc.BizUtil.growl                 notes the growls shown
 *     isc.BizDataGrid zoom, add, startEditingRow   dataGridZoom, dataGridNew, dataGridEdit
 *     isc.ListGrid removeSelectedData, recordDrop  dataGridRemove, dataGridMove (on a data grid)
 *     isc.ListGrid setEditValue         dataEnter into an inline data grid's cells, after dataGridNew or dataGridEdit
 *     isc.ListGrid handleSortChanged    listGridSort, when the user sorted a list grid
 *     isc.ListGrid filterData           listGridFilter and listGridClearFilter, from the criteria a BizListGrid's user set
 *     isc.BizLookupDescriptionItem.zoom lookupDescriptionNew, lookupDescriptionEdit
 *     isc.BizUtil.openContentUpload     marks the click handled: the core records the upload from its page
 * - it handles SmartClient's ValuesManager.itemChanged() notification, which a view's forms fire when the user
 *   changes a field: the field's final value becomes a dataEnter step before the next step or server response;
 * - it wraps isc.ask, to see a confirmation the user declined (confirm="false"), and notes a server-side question the
 *   user answered No or Cancel (the step before it gets confirm="false");
 * - a click on a data grid row selects it (dataGridSelect), and on a link in the row follows it (dataGridLink); a click
 *   on a row of a list grid whose selection does something selects it (listGridSelect). A selection is written only if
 *   the next step does not act on the same grid's row;
 * - a lookup's pick is lookupDescriptionPick (rowContaining the chosen description), or, when the user typed into it
 *   first, lookupDescriptionAutoComplete (search and pick);
 * - it describes, for the core, a click that recorded nothing (an "unmapped" comment) and what a Check mode click
 *   checks (a growl or dialog message, a field, a grid row or a grid);
 * - a page load with a=e, a=l, a=t, a=m or a=c in its URL (a link, a typed URL, a reload) is recorded as navigateEdit,
 *   navigateList, navigateTree, navigateMap or navigateCalendar; a tree, map or calendar menu item likewise.
 */
(function () {
	"use strict";

	window.SKYVE = window.SKYVE || {};
	if (SKYVE.SailRecorder) {
		// loaded again by the Control Panel's Start after an earlier recording in this page stopped
		SKYVE.SailRecorder.start();
		return;
	}

	/** The Control Panel's recording actions, which are not recorded. */
	var CONTROL_ACTIONS = { StartSAILRecording: true, StopSAILRecording: true, DownloadSAILRecording: true };
	var STOP_ACTION = "StopSAILRecording";
	/** BizButton types and the steps they record; any other type is an unmapped gesture. */
	var BUTTON_STEPS = { OK: "ok", Save: "save", Cancel: "cancel", Delete: "delete", ZoomOut: "zoomOut", Remove: "remove" };
	var ACTION_TYPES = { Download: true };

	/** The recorder (recorder-core.js) this adapter records for. */
	var core = null;
	/** Form items the user changed whose values are not yet written as dataEnter steps, in the order first changed. */
	var changed = [];
	/** The step the button being pressed recorded, while its action may ask for a confirmation. */
	var confirmable = null;
	/** A navigateList step for the page load whose query or document is known once the page has its data sources. */
	var pageList = null;
	/** A grid row selection not yet written: { owner, step }. The next step drops it if it acts on the same grid. */
	var pendingSelect = null;
	/** Cells of inline data grids the user changed, not yet written: { dataGrid, rowNum, field }. */
	var gridEdits = [];
	/** What the user last typed into each lookup's combo, by the lookup item's ID. */
	var lookupTyped = {};
	/** The data grid a row was just dragged in: the click that ends the drag is not a selection. */
	var droppedOn = null;
	/**
	 * A row link's step, held until its javascript: runs: an action link asks for confirmation then (isc.ask), and the
	 * answer may decline the step. Released at the next gesture, step or response if nothing asked.
	 */
	var heldLink = null;

	/** Lets a held row link's step go, when nothing asked about it. */
	function releaseLink() {
		var step = heldLink;
		heldLink = null;
		if (step && step.hold) {
			delete step.hold;
			core.send();
		}
	}

	// ---------------------------------------------------------------- what the page shows

	/** The text of markup as the SAIL SmartClient helper (util/sail/smartclient-sail.js) compares it. */
	function strip(html) {
		return String(html == null ? "" : html)
			.replace(/<[^>]*>/g, "")
			.replace(/&nbsp;/g, " ")
			.replace(/\s+/g, " ")
			.trim();
	}

	/** The top-most edit view: the last WindowStack window holding a view, or the harness's current view. */
	function topView() {
		var stack = isc.WindowStack._stack || [];
		for (var i = stack.length - 1; i >= 0; i--) {
			var items = stack[i].items;
			var item = items && items[0];
			if (item && item._vm) {
				return item;
			}
		}
		return isc.BizUtil.getCurrentView();
	}

	/** "module.Document" for an edit view, "list" for the list view. */
	function describe(view) {
		if (!view) {
			return "";
		}
		if (isc.ListView && view === isc.ListView.contents) {
			return "list";
		}
		return view._mod ? view._mod + "." + view._doc : "";
	}

	/**
	 * Describes the screen a view shows, for the script's heading comments: module and document (and its name),
	 * view and renderer; or the list view's data source.
	 */
	function screenOf(view) {
		if (!view) {
			return "";
		}
		if (isc.ListView && view === isc.ListView.contents) {
			var grid = isc.ListView._grid;
			var ds = grid && (grid._dataSource || (grid.grid && grid.grid.dataSource));
			var id = ds && (ds.ID || ds);
			return "list " + (isc.isA.String(id) ? id : "") + ", SmartClient";
		}
		return view._mod ? view._mod + "." + view._doc + " (" + strip(view._singular) + "), " +
								(view._sailViewType || "edit") + " view, SmartClient" : "";
	}

	/** The binding a form item's name stands for: org.skyve.impl.bind.BindUtil.unsanitiseBinding(). */
	function unsanitise(name) {
		return String(name).replace(/_(\d*)_/g, "[$1]").replace(/_/g, ".");
	}

	/** The canvas whose DOM holds an element. */
	function canvasOf(element) {
		for (var e = element; e && e.getAttribute; e = e.parentNode) {
			var id = e.getAttribute("eventproxy") || e.id;
			var canvas = id && window[id];
			if (canvas && isc.isA.Canvas(canvas)) {
				return canvas;
			}
		}
		return null;
	}

	// ---------------------------------------------------------------- data entry

	/** ValuesManager.itemChanged(): the user changed a form item of a view. */
	function itemChanged(changedItem) {
		// a part of an item (a date item's text field) stands for the item
		var item = changedItem;
		while (item && item.parentItem) {
			item = item.parentItem;
		}
		if (!core || !core.active() || !item || !item.form || !item.form._view) {
			return;
		}
		if (isc.isA.BizLookupDescriptionItem && isc.isA.BizLookupDescriptionItem(item)) {
			return; // a lookup's pick is seen by its combo: see watchLookup()
		}
		for (var i = 0; i < changed.length; i++) {
			if (changed[i] === item) {
				return;
			}
		}
		changed.push(item);
	}

	/** The plain text of rich text HTML, a line for each line break or block, as the SAIL helper reads it. */
	function htmlLines(html) {
		var holder = document.createElement("div");
		holder.innerHTML = String(html == null ? "" : html);
		var blocks = /^(P|DIV|LI|UL|OL|H[1-6]|BLOCKQUOTE|PRE|TABLE|TR)$/;
		var out = "";
		var newLine = function () {
			if (out.length > 0 && out.charAt(out.length - 1) !== "\n") {
				out += "\n";
			}
		};
		var walk = function (node) {
			if (node.nodeType === 3) {
				var text = node.nodeValue.replace(/[\r\n]+/g, " ");
				out += (out.length === 0 || out.charAt(out.length - 1) === "\n") ? text.replace(/^\s+/, "") : text;
			} else if (node.nodeType === 1) {
				if (node.nodeName === "BR") {
					out += "\n";
					return;
				}
				var block = blocks.test(node.nodeName);
				if (block) {
					newLine();
				}
				for (var child = node.firstChild; child; child = child.nextSibling) {
					walk(child);
				}
				if (block) {
					newLine();
				}
			}
		};
		walk(holder);
		return out.replace(/ /g, " ").split("\n").map(function (line) {
			return line.replace(/\s+/g, " ").trim();
		}).join("\n").replace(/^\n+|\n+$/g, "");
	}

	/**
	 * The dataEnter step for a changed form item: { binding, value[, match] }, or { unmapped } for a widget phase 1
	 * does not record, or null if the item is gone.
	 */
	function dataEntry(item) {
		if (item.destroyed || !item.form || item.form.destroyed) {
			return null;
		}
		var binding = unsanitise(item.name);
		var value = item.getValue();
		if (isc.isA.PasswordItem && isc.isA.PasswordItem(item)) {
			return { unmapped: "typed into the password field " + binding + ": passwords are never recorded" };
		}
		if (isc.isA.SliderItem(item)) {
			return (value == null) ? { unmapped: "cleared " + binding } : { binding: binding, value: String(value) };
		}
		if (isc.isA.BizLookupDescriptionItem && isc.isA.BizLookupDescriptionItem(item)) {
			return lookupEntry(item, binding);
		}
		if (item.canvas && !isc.isA.RichTextItem(item)) {
			return { unmapped: "changed " + binding + " (a " + item.getClassName() + ", which SAIL cannot enter)" };
		}
		if (isc.isA.CheckboxItem(item)) {
			return (value === true || value === false) ? { binding: binding, value: String(value) } :
						{ unmapped: "cleared the check box " + binding };
		}
		var text;
		if (isc.isA.RichTextItem(item) || (isc.isA.BizHTMLItem && isc.isA.BizHTMLItem(item))) {
			text = htmlLines(value);
		} else if (item.dateTextField || item.timeTextField) {
			var field = item.dateTextField || item.timeTextField;
			text = field.getElementValue ? field.getElementValue() : item.getDisplayValue();
		} else if (item.getValueMap && item.getValueMap() && !isc.isA.TextItem(item)) {
			// combos and radios: the option's code, which does not change with the language or the options' order
			return (value == null || value === "") ? { unmapped: "cleared " + binding } : { binding: binding, value: String(value), match: "code" };
		} else if (isc.isA.Date(value)) {
			text = item.getDisplayValue();
		} else {
			text = (value == null) ? "" : String(value);
		}
		text = (text == null) ? "" : String(text);
		return (text.trim() === "") ? { unmapped: "cleared " + binding + " (SAIL cannot enter an empty value)" } : { binding: binding, value: text };
	}

	/** Writes the changed form items' final values as dataEnter steps. */
	function flushDataEntry() {
		var items = changed;
		changed = [];
		for (var i = 0; i < items.length; i++) {
			var item = items[i];
			var entry = dataEntry(item);
			if (entry) {
				var owner = item.form._view;
				var view = describe(owner);
				var screen = screenOf(owner);
				if (entry.unmapped) {
					core.push({ op: "unmapped", text: entry.unmapped, view: view, screen: screen });
				} else if (entry.op) {
					entry.view = view;
					entry.screen = screen;
					entry.label = strip(item.title || item.name).replace(/\s*\*$/, "");
					if (owner._sailEntered) {
						owner._sailEntered[item.name] = true;
					}
					core.push(entry);
				} else {
					var step = { op: "dataEnter", binding: entry.binding, value: entry.value, view: view, screen: screen,
									label: strip(item.title || item.name).replace(/\s*\*$/, "") };
					if (entry.match) {
						step.match = entry.match;
					}
					// what the field showed before the user changed it, from the last response's snapshot
					var shown = owner._sailShown;
					if (shown && Object.prototype.hasOwnProperty.call(shown, item.name)) {
						step.before = shown[item.name];
					}
					if (owner._sailEntered) {
						owner._sailEntered[item.name] = true;
					}
					core.push(step);
				}
			}
		}
	}

	// ---------------------------------------------------------------- context: what the screen showed

	var SHOWN_LENGTH = 60;

	function shorten(text) {
		return (text.length > SHOWN_LENGTH) ? text.substring(0, SHOWN_LENGTH) + "..." : text;
	}

	/** The text a form item shows, or null for an item whose value is not text (a grid, a map, an image). */
	function shownText(item) {
		var value = item.getValue();
		if (isc.isA.CheckboxItem(item)) {
			return (value == null) ? "" : String(value);
		}
		if (isc.isA.RichTextItem(item) || (isc.isA.BizHTMLItem && isc.isA.BizHTMLItem(item))) {
			return shorten(htmlLines(value).replace(/\n/g, " "));
		}
		if (item.canvas && !isc.isA.SliderItem(item) && !(isc.isA.BizLookupDescriptionItem && isc.isA.BizLookupDescriptionItem(item))) {
			return null;
		}
		if (item.dateTextField || item.timeTextField) {
			var field = item.dateTextField || item.timeTextField;
			return shorten(String((field.getElementValue ? field.getElementValue() : item.getDisplayValue()) || ""));
		}
		if ((item.getValueMap && item.getValueMap() && !isc.isA.TextItem(item)) ||
				(isc.isA.BizLookupDescriptionItem && isc.isA.BizLookupDescriptionItem(item))) {
			return shorten(strip(item.getDisplayValue()));
		}
		return (value == null) ? "" : shorten(isc.isA.Date(value) ? String(item.getDisplayValue()) : String(value));
	}

	/** What each bound form item of a view shows, by item name. */
	function shownValues(view) {
		var result = {};
		(isc.Canvas._canvasList || []).forEach(function (c) {
			if (c && !c.destroyed && (c._view === view) && isc.isA.DynamicForm(c)) {
				(c.getItems() || []).forEach(function (item) {
					var name = item.name;
					if (name && (name.charAt(0) !== "_") && !isc.isA.PasswordItem(item) && !isc.isA.StaticTextItem(item) &&
							!isc.isA.ButtonItem(item) && !isc.isA.SpacerItem(item)) {
						var text = shownText(item);
						if (text != null) {
							result[name] = text;
						}
					}
				});
			}
		});
		return result;
	}

	/**
	 * After a response for the same record: records, as context, the values the response changed on screen,
	 * leaving out what the user entered. Then keeps what the view shows for the next data entry and response.
	 */
	function recordChangedValues(view, values) {
		var before = view._sailShown;
		var sameRecord = before && values && (view._sailBizId === values.bizId);
		var entered = view._sailEntered || {};
		var now = shownValues(view);
		// the values the application showed (not the user's entries), which a later step may reuse (a capture)
		var shown = {};
		if (sameRecord) {
			var changes = [];
			Object.keys(now).forEach(function (name) {
				if (!entered[name] && Object.prototype.hasOwnProperty.call(before, name) && (before[name] !== now[name])) {
					changes.push(unsanitise(name) + " \"" + before[name] + "\" -> \"" + now[name] + "\"");
					if (now[name]) {
						shown[unsanitise(name)] = now[name];
					}
				}
			});
			if (changes.length) {
				core.push({ op: "context", text: "changed on screen: " + changes.join("; "), view: describe(view), screen: screenOf(view),
							shown: JSON.stringify(shown) });
			}
		} else {
			Object.keys(now).forEach(function (name) {
				if (now[name] && (now[name].indexOf("...") < 0)) {
					shown[unsanitise(name)] = now[name];
				}
			});
			if (Object.keys(shown).length) {
				core.push({ op: "context", view: describe(view), screen: screenOf(view), shown: JSON.stringify(shown) });
			}
		}
		view._sailShown = now;
		view._sailBizId = values ? values.bizId : null;
		view._sailEntered = {};
	}

	/** Records, as context, the messages and errors a response showed. */
	function recordShown(view, parts, errors) {
		var texts = parts.filter(function (text) {
			return !!text;
		});
		if (texts.length) {
			var step = { op: "context", text: "shown: " + texts.join("; "), view: describe(view), screen: screenOf(view) };
			// the action before showed errors: its replay must not expect success
			if (errors || texts.some(function (text) {
				return /^(error|fatal) /.test(text);
			})) {
				step.errors = "true";
			}
			core.push(step);
		}
	}

	/** Notes the field errors a view shows, when validation in the browser stops a request. */
	function recordErrors(view) {
		var all = (view._vm && view._vm.getErrors) ? view._vm.getErrors() : null;
		var errors = [];
		Object.keys(all || {}).forEach(function (name) {
			var message = all[name];
			errors.push(unsanitise(name) + " error \"" + strip(isc.isA.Array(message) ? message.join(" ") : message) + "\"");
		});
		recordShown(view, errors.length ? errors : ["errors"], true);
	}

	function messageTexts(growls, messages) {
		var result = [];
		(growls || []).forEach(function (g) {
			result.push((g.severity || "info") + " growl \"" + strip(g.summary) + "\"");
		});
		(messages || []).forEach(function (m) {
			result.push((m.severity || "info") + " message \"" + strip(m.summary) + "\"");
		});
		return result;
	}

	// ---------------------------------------------------------------- list grids

	/** The list a BizListGrid shows, for its step: {} for the list view's own grid, which the context names. */
	function gridList(listGrid) {
		if (isc.ListView && listGrid === isc.ListView._grid) {
			return {};
		}
		var ds = listGrid._dataSource || (listGrid.grid && listGrid.grid.dataSource);
		var id = ds && (ds.ID || ds);
		if (!isc.isA.String(id) || id.indexOf("_") < 0) {
			return {};
		}
		var underscore = id.indexOf("_");
		var result = { module: id.substring(0, underscore) };
		var rest = id.substring(underscore + 1);
		var model = rest.indexOf("__");
		if (model >= 0) {
			result.document = rest.substring(0, model);
			result.model = rest.substring(model + 2);
		} else {
			// a document's default query is named after the document when the module does not declare one
			var modoc = String((ds && ds.modoc) || "");
			if (modoc.substring(modoc.indexOf(".") + 1) === rest) {
				result.document = rest;
			} else {
				result.query = rest;
			}
		}
		return result;
	}

	/** Names a list grid for a comment. */
	function describeGrid(listGrid) {
		var list = gridList(listGrid);
		return list.module ? list.module + "." + (list.query || list.model || list.document) : "of the list";
	}

	/** The text each cell of a row shows, skipping the list view's tag and flag columns, as rowContaining reads it. */
	function rowTexts(grid, rowNum) {
		var record = grid.getRecord(rowNum);
		var fields = grid.getFields();
		var texts = [];
		for (var i = 0; i < fields.length; i++) {
			var name = String(fields[i].name || "");
			if (name !== "bizTagged" && name !== "bizFlagComment") {
				texts.push(record ? strip(grid.getCellValue(record, rowNum, i)) : "");
			}
		}
		return texts;
	}

	function contains(texts, text) {
		for (var i = 0; i < texts.length; i++) {
			if (texts[i].indexOf(text) >= 0) {
				return true;
			}
		}
		return false;
	}

	/**
	 * How a list grid row step names the row: { rowContaining } with the row's bizKey, or else its most distinctive
	 * cell, such that the first loaded row containing the text is this row; { row } only if no text does that.
	 */
	function rowLocator(grid, rowNum, record) {
		var data = grid.getData();
		var total = grid.getTotalRows();
		var rows = [];
		for (var r = 0; r < total; r++) {
			rows.push((!data || !data.rowIsLoaded || data.rowIsLoaded(r)) ? rowTexts(grid, r) : null);
		}
		var mine = rows[rowNum] || rowTexts(grid, rowNum);
		var candidates = [];
		var bizKey = record && record.bizKey ? strip(record.bizKey) : "";
		if (bizKey && contains(mine, bizKey)) {
			candidates.push(bizKey);
		}
		mine.forEach(function (text) {
			if (text && candidates.indexOf(text) < 0) {
				candidates.push(text);
			}
		});
		var best = null;
		var bestCount = 0;
		for (var c = 0; c < candidates.length; c++) {
			var text = candidates[c];
			var first = -1;
			var count = 0;
			for (var i = 0; i < rows.length; i++) {
				if (rows[i] && contains(rows[i], text)) {
					if (first < 0) {
						first = i;
					}
					count++;
				}
			}
			if (first === rowNum) {
				if (text === bizKey) {
					// the bizKey finds the row: it describes the record best
					return { rowContaining: text };
				}
				// otherwise the text the fewest rows show, then the leftmost column's (usually what names the row)
				if ((best == null) || (count < bestCount)) {
					best = text;
					bestCount = count;
				}
			}
		}
		return (best == null) ? { row: String(rowNum) } : { rowContaining: best };
	}

	// ---------------------------------------------------------------- lookups

	/** The lookup item a form (a lookup's own combo form) belongs to, or null. */
	function lookupOfForm(form) {
		var item = form && form.canvasItem;
		return (item && isc.isA.BizLookupDescriptionItem && isc.isA.BizLookupDescriptionItem(item)) ? item : null;
	}

	/**
	 * The step for a lookup the user picked from: lookupDescriptionAutoComplete when the user typed into it first
	 * (search what was typed, pick the description chosen), otherwise lookupDescriptionPick of the row containing the
	 * description chosen. Never a row number, and never dataEnter.
	 */
	function lookupEntry(item, binding) {
		var typed = lookupTyped[item.getID()];
		delete lookupTyped[item.getID()];
		var combo = item._form ? item._form.getItem("_combo") : null;
		var description = combo ? strip(combo.getDisplayValue()) : "";
		if ((item.getValue() == null) || !description) {
			return { unmapped: "cleared the lookup " + binding };
		}
		if (typed && typed.trim()) {
			return { op: "lookupDescriptionAutoComplete", binding: binding, search: typed, pick: description };
		}
		return { op: "lookupDescriptionPick", binding: binding, rowContaining: description };
	}

	/** Lookup combos whose changed() is wrapped, to undo when recording stops. */
	var watchedCombos = [];

	/**
	 * Wraps a lookup's combo's changed(), which runs when the user picks a row (the lookup then has a value) or types
	 * (its value is the text). A pick in a view's form is queued as the lookup's data entry; in an inline grid the
	 * grid's setEditValue() sees it.
	 */
	function watchLookup(item) {
		var combo = (item && item._form) ? item._form.getItem("_combo") : null;
		if (!combo || combo._sailWatched) {
			return;
		}
		var original = combo.changed;
		combo._sailWatched = true;
		combo._sailChanged = original;
		watchedCombos.push(combo);
		combo.changed = function (form, comboItem, value) {
			var result = isc.isA.Function(original) ? original.apply(this, arguments) : undefined;
			if (core && core.active()) {
				if ((value != null) && (this.getDisplayValue() == value)) {
					lookupTyped[item.getID()] = String(value);
				} else if ((item.getValue() != null) && item.form && item.form._view && (changed.indexOf(item) < 0)) {
					changed.push(item);
				}
			}
			return result;
		};
	}

	function unwatchLookups() {
		watchedCombos.forEach(function (combo) {
			if (!combo.destroyed) {
				combo.changed = combo._sailChanged;
				delete combo._sailChanged;
				delete combo._sailWatched;
			}
		});
		watchedCombos = [];
	}

	/** Keeps what the user types into a lookup's combo, for the autocomplete step its pick becomes. */
	function onInput(event) {
		if (!core || !core.active()) {
			return;
		}
		var target = event.target;
		if (!target || (target.tagName !== "INPUT")) {
			return;
		}
		var lookup = lookupOfForm(canvasOf(target));
		if (lookup) {
			lookupTyped[lookup.getID()] = target.value;
		}
	}

	// ---------------------------------------------------------------- data grids and list grid rows

	/** The BizDataGrid a ListGrid is the grid of, or null. */
	function dataGridOf(grid) {
		for (var g = grid; g; g = g.parentElement) {
			if (isc.isA.BizDataGrid(g)) {
				return (g.grid === grid) ? g : null;
			}
		}
		return null;
	}

	/** The BizListGrid a ListGrid is the grid of, or null. */
	function listGridOf(grid) {
		for (var g = grid; g; g = g.parentElement) {
			if (isc.isA.BizListGrid(g)) {
				return (g.grid === grid) ? g : null;
			}
		}
		return null;
	}

	/** What each cell of a row shows, by column binding, as JSON: what a later step may capture (captureCell). */
	function cellsOf(grid, rowNum) {
		var record = grid.getRecord(rowNum);
		var result = {};
		grid.getFields().forEach(function (field, i) {
			var name = String(field.name || "");
			if (record && name && (name.charAt(0) !== "_") && (name !== "bizTagged") && (name !== "bizFlagComment")) {
				var text = strip(grid.getCellValue(record, rowNum, i));
				if (text) {
					result[unsanitise(name)] = text;
				}
			}
		});
		return JSON.stringify(result);
	}

	/** Adds a row to a step: rowContaining (or row), the row's texts for the checks and its cells for captures. */
	function withRow(step, grid, rowNum) {
		var located = rowLocator(grid, rowNum, grid.getRecord(rowNum));
		if (located.rowContaining != null) {
			step.rowContaining = located.rowContaining;
		} else {
			step.row = located.row;
		}
		step.rowTexts = rowTexts(grid, rowNum).filter(function (text) {
			return !!text;
		}).join(" | ");
		step.cells = cellsOf(grid, rowNum);
		return step;
	}

	function dataGridStep(op, dataGrid, rowNum) {
		var step = { op: op, binding: unsanitise(dataGrid._b) };
		return (rowNum == null) ? step : withRow(step, dataGrid.grid, rowNum);
	}

	/** The row a data grid's tool acts on: the selected row, or else the row last clicked. */
	function selectedRow(owner) {
		var grid = owner.grid;
		var record = grid.getSelectedRecord ? grid.getSelectedRecord() : null;
		if (record) {
			return grid.getRecordIndex(record);
		}
		return (owner._eventRowNum != null) ? owner._eventRowNum : -1;
	}

	/** Records a step that acts on a grid, dropping a selection of the same grid's row that it makes needless. */
	function recordOnGrid(step, owner) {
		if (pendingSelect && (pendingSelect.owner === owner)) {
			pendingSelect = null;
		}
		return core.record(step);
	}

	/** Writes the cells the user changed in inline data grids as dataEnter steps into the grid's columns. */
	function flushGridEdits() {
		var edits = gridEdits;
		gridEdits = [];
		edits.forEach(function (edit) {
			var dataGrid = edit.dataGrid;
			var grid = dataGrid.grid;
			if (!grid || grid.destroyed) {
				return;
			}
			var displayFor = grid.getFields().some(function (f) {
				return f.displayField === edit.field;
			});
			if (displayFor) {
				return; // the description an inline lookup shows: its column's step says it
			}
			var editing = (grid.getEditRow() === edit.rowNum);
			var now = editing ? grid.getEditValue(edit.rowNum, grid.getFieldNum(edit.field)) :
							((grid.getRecord(edit.rowNum) || {})[edit.field]);
			if (sameValue(now, edit.original)) {
				return; // stored again unchanged, as the grid does for every editor when the row's edit ends
			}
			var form = editing && grid.getEditForm ? grid.getEditForm() : null;
			var item = form ? form.getItem(edit.field) : null;
			var column = grid.getField(edit.field);
			var entry;
			if (item && column && column.displayField && (column.displayField !== edit.field)) {
				var picked = strip(item.getDisplayValue());
				entry = picked ? { op: "lookupDescriptionPick", binding: unsanitise(edit.field), rowContaining: picked } :
									{ unmapped: "cleared the lookup " + unsanitise(edit.field) };
			} else {
				entry = item ? dataEntry(item) : committedEntry(grid, edit.rowNum, edit.field);
			}
			if (!entry) {
				return;
			}
			var view = describe(dataGrid._view);
			var screen = screenOf(dataGrid._view);
			if (entry.unmapped) {
				core.push({ op: "unmapped", text: entry.unmapped + " (in the data grid " + unsanitise(dataGrid._b) + ")", view: view, screen: screen });
				return;
			}
			var field = grid.getField(edit.field);
			var step = entry.op ? entry : { op: "dataEnter", binding: entry.binding, value: entry.value };
			if (entry.match) {
				step.match = entry.match;
			}
			step.grid = unsanitise(dataGrid._b);
			step.view = view;
			step.screen = screen;
			step.label = strip((field && field.title) || edit.field);
			core.push(step);
		});
	}

	/** True if two cell values are the same (empty values are the same, and dates by their time). */
	function sameValue(a, b) {
		var empty = function (v) {
			return (v == null) || (v === "");
		};
		if (empty(a) || empty(b)) {
			return empty(a) && empty(b);
		}
		if (isc.isA.Date(a) && isc.isA.Date(b)) {
			return a.getTime() === b.getTime();
		}
		return String(a) === String(b);
	}

	/** A dataEnter entry for a committed inline cell, from the row's value, as dataEntry() writes a field's. */
	function committedEntry(grid, rowNum, fieldName) {
		var record = (grid.getEditedRecord ? grid.getEditedRecord(rowNum) : null) || grid.getRecord(rowNum);
		var field = grid.getField(fieldName);
		var binding = unsanitise(fieldName);
		var value = record ? record[fieldName] : null;
		if ((value == null) || (value === "")) {
			return { unmapped: "cleared " + binding };
		}
		if ((value === true) || (value === false)) {
			return { binding: binding, value: String(value) };
		}
		if (field && field.displayField && (field.displayField !== fieldName)) {
			// an inline lookup: its column shows the description it picked
			var description = strip(record[field.displayField] || grid.getCellValue(record, rowNum, grid.getFieldNum(fieldName)));
			return description ? { op: "lookupDescriptionPick", binding: binding, rowContaining: description } : { unmapped: "cleared the lookup " + binding };
		}
		if (field && field.valueMap) {
			return { binding: binding, value: String(value), match: "code" };
		}
		if (isc.isA.Date(value)) {
			return { binding: binding, value: strip(grid.getCellValue(record, rowNum, grid.getFieldNum(fieldName))) };
		}
		return { binding: binding, value: String(value) };
	}

	/**
	 * A mouse button going down, before SmartClient acts on it (it answers a dialog, or runs a row's link, on the button
	 * going up, before the DOM's click): on a link in a data grid row, dataGridLink (an action link may ask for
	 * confirmation, so the step is what the isc.ask wrapper answers); on a server-side question's No or Cancel, the
	 * step that raised the question is declined.
	 */
	/** Notes that the user touched a list grid (its filter row or tools): its next fetch may be the user's filter. */
	function touch(event) {
		if (!core || !core.active() || core.isPanelEvent(event)) {
			return;
		}
		for (var c = canvasOf(event.target); c; c = c.parentElement) {
			if (isc.isA.BizListGrid(c)) {
				c._sailTouched = true;
				return;
			}
		}
	}

	function onMouseDown(event) {
		if (!core || !core.active() || core.isPanelEvent(event)) {
			return;
		}
		touch(event);
		confirmable = null;
		droppedOn = null;
		releaseLink();
		var target = event.target;
		var canvas = canvasOf(target);
		if (!canvas) {
			return;
		}
		if (isc.isA.Button(canvas) || isc.isA.StretchImgButton(canvas)) {
			for (var d = canvas.parentElement; d; d = d.parentElement) {
				if (isc.isA.Dialog(d)) {
					var answer = strip(canvas.getTitle ? canvas.getTitle() : canvas.title);
					// a server-side question is a Dialog of its own (isc.ask uses isc.Dialog.Warn, wrapped below)
					if ((d !== isc.Dialog.Warn) && (String(d.icon || "").indexOf("ask") >= 0) && ((answer === isc.Dialog.NO_BUTTON_TITLE) ||
							(answer === isc.Dialog.CANCEL_BUTTON_TITLE) || (answer === "No") || (answer === "Cancel"))) {
						core.push({ op: "context", text: "answered \"" + answer + "\" to \"" + strip(d.title) + "\"", declined: "true" });
					}
					return;
				}
			}
			return;
		}
		var link = target.closest ? target.closest("a[href]") : null;
		if (link && isc.isA.GridBody && isc.isA.GridBody(canvas)) {
			var dataGrid = dataGridOf(canvas.grid);
			var rowNum = canvas.getEventRow ? canvas.getEventRow() : -1;
			if (dataGrid && (rowNum != null) && (rowNum >= 0) && canvas.grid.getRecord(rowNum)) {
				var step = dataGridStep("dataGridLink", dataGrid, rowNum);
				step.text = strip(link.innerText || link.textContent);
				step.hold = true;
				heldLink = recordOnGrid(step, dataGrid);
				confirmable = heldLink;
			}
		}
	}

	/**
	 * A click, once SmartClient has selected the row: on a data grid row, a selection (dataGridSelect); on a row of a
	 * list grid whose selection does something, listGridSelect. A link in the row recorded its step going down.
	 */
	function onClick(event) {
		if (!core || !core.active() || core.isPanelEvent(event)) {
			return;
		}
		var target = event.target;
		var canvas = canvasOf(target);
		if (!canvas || !(isc.isA.GridBody && isc.isA.GridBody(canvas)) || (target.closest && target.closest("a[href]"))) {
			return;
		}
		var grid = canvas.grid;
		var dataGrid = dataGridOf(grid);
		var rowNum = canvas.getEventRow ? canvas.getEventRow() : -1;
		if (dataGrid) {
			if (droppedOn === dataGrid) {
				droppedOn = null; // the click that ended a row's drag
				return;
			}
			if ((rowNum == null) || (rowNum < 0) || !grid.getRecord(rowNum) || (grid.getEditRow && (grid.getEditRow() === rowNum))) {
				return;
			}
			pendingSelect = { owner: dataGrid, step: dataGridStep("dataGridSelect", dataGrid, rowNum) };
			core.handled();
			return;
		}
		var listGrid = listGridOf(grid);
		if (listGrid && (listGrid.selectedIdBinding || listGrid.bizSelected) && (rowNum != null) && (rowNum >= 0) && grid.getRecord(rowNum)) {
			var select = gridList(listGrid);
			select.op = "listGridSelect";
			pendingSelect = { owner: listGrid, step: withRow(select, grid, rowNum) };
			core.handled();
		}
	}

	/** Writes the pending selection, the inline grid cells and the form fields the user changed. */
	function flush() {
		releaseLink();
		flushGridEdits();
		flushDataEntry();
		if (pendingSelect) {
			var select = pendingSelect.step;
			pendingSelect = null;
			core.push(select);
		}
	}

	// ---------------------------------------------------------------- list grid filters and sorting

	/** SmartClient's filter operators and the listGridFilter operators they are. */
	var OPERATORS = { iContains: "contains", contains: "contains", iStartsWith: "startsWith", startsWith: "startsWith",
						equals: "equals", iEquals: "equals", notEqual: "notEquals", iNotEqual: "notEquals",
						lessThan: "lessThan", greaterThan: "greaterThan" };
	var MONTHS = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

	function pad(n) {
		return (n < 10) ? "0" + n : String(n);
	}

	/** A filter value as listGridFilter writes it: an enum's label, a boolean, a date as Skyve's default converter. */
	function filterText(grid, fieldName, value) {
		var field = grid.getField(fieldName) || {};
		if ((value === true) || (value === false)) {
			return String(value);
		}
		if (field.valueMap && !isc.isA.Array(field.valueMap) && (field.valueMap[value] != null)) {
			return strip(field.valueMap[value]);
		}
		if (isc.isA.Date(value)) {
			var text = pad(value.getDate()) + "-" + MONTHS[value.getMonth()] + "-" + value.getFullYear();
			if (field.type === "datetime") {
				text += " " + pad(value.getHours()) + ":" + pad(value.getMinutes());
			} else if (field.type === "time") {
				text = pad(value.getHours()) + ":" + pad(value.getMinutes());
			}
			return text;
		}
		return String(value);
	}

	/**
	 * The filter a BizListGrid shows, as { ok, columns: { field: { operator, value } } }: ok is false when it cannot be
	 * written as per-column filters ANDed together (an OR, a nested clause, two clauses on one column, an operator
	 * listGridFilter has not).
	 */
	function filterState(listGrid) {
		var grid = listGrid.grid;
		var advanced = listGrid._advancedFilter && listGrid._advancedFilter.toggleButton && listGrid._advancedFilter.toggleButton.selected;
		var criteria = advanced ? listGrid._advancedFilter.getCriteria() : (grid.getFilterEditorCriteria ? grid.getFilterEditorCriteria(true) : null);
		var result = { ok: true, columns: {} };
		if (!criteria) {
			return result;
		}
		var add = function (fieldName, operator, value) {
			if ((fieldName == null) || (value == null) || (value === "") || (fieldName === "bizTagged") || (fieldName === "bizFlagComment")) {
				return;
			}
			var mapped = OPERATORS[operator];
			if (!mapped || result.columns[fieldName] || isc.isA.Array(value)) {
				result.ok = false;
				return;
			}
			result.columns[fieldName] = { operator: mapped, value: filterText(grid, fieldName, value) };
		};
		if (criteria.criteria || (criteria._constructor === "AdvancedCriteria")) {
			if (criteria.operator && (criteria.operator !== "and")) {
				result.ok = false;
				return result;
			}
			(criteria.criteria || []).forEach(function (clause) {
				if (clause.criteria) {
					result.ok = false;
				} else {
					add(clause.fieldName, clause.operator, clause.value);
				}
			});
		} else {
			Object.keys(criteria).forEach(function (fieldName) {
				var field = grid.getField(fieldName) || {};
				var text = !field.type || (field.type === "text") || (field.type === "string");
				add(fieldName, (text && !field.valueMap) ? "iContains" : "equals", criteria[fieldName]);
			});
		}
		return result;
	}

	/** True if the gesture being handled is on the list grid (its filter row, toolbar or advanced filter). */
	function gestureOn(owner) {
		var target = core.gestureTarget();
		var canvas = target ? canvasOf(target) : null;
		return !!canvas && ((canvas === owner) || owner.contains(canvas, true));
	}

	/** A list grid is about to fetch: records how the user changed its filter, column by column. */
	function filtered(listGrid) {
		var before = listGrid._sailFilter || { ok: true, columns: {} };
		var now = filterState(listGrid);
		listGrid._sailFilter = now;
		// the filter row can fetch a moment after the user typed (a delayed fetch), outside the gesture
		var touched = listGrid._sailTouched;
		delete listGrid._sailTouched;
		if (!gestureOn(listGrid) && !touched) {
			return;
		}
		var list = function (op) {
			var step = gridList(listGrid);
			step.op = op;
			return step;
		};
		if (!now.ok) {
			core.unmapped("filtered the list " + describeGrid(listGrid) + " in a way SAIL cannot write as column filters ANDed together");
			return;
		}
		var names = Object.keys(now.columns);
		var old = Object.keys(before.columns);
		if (!names.length && old.length) {
			recordOnGrid(list("listGridClearFilter"), listGrid);
			return;
		}
		names.forEach(function (name) {
			var was = before.columns[name];
			var is = now.columns[name];
			if (!was || (was.operator !== is.operator) || (was.value !== is.value)) {
				var step = list("listGridFilter");
				step.column = unsanitise(name);
				step.value = is.value;
				if (is.operator !== "contains") {
					step.operator = is.operator;
				}
				recordOnGrid(step, listGrid);
			}
		});
		old.forEach(function (name) {
			if (!now.columns[name]) {
				var step = list("listGridFilter");
				step.column = unsanitise(name);
				recordOnGrid(step, listGrid);
			}
		});
	}

	// ---------------------------------------------------------------- trees, maps and calendars

	/** What a list-like menu item or page names: { document[, model] } or { query } (a document's default query). */
	function listTarget(module, name) {
		var model = name.indexOf("__");
		if (model >= 0) {
			return { document: name.substring(0, model), model: name.substring(model + 2) };
		}
		var ds = isc.DataSource.get(module + "_" + name);
		var modoc = String((ds && ds.modoc) || "");
		if (modoc.substring(modoc.indexOf(".") + 1) === name) {
			return { document: name };
		}
		return /^[A-Z]/.test(name) ? { document: name } : { query: name };
	}

	/** The step for a tree, map or calendar menu item, or null for any other item. */
	function listMenuStep(module, leaf) {
		var op = { tree: "navigateTree", map: "navigateMap", cal: "navigateCalendar" }[leaf.ref];
		if (!op) {
			return null;
		}
		var name = String(leaf.name || "");
		var step = { op: op, module: module };
		if (op === "navigateMap") {
			var underscore = name.lastIndexOf("_");
			if (underscore > 0) {
				step.geometryBinding = name.substring(underscore + 1);
				name = name.substring(0, underscore);
			}
		}
		var target = listTarget(module, name);
		Object.keys(target).forEach(function (key) {
			step[key] = target[key];
		});
		return step;
	}

	// ---------------------------------------------------------------- the view's title

	/** The title the top view shows, as captureTitle reads it. */
	function viewTitle() {
		var view = topView();
		var header = (view && describe(view) === "list") ? isc.ListView._header : (view && view._header);
		var member = header && header.getMember ? header.getMember(0) : null;
		var element = member && member.getHandle ? member.getHandle() : null;
		var title = element ? element.querySelector(".titleBar:not(i)") : null;
		return title ? strip(title.innerText || title.textContent) : (element ? strip(element.innerText) : "");
	}

	// ---------------------------------------------------------------- the wrapped methods

	function recordMenu(viewer, leaf) {
		var labels = [];
		var tree = viewer.data;
		for (var node = leaf; node && node !== tree.getRoot(); node = tree.getParent(node)) {
			labels.unshift(strip(node.desc));
		}
		var module = tree.getRoot().name;
		var listStep = listMenuStep(leaf.module || module, leaf);
		if (listStep) {
			core.record(listStep);
			return;
		}
		var step = { op: "navigateMenu", module: module, item: labels.join("/") };
		if (leaf.ref === "edit") {
			step.target = (leaf.module || module) + "." + leaf.name;
		}
		core.record(step);
	}

	/** Records what a BizButton's action does, before it does it. Returns the step, or null. */
	function recordButton(button) {
		var name = button.actionName;
		if (CONTROL_ACTIONS[name]) {
			return null;
		}
		var op = BUTTON_STEPS[button.type];
		if (op) {
			return core.record({ op: op });
		}
		if (!button.type || ACTION_TYPES[button.type]) {
			return core.record({ op: "action", name: name });
		}
		if (button.type === "Upload") {
			// the core records the upload when the upload page sends the file
			core.handled();
			return null;
		}
		return core.unmapped("pressed " + strip(button.title || name) + " (a " + button.type + " action, not recorded yet)");
	}

	function wrapButton(button) {
		if (button._sailAction || !isc.isA.Function(button.action)) {
			return;
		}
		var action = button.action;
		button._sailAction = action;
		button.action = function () {
			if (!core.active()) {
				return action.apply(this, arguments);
			}
			if (this.actionName === STOP_ACTION) {
				// the server builds the script when this action reaches it: send every step first
				var me = this;
				var args = arguments;
				flushDataEntry();
				core.whenSent(function () {
					core.stop(true);
					action.apply(me, args);
				});
				return undefined;
			}
			// a click that a field's blur swallowed is pressed again once the blur's server call returns: record that one
			var swallowed = this._view && this._view._blurry;
			core.beginHold();
			try {
				confirmable = swallowed ? null : recordButton(this);
				return action.apply(this, arguments);
			} finally {
				confirmable = null;
				core.endHold();
			}
		};
	}

	function unwrapButtons() {
		(isc.Canvas._canvasList || []).forEach(function (c) {
			if (c && c._sailAction) {
				c.action = c._sailAction;
				delete c._sailAction;
			}
		});
	}

	function attach(recorder) {
		core = recorder;
		var wrap = core.wrap;
		wrap(isc.BizUtil, "handleMenuItemClick", function (original) {
			return function (viewer, leaf) {
				if (core.active()) {
					recordMenu(viewer, leaf);
				}
				return original.apply(this, arguments);
			};
		});

		var buttonProto = isc.BizButton.getPrototype();
		wrap(buttonProto, "initWidget", function (original) {
			return function () {
				var result = original.apply(this, arguments);
				if (core.active()) {
					wrapButton(this);
				}
				return result;
			};
		});
		(isc.Canvas._canvasList || []).forEach(function (c) {
			if (c && !c.destroyed && isc.isA.BizButton(c)) {
				wrapButton(c);
			}
		});
		core.onDetach(unwrapButtons);

		wrap(isc.BizTabPane.getPrototype(), "tabSelected", function (original) {
			return function (tabNum) {
				// a tab the page selects by itself (a server response, a view opening) is not a gesture
				var target = core.gestureTarget();
				var on = target ? canvasOf(target) : null;
				if (core.active() && on && this.tabBar && this.tabBar.contains(on, true)) {
					var titles = [];
					var tab = this.getTabObject(tabNum);
					titles.unshift(strip(tab ? tab.title : ""));
					for (var p = this.parentElement; p; p = p.parentElement) {
						if (isc.isA.TabSet(p) && p.getSelectedTab) {
							var outer = p.getSelectedTab();
							if (outer) {
								titles.unshift(strip(outer.title));
							}
						}
					}
					core.record({ op: "tabSelect", path: titles.join("::") });
				}
				return original.apply(this, arguments);
			};
		});

		wrap(isc.BizListGrid.getPrototype(), "zoom", function (original) {
			return function (zoomToNew) {
				if (core.active()) {
					var step = gridList(this);
					if (zoomToNew) {
						step.op = "listGridNew";
					} else {
						var located = rowLocator(this.grid, this._eventRowNum, this._eventRecord);
						step.op = "listGridZoom";
						var eventRecord = this._eventRecord;
						if (eventRecord && eventRecord.bizKey) {
							step.rowBizKey = strip(eventRecord.bizKey);
						}
						step.rowTexts = rowTexts(this.grid, this._eventRowNum).filter(function (text) {
							return !!text;
						}).join(" | ");
						if (located.rowContaining != null) {
							step.rowContaining = located.rowContaining;
						} else {
							step.row = located.row;
						}
						step.cells = cellsOf(this.grid, this._eventRowNum);
					}
					recordOnGrid(step, this);
				}
				return original.apply(this, arguments);
			};
		});

		attachPhase5();

		var editViewProto = isc.EditView.getPrototype();
		wrap(editViewProto, "scatter", function (original) {
			return function (values) {
				// a server response replaces the values: the user's entries so far are final
				if (core.active()) {
					releaseLink();
					flushDataEntry();
				}
				var result = original.apply(this, arguments);
				if (core.active()) {
					this._sailViewType = (values && values.created) ? "edit" : "create";
					recordChangedValues(this, values);
				}
				return result;
			};
		});
		wrap(editViewProto, "_displayGrowlsAndMessages", function (original) {
			return function (growls, messages) {
				// growls are noted by isc.BizUtil.growl, below
				if (core.active()) {
					recordShown(this, messageTexts(null, messages));
				}
				return original.apply(this, arguments);
			};
		});
		wrap(isc.BizUtil, "growl", function (original) {
			return function (growls) {
				if (core.active()) {
					recordShown(topView(), messageTexts(isc.isA.Array(growls) ? growls : null, null));
				}
				return original.apply(this, arguments);
			};
		});
		wrap(editViewProto, "_displayFailure", function (original) {
			return function (dsResponse, data) {
				if (core.active() && dsResponse) {
					var status = dsResponse.status;
					if ((status === -1) || (status === isc.EditView._UNAVAILABLE)) {
						// the growls are noted by isc.BizUtil.growl
						recordShown(this, messageTexts(null, dsResponse._messages).concat(["problem \"" + shorten(strip(data)) + "\""]), true);
					} else if (dsResponse.errors) {
						var errors = [];
						var all = isc.isA.Array(dsResponse.errors) ? dsResponse.errors[0] : dsResponse.errors;
						Object.keys(all || {}).forEach(function (name) {
							errors.push(unsanitise(name) + " error \"" + strip(all[name]) + "\"");
						});
						recordShown(this, errors, true);
					}
				}
				return original.apply(this, arguments);
			};
		});
		wrap(editViewProto, "gather", function (original) {
			return function (validate) {
				var result = original.apply(this, arguments);
				if (core.active() && validate && !result) {
					recordErrors(this);
				}
				return result;
			};
		});

		var vmProto = isc.ValuesManager.getPrototype();
		var hadItemChanged = Object.prototype.hasOwnProperty.call(vmProto, "itemChanged");
		var vmItemChanged = vmProto.itemChanged;
		vmProto.itemChanged = function (item) {
			itemChanged(item);
			if (isc.isA.Function(vmItemChanged)) {
				return vmItemChanged.apply(this, arguments);
			}
			return undefined;
		};
		core.onDetach(function () {
			if (hadItemChanged) {
				vmProto.itemChanged = vmItemChanged;
			} else {
				delete vmProto.itemChanged;
			}
		});

		wrap(isc, "ask", function (original) {
			return function (message, callback) {
				var step = confirmable;
				confirmable = null;
				if (step && (step === heldLink)) {
					heldLink = null; // the answer below lets it go
				}
				var args = Array.prototype.slice.call(arguments);
				if (core.active() && step && isc.isA.Function(callback)) {
					step.hold = true;
					args[1] = function (value) {
						delete step.hold;
						if (!value) {
							if (step.op === "cancel") {
								step.op = "unmapped";
								step.text = "pressed Cancel and kept the unsaved changes";
							} else {
								step.confirm = "false";
							}
						}
						core.send();
						return callback.apply(this, arguments);
					};
				}
				return original.apply(this, args);
			};
		});
	}

	/** Wraps the data grid, list grid, lookup and upload methods, and listens for row clicks and lookup typing. */
	function attachPhase5() {
		var wrap = core.wrap;
		var dataGridProto = isc.BizDataGrid.getPrototype();
		wrap(dataGridProto, "zoom", function (original) {
			return function (zoomToNew) {
				if (core.active()) {
					if (zoomToNew) {
						recordOnGrid(dataGridStep("dataGridNew", this, null), this);
					} else {
						var rowNum = selectedRow(this);
						if (rowNum >= 0) {
							recordOnGrid(dataGridStep("dataGridZoom", this, rowNum), this);
						}
					}
				}
				return original.apply(this, arguments);
			};
		});
		wrap(dataGridProto, "add", function (original) {
			return function () {
				if (core.active()) {
					recordOnGrid(dataGridStep("dataGridNew", this, null), this);
					// add() starts editing the new row when the server returns it: not a dataGridEdit
					this._sailAdding = true;
				}
				return original.apply(this, arguments);
			};
		});
		wrap(dataGridProto, "startEditingRow", function (original) {
			return function (rowNum) {
				if (this._sailAdding) {
					delete this._sailAdding;
				} else if (core.active() && (rowNum != null) && (rowNum >= 0) && this.grid.getRecord(rowNum)) {
					recordOnGrid(dataGridStep("dataGridEdit", this, rowNum), this);
				}
				return original.apply(this, arguments);
			};
		});

		var gridProto = isc.ListGrid.getPrototype();
		wrap(gridProto, "removeSelectedData", function (original) {
			return function () {
				var dataGrid = core.active() ? dataGridOf(this) : null;
				if (dataGrid) {
					var rowNum = selectedRow(dataGrid);
					if (rowNum >= 0) {
						recordOnGrid(dataGridStep("dataGridRemove", dataGrid, rowNum), dataGrid);
					}
				}
				return original.apply(this, arguments);
			};
		});
		wrap(gridProto, "recordDrop", function (original) {
			return function (dropRecords) {
				var dataGrid = core.active() ? dataGridOf(this) : null;
				var record = (dataGrid && dropRecords && dropRecords.length) ? dropRecords[0] : null;
				var step = record ? dataGridStep("dataGridMove", dataGrid, this.getRecordIndex(record)) : null;
				var result = original.apply(this, arguments);
				if (step) {
					var to = this.getRecordIndex(record);
					droppedOn = dataGrid;
					if (to >= 0) {
						step.to = String(to);
						recordOnGrid(step, dataGrid);
					}
				}
				return result;
			};
		});
		// the grid stores each inline editor's value with setEditValue (its private _editorChange is not in every build)
		wrap(gridProto, "setEditValue", function (original) {
			return function (rowNum, colNum) {
				var dataGrid = core.active() ? dataGridOf(this) : null;
				if (dataGrid && (rowNum != null) && (rowNum >= 0)) {
					var field = isc.isA.String(colNum) ? colNum : this.getFieldName(colNum);
					var known = gridEdits.some(function (edit) {
						return (edit.dataGrid === dataGrid) && (edit.rowNum === rowNum) && (edit.field === field);
					});
					if (field && !known) {
						var record = this.getRecord(rowNum);
						gridEdits.push({ dataGrid: dataGrid, rowNum: rowNum, field: field, original: record ? record[field] : null });
					}
				}
				return original.apply(this, arguments);
			};
		});
		wrap(gridProto, "handleSortChanged", function (original) {
			return function (sortSpecifiers) {
				var listGrid = core.active() ? listGridOf(this) : null;
				if (listGrid && gestureOn(listGrid)) {
					var specs = (sortSpecifiers || []).filter(function (spec) {
						return spec && spec.property;
					});
					if (specs.length === 1) {
						var step = gridList(listGrid);
						step.op = "listGridSort";
						step.column = unsanitise(specs[0].property);
						if (specs[0].direction === "descending") {
							step.descending = "true";
						}
						recordOnGrid(step, listGrid);
					} else {
						core.unmapped(specs.length ? "sorted the list " + describeGrid(listGrid) + " by more than one column" :
															"cleared the sort of the list " + describeGrid(listGrid));
					}
				}
				return original.apply(this, arguments);
			};
		});

		// a BizListGrid's grid fetches through its own filterData, which calls this one (Super): a filter row submit, the
		// advanced filter's Filter button and a refresh (Clear filter) all do
		wrap(gridProto, "filterData", function (original) {
			return function () {
				var listGrid = core.active() ? listGridOf(this) : null;
				if (listGrid) {
					filtered(listGrid);
				}
				return original.apply(this, arguments);
			};
		});

		wrap(isc.BizLookupDescriptionItem.getPrototype(), "init", function (original) {
			return function () {
				var result = original.apply(this, arguments);
				if (core.active()) {
					watchLookup(this);
				}
				return result;
			};
		});
		(isc.Canvas._canvasList || []).forEach(function (c) {
			if (c && !c.destroyed && isc.isA.DynamicForm(c) && lookupOfForm(c)) {
				watchLookup(lookupOfForm(c));
			}
		});
		core.onDetach(unwatchLookups);

		wrap(isc.BizLookupDescriptionItem.getPrototype(), "zoom", function (original) {
			return function (zoomToNew) {
				if (core.active()) {
					var step = { op: zoomToNew ? "lookupDescriptionNew" : "lookupDescriptionEdit", binding: unsanitise(this.name) };
					var dataGrid = this.grid ? dataGridOf(this.grid) : null;
					if (dataGrid) {
						step.grid = unsanitise(dataGrid._b);
					}
					core.record(step);
				}
				return original.apply(this, arguments);
			};
		});
		wrap(isc.BizUtil, "openContentUpload", function (original) {
			return function () {
				if (core.active()) {
					// the core records the upload when the upload page sends the file
					core.handled();
				}
				return original.apply(this, arguments);
			};
		});

		core.listen(document, "input", onInput, true);
		core.listen(document, "mousedown", onMouseDown, true);
		core.listen(document, "keydown", touch, true);
		core.listen(document, "click", onClick, true);
	}

	// ---------------------------------------------------------------- unmapped clicks

	/** Describes the control a click landed on, or null if it was not on a control (a field, a label, the page). */
	function control(element) {
		var link = element && element.closest ? element.closest("a[href]") : null;
		if (link) {
			return "link \"" + strip(link.textContent) + "\" (" + link.getAttribute("href") + ")";
		}
		var canvas = canvasOf(element);
		for (var c = canvas; c; c = c.parentElement) {
			if (isc.isA.Dialog(c) || (isc.isA.TabBar && isc.isA.TabBar(c)) || (c.ID === "adminStack") || isc.isA.DynamicForm(c)) {
				// dialogs answer a step's question; tabs and the menu record their own steps; fields are data entry
				return null;
			}
			if (isc.isA.BizDataGrid(c)) {
				// a data grid's rows and tools record their steps (a selection, a remove after its confirmation)
				return null;
			}
			if (c.parentElement && (c.parentElement.filterEditor === c)) {
				return null; // a list grid's filter row: its filter is recorded when the grid fetches
			}
			if (isc.isA.Window(c) && c.header && c.header.contains(canvas, true)) {
				return null; // a window's close box
			}
		}
		if (!canvas || (canvas.isDisabled && canvas.isDisabled())) {
			return null;
		}
		if (isc.isA.Menu(canvas)) {
			var text = strip(element.textContent);
			return text ? "menu item \"" + text + "\"" : null;
		}
		if (isc.isA.GridBody && isc.isA.GridBody(canvas)) {
			// a row click selects; it is a gesture of its own on grids that act on a selection, and on data grids
			return null; // a row click selects: recorded as a selection where it matters
		}
		if (isc.isA.Button(canvas) || isc.isA.StretchImgButton(canvas) || isc.isA.ImgButton(canvas)) {
			var title = strip(canvas.title || canvas.prompt || element.textContent);
			return canvas.getClassName() + " #" + canvas.getID() + (title ? " \"" + title + "\"" : "");
		}
		return null;
	}

	// ---------------------------------------------------------------- starting and stopping

	/** Records the page load as a navigation, when its URL opened a view. */
	function recordPageLoad() {
		var params = {};
		location.search.replace(/^\?/, "").split("&").forEach(function (pair) {
			var equals = pair.indexOf("=");
			if (equals > 0) {
				params[decodeURIComponent(pair.substring(0, equals))] = decodeURIComponent(pair.substring(equals + 1).replace(/\+/g, " "));
			}
		});
		if (!params.m) {
			return;
		}
		if (params.a === "e" && params.d) {
			var edit = { op: "navigateEdit", module: params.m, document: params.d, target: params.m + "." + params.d, view: "" };
			if (params.i) {
				edit.bizId = params.i;
			}
			core.push(edit);
		} else if (params.a === "l" && params.q) {
			var list = { op: "navigateList", module: params.m, view: "" };
			if (params.d) {
				list.document = params.d;
				list.model = params.q;
			} else {
				// the page defines its data sources after this script: resolve q once connected
				list.query = params.q;
				pageList = list;
			}
			core.push(list);
		} else if (((params.a === "t") || (params.a === "m") || (params.a === "c")) && params.q) {
			var step = { op: { t: "navigateTree", m: "navigateMap", c: "navigateCalendar" }[params.a], module: params.m, view: "" };
			if (params.d) {
				step.document = params.d;
				step.model = params.q;
			} else {
				step.query = params.q;
			}
			if (params.b) {
				step.geometryBinding = params.b;
			}
			core.push(step);
		} else if (params.a) {
			core.push({ op: "unmapped", text: "opened " + location.search + " (not recorded yet)", view: "" });
		}
	}

	/**
	 * Names the page load's list by document when q named a document (its default query), as the data source the
	 * page defined for it shows: a data source module_q driven by module.q.
	 */
	function resolvePageList() {
		var list = pageList;
		pageList = null;
		if (list) {
			var ds = isc.DataSource.get(list.module + "_" + list.query);
			var modoc = String((ds && ds.modoc) || "");
			if (modoc.substring(modoc.indexOf(".") + 1) === list.query) {
				list.document = list.query;
				delete list.query;
			}
		}
	}

	// ---------------------------------------------------------------- Check mode

	/** A rectangle in the window from a SmartClient page rectangle [left, top, width, height]. */
	function windowRect(pageRect) {
		return { left: pageRect[0] - isc.Page.getScrollLeft(), top: pageRect[1] - isc.Page.getScrollTop(),
					width: pageRect[2], height: pageRect[3] };
	}

	/** The form item of a view under the pointer: the item whose rectangle holds it. */
	function itemAt(form, event) {
		var items = form.getItems() || [];
		for (var i = 0; i < items.length; i++) {
			var item = items[i];
			if (item.isVisible && item.isVisible() && item.isDrawn && item.isDrawn() && item.getPageRect) {
				var r = item.getPageRect();
				if (r && (event.pageX >= r[0]) && (event.pageX <= r[0] + r[2]) && (event.pageY >= r[1]) && (event.pageY <= r[1] + r[3])) {
					return item;
				}
			}
		}
		return null;
	}

	/**
	 * A check of a field: testMessage with the validation error it shows, or else testValue with what it shows, as
	 * dataEnter would enter it.
	 */
	function fieldCheck(item) {
		while (item.parentItem) {
			item = item.parentItem;
		}
		if (!item.name || (item.name.charAt(0) === "_") || !item.form || !item.form._view) {
			return null;
		}
		var view = item.form._view;
		// a field showing a validation error checks the error, which SAIL reads as a message
		var errors = item.form.getFieldErrors ? item.form.getFieldErrors(item.name) : null;
		var error = strip(isc.isA.Array(errors) ? errors[0] : errors);
		if (error) {
			return { rect: windowRect(item.getPageRect()), label: "testMessage \"" + shorten(error) + "\"",
						step: { op: "testMessage", contains: error, view: describe(view), screen: screenOf(view) } };
		}
		var entry = dataEntry(item);
		if (!entry || entry.unmapped) {
			return null;
		}
		return { rect: windowRect(item.getPageRect()), label: "testValue " + entry.binding,
					step: { op: "testValue", binding: entry.binding, value: entry.value, view: describe(view), screen: screenOf(view),
							label: strip(item.title || item.name).replace(/\s*\*$/, "") } };
	}

	/**
	 * A check of a grid: on a row, testGridRows that a row containing its most distinctive text is there; elsewhere on
	 * the grid, testGridRows of its row count.
	 */
	function gridCheck(owner, event) {
		var grid = owner.grid;
		var body = grid && grid.body;
		if (!body) {
			return null;
		}
		var step = isc.isA.BizDataGrid(owner) ? { binding: unsanitise(owner._b) } : gridList(owner);
		step.op = "testGridRows";
		var row = -1;
		if (event.target && body.getHandle() && body.getHandle().contains(event.target)) {
			row = body.getEventRow(event.pageY - body.getPageTop() + body.getScrollTop());
		}
		var data = grid.getData();
		if ((row >= 0) && grid.getRecord(row) && (!data || !data.rowIsLoaded || data.rowIsLoaded(row))) {
			var located = rowLocator(grid, row, grid.getRecord(row));
			if (located.rowContaining == null) {
				return null;
			}
			step.rowContaining = located.rowContaining;
			step.minRows = "1";
			return { rect: { left: body.getPageLeft() - isc.Page.getScrollLeft(), top: body.getRowPageTop(row) - isc.Page.getScrollTop(),
								width: body.getVisibleWidth(), height: body.getRowSize(row) },
						label: "testGridRows containing \"" + located.rowContaining + "\"", step: step };
		}
		var count = grid.getTotalRows();
		step.minRows = String(count);
		step.maxRows = String(count);
		return { rect: windowRect(owner.getPageRect()), label: "testGridRows " + count + " rows", step: step };
	}

	/**
	 * What a click in Check mode checks at the pointer: a growl or dialog message (testMessage), a field (testValue)
	 * or a grid (testGridRows). Returns { rect, label, step } or null.
	 */
	function checkTarget(element, event) {
		var growl = (element && element.closest) ? element.closest(".ui-growl-item") : null;
		if (growl) {
			var title = growl.querySelector(".ui-growl-title");
			var text = strip(title ? title.textContent : growl.textContent);
			var g = growl.getBoundingClientRect();
			return text ? { rect: { left: g.left, top: g.top, width: g.width, height: g.height }, label: "testMessage \"" + text + "\"",
								step: { op: "testMessage", contains: text } } : null;
		}
		var canvas = canvasOf(element);
		for (var c = canvas; c; c = c.parentElement) {
			if (isc.isA.Dialog(c)) {
				var message = strip(c.message);
				return message ? { rect: windowRect(c.getPageRect()), label: "testMessage \"" + shorten(message) + "\"",
									step: { op: "testMessage", contains: message } } : null;
			}
			if (isc.isA.BizListGrid(c) || isc.isA.BizDataGrid(c)) {
				return gridCheck(c, event);
			}
			if (isc.isA.DynamicForm(c) && c._view) {
				var item = itemAt(c, event);
				return item ? fieldCheck(item) : null;
			}
		}
		return null;
	}

	// ---------------------------------------------------------------- the adapter

	/** Sends a request through SmartClient's RPCManager, so that the page counts as busy until it returns. */
	function transport(method, url, body, callback) {
		var request = {
			actionURL: url,
			httpMethod: method,
			useSimpleHttp: true,
			serverOutputAsString: true,
			showPrompt: false,
			willHandleError: true,
			callback: function (response, data) {
				callback(response.httpResponseCode, data);
			}
		};
		if (body != null) {
			request.contentType = "application/json";
			request.data = body;
		}
		isc.RPCManager.sendRequest(request);
	}

	/** The header menu's Logout item, which signs out (and so ends the recording). */
	function isSignOut(element) {
		var canvas = canvasOf(element);
		return !!canvas && isc.isA.Menu(canvas) && /^\s*Logout\s*$/.test(strip(element.textContent || ""));
	}

	var recorder = SKYVE.SailRecorderCore.create({
		transport: transport,
		attach: attach,
		flush: flush,
		describe: function () {
			var view = topView();
			return { view: describe(view), screen: screenOf(view), title: viewTitle() };
		},
		connected: resolvePageList,
		control: control,
		checkTarget: checkTarget,
		isSignOut: isSignOut,
		signOut: function () {
			window.location = "loggedOut";
		},
		idle: function () {
			// a pending selection is not data in flight: the next step decides whether it is written
			return (changed.length === 0) && (gridEdits.length === 0);
		},
		reset: function () {
			heldLink = null;
			changed = [];
			confirmable = null;
			pageList = null;
			pendingSelect = null;
			gridEdits = [];
			lookupTyped = {};
		}
	});
	SKYVE.SailRecorder = recorder;

	// Loaded with the page (not by the Control Panel's Start): the URL that opened it is the user's navigation
	var duringPageLoad = (document.readyState !== "complete");
	if (duringPageLoad && !document.body) {
		// the panel needs the body: start once the page has parsed it
		document.addEventListener("DOMContentLoaded", function () {
			recorder.start();
			recordPageLoad();
		});
	} else {
		recorder.start();
		if (duringPageLoad) {
			recordPageLoad();
		}
	}
})();
