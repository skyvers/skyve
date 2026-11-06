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
 * - it handles SmartClient's ValuesManager.itemChanged() notification, which a view's forms fire when the user
 *   changes a field: the field's final value becomes a dataEnter step before the next step or server response;
 * - it wraps isc.ask, to see a confirmation the user declined (confirm="false");
 * - it describes, for the core, a click that recorded nothing (an "unmapped" comment) and what a Check mode click
 *   checks (a growl or dialog message, a field, a grid row or a grid);
 * - a page load with a=e or a=l in its URL (a link, a typed URL, a reload) is recorded as navigateEdit or navigateList.
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
		if ((isc.isA.BizLookupDescriptionItem && isc.isA.BizLookupDescriptionItem(item)) || (item.canvas && !isc.isA.RichTextItem(item))) {
			return { unmapped: "changed " + binding + " (a " + item.getClassName() + ", recorded in a later phase)" };
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
		if (sameRecord) {
			var changes = [];
			Object.keys(now).forEach(function (name) {
				if (!entered[name] && Object.prototype.hasOwnProperty.call(before, name) && (before[name] !== now[name])) {
					changes.push(unsanitise(name) + " \"" + before[name] + "\" -> \"" + now[name] + "\"");
				}
			});
			if (changes.length) {
				core.push({ op: "context", text: "changed on screen: " + changes.join("; "), view: describe(view), screen: screenOf(view) });
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

	// ---------------------------------------------------------------- the wrapped methods

	function recordMenu(viewer, leaf) {
		var labels = [];
		var tree = viewer.data;
		for (var node = leaf; node && node !== tree.getRoot(); node = tree.getParent(node)) {
			labels.unshift(strip(node.desc));
		}
		var module = tree.getRoot().name;
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
					}
					core.record(step);
				}
				return original.apply(this, arguments);
			};
		});

		var editViewProto = isc.EditView.getPrototype();
		wrap(editViewProto, "scatter", function (original) {
			return function (values) {
				// a server response replaces the values: the user's entries so far are final
				if (core.active()) {
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
			for (var g = canvas.grid; g; g = g.parentElement) {
				if (isc.isA.BizDataGrid(g)) {
					return "a row of the data grid " + unsanitise(g._b);
				}
				if (isc.isA.BizListGrid(g)) {
					return g.bizSelected ? "a row of the list grid " + describeGrid(g) + ", which selects it" : null;
				}
			}
			return null;
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
		flush: flushDataEntry,
		describe: function () {
			var view = topView();
			return { view: describe(view), screen: screenOf(view) };
		},
		connected: resolvePageList,
		control: control,
		checkTarget: checkTarget,
		isSignOut: isSignOut,
		signOut: function () {
			window.location = "loggedOut";
		},
		idle: function () {
			return changed.length === 0;
		},
		reset: function () {
			changed = [];
			confirmable = null;
			pageList = null;
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
