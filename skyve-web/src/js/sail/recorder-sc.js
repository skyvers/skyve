/*
 * SAIL recorder for SmartClient: records what the user does as SAIL steps. See docs/sail.md, "Recording".
 *
 * Only a session that is recording loads this script: desktop/home.xhtml adds it when DesktopView.isSailRecording()
 * is true, and the Control Panel loads it when recording starts. The steps go to the sailrecording endpoint
 * (org.skyve.impl.sail.recording.SailRecordingServlet), which builds the script when recording stops.
 *
 * Skyve's client code has no recorder checks in it. The recorder attaches itself when it loads, and detaches when
 * recording stops:
 * - it wraps these methods of Skyve's SmartClient classes, calling the original and recording what it did (each is
 *   marked in skyve-web/src/js/desktop with a comment pointing here):
 *     isc.BizUtil.handleMenuItemClick   navigateMenu
 *     isc.BizButton.initWidget          wraps each button's action: action, save, ok, cancel, delete, zoomOut, remove
 *     isc.BizTabPane.tabSelected        tabSelect, when the user selected the tab
 *     isc.BizListGrid.zoom              listGridZoom (rowContaining), listGridNew
 *     isc.EditView.scatter              writes the pending dataEnter steps before a response replaces the values
 * - it handles SmartClient's ValuesManager.itemChanged() notification, which a view's forms fire when the user
 *   changes a field: the field's final value becomes a dataEnter step before the next step or server response;
 * - it wraps isc.ask, to see a confirmation the user declined (confirm="false");
 * - DOM listeners on document, in the capture phase, mark when a user gesture starts, so that only gestures are
 *   recorded, and a click on a control that no step records becomes an "unmapped" comment. A gesture is never dropped.
 * - a page load with a=e or a=l in its URL (a link, a typed URL, a reload) is recorded as navigateEdit or navigateList.
 *
 * The recorder only ever runs while recording, but it must not slow down the page: it does its work when a gesture
 * or response happens, and uses no timers.
 */
(function () {
	"use strict";

	window.SKYVE = window.SKYVE || {};
	if (SKYVE.SailRecorder) {
		// loaded again by the Control Panel's Start after an earlier recording in this page stopped
		SKYVE.SailRecorder.start();
		return;
	}

	var ENDPOINT = "sailrecording";
	/** The Control Panel's recording actions, which are not recorded. */
	var CONTROL_ACTIONS = { StartSAILRecording: true, StopSAILRecording: true, DownloadSAILRecording: true };
	var STOP_ACTION = "StopSAILRecording";
	/** BizButton types and the steps they record; any other type is an unmapped gesture. */
	var BUTTON_STEPS = { OK: "ok", Save: "save", Cancel: "cancel", Delete: "delete", ZoomOut: "zoomOut", Remove: "remove" };
	var ACTION_TYPES = { Download: true };

	var active = false;
	/** Identifies this page's steps, so that the server adds a step resent from this page once. */
	var pageId = Date.now().toString(36) + Math.random().toString(36).substring(2, 8);
	var sequence = 0;
	/** The CSRF token for this page, from the endpoint. */
	var token = null;
	/** Steps not yet acknowledged by the server, in order. */
	var queue = [];
	/** How many steps at the head of the queue the request in flight carries, or 0. */
	var inFlight = 0;
	/** Form items the user changed whose values are not yet written as dataEnter steps, in the order first changed. */
	var changed = [];
	/** The step the button being pressed recorded, while its action may ask for a confirmation. */
	var confirmable = null;
	/** More than 0 while a button's action runs: its step is sent once the action has asked any confirmation. */
	var holdSends = 0;
	/** The gesture being handled: { target, steps } while a DOM event is being dispatched, else null. */
	var gesture = null;
	/** Functions that undo what start() attached. */
	var detach = [];
	/** Callbacks waiting for the steps recorded so far to be sent. */
	var drained = [];
	/** True while asking the endpoint for this page's token. */
	var connecting = false;
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

	// ---------------------------------------------------------------- recording and sending

	/** Records a step, after writing any pending data entry. Returns the step. */
	function record(step) {
		flushDataEntry();
		return push(step);
	}

	function push(step) {
		step.id = pageId + "-" + (++sequence);
		if (step.view === undefined) {
			step.view = describe(topView());
		}
		queue.push(step);
		if (gesture) {
			gesture.steps++;
		}
		send();
		return step;
	}

	/** Records a gesture that has no step yet, as an "unmapped" comment, once if it repeats (a double click). */
	function unmapped(text) {
		var last = queue.length ? queue[queue.length - 1] : null;
		if (last && (last.op === "unmapped") && (last.text === text)) {
			if (gesture) {
				gesture.steps++;
			}
			return last;
		}
		return record({ op: "unmapped", text: text });
	}

	/** Copies steps to send, without the recorder's own flags. */
	function sendable(steps) {
		return steps.map(function (s) {
			var copy = {};
			for (var key in s) {
				if (Object.prototype.hasOwnProperty.call(s, key) && (key !== "hold")) {
					copy[key] = s[key];
				}
			}
			return copy;
		});
	}

	/** Calls back once every step recorded so far has been sent, or sending has failed or stopped. */
	function whenSent(callback) {
		drained.push(callback);
		send();
	}

	function notifyDrained() {
		var callbacks = drained;
		drained = [];
		callbacks.forEach(function (callback) {
			callback();
		});
	}

	/**
	 * Sends the steps not yet sent, one request at a time, up to any step still waiting for the user's answer to a
	 * confirmation. The request goes through SmartClient's RPCManager, so that the page counts as busy until the
	 * server has the steps.
	 */
	function send() {
		if (inFlight || holdSends) {
			return;
		}
		if (token == null) {
			if (!connecting) {
				notifyDrained();
			}
			return;
		}
		var count = 0;
		while ((count < queue.length) && !queue[count].hold) {
			count++;
		}
		if (count === 0) {
			notifyDrained();
			return;
		}
		inFlight = count;
		isc.RPCManager.sendRequest({
			actionURL: ENDPOINT,
			httpMethod: "POST",
			useSimpleHttp: true,
			contentType: "application/json",
			data: JSON.stringify({ _csrf: token, steps: sendable(queue.slice(0, count)) }),
			serverOutputAsString: true,
			showPrompt: false,
			willHandleError: true,
			callback: function (response, data) {
				var status = response.httpResponseCode;
				inFlight = 0;
				if (status === 200) {
					queue.splice(0, count);
					if (limitReached(data)) {
						return;
					}
					send();
				} else if (status === 403) {
					// the session no longer records (stopped in another tab, signed out, or not allowed)
					stop();
				} else {
					// kept: sent again with the next step, or from the page's beacon as it unloads
					notifyDrained();
				}
			}
		});
	}

	/**
	 * True if the server's answer says the recording is full: the recorder stops, and tells the user to stop the
	 * recording and download what it holds. The server keeps the steps it has.
	 */
	function limitReached(data) {
		var answer = null;
		try {
			answer = JSON.parse(data);
		} catch (e) {
			answer = null;
		}
		if (!answer || !answer.limitReached) {
			return false;
		}
		stop();
		isc.warn("The SAIL recording has reached its limit of " + answer.maximumSteps +
					" steps and records nothing more. Stop it in the Control Panel and download it.");
		return true;
	}

	/** Sends what is left as the page unloads; the server adds a step it has already received once. */
	function onPageHide() {
		flushDataEntry();
		if (queue.length && (token != null) && navigator.sendBeacon) {
			navigator.sendBeacon(ENDPOINT, new Blob([JSON.stringify({ _csrf: token, steps: sendable(queue) })], { type: "application/json" }));
		}
	}

	// ---------------------------------------------------------------- data entry

	/** ValuesManager.itemChanged(): the user changed a form item of a view. */
	function itemChanged(changedItem) {
		// a part of an item (a date item's text field) stands for the item
		var item = changedItem;
		while (item && item.parentItem) {
			item = item.parentItem;
		}
		if (!active || !item || !item.form || !item.form._view) {
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
				var view = describe(item.form._view);
				if (entry.unmapped) {
					push({ op: "unmapped", text: entry.unmapped, view: view });
				} else {
					var step = { op: "dataEnter", binding: entry.binding, value: entry.value, view: view };
					if (entry.match) {
						step.match = entry.match;
					}
					push(step);
				}
			}
		}
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

	function wrap(owner, name, wrapper) {
		var original = owner[name];
		owner[name] = wrapper(original);
		detach.push(function () {
			owner[name] = original;
		});
	}

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
		record(step);
	}

	/** Records what a BizButton's action does, before it does it. Returns the step, or null. */
	function recordButton(button) {
		var name = button.actionName;
		if (CONTROL_ACTIONS[name]) {
			return null;
		}
		var op = BUTTON_STEPS[button.type];
		if (op) {
			return record({ op: op });
		}
		if (!button.type || ACTION_TYPES[button.type]) {
			return record({ op: "action", name: name });
		}
		return unmapped("pressed " + strip(button.title || name) + " (a " + button.type + " action, not recorded yet)");
	}

	function wrapButton(button) {
		if (button._sailAction || !isc.isA.Function(button.action)) {
			return;
		}
		var action = button.action;
		button._sailAction = action;
		button.action = function () {
			if (!active) {
				return action.apply(this, arguments);
			}
			if (this.actionName === STOP_ACTION) {
				// the server builds the script when this action reaches it: send every step first
				var me = this;
				var args = arguments;
				flushDataEntry();
				whenSent(function () {
					stop();
					action.apply(me, args);
				});
				return undefined;
			}
			// a click that a field's blur swallowed is pressed again once the blur's server call returns: record that one
			var swallowed = this._view && this._view._blurry;
			holdSends++;
			try {
				confirmable = swallowed ? null : recordButton(this);
				return action.apply(this, arguments);
			} finally {
				confirmable = null;
				holdSends--;
				send();
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

	function attach() {
		wrap(isc.BizUtil, "handleMenuItemClick", function (original) {
			return function (viewer, leaf) {
				if (active) {
					recordMenu(viewer, leaf);
				}
				return original.apply(this, arguments);
			};
		});

		var buttonProto = isc.BizButton.getPrototype();
		wrap(buttonProto, "initWidget", function (original) {
			return function () {
				var result = original.apply(this, arguments);
				if (active) {
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
		detach.push(unwrapButtons);

		wrap(isc.BizTabPane.getPrototype(), "tabSelected", function (original) {
			return function (tabNum) {
				// a tab the page selects by itself (a server response, a view opening) is not a gesture
				var on = gesture ? canvasOf(gesture.target) : null;
				if (active && on && this.tabBar && this.tabBar.contains(on, true)) {
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
					record({ op: "tabSelect", path: titles.join("::") });
				}
				return original.apply(this, arguments);
			};
		});

		wrap(isc.BizListGrid.getPrototype(), "zoom", function (original) {
			return function (zoomToNew) {
				if (active) {
					var step = gridList(this);
					if (zoomToNew) {
						step.op = "listGridNew";
					} else {
						var located = rowLocator(this.grid, this._eventRowNum, this._eventRecord);
						step.op = "listGridZoom";
						if (located.rowContaining != null) {
							step.rowContaining = located.rowContaining;
						} else {
							step.row = located.row;
						}
					}
					record(step);
				}
				return original.apply(this, arguments);
			};
		});

		wrap(isc.EditView.getPrototype(), "scatter", function (original) {
			return function () {
				// a server response replaces the values: the user's entries so far are final
				if (active) {
					flushDataEntry();
				}
				return original.apply(this, arguments);
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
		detach.push(function () {
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
				if (active && step && isc.isA.Function(callback)) {
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
						send();
						return callback.apply(this, arguments);
					};
				}
				return original.apply(this, args);
			};
		});

		listen(document, "mousedown", startGesture, true);
		listen(document, "keydown", startGesture, true);
		listen(window, "mouseup", endGesture, false);
		listen(window, "keyup", endKeyGesture, false);
		listen(window, "click", clicked, false);
		listen(window, "pagehide", onPageHide, false);
	}

	function listen(target, type, handler, capture) {
		target.addEventListener(type, handler, capture);
		detach.push(function () {
			target.removeEventListener(type, handler, capture);
		});
	}

	// ---------------------------------------------------------------- gestures

	/** A mouse button or key went down: SmartClient handles what it does until it comes up. */
	function startGesture(event) {
		gesture = { target: event.target, steps: 0 };
	}

	function endKeyGesture() {
		gesture = null;
	}

	function endGesture() {
		// a click is handled on the mouse up; keep the gesture for the click event that follows
		if (gesture && gesture.ended) {
			gesture = null;
		} else if (gesture) {
			gesture.ended = true;
		}
	}

	/**
	 * A click has been handled. If it recorded nothing but was on a control, it was a gesture the recorder cannot map:
	 * record it as a comment, so that the editor sees the gap.
	 */
	function clicked(event) {
		var g = gesture;
		gesture = null;
		if (!active || !g || g.steps > 0) {
			return;
		}
		var description = control(event.target);
		if (description) {
			unmapped("click on " + description);
		}
	}

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
			push(edit);
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
			push(list);
		} else if (params.a) {
			push({ op: "unmapped", text: "opened " + location.search + " (not recorded yet)", view: "" });
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

	/** Asks the endpoint whether this session records, and for this page's token. */
	function connect() {
		connecting = true;
		isc.RPCManager.sendRequest({
			actionURL: ENDPOINT,
			httpMethod: "GET",
			useSimpleHttp: true,
			serverOutputAsString: true,
			showPrompt: false,
			willHandleError: true,
			callback: function (response, data) {
				var state = null;
				try {
					state = (response.httpResponseCode === 200) ? JSON.parse(data) : null;
				} catch (e) {
					state = null;
				}
				connecting = false;
				if (state && state.recording) {
					token = state._csrf;
					resolvePageList();
					send();
				} else {
					stop();
				}
			}
		});
	}

	function start() {
		if (active) {
			return;
		}
		active = true;
		queue = [];
		changed = [];
		token = null;
		pageId = Date.now().toString(36) + Math.random().toString(36).substring(2, 8);
		sequence = 0;
		attach();
		connect();
	}

	function stop() {
		if (!active) {
			return;
		}
		active = false;
		while (detach.length) {
			detach.pop()();
		}
		queue = [];
		changed = [];
		gesture = null;
		confirmable = null;
		token = null;
		connecting = false;
		pageList = null;
		notifyDrained();
	}

	SKYVE.SailRecorder = {
		start: start,
		stop: stop,
		/** True while recording. */
		active: function () {
			return active;
		},
		/** True once the endpoint has confirmed the session records and every step so far has reached it. */
		ready: function () {
			return active && (token != null) && !inFlight && (queue.length === 0) && (changed.length === 0);
		},
		/** The steps not yet acknowledged by the server, for diagnosis. */
		pending: function () {
			return queue.slice();
		}
	};

	// Loaded with the page (not by the Control Panel's Start): the URL that opened it is the user's navigation
	var duringPageLoad = (document.readyState !== "complete");
	start();
	if (duringPageLoad) {
		recordPageLoad();
	}
})();
