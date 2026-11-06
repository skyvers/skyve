/*
 * SAIL SmartClient helper, injected into the page by util.sail.SmartClientSelenide.
 *
 * SmartClientSAILViewVisitor describes widgets semantically ("the form item named text",
 * "the New button of the containerGrid data grid") instead of re-deriving SmartClient IDs.
 * This script resolves those descriptions against the live view that the browser shows:
 * the top-most window in isc.WindowStack, or the harness's current view. The resolved object
 * is turned into an isc.AutoTest locator, any AutoTest suffix in the SAIL locator is appended,
 * and isc.AutoTest.getElement() finds the DOM element.
 *
 * A SAIL locator is "sail:" + a JSON spec + an optional AutoTest suffix, e.g.
 *   sail:{"view":"kitchensink.KitchenSink","kind":"item","name":"text"}/element
 * Spec properties:
 *   view    - "module.Document" expected on the top-most edit view (omitted for list views)
 *   kind    - item | membership | button | zoomIn | tab | dataGrid | listGrid | list | cell | dialog
 *   name    - item name (sanitised binding), action name, binding, tab title or dialog button
 *   ordinal - zero-based index of a list grid in its view (kind listGrid)
 *   part    - new | zoom | edit | remove | rows (grids)
 */
(function () {
	"use strict";

	var VERSION = 8;
	if (window._sail && window._sail.version === VERSION) {
		return;
	}

	var S = { version: VERSION };

	/** True when the SmartClient runtime and the Skyve desktop harness are loaded. */
	S.loaded = function () {
		return !!(window.isc && isc.AutoTest && isc.BizUtil && isc.WindowStack && isc.Canvas);
	};

	// ---------------------------------------------------------------- deferred work
	//
	// SmartClient and Skyve start some work from timers rather than straight from the event: a dialog's
	// callback (Window.returnValue() delays it 50 ms), a blur's rerender and the click it swallowed
	// (EditView._rerenderBlurryAction, BizButton action), a grid row selection's server call (bizSelected,
	// after the double click delay), the refresh of a view's list grids, and SmartClient's redraw and reflow
	// queues (0 ms) after any change. All of them go through isc.Timer.setTimeout(), so the helper counts the
	// timers that have not fired yet, and the page is not settled until they have.
	// Left out, because they do not follow from a step and could keep the page busy for ever: background repeat
	// timers, timers of a second or more (hovers, view refreshes), mouse-still-down repeats, and maps, which
	// charts retry every 100 ms while their scripts are not loaded (offline: never).

	var deferred = {};
	var timerTokens = {};
	var sequence = 0;

	function describeAction(action) {
		if (isc.isA.Function(action)) {
			return "function";
		}
		var target = action && action.target;
		var owner = target && target.getClassName ? target.getClassName() : "isc";
		return owner + "." + (action && action.methodName);
	}

	function ignored(action, delay, backgroundRepeatTimer) {
		if (backgroundRepeatTimer || isc.isA.String(action) || (delay != null && delay >= 1000)) {
			return true;
		}
		var target = action && action.target;
		var method = action && action.methodName;
		if (method === "$kq" || method === "_handleMouseStillDown") {
			return true;
		}
		// Skyve's maps and charts (BizMap, BizMapPicker, BizChart) retry until their script loads, which offline is never
		var name = target && target.getClassName ? target.getClassName() : "";
		return /^Biz(Map|Chart)/.test(name);
	}

	function instrument() {
		if (isc.Timer._sailSetTimeout) {
			return;
		}
		var setTimeout = isc.Timer.setTimeout;
		isc.Timer._sailSetTimeout = setTimeout;
		isc.Timer.setTimeout = function (action, delay, units, frequentTimer, backgroundRepeatTimer) {
			if (action && action.action != null) {
				// the parameter object form
				return isc.Timer.setTimeout(action.action, action.delay, action.units, frequentTimer, backgroundRepeatTimer);
			}
			if (action == null || ignored(action, delay, backgroundRepeatTimer)) {
				return setTimeout.apply(this, arguments);
			}
			var token = ++sequence;
			deferred[token] = describeAction(action);
			var wrapped = function () {
				delete deferred[token];
				if (isc.isA.Function(action)) {
					return action();
				}
				var target = action.target || window;
				var method = isc.isA.Function(action.methodName) ? action.methodName : target[action.methodName];
				return method.apply(target, action.args || []);
			};
			var id = setTimeout.call(this, wrapped, delay, units, frequentTimer, backgroundRepeatTimer);
			timerTokens[id] = token;
			return id;
		};
		var clear = isc.Timer.clear;
		isc.Timer.clear = function (id) {
			var token = timerTokens[id];
			if (token) {
				delete deferred[token];
				delete timerTokens[id];
			}
			return clear.apply(this, arguments);
		};
	}

	/** The deferred calls that have not fired yet. */
	S.deferred = function () {
		return Object.keys(deferred).map(function (k) {
			return deferred[k];
		});
	};

	/** True when no RPC is pending, no tracked deferred call is waiting to fire, and the system is done (see systemDone()). */
	S.idle = function () {
		if (!S.loaded()) {
			return false;
		}
		instrument();
		return !isc.RPCManager.requestsArePending() && S.deferred().length === 0 && systemDone() === true;
	};

	var busyCanvas = null;

	/**
	 * AutoTest.isSystemDone() without its network check, and counting only canvases that are shown:
	 * a grid under a tab that is not selected never fetches, so AutoTest would wait for it for ever
	 * (seen on ConditionsFixture). Falls back to AutoTest if the canvas check is unavailable.
	 */
	function systemDone() {
		busyCanvas = null;
		if (!isc.Page.isLoaded() || (isc.EH.resizeTimer != null)) {
			return false;
		}
		var list = isc.Canvas._canvasList || [];
		for (var i = 0; i < list.length; i++) {
			var c = list[i];
			if (!c || c.destroyed) {
				continue;
			}
			// the method's name in the debug and in the production build
			var done = c._isProcessingDone || c.$124j;
			if (!isc.isA.Function(done)) {
				return isc.AutoTest.isSystemDone({ includeNetworkOperations: false });
			}
			if (!done.call(c, true) && S.shown(c)) {
				busyCanvas = c;
				return false;
			}
		}
		return true;
	}

	/** Explains why the system is not done, for diagnosis. */
	function notDone() {
		if (!isc.Page.isLoaded()) {
			return "the page is not loaded";
		}
		if (isc.EH.resizeTimer != null) {
			return "a page resize is pending";
		}
		return busyCanvas ? "canvas " + busyCanvas.getClassName() + " " + busyCanvas.ID + " is not done processing" :
							String(isc.AutoTest._isSystemDoneLog || isc.AutoTest.$130x || "AutoTest says the system is not done");
	}

	/** The top-most edit view: the last WindowStack window holding a view, or the harness's current view. */
	S.topView = function () {
		var stack = isc.WindowStack._stack || [];
		for (var i = stack.length - 1; i >= 0; i--) {
			var items = stack[i].items;
			var item = items && items[0];
			if (item && item._vm) {
				return item;
			}
		}
		return isc.BizUtil.getCurrentView();
	};

	/** A short description of a view, "module.Document" or "list". */
	S.describe = function (view) {
		if (!view) {
			return "no view";
		}
		if (isc.ListView && view === isc.ListView.contents) {
			return "list";
		}
		var mod = view._moduleName || view._mod;
		var doc = view._documentName || view._doc;
		return mod ? mod + "." + doc : String(view.getID ? view.getID() : view);
	};

	/** True once SmartClient has loaded and settled: the condition the harness waits on after every step. */
	S.settled = function () {
		return document.readyState === "complete" && S.idle();
	};

	/** What the browser is showing, for error messages and waits. */
	S.state = function () {
		if (!S.loaded()) {
			return { loaded: false, url: String(location.href), title: document.title };
		}
		var rpc = isc.RPCManager.requestsArePending();
		var done = systemDone();
		var waiting = S.deferred();
		return {
			loaded: true,
			idle: !rpc && waiting.length === 0 && done === true,
			busy: rpc ? "requests are pending" : waiting.length ? "deferred calls pending: " + waiting.join(", ") : done !== true ? notDone() : null,
			view: S.describe(S.topView()),
			windows: (isc.WindowStack._stack || []).length,
			dialog: S.dialog()
		};
	};

	/** True if the canvas and all its ancestors are drawn and visible. */
	S.shown = function (canvas) {
		for (var c = canvas; c; c = c.parentElement) {
			if (c.destroyed || !c.isDrawn() || !c.isVisible()) {
				return false;
			}
		}
		return true;
	};

	function itemShown(item) {
		var form = item.form;
		if (form && form.grid && !form.isDrawn() && form.grid.getEditRow && form.grid.getEditRow() != null) {
			// a grid's row editor: its form is never drawn, as its items are drawn in the grid's body
			return S.shown(form.grid) && item.isVisible() && item.isDrawn();
		}
		return !!form && S.shown(form) && item.isVisible() && item.isDrawn();
	}

	function isFormItem(o) {
		return !!o && isc.isA.FormItem(o);
	}

	function shownObject(o) {
		return isFormItem(o) ? itemShown(o) : S.shown(o);
	}

	/** Prefers the first shown candidate, else the first candidate. */
	function pick(list) {
		for (var i = 0; i < list.length; i++) {
			if (shownObject(list[i])) {
				return list[i];
			}
		}
		return list.length ? list[0] : null;
	}

	function canvases() {
		return (isc.Canvas._canvasList || []).filter(function (c) {
			return c && !c.destroyed && !c.destroying;
		});
	}

	/** Canvases the Skyve renderer created for a view (they carry _view). */
	function owned(view, test) {
		return canvases().filter(function (c) {
			return c._view === view && test(c);
		});
	}

	function strip(html) {
		return String(html == null ? "" : html)
			.replace(/<[^>]*>/g, "")
			.replace(/&nbsp;/g, " ")
			.replace(/\s+/g, " ")
			.trim();
	}

	function items(view, name) {
		var result = [];
		owned(view, function (c) {
			return isc.isA.DynamicForm(c);
		}).forEach(function (form) {
			(form.getItems() || []).forEach(function (item) {
				if (item.name === name) {
					result.push(item);
				}
			});
		});
		return result;
	}

	function cells(view, binding, field) {
		var result = [];
		dataGrids(view, binding).forEach(function (grid) {
			var list = grid.grid;
			if (list && list.getEditRow && list.getEditRow() != null && list.getEditForm) {
				var form = list.getEditForm();
				var item = form && form.getItem(field);
				if (item) {
					result.push(item);
				}
			}
		});
		return result;
	}

	function buttons(view, name) {
		var list = owned(view, function (c) {
			return isc.isA.BizButton(c) && c.actionName === name;
		});
		var panel = view._actionPanel;
		// action panel buttons first, then form buttons
		list.sort(function (a, b) {
			var pa = panel && panel.contains(a) ? 0 : 1;
			var pb = panel && panel.contains(b) ? 0 : 1;
			return pa - pb;
		});
		return list;
	}

	function tabs(view, title) {
		var result = [];
		canvases().filter(function (c) {
			return isc.isA.TabSet(c) && view.contains(c);
		}).forEach(function (set) {
			(set.tabs || []).forEach(function (tab, i) {
				if (strip(tab.title) === title) {
					var button = set.getTab(i);
					if (button) {
						result.push(button);
					}
				}
			});
		});
		return result;
	}

	function dataGrids(view, binding) {
		var grids = (view._grids && view._grids[binding]) || {};
		return Object.keys(grids).map(function (id) {
			return grids[id];
		}).filter(function (g) {
			return g && !g.destroyed && isc.isA.BizDataGrid(g);
		});
	}

	function gridPart(grid, part) {
		switch (part) {
			case "new":
				return grid._newButton;
			case "zoom":
				return grid._zoomButton;
			case "edit":
				return grid._editButton;
			case "remove":
				return grid.deleteSelectionButton;
			case "rows":
				return grid.grid;
			default:
				return grid;
		}
	}

	/**
	 * The dialog on top, if one is showing: isc.say, isc.warn and isc.ask share isc.Dialog.Warn, and Skyve shows a
	 * server-side question in a Dialog of its own.
	 */
	function activeDialog() {
		var top = null;
		canvases().forEach(function (c) {
			if (isc.isA.Dialog(c) && c.isDrawn() && c.isVisible() && (!top || c.getZIndex() > top.getZIndex())) {
				top = c;
			}
		});
		return top;
	}

	/** The dialog on top, as { kind: say|warn|ask, title, message, buttons }, or null. */
	S.dialog = function () {
		var d = activeDialog();
		if (!d) {
			return null;
		}
		var icon = String(d.icon || "");
		var kind = icon.indexOf("warn") >= 0 || icon.indexOf("error") >= 0 ? "warn"
				: icon.indexOf("ask") >= 0 || icon.indexOf("confirm") >= 0 ? "ask"
				: "say";
		var titles = d.toolbar ? d.toolbar.getMembers().map(function (b) {
			return strip(b.getTitle ? b.getTitle() : b.title);
		}) : [];
		return { kind: kind, title: strip(d.title), message: strip(d.message), buttons: titles };
	};

	/**
	 * The dialog on top (as S.dialog() describes it) when the element is not part of it, so that a click on the element
	 * would land on the dialog's modal mask; otherwise null.
	 */
	S.dialogInTheWay = function (element) {
		var d = activeDialog();
		if (!d) {
			return null;
		}
		var canvas = canvasOf(element);
		if (canvas && ((canvas === d) || d.contains(canvas, true))) {
			return null;
		}
		return S.dialog();
	};

	function dialogButton(name) {
		var d = activeDialog();
		if (!d || !d.toolbar) {
			return null;
		}
		var wanted = {
			ok: [isc.Dialog.OK_BUTTON_TITLE, "OK"],
			yes: [isc.Dialog.YES_BUTTON_TITLE, "Yes"],
			no: [isc.Dialog.NO_BUTTON_TITLE, "No"],
			cancel: [isc.Dialog.CANCEL_BUTTON_TITLE, "Cancel"]
		}[name] || [name];
		var members = d.toolbar.getMembers().filter(function (b) {
			return b.isVisible && b.isVisible();
		});
		for (var i = 0; i < members.length; i++) {
			var title = strip(members[i].getTitle ? members[i].getTitle() : members[i].title);
			if (wanted.indexOf(title) >= 0) {
				return members[i];
			}
		}
		// a single-button dialog: its button acknowledges it
		return name === "ok" && members.length === 1 ? members[0] : null;
	}

	/** Splits a SAIL locator into its JSON spec and AutoTest suffix. */
	S.parse = function (locator) {
		if (locator.indexOf("sail:") !== 0) {
			return { spec: null, suffix: locator };
		}
		var depth = 0;
		var inString = false;
		var escaped = false;
		for (var i = 5; i < locator.length; i++) {
			var ch = locator.charAt(i);
			if (inString) {
				if (escaped) {
					escaped = false;
				} else if (ch === "\\") {
					escaped = true;
				} else if (ch === "\"") {
					inString = false;
				}
			} else if (ch === "\"") {
				inString = true;
			} else if (ch === "{") {
				depth++;
			} else if (ch === "}") {
				depth--;
				if (depth === 0) {
					return { spec: JSON.parse(locator.substring(5, i + 1)), suffix: locator.substring(i + 1) };
				}
			}
		}
		throw new Error("Malformed SAIL locator " + locator);
	};

	/** Resolves a spec to a SmartClient object: { obj } or { error, retry }. */
	S.resolve = function (spec) {
		if (spec.kind === "dialog") {
			var button = dialogButton(spec.name);
			return button ? { obj: button } : { error: "no " + spec.name + " button on a showing dialog", retry: true };
		}

		var view = S.topView();
		var showing = S.describe(view);
		if (spec.kind === "list") {
			if (showing !== "list" || !isc.ListView._grid) {
				return { error: "expected a list view but the browser shows " + showing, retry: true };
			}
			var part = gridPart(isc.ListView._grid, spec.part);
			return part ? { obj: part } : { error: "the list view has no " + spec.part + " yet", retry: true };
		}
		if (spec.view && showing !== spec.view) {
			return { error: "expected view " + spec.view + " but the browser shows " + showing, retry: true, wrongView: true };
		}

		var found = null;
		var what = "";
		switch (spec.kind) {
			case "item":
				found = pick(items(view, spec.name));
				what = "form item named " + spec.name;
				break;
			case "membership":
				var lists = ((view._grids && view._grids[spec.name]) ? Object.keys(view._grids[spec.name]).map(function (id) {
					return view._grids[spec.name][id];
				}) : []).filter(function (g) {
					return g && !g.destroyed && isc.isA.BizListMembership(g);
				});
				found = pick(lists);
				what = "list membership " + spec.name;
				break;
			case "cell":
				found = pick(cells(view, spec.name, spec.field));
				what = "inline editor for column " + spec.field + " of data grid " + spec.name + " (is a row being edited?)";
				break;
			case "button":
				found = pick(buttons(view, spec.name));
				what = "button for action " + spec.name;
				break;
			case "zoomIn":
				found = pick(owned(view, function (c) {
					return isc.isA.BizZoomIn(c) && c.binding === spec.name;
				}));
				what = "zoom in button for " + spec.name;
				break;
			case "tab":
				found = pick(tabs(view, spec.name));
				what = "tab titled " + spec.name;
				break;
			case "dataGrid":
				var grid = pick(dataGrids(view, spec.name));
				found = grid ? gridPart(grid, spec.part) : null;
				what = "data grid " + spec.name + (spec.part ? " " + spec.part : "");
				break;
			case "listGrid":
				var grids = owned(view, function (c) {
					return isc.isA.BizListGrid(c);
				});
				var listGrid = grids[spec.ordinal || 0];
				found = listGrid ? gridPart(listGrid, spec.part) : null;
				what = "list grid #" + (spec.ordinal || 0) + (spec.part ? " " + spec.part : "") + " of " + grids.length;
				break;
			default:
				return { error: "unknown SAIL locator kind " + spec.kind };
		}
		return found ? { obj: found } : { error: "no " + what + " on view " + showing, retry: true };
	};

	/** The AutoTest locator of a SmartClient object, without the trailing objectType marker. */
	S.objectLocator = function (obj) {
		var locator = isc.AutoTest.getObjectLocator(obj);
		return locator ? locator.replace(/\/objectType=[A-Za-z]+$/, "") : null;
	};

	function disabled(obj) {
		if (!obj) {
			return false;
		}
		if (isFormItem(obj)) {
			return !!(obj.isDisabled() || (obj.isReadOnly && obj.isReadOnly()));
		}
		return !!(obj.isDisabled && obj.isDisabled());
	}

	/**
	 * Finds the element for a SAIL or AutoTest locator.
	 * Returns { element, locator, shown, disabled } or { error, retry }.
	 */
	S.find = function (locator) {
		if (!S.loaded()) {
			return { error: "SmartClient is not loaded on " + location.href, retry: true };
		}
		var parsed = S.parse(locator);
		var full = parsed.suffix;
		var obj = null;
		if (parsed.spec) {
			var resolved = S.resolve(parsed.spec);
			if (!resolved.obj) {
				return resolved.error ? resolved : { error: "nothing found for " + locator, retry: true };
			}
			obj = resolved.obj;
			var root = S.objectLocator(obj);
			if (!root) {
				return { error: "no AutoTest locator for " + locator, retry: true };
			}
			full = root + parsed.suffix;
		}
		var element = isc.AutoTest.getElement(full);
		if (!element && obj && isFormItem(obj) && !parsed.suffix) {
			// container items (such as radio groups) have no element of their own: use their first input
			element = itemElement(obj);
		}
		if (!element) {
			if (obj && !shownObject(obj)) {
				return { error: "found " + full + " but it is not shown (hidden, or on a tab that is not selected)", retry: true };
			}
			return { error: "AutoTest found no element for " + full, retry: true };
		}
		return {
			element: element,
			locator: full,
			shown: obj ? shownObject(obj) : true,
			disabled: disabled(obj)
		};
	};

	/** A DOM element for a form item: its focus or data element, or that of its first sub-item. */
	function itemElement(item) {
		var element = (item.getFocusElement && item.getFocusElement()) || (item.getDataElement && item.getDataElement());
		if (!element && item.items && item.items.length) {
			element = itemElement(item.items[0]);
		}
		return element || null;
	}

	/** True if the element a locator finds now has the focus, or holds the element that has it. */
	S.hasFocus = function (locator) {
		var element = S.find(locator).element;
		var active = document.activeElement;
		return !!element && !!active && (active === element || element.contains(active));
	};

	/**
	 * Takes the focus away from the form item a locator names, if the item still has it, and returns whether it did, so
	 * that SmartClient runs the item's exit and blur handlers. Tab can leave the focus in the item: a date or time item
	 * moves it from its text field to its picker icon, and a modal window's only field takes it back. The elements are
	 * looked up afresh, as a redraw (such as a blur handler's rerender) replaces a widget's elements.
	 */
	S.blurIfFocused = function (locator) {
		var active = document.activeElement;
		if (!active || active === document.body) {
			return false;
		}
		var found = S.find(locator);
		var element = found.element;
		var inside = !!element && (active === element || element.contains(active));
		var parsed = S.parse(locator);
		if (!inside && parsed.spec) {
			// the focus may be on another part of the item, such as a date item's picker icon
			var item = S.resolve(parsed.spec).obj;
			var focus = item && item.form && item.form.getFocusSubItem ? item.form.getFocusSubItem() : null;
			for (var f = focus; f && !inside; f = f.parentItem) {
				inside = (f === item);
			}
		}
		if (inside) {
			active.blur();
			return true;
		}
		return false;
	};

	/** The canvas whose DOM contains an element. */
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

	/**
	 * True while SmartClient would read a click on the element as the second click of a double click: the same widget
	 * was clicked less than EventHandler.DOUBLE_CLICK_DELAY ago. A button then fires doubleClick, not its action.
	 */
	S.clickWouldPair = function (element) {
		var EH = isc.EH;
		var canvas = canvasOf(element);
		if (!canvas || !EH.lastClickTarget || EH.lastClickTime == null) {
			return false;
		}
		var same = (EH.lastClickTarget === canvas) || (canvas.contains && canvas.contains(EH.lastClickTarget)) ||
					(EH.lastClickTarget.contains && EH.lastClickTarget.contains(canvas));
		return same && (isc.timeStamp() - EH.lastClickTime) < EH.DOUBLE_CLICK_DELAY;
	};

	/**
	 * Scrolls an element into view the SmartClient way: Canvas.scrollIntoView() on the nearest
	 * canvas, which also scrolls every ancestor's viewport (SmartClient's custom scrollbars do not
	 * respond to DOM scrollIntoView()). Falls back to the DOM call.
	 */
	S.reveal = function (element) {
		var canvas = canvasOf(element);
		if (canvas && canvas.scrollIntoView) {
			var rect = element.getBoundingClientRect();
			var x = rect.left + isc.Page.getScrollLeft() - canvas.getPageLeft() + canvas.getScrollLeft();
			var y = rect.top + isc.Page.getScrollTop() - canvas.getPageTop() + canvas.getScrollTop();
			canvas.scrollIntoView(Math.max(0, Math.round(x)), Math.max(0, Math.round(y)),
					Math.round(rect.width), Math.round(rect.height), "center", "center");
		}
		var after = element.getBoundingClientRect();
		if (after.bottom > window.innerHeight || after.top < 0 || after.right > window.innerWidth || after.left < 0) {
			element.scrollIntoView({ block: "center", inline: "nearest" });
		}
		after = element.getBoundingClientRect();
		var cx = after.left + after.width / 2;
		var cy = after.top + after.height / 2;
		var hit = document.elementFromPoint(cx, cy);
		return {
			inViewport: after.bottom > 0 && after.top < window.innerHeight && after.right > 0 && after.left < window.innerWidth,
			covered: !!hit && hit !== element && !element.contains(hit) && !hit.contains(element)
		};
	};

	function formatValue(item, value) {
		if (value == null) {
			return "";
		}
		if (isc.isA.Date(value)) {
			var display = item.getDisplayValue ? item.getDisplayValue() : null;
			return display == null ? String(value) : String(display);
		}
		return String(value);
	}

	/** The zero-based position of a value among the options a combo's pick list or a radio group offers. */
	function optionIndex(item, value) {
		var data = item.getClientPickListData ? item.getClientPickListData() : null;
		if (data && data.length) {
			for (var i = 0; i < data.length; i++) {
				var v = data[i][item.getValueFieldName ? item.getValueFieldName() : item.name];
				if ((v == null && value == null) || (v != null && value != null && String(v) === String(value))) {
					return i;
				}
			}
			return -1;
		}
		var map = item.getValueMap();
		var keys = isc.isA.Array(map) ? map.map(String) : Object.keys(map);
		return value == null ? -1 : keys.indexOf(String(value));
	}

	/** True if the item keeps new lines: a text area or a rich text editor. */
	function multiLine(item) {
		return isFormItem(item) && (isc.isA.TextAreaItem(item) || isc.isA.RichTextItem(item));
	}

	/** True if the form item a locator names keeps new lines, so that dataEnter types them and testValue compares them. */
	S.multiLine = function (locator) {
		return multiLine(S.resolve(S.parse(locator).spec).obj);
	};

	var BLOCKS = /^(P|DIV|LI|UL|OL|H[1-6]|BLOCKQUOTE|PRE|TABLE|TR)$/;

	/**
	 * The text of rich text HTML as a browser lays it out: a line break, or the start and end of a block (paragraph, div,
	 * list item, heading), starts a new line, so that each block is on its own line and an empty block with a line
	 * break is an empty line. The spaces in each line are collapsed; the HTML's own new lines are only layout.
	 */
	function htmlLines(html) {
		var holder = document.createElement("div");
		holder.innerHTML = String(html == null ? "" : html);
		var out = "";
		var newLine = function () {
			if (out.length > 0 && out.charAt(out.length - 1) !== "\n") {
				out += "\n";
			}
		};
		var walk = function (node) {
			if (node.nodeType === 3) {
				var text = node.nodeValue.replace(/[\r\n]+/g, " ");
				// white space at the start of a line is not shown (as between blocks)
				out += (out.length === 0 || out.charAt(out.length - 1) === "\n") ? text.replace(/^\s+/, "") : text;
			}
			else if (node.nodeType === 1) {
				if (node.nodeName === "BR") {
					out += "\n";
					return;
				}
				var block = BLOCKS.test(node.nodeName);
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
		return out.replace(/\u00a0/g, " ").split("\n").map(function (line) {
			return line.replace(/\s+/g, " ").trim();
		}).join("\n").replace(/^\n+|\n+$/g, "");
	}

	/**
	 * The value a form item displays, as a string, for testValue.
	 * Returns { value, alternatives[], type } or { error }.
	 */
	S.value = function (locator) {
		var parsed = S.parse(locator);
		if (!parsed.spec) {
			return { error: "testValue needs a SAIL locator" };
		}
		var resolved = S.resolve(parsed.spec);
		if (!resolved.obj) {
			return resolved;
		}
		var item = resolved.obj;
		if (item && item._candidateList) {
			// list membership: the members, in order, |-separated
			var members = (item._memberList.getData() || []).map(function (r) {
				return strip(r.bizKey);
			});
			return { value: members.join("|"), alternatives: [], type: item.getClassName() };
		}
		if (!isFormItem(item)) {
			return { error: locator + " is not a form item" };
		}
		var type = item.getClassName();
		var value = item.getValue();
		var alternatives = [];
		var result;
		if (isc.isA.CheckboxItem(item)) {
			// only a real boolean is checked or unchecked; anything else (such as an unevaluated
			// default expression) is shown as is, so that it neither passes for true nor stops data entry
			result = value == null ? "" : (value === true || value === false) ? String(value) : JSON.stringify(value);
		} else if (isc.isA.RichTextItem(item)) {
			var html = value == null ? "" : String(value);
			var div = document.createElement("div");
			div.innerHTML = html;
			result = htmlLines(html);
			// the text on one line, as this read it before rich text kept its lines, and the HTML
			alternatives.push(strip(div.textContent || div.innerText || ""));
			alternatives.push(html);
		} else if (isc.isA.BizHTMLItem && isc.isA.BizHTMLItem(item)) {
			var markup = value == null ? "" : String(value);
			var holder = document.createElement("div");
			holder.innerHTML = markup;
			result = strip(holder.textContent || holder.innerText || "");
			alternatives.push(markup);
		} else if (isc.isA.SliderItem(item)) {
			result = value == null ? "" : String(value);
		} else if (item.dateTextField || item.timeTextField) {
			var field = item.dateTextField || item.timeTextField;
			result = field.getElementValue ? String(field.getElementValue()) : formatValue(item, value);
			alternatives.push(formatValue(item, value));
		} else if (isc.isA.BizLookupDescriptionItem && isc.isA.BizLookupDescriptionItem(item)) {
			var combo = item.canvas && item.canvas.getItem ? item.canvas.getItem("_combo") : null;
			result = combo ? strip(combo.getDisplayValue()) : strip(item.getDisplayValue());
			alternatives.push(value == null ? "" : String(value));
		} else if (item.getValueMap && item.getValueMap() && !isc.isA.TextItem(item)) {
			// combos and radios display the value map's text; the code and the zero-based option index
			// (what dataEnter takes) are accepted too
			result = strip(item.getDisplayValue());
			alternatives.push(value == null ? "" : String(value));
			var index = optionIndex(item, value);
			if (index >= 0) {
				alternatives.push(String(index));
			}
		} else if (isc.isA.CanvasItem(item) && item.canvas && item.canvas.getItem && item.canvas.getItem("value")) {
			// geometry: an inner form holds the text
			result = String(item.canvas.getItem("value").getElementValue() || "");
			alternatives.push(value == null ? "" : String(value));
		} else if (item.getElementValue && item.hasDataElement && item.hasDataElement()) {
			result = String(item.getElementValue() == null ? "" : item.getElementValue());
			alternatives.push(formatValue(item, value));
		} else {
			result = item.getDisplayValue ? strip(item.getDisplayValue()) : formatValue(item, value);
			alternatives.push(formatValue(item, value));
		}
		return { value: result, alternatives: alternatives, type: type, multiLine: multiLine(item) };
	};

	/**
	 * The index of the first data column of a grid, skipping list views' tag and flag columns and
	 * check box columns, so that clicking a row selects it instead of tagging, flagging or toggling it.
	 */
	S.dataColumn = function (locator) {
		var resolved = S.resolve(S.parse(locator).spec);
		var grid = resolved.obj;
		if (!grid || !grid.getFields) {
			return 0;
		}
		var fields = grid.getFields();
		var fallback = -1;
		for (var i = 0; i < fields.length; i++) {
			var name = String(fields[i].name || "");
			if (name !== "bizTagged" && name !== "bizFlagComment" && name.charAt(0) !== "_") {
				// clicking a check box cell would toggle it
				if (fields[i].type !== "boolean") {
					return i;
				}
				if (fallback < 0) {
					fallback = i;
				}
			}
		}
		return fallback < 0 ? 0 : fallback;
	};

	/** Scrolls a grid's row into view with ListGrid.scrollToRow(); returns false if the locator is not a grid. */
	S.scrollToRow = function (locator, row) {
		var g = gridOf(locator);
		if (!g.grid || !g.grid.scrollToRow) {
			return false;
		}
		g.grid.scrollToRow(row);
		return true;
	};

	/** True if a grid's row is selected (clicking it again would read as a double click, which zooms). */
	S.rowSelected = function (locator, row) {
		var resolved = S.resolve(S.parse(locator).spec);
		var grid = resolved.obj;
		if (!grid || !grid.getRecord || !grid.isSelected) {
			return false;
		}
		var record = grid.getRecord(row);
		return !!record && grid.isSelected(record);
	};

	/** The row a data grid is editing inline, or -1. */
	S.editRow = function (locator) {
		var resolved = S.resolve(S.parse(locator).spec);
		var grid = resolved.obj;
		var list = grid && grid.grid ? grid.grid : grid;
		if (!list || !list.getEditRow) {
			return -1;
		}
		var row = list.getEditRow();
		return row == null ? -1 : row;
	};

	function same(a, b) {
		if (a === b) {
			return true;
		}
		if (a == null || b == null) {
			return a == b;
		}
		if (isc.isA.Date(a) && isc.isA.Date(b)) {
			return a.getTime() === b.getTime();
		}
		return String(a) === String(b);
	}

	/**
	 * Commits what was typed into an item, the way leaving the field does, and reports where the value is.
	 * Blur is what normally commits a SmartClient text field, and a browser window without OS focus
	 * (a headed run behind another window) does not deliver focus and blur events reliably, so the typed
	 * text could stay in the DOM only. FormItem.updateValue() takes the element's value into the item,
	 * runs its change handlers and updates the form's ValuesManager; it does nothing if they already agree.
	 *
	 * @param locator	the SAIL locator of the item (with the suffix of the element typed into)
	 * @return { element, stored, committed, focused } or { error }
	 */
	S.commit = function (locator) {
		var parsed = S.parse(locator);
		var resolved = S.resolve(parsed.spec);
		var item = resolved.obj;
		if (!isFormItem(item)) {
			return { error: resolved.error || (locator + " is not a form item") };
		}
		var typed = item;
		if (parsed.suffix) {
			var element = isc.AutoTest.getElement(isc.AutoTest.getObjectLocator(item).replace(/\/objectType=[A-Za-z]+$/, "") + parsed.suffix);
			var owner = element ? isc.AutoTest.getObject(isc.AutoTest.getLocator(element)) : null;
			if (isFormItem(owner)) {
				typed = owner;
			}
		}
		if (isc.isA.RichTextItem(item)) {
			var html = item.canvas ? item.canvas.getValue() : null;
			if (!same(item.getValue(), html)) {
				item.storeValue(html);
			}
		}
		else {
			if (typed !== item && typed.updateValue) {
				typed.updateValue();
			}
			// date and time items take their value from their text field
			if (item.updateValue && (typed === item || item.dateTextField || item.timeTextField)) {
				item.updateValue();
			}
		}
		var form = item.form;
		var vm = form && form.valuesManager;
		var stored = item.getValue();
		var elementValue = isc.isA.RichTextItem(item) ? strip(item.canvas.getValue()) :
							(typed.getElementValue ? typed.getElementValue() : null);
		var cls = typed.getClassName();
		return {
			element: elementValue == null ? "" : String(elementValue),
			// a plain text field shows exactly what was typed (masked, formatted and date fields need not)
			plain: (cls === "TextItem" || cls === "PasswordItem" || cls === "TextAreaItem") && !typed.mask &&
					!typed.formatter && !typed.formatEditorValue && typed === item,
			stored: stored == null ? "" : String(stored),
			committed: !vm || same(stored, vm.getValue(item.name)),
			focused: document.hasFocus()
		};
	};

	// ---------------------------------------------------------------- grid rows

	/** The grid (a ListGrid) a SAIL grid locator resolves to: list views, list grids and data grids give their inner grid. */
	function gridOf(locator) {
		var resolved = S.resolve(S.parse(locator).spec);
		if (!resolved.obj) {
			return { error: resolved.error || "no grid for " + locator };
		}
		var grid = resolved.obj;
		if (grid.grid && isc.isA.ListGrid(grid.grid)) {
			grid = grid.grid;
		}
		return isc.isA.ListGrid(grid) ? { grid: grid } : { error: locator + " is not a grid" };
	}

	/**
	 * Checks that a data grid's row can be dragged to a position, and remembers the row's record so that dataGridMoved()
	 * can tell where it went: { rows } or { error }. Only an ordered data grid (SmartClient's canReorderRecords) lets a
	 * user drag its rows.
	 */
	S.dataGridMovePlan = function (locator, from, to) {
		var g = gridOf(locator);
		if (g.error) {
			return g;
		}
		var grid = g.grid;
		if (!grid.canReorderRecords) {
			return { error: "it is not an ordered grid, so its rows cannot be dragged (or it may not be changed)" };
		}
		var rows = grid.getTotalRows();
		if (from < 0 || from >= rows) {
			return { error: "it has no row " + from + " (it has " + rows + " rows)" };
		}
		if (to >= rows) {
			return { error: "position " + to + " is past its last row (it has " + rows + " rows)" };
		}
		grid._sailMoving = grid.getRecord(from);
		return { rows: rows };
	};

	/**
	 * The header button of a grid's column, to click to sort the grid by it: { header, sorted (the column is the grid's
	 * only sort), descending } or { error }.
	 */
	S.gridHeader = function (locator, column) {
		var g = gridOf(locator);
		if (g.error) {
			return g;
		}
		var grid = g.grid;
		var fieldNum = grid.getFieldNum(column);
		if (fieldNum < 0) {
			return { error: "the grid has no column " + column };
		}
		if (!grid.canSort) {
			return { error: "the grid cannot be sorted (an ordered grid keeps its rows' order)" };
		}
		var button = grid.getFieldHeaderButton(fieldNum);
		var element = button && button.getHandle ? button.getHandle() : null;
		if (!element) {
			return { error: "column " + column + " has no header shown" };
		}
		var spec = grid.getSort ? (grid.getSort() || []) : [];
		var sorted = spec.length === 1 && spec[0].property === column;
		return { header: element, sorted: sorted, descending: sorted && spec[0].direction === "descending" };
	};

	/** True if the record dataGridMovePlan() remembered is now the data grid's row at the position; forgets it. */
	S.dataGridMoved = function (locator, to) {
		var g = gridOf(locator);
		if (g.error) {
			return false;
		}
		var moving = g.grid._sailMoving;
		delete g.grid._sailMoving;
		return !!moving && g.grid.getRecord(to) === moving;
	};

	/** The text a row shows, cell by cell, skipping list views' tag and flag columns. */
	function rowTexts(grid, rowNum) {
		var record = grid.getRecord(rowNum);
		var fields = grid.getFields();
		var texts = [];
		for (var i = 0; i < fields.length; i++) {
			var name = String(fields[i].name || "");
			if (name === "bizTagged" || name === "bizFlagComment") {
				continue;
			}
			var value = record ? grid.getCellValue(record, rowNum, i) : null;
			texts.push(strip(value));
		}
		return texts;
	}

	function rowContains(grid, rowNum, text) {
		var texts = rowTexts(grid, rowNum);
		for (var i = 0; i < texts.length; i++) {
			if (texts[i].indexOf(text) >= 0) {
				return true;
			}
		}
		return false;
	}

	/**
	 * The rows a grid has: { rows, loaded }. A list grid pages its rows, so it counts as loaded once its length is known
	 * and its first page has arrived; rowContaining then searches the rows it has fetched, in the order shown.
	 */
	function loadedRows(grid) {
		var total = grid.getTotalRows();
		var data = grid.getData();
		// A plain array counts as loaded: some list grids never fetch (User Hits: autoFetchData false, data stays []),
		// and callers count only once the page has settled, i.e. after any fetch a draw started has returned
		if (data && data.lengthIsKnown && !data.lengthIsKnown()) {
			return { rows: 0, loaded: false };
		}
		var loaded = !(data && data.rowIsLoaded && total > 0 && !data.rowIsLoaded(0));
		return { rows: total, loaded: loaded };
	}

	function rowFetched(grid, rowNum) {
		var data = grid.getData();
		return !(data && data.rowIsLoaded) || data.rowIsLoaded(rowNum);
	}

	/** The zero-based index of the first row showing the text in one of its cells, or -1 (or { error }). */
	S.rowIndex = function (locator, text) {
		var g = gridOf(locator);
		if (g.error) {
			return { error: g.error, retry: true };
		}
		var state = loadedRows(g.grid);
		if (!state.loaded) {
			return { error: "the grid's rows are still loading", retry: true };
		}
		for (var i = 0; i < state.rows; i++) {
			if (rowFetched(g.grid, i) && rowContains(g.grid, i, text)) {
				return { row: i };
			}
		}
		return { row: -1, rows: state.rows };
	};

	/** Counts a grid's rows, or only those showing the text: { count, loaded } or { error }. */
	S.countRows = function (locator, text) {
		var g = gridOf(locator);
		if (g.error) {
			return { error: g.error };
		}
		var state = loadedRows(g.grid);
		if (!state.loaded) {
			return { count: -1, loaded: false };
		}
		if (text == null) {
			return { count: state.rows, loaded: true };
		}
		var count = 0;
		for (var i = 0; i < state.rows; i++) {
			if (rowFetched(g.grid, i) && rowContains(g.grid, i, text)) {
				count++;
			}
		}
		return { count: count, loaded: true };
	};

	/** Asks a list grid to fetch its rows again (a data grid shows its bean's rows, so it has nothing to refetch). */
	S.refreshGrid = function (locator) {
		var resolved = S.resolve(S.parse(locator).spec);
		var owner = resolved.obj;
		if (owner && owner.grid && isc.isA.BizListGrid(owner)) {
			if (owner.refresh) {
				owner.refresh();
			}
			else {
				owner.grid.invalidateCache();
			}
			return true;
		}
		var g = gridOf(locator);
		if (g.grid && g.grid.dataSource && g.grid.invalidateCache) {
			g.grid.invalidateCache();
			return true;
		}
		return false;
	};

	// ---------------------------------------------------------------- list grid filters and sorting

	/** The BizListGrid a SAIL list grid locator names (a list view's, or one in a view): { owner, grid } or { error, retry }. */
	function listOwner(locator) {
		var spec = S.parse(locator).spec;
		var whole = {};
		Object.keys(spec || {}).forEach(function (key) {
			if (key !== "part") {
				whole[key] = spec[key];
			}
		});
		var resolved = S.resolve(whole);
		var owner = resolved.obj;
		if (!owner) {
			return { error: resolved.error || "no list grid for " + locator, retry: true };
		}
		if (!isc.isA.BizListGrid(owner) || !owner.grid) {
			return { error: locator + " is not a list grid" };
		}
		return { owner: owner, grid: owner.grid };
	}

	/** A grid field by name, shown or hidden, or the data source's field. */
	function fieldDef(grid, name) {
		var fields = grid.getAllFields ? grid.getAllFields() : grid.getFields();
		for (var i = 0; i < fields.length; i++) {
			if (fields[i].name === name) {
				return fields[i];
			}
		}
		return grid.getDataSource() ? grid.getDataSource().getField(name) : null;
	}

	/** The name of the grid field for a column (its sanitised binding, or its _display_ field), or null. */
	function listField(grid, column) {
		if (fieldDef(grid, column)) {
			return column;
		}
		return fieldDef(grid, "_display_" + column) ? "_display_" + column : null;
	}

	/** The criteria the grid fetched its rows with, as JSON, to tell when a gesture's fetch has happened. */
	function fetchedCriteria(grid) {
		var data = grid.getData();
		return JSON.stringify(data && data.getCriteria ? data.getCriteria() : null);
	}

	/**
	 * What a column holds, which decides its filter row cell and the operators it takes: association (a select of
	 * records), enum (a select of options), boolean (a check box), date, datetime, time, number or text.
	 */
	function columnKind(grid, name) {
		var field = fieldDef(grid, name);
		var properties = field.filterEditorProperties || field.editorProperties;
		if (properties && properties.optionDataSource) {
			return "association";
		}
		if (field.valueMap) {
			return "enum";
		}
		var base = isc.SimpleType.getBaseType(field.type || "text", grid.getDataSource()) || "text";
		if (base === "boolean" || base === "date" || base === "datetime" || base === "time") {
			return base;
		}
		if (base === "integer" || base === "float" || base === "localeInt" || base === "localeFloat" || base === "localeCurrency") {
			return "number";
		}
		return "text";
	}

	/** The SAIL operators each kind of column takes. */
	var KIND_OPERATORS = {
		association: ["equals"],
		enum: ["equals"],
		boolean: ["equals"],
		date: ["equals", "lessThan", "greaterThan"],
		datetime: ["equals", "lessThan", "greaterThan"],
		time: ["equals", "lessThan", "greaterThan"]
	};

	/**
	 * The operator the filter row applies to a kind of column: text is matched as a substring (case-insensitively),
	 * anything else by equality. The grid does not let a user choose another operator there.
	 */
	function rowOperator(kind) {
		return kind === "text" ? "contains" : "equals";
	}

	/** The SmartClient operator a FilterBuilder clause uses for a SAIL operator, as the clause's operator picker titles it. */
	var CLAUSE_OPERATORS = {
		contains: "iContains",
		startsWith: "iStartsWith",
		equals: "equals",
		notEquals: "notEqual",
		lessThan: "lessThan",
		greaterThan: "greaterThan"
	};

	/** The options of an enum column: [{ code, label }]. */
	function enumOptions(field) {
		var map = field.valueMap || {};
		return (isc.isA.Array(map) ? map.map(String) : Object.keys(map)).map(function (code) {
			return { code: code, label: strip(isc.isA.Array(map) ? code : map[code]) };
		});
	}

	/**
	 * The option a value names: the one whose shown label is the value, or else the one whose code is. Returns
	 * { option } or { error } when neither matches, or more than one does, listing the options as code=label.
	 */
	function namedOption(options, value, what) {
		var byLabel = options.filter(function (o) {
			return o.label === value;
		});
		var matches = byLabel.length ? byLabel : options.filter(function (o) {
			return o.code === value;
		});
		if (matches.length === 1) {
			return { option: matches[0] };
		}
		return {
			error: (matches.length ? "more than one option of " : "no option of ") + what + " has the label or code '" + value +
					"'; the options (code=label) are " + options.map(function (o) {
						return o.code + "=" + o.label;
					}).join(", ")
		};
	}

	/** The format a date, date time or time column shows, as an example of now, for messages. */
	function exampleText(field, kind) {
		var type = isc.SimpleType.getType(field.type);
		var editor = type && type.editorType && isc[type.editorType];
		var formatter = editor && editor.getInstanceProperty ? editor.getInstanceProperty("dateFormatter") : null;
		var now = new Date();
		if (kind === "time") {
			return isc.Time.toShortTime(now, formatter || "toShortPadded24HourTime", true);
		}
		return formatter && now[formatter] ? now[formatter]() : String(now);
	}

	/**
	 * The value a filter compares a column with, from the step's text: { value } or { error }.
	 * An enum takes an option's label or code, a boolean true or false, and a date, date time or time the text the
	 * column shows (as dataEnter takes it for such a field).
	 */
	function columnValue(grid, name, kind, text) {
		var field = fieldDef(grid, name);
		if (kind === "enum") {
			var named = namedOption(enumOptions(field), text, "column " + name);
			return named.error ? named : { value: named.option.code };
		}
		if (kind === "boolean") {
			if (text !== "true" && text !== "false") {
				return { error: "a boolean column is filtered by true or false, not '" + text + "'" };
			}
			return { value: text === "true" };
		}
		if (kind === "date" || kind === "datetime") {
			var type = isc.SimpleType.getType(field.type);
			var editor = type && type.editorType && isc[type.editorType];
			var formatter = editor && editor.getInstanceProperty ? editor.getInstanceProperty("dateFormatter") : null;
			var date = isc.BizDateItem.parseInput(text, formatter);
			if (!date) {
				return { error: "'" + text + "' is not a " + kind + " in the column's format, such as " + exampleText(field, kind) };
			}
			return { value: date };
		}
		if (kind === "time") {
			if (!/^\d?\d:\d\d(:\d\d)?\s*([aApP][mM])?$/.test(text)) {
				return { error: "'" + text + "' is not a time in the column's format, such as " + exampleText(field, kind) };
			}
			return { value: isc.Time.parseInput(text) };
		}
		if (kind === "number" && !isNaN(Number(text))) {
			return { value: Number(text) };
		}
		return { value: text };
	}

	/** The top level clauses of advanced criteria, or the clauses simple filter row criteria stand for. */
	function clauses(criteria) {
		if (!criteria || isc.isA.emptyObject(criteria)) {
			return [];
		}
		if (!isc.DataSource.isAdvancedCriteria(criteria)) {
			criteria = isc.DataSource.convertCriteria(criteria, "substring");
		}
		if (criteria.operator !== "and" || !criteria.criteria) {
			return [criteria];
		}
		return criteria.criteria.slice();
	}

	/**
	 * The list grid's state: { advanced, criteria (JSON of the criteria of its rows), filtered, loaded, sort (JSON) } or
	 * { error }.
	 */
	S.listState = function (locator) {
		var l = listOwner(locator);
		if (l.error) {
			return l;
		}
		var state = loadedRows(l.grid);
		var data = l.grid.getData();
		return {
			advanced: !!l.owner._advancedFilter.toggleButton.selected,
			criteria: fetchedCriteria(l.grid),
			filtered: clauses(data && data.getCriteria ? data.getCriteria() : null).length > 0,
			loaded: state.loaded,
			sort: JSON.stringify(l.grid.getSort() || [])
		};
	};

	/** The filter row cell of a column, if the row shows the column: { item, button } or null. */
	function rowCell(grid, name) {
		var editor = grid.filterEditor;
		var form = editor && editor.getEditForm ? editor.getEditForm() : null;
		var item = form ? form.getItem(name) : null;
		if (!item || !editor.actionButton || !S.shown(editor) || !item.isVisible()) {
			return null;
		}
		return { item: item, button: isc.AutoTest.getElement(S.objectLocator(editor.actionButton)) };
	}

	/** The element a user types into for a filter row cell (a date or time item's text field). */
	function typingElement(item) {
		var field = item.dateTextField || item.timeTextField || item;
		return field.getFocusElement() || field.getDataElement() || itemElement(item);
	}

	/**
	 * Works out how a user filters a column of a list grid, without changing anything.
	 * The filter row is used while the grid is in simple filtering, the row shows the column and its own operator for the
	 * column is the one wanted (or the column is being cleared). Otherwise the advanced filter is used, with the criteria
	 * the grid has, the column's clause replaced; the caller presses the toggle (if the grid is in simple filtering), then
	 * calls listAdvancedCriteria and presses Filter.
	 * Returns { mode: "row", cell: type|select|checkbox, ... } or { mode: "advanced", toggle, criteria } or { error, retry }.
	 */
	S.listFilterPlan = function (locator, column, value, operator) {
		var l = listOwner(locator);
		if (l.error) {
			return l;
		}
		var grid = l.grid;
		var name = listField(grid, column);
		if (!name) {
			return { error: "the list grid has no column " + column };
		}
		var kind = columnKind(grid, name);
		var allowed = KIND_OPERATORS[kind];
		if (value != null && allowed && allowed.indexOf(operator) < 0) {
			return { error: "column " + column + " holds " + (kind === "enum" || kind === "association" ? "an " : "a ") + kind +
						", which is filtered with " + allowed.join(", ") + ", not " + operator };
		}
		var converted = value == null ? { value: null } : columnValue(grid, name, kind, value);
		if (converted.error) {
			return { error: "column " + column + ": " + converted.error };
		}
		var filter = l.owner._advancedFilter;
		var advanced = !!filter.toggleButton.selected;
		var cell = advanced ? null : rowCell(grid, name);
		if (cell && (value == null || rowOperator(kind) === operator)) {
			var item = cell.item;
			var result = { mode: "row", kind: kind, button: cell.button };
			if (isc.isA.SelectItem(item)) {
				result.cell = "select";
				result.opener = itemElement(item);
				result.multiple = !!item.multiple;
			}
			else if (isc.isA.CheckboxItem(item)) {
				var states = [null, true, false]; // the order a click cycles through
				var from = states.indexOf(item.getValue() == null ? null : item.getValue());
				var to = states.indexOf(converted.value);
				result.cell = "checkbox";
				result.element = isc.AutoTest.getElement(S.objectLocator(item) + "/valueicon") || itemElement(item);
				result.clicks = (to - Math.max(from, 0) + 3) % 3;
			}
			else {
				var input = typingElement(item);
				result.cell = "type";
				result.input = input;
				result.current = input && input.value != null ? String(input.value) : "";
			}
			return result;
		}
		if (kind === "association") {
			return { error: "column " + column + " is an association, which SmartClient filters by choosing a record in the filter row, " +
						(advanced ? "and the grid is in advanced filtering, which hides the row" : "and the row does not show the column") };
		}
		var existing = advanced ? filter.getCriteria() : grid.getFilterEditorCriteria(true);
		var kept = clauses(existing).filter(function (c) {
			return c.fieldName !== name;
		});
		if (value != null) {
			kept.push({ fieldName: name, operator: CLAUSE_OPERATORS[operator], value: converted.value });
		}
		// the criteria keep their dates here, as they would not survive the trip to the harness and back
		l.owner._sailCriteria = { _constructor: "AdvancedCriteria", operator: "and", criteria: kept };
		return {
			mode: "advanced",
			toggle: advanced ? null : isc.AutoTest.getElement(S.objectLocator(filter.toggleButton)),
			criteria: JSON.stringify(l.owner._sailCriteria)
		};
	};

	/**
	 * The rows to click in a filter row select's open pick list so that it chooses exactly the option the value names
	 * (none when the value is null): { rows (elements), multiple } or { error, retry } while the pick list is not shown
	 * or loading. A multiple select toggles rows; a single select takes one click.
	 */
	S.listSelectRows = function (locator, column, value) {
		var l = listOwner(locator);
		if (l.error) {
			return l;
		}
		var name = listField(l.grid, column);
		var cell = name ? rowCell(l.grid, name) : null;
		if (!cell || !isc.isA.SelectItem(cell.item)) {
			return { error: "the filter row has no select for column " + column };
		}
		var item = cell.item;
		var list = item.pickList;
		if (!list || !S.shown(list)) {
			return { error: "the select's pick list is not showing", retry: true };
		}
		var state = loadedRows(list);
		if (!state.loaded) {
			return { error: "the select's options are still loading", retry: true };
		}
		var valueField = item.getValueFieldName();
		var displayField = item.getDisplayFieldName ? item.getDisplayFieldName() : null;
		var options = [];
		for (var i = 0; i < state.rows; i++) {
			var record = rowFetched(list, i) ? list.getRecord(i) : null;
			var code = record ? record[valueField] : null;
			var label = record && displayField && record[displayField] != null ? record[displayField] : item.mapValueToDisplay(code);
			options.push({ row: i, code: code == null ? "" : String(code), label: strip(label) });
		}
		var wanted = null;
		if (value != null) {
			var named = namedOption(options.filter(function (o) {
				return o.code !== "";
			}), value, "column " + column);
			if (named.error) {
				return { error: named.error };
			}
			wanted = named.option;
		}
		var element = function (row) {
			return isc.AutoTest.getElement(S.objectLocator(list) + "/body/row[" + row + "]/col[0]");
		};
		if (!item.multiple) {
			var target = wanted || options.filter(function (o) {
				return o.code === "";
			})[0];
			if (!target) {
				return { error: "the select of column " + column + " has no empty option to clear it with" };
			}
			var current = item.getValue();
			return { rows: [element(target.row)], multiple: false, changed: (current == null ? "" : String(current)) !== target.code };
		}
		var selected = item.getValue() || [];
		var rows = [];
		options.forEach(function (o) {
			var on = selected.map(String).indexOf(o.code) >= 0;
			if (on !== (wanted === o)) {
				rows.push(element(o.row));
			}
		});
		return { rows: rows, multiple: true };
	};

	/**
	 * Puts the criteria listFilterPlan worked out into the advanced filter's clauses, as a saved snapshot does, and returns
	 * its Filter button. The grid must be in advanced filtering (its toggle pressed).
	 */
	S.listAdvancedCriteria = function (locator) {
		var l = listOwner(locator);
		if (l.error) {
			return l;
		}
		var filter = l.owner._advancedFilter;
		if (!filter.toggleButton.selected) {
			return { error: "the list grid is not in advanced filtering" };
		}
		if (!l.owner._sailCriteria) {
			return { error: "no criteria were worked out for the advanced filter" };
		}
		filter.setCriteria(l.owner._sailCriteria);
		delete l.owner._sailCriteria;
		return { button: isc.AutoTest.getElement(S.objectLocator(filter._filterButton)) };
	};

	/** The list grid toolbar's Clear (filter) button. */
	S.listClearButton = function (locator) {
		var l = listOwner(locator);
		if (l.error) {
			return l;
		}
		var tools = l.owner.getMembers()[0];
		var buttons = (tools && tools.getMembers ? tools.getMembers() : []).filter(function (m) {
			return isc.isA.Canvas(m) && /icons\/filter_delete\.png$/.test(String(m.icon || ""));
		});
		if (!buttons.length) {
			return { error: "the list grid's toolbar has no Clear filter button (does it show filters?)" };
		}
		return { button: isc.AutoTest.getElement(S.objectLocator(buttons[0])), disabled: buttons[0].isDisabled() };
	};

	/**
	 * The sort a list grid shows, and the header button that sorts by a column: { sorted (the column is the only sort),
	 * descending, header, canSort } or { error, retry }.
	 */
	S.listSortPlan = function (locator, column) {
		var l = listOwner(locator);
		if (l.error) {
			return l;
		}
		var grid = l.grid;
		var name = listField(grid, column);
		if (!name) {
			return { error: "the list grid has no column " + column };
		}
		var sort = grid.getSort() || [];
		var header = grid.getFieldHeaderButton ? grid.getFieldHeaderButton(grid.getFieldNum(name)) : null;
		return {
			sorted: sort.length === 1 && sort[0].property === name,
			descending: sort.length > 0 && sort[0].direction === "descending",
			header: header ? header.getClipHandle() : null,
			canSort: grid.canSort !== false && fieldDef(grid, name).canSort !== false
		};
	};

	// ---------------------------------------------------------------- options and pick lists

	/**
	 * The zero-based position, as dataEnter takes it, of the combo or radio option whose code or label is the value:
	 * { index } or { error, options }. A combo counts its empty first entry when it has one.
	 */
	S.optionIndex = function (locator, value, byCode) {
		var resolved = S.resolve(S.parse(locator).spec);
		var item = resolved.obj;
		if (!isFormItem(item)) {
			return { error: resolved.error || locator + " is not a form item", retry: !!resolved.retry };
		}
		var options = [];
		var data = item.getClientPickListData ? item.getClientPickListData() : null;
		if (data && data.length) {
			var valueField = item.getValueFieldName ? item.getValueFieldName() : item.name;
			var displayField = item.getDisplayFieldName ? item.getDisplayFieldName() : null;
			data.forEach(function (row) {
				var code = row[valueField];
				var label = displayField && row[displayField] != null ? row[displayField] : item.mapValueToDisplay(code);
				options.push({ code: code == null ? "" : String(code), label: strip(label) });
			});
		}
		else {
			var map = item.getValueMap() || {};
			(isc.isA.Array(map) ? map.map(String) : Object.keys(map)).forEach(function (code) {
				options.push({ code: code, label: strip(isc.isA.Array(map) ? code : map[code]) });
			});
		}
		var matches = [];
		options.forEach(function (option, i) {
			if ((byCode ? option.code : option.label) === value) {
				matches.push(i);
			}
		});
		if (matches.length === 1) {
			return { index: matches[0] };
		}
		return {
			error: (matches.length ? "more than one option has the " : "no option has the ") + (byCode ? "code " : "label ") + value,
			options: options.map(function (o) {
				return o.code + "=" + o.label;
			})
		};
	};

	/**
	 * The first row, in the order shown, of a lookup description's open pick list that shows the text in a cell:
	 * { row, rows } (row -1 when none does), or { error, retry } while the pick list is not shown or its rows are loading.
	 */
	S.pickListRow = function (locator, text) {
		var resolved = S.resolve(S.parse(locator).spec);
		var item = resolved.obj;
		// a form's lookup description holds a combo box; an inline data grid row edits the column with a plain one
		var combo = item && item.canvas && item.canvas.getItem ? item.canvas.getItem("_combo") :
					(item && isc.isA.ComboBoxItem(item) ? item : null);
		if (!combo) {
			return { error: resolved.error || locator + " is not a lookup description", retry: !!resolved.retry };
		}
		var list = combo.pickList;
		if (!list || !S.shown(list)) {
			return { error: "the lookup's pick list is not showing", retry: true };
		}
		var state = loadedRows(list);
		if (!state.loaded) {
			return { error: "the lookup's pick list rows are still loading", retry: true };
		}
		for (var i = 0; i < state.rows; i++) {
			if (rowFetched(list, i) && rowContains(list, i, text)) {
				return { row: i, rows: state.rows };
			}
		}
		return { row: -1, rows: state.rows };
	};

	// ---------------------------------------------------------------- links

	/** The displayed links (<a>) of the page, in document order. */
	function links() {
		return Array.prototype.slice.call(document.querySelectorAll("a")).filter(function (a) {
			return a.getClientRects().length > 0 && getComputedStyle(a).visibility !== "hidden";
		});
	}

	/** The visible texts of the page's displayed links, at most 30, for a failure. */
	S.linkTexts = function () {
		return links().map(function (a) {
			return (a.innerText || "").replace(/\s+/g, " ").trim();
		}).filter(function (t) {
			return t;
		}).slice(0, 30);
	};

	/**
	 * The first displayed link whose visible text contains the text (case-sensitive), or with exact, whose visible text
	 * (trimmed, white space collapsed) equals it: { element, href, target, newPage }, where newPage says whether following
	 * it loads another page (an http(s) URL that is not just this page with another fragment), or null if no link does.
	 */
	S.link = function (text, exact) {
		var found = links().filter(function (a) {
			var shown = a.innerText || "";
			return exact ? (shown.replace(/\s+/g, " ").trim() === text) : (shown.indexOf(text) >= 0);
		})[0];
		if (!found) {
			return null;
		}
		var href = found.getAttribute("href");
		var newPage = false;
		if (href && href.charAt(0) !== "#") {
			var url = new URL(found.href, location.href);
			var here = new URL(location.href);
			newPage = /^https?:$/.test(url.protocol) &&
						!(url.origin === here.origin && url.pathname === here.pathname && url.search === here.search && url.hash);
		}
		return { element: found, href: found.href, target: found.getAttribute("target") || "", newPage: newPage };
	};

	/**
	 * The text a grid cell shows, as a user reads it: { text } or { error, retry }. The column is the grid field named by
	 * the (sanitised) binding. A row being edited inline reads as its editor's value.
	 */
	S.cellText = function (locator, rowNum, field) {
		var g = gridOf(locator);
		if (g.error) {
			return { error: g.error, retry: true };
		}
		var grid = g.grid;
		var state = loadedRows(grid);
		if (!state.loaded) {
			return { error: "the grid's rows are still loading", retry: true };
		}
		if (rowNum >= state.rows) {
			return { error: "the grid has " + state.rows + " rows" };
		}
		var fieldNum = grid.getFieldNum(field);
		if (fieldNum < 0) {
			return { error: "the grid shows no field " + field + ": its fields are " + grid.getFields().map(function (f) {
				return f.name;
			}).join(", ") };
		}
		if (grid.getEditRow && grid.getEditRow() === rowNum && grid.getEditValue) {
			var edited = grid.getEditValue(rowNum, fieldNum);
			return { text: edited == null ? "" : String(edited) };
		}
		var record = grid.getRecord(rowNum);
		return { text: strip(record ? grid.getCellValue(record, rowNum, fieldNum) : "") };
	};

	/** The title the current view shows in its header, or null. */
	S.title = function () {
		var view = S.topView();
		var header = (view && S.describe(view) === "list") ? isc.ListView._header : (view && view._header);
		var member = header && header.getMember ? header.getMember(0) : null;
		var element = member && member.getHandle ? member.getHandle() : null;
		var title = element ? element.querySelector(".titleBar:not(i)") : null;
		return title ? strip(title.innerText || title.textContent) : (element ? strip(element.innerText) : null);
	};

	/**
	 * The first displayed link in a grid row whose visible text contains the text (case-sensitive):
	 * { element, href, newPage, links } (element null if none does; links are the row's link texts) or { error, retry }.
	 * The row is scrolled into view the grid's way first.
	 */
	S.rowLink = function (locator, rowNum, text) {
		var g = gridOf(locator);
		if (g.error) {
			return { error: g.error, retry: true };
		}
		var grid = g.grid;
		var state = loadedRows(grid);
		if (!state.loaded) {
			return { error: "the grid's rows are still loading", retry: true };
		}
		if (rowNum >= state.rows) {
			return { error: "the grid has " + state.rows + " rows" };
		}
		grid.scrollToRow(rowNum);
		var body = grid.body;
		var row = body && body.getTableElement ? body.getTableElement(rowNum) : null;
		if (!row) {
			return { error: "row " + rowNum + " is not drawn yet", retry: true };
		}
		var shown = Array.prototype.slice.call(row.querySelectorAll("a")).filter(function (a) {
			return a.getClientRects().length > 0 && getComputedStyle(a).visibility !== "hidden";
		});
		var found = shown.filter(function (a) {
			return (a.innerText || "").indexOf(text) >= 0;
		})[0] || null;
		var newPage = false;
		var href = found ? found.getAttribute("href") : null;
		if (href && href.charAt(0) !== "#" && !/^javascript:/i.test(href)) {
			var url = new URL(found.href, location.href);
			newPage = /^https?:$/.test(url.protocol);
		}
		return {
			element: found,
			href: href,
			newPage: newPage,
			links: shown.map(function (a) {
				return strip(a.innerText);
			})
		};
	};

	// ---------------------------------------------------------------- widget state, messages, menus

	/** { found, shown, enabled } for a widget, without failing when it is missing. */
	S.widgetState = function (locator) {
		var parsed = S.parse(locator);
		var resolved = S.resolve(parsed.spec);
		if (!resolved.obj) {
			// on the wrong view nothing can be said about the widget: the caller fails rather than read it as hidden
			return { found: false, shown: false, enabled: false, why: resolved.error, wrongView: !!resolved.wrongView };
		}
		var obj = resolved.obj;
		var shown = shownObject(obj);
		if (shown && parsed.spec.kind === "tab") {
			// a tab hidden by its condition is removed from or hidden in the tab bar
			shown = obj.isVisible() && obj.isDrawn();
		}
		return { found: true, shown: shown, enabled: !disabled(obj) };
	};

	/** The messages showing: [{ type: info|warning|error, text }]. */
	S.messages = function () {
		var result = [];
		var d = S.dialog();
		if (d) {
			if (d.kind === "say") {
				result.push({ type: "info", text: d.message });
			}
			else if (d.kind === "warn") {
				// Skyve shows server problems (and validation) in a warning dialog titled Problems
				var problems = (d.title === "Problems") || (isc.i18nMessages && d.title === isc.i18nMessages.editView_Problems);
				result.push({ type: problems ? "error" : "warning", text: d.message });
			}
		}
		S.problems().errors.forEach(function (e) {
			result.push({ type: "error", text: e });
		});
		// PrimeFaces growls (Skyve's desktop page has a growl): counted as soon as they are in the page, as
		// PrimeFaces fades them in with jQuery after the response has arrived
		Array.prototype.slice.call(document.querySelectorAll(".ui-growl-item")).forEach(function (g) {
			var cls = (g.querySelector(".ui-growl-image") || g).className;
			var type = /error|fatal/.test(cls) ? "error" : /warn/.test(cls) ? "warning" : "info";
			result.push({ type: type, text: strip(g.innerText || g.textContent) });
		});
		return result;
	};

	/**
	 * Finds a menu entry: the module's section by title, then groups and the item by their shown text.
	 * Returns { present, header (the section header element, when it is collapsed), row (the item's cell element) }.
	 */
	S.menu = function (moduleTitle, labels) {
		var stack = isc.Canvas.getById("adminStack");
		if (!stack) {
			return { present: false, why: "no menu on this page" };
		}
		var index = -1;
		for (var i = 0; i < stack.sections.length; i++) {
			if (strip(stack.sections[i].title) === moduleTitle) {
				index = i;
				break;
			}
		}
		if (index < 0) {
			return { present: false, why: "no menu section " + moduleTitle };
		}
		var section = stack.sections[index];
		var result = { present: true, expanded: stack.sectionIsExpanded(section) };
		if (!result.expanded) {
			var header = stack.getSectionHeader(index);
			result.header = header ? header.getClipHandle() : null;
		}
		if (!labels || labels.length === 0) {
			return result;
		}
		var tree = section.items && section.items[0];
		if (!tree || !tree.data) {
			return { present: false, why: "the " + moduleTitle + " section has no menu tree" };
		}
		var node = tree.data.getRoot();
		var path = [];
		for (var l = 0; l < labels.length; l++) {
			var children = tree.data.getChildren(node) || [];
			var next = null;
			for (var c = 0; c < children.length; c++) {
				if (strip(children[c].desc) === labels[l]) {
					next = children[c];
					break;
				}
			}
			if (!next) {
				return { present: false, why: "no menu entry " + labels.slice(0, l + 1).join(" / ") };
			}
			node = next;
			path.push(next);
		}
		if (!result.expanded) {
			return result; // the caller expands the section and asks again
		}
		// a closed group on the way: the caller opens it as a user does (double-clicking its row) and asks again
		for (var f = 0; f < path.length - 1; f++) {
			if (!tree.data.isOpen(path[f])) {
				var folderRow = tree.getRecordIndex(path[f]);
				if (folderRow >= 0) {
					tree.scrollToRow(folderRow);
				}
				result.folder = strip(path[f].desc);
				result.folderRow = folderRow < 0 ? null : isc.AutoTest.getElement(S.objectLocator(tree) + "/body/row[" + folderRow + "]/col[0]");
				return result;
			}
		}
		var row = tree.getRecordIndex(node);
		if (row >= 0) {
			// a menu tree's rows scroll inside its section: bring the row into view the grid's way
			tree.scrollToRow(row);
		}
		result.row = row < 0 ? null : isc.AutoTest.getElement(S.objectLocator(tree) + "/body/row[" + row + "]/col[0]");
		return result;
	};

	// ---------------------------------------------------------------- list membership and the HTML editor

	/** A list membership's candidates and members (their shown text). */
	S.membership = function (locator) {
		var resolved = S.resolve(S.parse(locator).spec);
		var m = resolved.obj;
		if (!m || !m._candidateList) {
			return { error: resolved.error || locator + " is not a list membership", retry: true };
		}
		var texts = function (list) {
			return (list.getData() || []).map(function (r) {
				return strip(r.bizKey);
			});
		};
		return { candidates: texts(m._candidateList), members: texts(m._memberList) };
	};

	/** The element of a list membership row or button: part is candidate, member, assign or unassign. */
	S.membershipElement = function (locator, part, text) {
		var resolved = S.resolve(S.parse(locator).spec);
		var m = resolved.obj;
		if (!m || !m._candidateList) {
			return null;
		}
		if (part === "assign" || part === "unassign") {
			var buttons = m.members[1].members;
			var b = buttons[part === "assign" ? 0 : 1];
			return isc.AutoTest.getElement(S.objectLocator(b));
		}
		var list = part === "candidate" ? m._candidateList : m._memberList;
		var data = list.getData() || [];
		for (var i = 0; i < data.length; i++) {
			if (strip(data[i].bizKey) === text) {
				return isc.AutoTest.getElement(S.objectLocator(list) + "/body/row[" + i + "]/col[0]");
			}
		}
		return null;
	};

	/** The HTML item's Edit button, and the editor window's Apply button and editor iframe when it is open. */
	S.htmlEditor = function (locator) {
		var resolved = S.resolve(S.parse(locator).spec);
		var item = resolved.obj;
		if (!item || !item._editButton) {
			return { error: resolved.error || locator + " is not an HTML editor", retry: true };
		}
		var result = { edit: isc.AutoTest.getElement(S.objectLocator(item._editButton)) };
		var editor = item._editor;
		if (editor && editor.status === "ready" && window.CKEDITOR && CKEDITOR.instances._CKEditor === editor) {
			result.frame = document.querySelector("iframe.cke_wysiwyg_frame");
			var stack = isc.WindowStack._stack;
			var top = stack[stack.length - 1];
			var apply = null;
			if (top) {
				canvases().forEach(function (c) {
					if (!apply && isc.isA.IButton(c) && top.contains(c) &&
							strip(c.getTitle()) === strip(isc.i18nMessages.bizHTMLItem_EditHTMLPopupApply)) {
						apply = c;
					}
				});
			}
			result.apply = apply ? isc.AutoTest.getElement(S.objectLocator(apply)) : null;
		}
		return result;
	};

	/** The Upload button of a content item (the main part of its split button). */
	S.uploadButton = function (locator) {
		var resolved = S.resolve(S.parse(locator).spec);
		var item = resolved.obj;
		if (!item || !item.canvas) {
			return { error: resolved.error || locator + " is not a content item", retry: true };
		}
		var title = strip(isc.i18nMessages.bizUtilSplitButton_UploadButtonTitle);
		var button = null;
		canvases().forEach(function (c) {
			if (!button && isc.isA.IButton(c) && !isc.isA.MenuButton(c) && item.canvas.contains(c) && strip(c.getTitle()) === title) {
				button = c;
			}
		});
		if (!button) {
			return { error: "no Upload button on " + locator + " (is it read only?)", retry: true };
		}
		return { element: isc.AutoTest.getElement(S.objectLocator(button)), disabled: button.isDisabled() };
	};

	/** The upload page's iframe in the top-most window, if one is open. */
	S.uploadFrame = function () {
		var frames = Array.prototype.slice.call(document.querySelectorAll("iframe")).filter(function (f) {
			return String(f.src || "").indexOf("upload.xhtml") >= 0 && f.getClientRects().length > 0;
		});
		return frames.length ? frames[frames.length - 1] : null;
	};

	/** The close button of the top-most window. */
	S.windowClose = function () {
		var stack = isc.WindowStack._stack || [];
		var top = stack[stack.length - 1];
		var button = top && top.closeButton;
		return button ? button.getClipHandle() : null;
	};

	/**
	 * The element at the centre of an element, if it belongs to the same canvas (SmartClient draws some widgets in
	 * several nested elements; the top one takes the click).
	 */
	S.sameCanvasAt = function (element) {
		var rect = element.getBoundingClientRect();
		var hit = document.elementFromPoint(rect.left + rect.width / 2, rect.top + rect.height / 2);
		var mine = canvasOf(element);
		return (hit && mine && canvasOf(hit) === mine) ? hit : null;
	};

	/**
	 * True if SmartClient's hover (the tooltip-like Label isc.Hover shows when the pointer rests on a widget, e.g. a
	 * text field's full value) is what is at the element's centre, covering it.
	 */
	S.hoverCovers = function (element) {
		var hover = isc.Hover && isc.Hover.hoverCanvas;
		if (!hover || !hover.isVisible || !hover.isVisible()) {
			return false;
		}
		var rect = element.getBoundingClientRect();
		var hit = document.elementFromPoint(rect.left + rect.width / 2, rect.top + rect.height / 2);
		for (var c = hit && canvasOf(hit); c; c = c.parentElement) {
			if (c === hover) {
				return true;
			}
		}
		return false;
	};

	/** True if the item runs a change handler (Skyve renders onChangedHandlers as the item's changed()), which may call the server. */
	S.hasChangeHandler = function (locator) {
		var resolved = S.resolve(S.parse(locator).spec);
		var item = resolved.obj;
		return !!(item && isFormItem(item) && isc.isA.Function(item.changed));
	};

	/** Sets a slider through its own widget API, as a drag would, and returns the new value. */
	S.slide = function (locator, value) {
		var resolved = S.resolve(S.parse(locator).spec);
		if (!resolved.obj || !resolved.obj.canvas) {
			return { error: resolved.error || "not a slider" };
		}
		var slider = resolved.obj.canvas;
		slider.setValue(Number(value));
		return { value: slider.getValue() };
	};

	/** Validation problems on the top-most view: errors per item, and any showing dialog. */
	S.problems = function () {
		var result = { dialog: S.dialog(), errors: [] };
		var view = S.topView();
		if (view && view._vm) {
			owned(view, function (c) {
				return isc.isA.DynamicForm(c);
			}).forEach(function (form) {
				var errors = form.getErrors ? form.getErrors() : null;
				if (errors) {
					Object.keys(errors).forEach(function (name) {
						var e = errors[name];
						result.errors.push(name + ": " + strip(isc.isA.Array(e) ? e.join(", ") : e));
					});
				}
			});
		}
		return result;
	};

	/** True if a data grid of the top-most view is editing a row inline. */
	S.inlineEditing = function () {
		var view = S.topView();
		if (!view || !view._vm) {
			return false;
		}
		return owned(view, function (c) {
			return isc.isA.BizDataGrid(c) && c.grid && c.grid.getEditRow && c.grid.getEditRow() != null;
		}).length > 0;
	};


	window._sail = S;
})();
