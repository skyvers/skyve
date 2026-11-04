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
 *   kind    - item | button | zoomIn | tab | dataGrid | listGrid | list | dialog
 *   name    - item name (sanitised binding), action name, binding, tab title or dialog button
 *   ordinal - zero-based index of a list grid in its view (kind listGrid)
 *   part    - new | zoom | edit | remove | rows (grids)
 */
(function () {
	"use strict";

	var VERSION = 1;
	if (window._sail && window._sail.version === VERSION) {
		return;
	}

	var S = { version: VERSION };

	/** True when the SmartClient runtime and the Skyve desktop harness are loaded. */
	S.loaded = function () {
		return !!(window.isc && isc.AutoTest && isc.BizUtil && isc.WindowStack && isc.Canvas);
	};

	/** True when no RPC is pending and AutoTest says the system is done. */
	S.idle = function () {
		return S.loaded() && !isc.RPCManager.requestsArePending() && isc.AutoTest.isSystemDone() === true;
	};

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

	/** What the browser is showing, for error messages and waits. */
	S.state = function () {
		if (!S.loaded()) {
			return { loaded: false, url: String(location.href), title: document.title };
		}
		return {
			loaded: true,
			idle: S.idle(),
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
		return !!item.form && S.shown(item.form) && item.isVisible() && item.isDrawn();
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

	/** The SmartClient dialog (isc.say, isc.warn, isc.ask all share isc.Dialog.Warn) if it is showing. */
	S.dialog = function () {
		var d = isc.Dialog && isc.Dialog.Warn;
		if (!d || !isc.isA.Canvas(d) || d.destroyed || !d.isDrawn() || !d.isVisible()) {
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

	function dialogButton(name) {
		var d = isc.Dialog && isc.Dialog.Warn;
		if (!S.dialog() || !d.toolbar) {
			return null;
		}
		var wanted = {
			ok: [isc.Dialog.OK_BUTTON_TITLE, "OK"],
			yes: [isc.Dialog.YES_BUTTON_TITLE, "Yes"],
			no: [isc.Dialog.NO_BUTTON_TITLE, "No"],
			cancel: [isc.Dialog.CANCEL_BUTTON_TITLE, "Cancel"]
		}[name] || [name];
		var members = d.toolbar.getMembers();
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
			return { obj: gridPart(isc.ListView._grid, spec.part) };
		}
		if (spec.view && showing !== spec.view) {
			return { error: "expected view " + spec.view + " but the browser shows " + showing, retry: true };
		}

		var found = null;
		var what = "";
		switch (spec.kind) {
			case "item":
				found = pick(items(view, spec.name));
				what = "form item named " + spec.name;
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
				return resolved;
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
		if (!isFormItem(item)) {
			return { error: locator + " is not a form item" };
		}
		var type = item.getClassName();
		var value = item.getValue();
		var alternatives = [];
		var result;
		if (isc.isA.CheckboxItem(item)) {
			result = value == null ? "" : String(!!value);
		} else if (isc.isA.RichTextItem(item)) {
			var html = value == null ? "" : String(value);
			var div = document.createElement("div");
			div.innerHTML = html;
			result = strip(div.textContent || div.innerText || "");
			alternatives.push(html);
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
			// combos and radios display the value map's text
			result = strip(item.getDisplayValue());
			alternatives.push(value == null ? "" : String(value));
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
		return { value: result, alternatives: alternatives, type: type };
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

	/** Ends inline editing in every data grid of the top-most view; true if a grid was editing. */
	S.endInlineEditing = function () {
		var view = S.topView();
		var ended = false;
		if (view && view._vm) {
			owned(view, function (c) {
				return isc.isA.BizDataGrid(c);
			}).forEach(function (grid) {
				var list = grid.grid;
				if (list && list.getEditRow && list.getEditRow() != null) {
					list.endEditing();
					ended = true;
				}
			});
		}
		return ended;
	};

	window._sail = S;
})();
