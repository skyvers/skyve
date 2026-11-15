/*
 * SAIL recorder core: what every renderer's recorder shares. See docs/sail.md, "Recording".
 *
 * A renderer bundle is this core plus a thin adapter (recorder-sc.js for SmartClient; recorder-pf.js for PrimeFaces
 * builds on the same core), concatenated by skyve-web/build.xml. The adapter calls SKYVE.SailRecorderCore.create(adapter)
 * and gets the recorder, which it publishes as SKYVE.SailRecorder.
 *
 * The core holds:
 * - the step model and server I/O: steps are queued and sent in order, one request at a time, to the sailrecording
 *   endpoint (org.skyve.impl.sail.recording.SailRecordingServlet), through the adapter's transport; the server's
 *   session holds the recording, and the panel reads it back in ranges (?from=&count=);
 * - gesture tracking: a gesture starts on a mouse button or key going down, and a click that recorded nothing is
 *   described by the adapter as an "unmapped" comment;
 * - the panel, in a Shadow DOM root so that the page's and the panel's CSS cannot reach each other: a floating bar
 *   (red dot, step count, Check, Note, Stop, expand) that expands into a right-hand slide-out showing the live script.
 *   The script is read-only: each gesture can be deleted, the last one undone, and the script copied or downloaded.
 *   The panel's own interactions are never recorded and never reach the page;
 * - Check mode: the adapter outlines the field, message or grid under the pointer, and a click (or an alt-click
 *   outside Check mode) adds testValue, testMessage or testGridRows; the click never reaches the page;
 * - the warning before signing out while recording;
 * - uploads, the same in every renderer: the content and action upload page (upload.xhtml) opens in an iframe, and the
 *   core listens to its file upload (jQuery fileuploadsend and fileuploadalways in that page) to record an upload step
 *   with the chosen file's name, and the refusal the page shows, if any;
 * - windows, the same in every renderer: each browser window (tab) has an id, kept in its sessionStorage, that every
 *   step carries; a click on a link that opens a new window (a target other than _self, _top or _parent, or a
 *   window.open from the link) is navigateLink window="new"; a window that has opened one says when it is the window
 *   in front again (a "focus" context entry); and the page load tells how it was loaded (a reload, Back or Forward) and
 *   whether it is the first page of a new window. The server works out the window steps from these
 *   (org.skyve.impl.sail.recording.SailRecordingWindows).
 *
 * Panel UI state (expanded, position, scroll) is kept in sessionStorage, never localStorage, and the panel works
 * without it. The recorder uses no timers: it works when a gesture or a response happens.
 */
(function () {
	"use strict";

	window.SKYVE = window.SKYVE || {};
	if (SKYVE.SailRecorderCore) {
		return;
	}

	var ENDPOINT = "sailrecording";
	var UI_STATE_KEY = "skyve.sail.recorder.panel";
	/** The browser window's (tab's) id, in its sessionStorage, so that each page of the window has the same one. */
	var WINDOW_KEY = "skyve.sail.recorder.window";
	/** Set in a window's sessionStorage when it opens a new window, until it is the window in front again. */
	var OPENER_KEY = "skyve.sail.recorder.opener";
	/**
	 * Set in a window's sessionStorage only while it calls window.open, which copies the sessionStorage into the new
	 * window: there it says the window id is a copy.
	 */
	var COPY_KEY = "skyve.sail.recorder.copy";
	/** How many entries the expanded panel loads at a time. */
	var RANGE = 50;

	/** Reads the panel's UI state from sessionStorage; an empty state if it cannot. */
	function loadUiState() {
		try {
			var text = window.sessionStorage.getItem(UI_STATE_KEY);
			return text ? (JSON.parse(text) || {}) : {};
		} catch (e) {
			return {};
		}
	}

	function saveUiState(state) {
		try {
			window.sessionStorage.setItem(UI_STATE_KEY, JSON.stringify(state));
		} catch (e) {
			// the panel works without it
		}
	}

	function sessionValue(key) {
		try {
			return window.sessionStorage.getItem(key);
		} catch (e) {
			return null;
		}
	}

	function setSessionValue(key, value) {
		try {
			if (value == null) {
				window.sessionStorage.removeItem(key);
			} else {
				window.sessionStorage.setItem(key, value);
			}
		} catch (e) {
			// without sessionStorage, each page counts as a window of its own
		}
	}

	/** The type of the page's own load (Navigation Timing): navigate, reload, back_forward or prerender. */
	function navigationType() {
		var entries = (window.performance && performance.getEntriesByType) ? performance.getEntriesByType("navigation") : [];
		return (entries && entries[0] && entries[0].type) ? entries[0].type : "navigate";
	}

	/**
	 * How the page was loaded: "navigate" (a link, a typed URL, a gesture), "reload", "back", "forward", or "traverse"
	 * when the browser went through its history in a direction it does not tell (Navigation Timing says only
	 * back_forward; the Navigation API's activation, where the browser has it, says which).
	 *
	 * @param restored	true when the page is being shown again from the back/forward cache, a move through the history
	 * 					that Navigation Timing (the page's first load) does not describe
	 */
	function loadKind(restored) {
		var type = restored ? "back_forward" : navigationType();
		if (type === "reload") {
			return "reload";
		}
		if (type !== "back_forward") {
			return "navigate";
		}
		var activation = window.navigation && window.navigation.activation;
		var from = activation && activation.from;
		var entry = activation && activation.entry;
		if (from && entry && (typeof from.index === "number") && (typeof entry.index === "number") && (from.index >= 0) && (entry.index >= 0)) {
			return (entry.index < from.index) ? "back" : "forward";
		}
		return "traverse";
	}

	/** The window id in the sessionStorage of the window that opened this one, or null if it has none or cannot be read. */
	function openerWindowId() {
		try {
			var opener = window.opener;
			return (opener && (opener !== window) && !opener.closed) ? opener.sessionStorage.getItem(WINDOW_KEY) : null;
		} catch (e) {
			return null; // another origin, or no sessionStorage
		}
	}

	/**
	 * This window's id, and whether this page is the first page of a window the recording has not seen: a tab with no
	 * id yet (a link with target _blank opens one without its opener's sessionStorage, as does a tab the browser or a
	 * test opens), or one whose id is a copy of its opener's: window.open copies the opener's sessionStorage, which
	 * carries COPY_KEY while the recorder's window.open runs (see openWindow()), and a window whose opener it can read
	 * has the opener's id. That holds on the window's first page, and on a later one when the first had no recorder (a
	 * PDF, another site). A window that keeps its own id is the same window on every page, however it loads them
	 * (location.replace too).
	 */
	function windowIdentity(newId) {
		var id = sessionValue(WINDOW_KEY);
		var copied = !!id && ((sessionValue(COPY_KEY) === "true") || (openerWindowId() === id));
		var first = !id || copied;
		setSessionValue(COPY_KEY, null);
		if (first) {
			id = newId;
			setSessionValue(WINDOW_KEY, id);
			setSessionValue(OPENER_KEY, null);
		}
		return { id: id, first: first };
	}

	/** A URL without its fragment. */
	function withoutFragment(url) {
		return String(url || "").replace(/#.*$/, "");
	}

	/** True if a link opens its page in another window: a target other than _self, _top or _parent. */
	function opensNewWindow(link) {
		var target = String(link.getAttribute("target") || "").trim().toLowerCase();
		return (target !== "") && (target !== "_self") && (target !== "_top") && (target !== "_parent");
	}

	/** A link's visible text, as navigateLink compares it. */
	function linkText(link) {
		return String(link.innerText || link.textContent || "").replace(/\s+/g, " ").trim();
	}

	function displayed(element) {
		return !!(element.offsetWidth || element.offsetHeight || element.getClientRects().length);
	}

	/**
	 * A URL as navigateLink href takes it: relative to the application's base (the folder of the page, where the
	 * sailrecording endpoint is) when it is under it, else absolute.
	 */
	function scriptUrl(url) {
		var base = new URL(".", location.href).href;
		return (url.indexOf(base) === 0) ? url.substring(base.length) || "./" : url;
	}

	/**
	 * The navigateLink step for a link: text when it finds the link (the first displayed link whose text contains it,
	 * or with exact, the first whose text equals it), else href (the URL the link opens, which must not be a script).
	 * Returns null if neither names it.
	 *
	 * @param link	the link
	 * @param url	the absolute URL it opens
	 */
	function linkStep(link, url) {
		var text = linkText(link);
		var links = Array.prototype.filter.call(document.querySelectorAll("a"), displayed);
		// the page the link is on, which the new window's page has as its referrer (Referrer-Policy same-origin), and
		// when: the server matches the new window to the link with them (SailRecordingWindows)
		var step = { op: "navigateLink", window: "new", url: url, from: withoutFragment(location.href), at: String(Date.now()) };
		if (text) {
			var containing = links.filter(function (a) {
				return linkText(a).indexOf(text) >= 0;
			})[0];
			if (containing === link) {
				step.text = text;
				return step;
			}
			var equal = links.filter(function (a) {
				return linkText(a) === text;
			})[0];
			if (equal === link) {
				step.text = text;
				step.exact = "true";
				return step;
			}
		}
		if (/^https?:/i.test(url)) {
			step.href = scriptUrl(url);
			return step;
		}
		return null;
	}

	var CSS = [
		":host { all: initial; }",
		":host, .panel { font: 12px/1.4 -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif; color: #1f2328; }",
		".bar { position: fixed; z-index: 2147483000; display: flex; align-items: center; gap: 6px; padding: 5px 8px;",
		"  background: #fff; border: 1px solid #c8ccd1; border-radius: 18px; box-shadow: 0 2px 8px rgba(0,0,0,.2);",
		"  cursor: move; user-select: none; }",
		".dot { width: 10px; height: 10px; border-radius: 50%; background: #d1242f; }",
		".stopped .dot { background: #8c959f; }",
		".count { min-width: 2em; text-align: right; font-variant-numeric: tabular-nums; }",
		"button { font: inherit; padding: 2px 8px; border: 1px solid #c8ccd1; border-radius: 12px; background: #f6f8fa; cursor: pointer; }",
		"button:hover { background: #eaeef2; }",
		"button.on { background: #1f6feb; border-color: #1f6feb; color: #fff; }",
		"button.primary { background: #1f6feb; border-color: #1f6feb; color: #fff; }",
		".note { display: none; align-items: center; gap: 4px; }",
		".note.open { display: flex; }",
		".note input { font: inherit; width: 180px; padding: 2px 6px; border: 1px solid #c8ccd1; border-radius: 4px; }",
		".drawer { position: fixed; z-index: 2147482999; top: 0; right: 0; bottom: 0; width: 440px; max-width: 90vw;",
		"  display: none; flex-direction: column; background: #fff; border-left: 1px solid #c8ccd1; box-shadow: -2px 0 8px rgba(0,0,0,.15); }",
		".drawer.open { display: flex; }",
		".head { display: flex; align-items: center; gap: 6px; padding: 8px; border-bottom: 1px solid #d0d7de; }",
		".head .title { flex: 1; font-weight: 600; }",
		".script { flex: 1; overflow: auto; padding: 6px 8px; font: 11px/1.45 ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; }",
		".entry { display: flex; align-items: flex-start; gap: 4px; padding: 1px 0; }",
		".entry pre { flex: 1; margin: 0; white-space: pre-wrap; word-break: break-word; font: inherit; }",
		".entry .comment { color: #8c959f; }",
		".entry button { padding: 0 5px; font-size: 10px; border-radius: 3px; }",
		".more { color: #57606a; text-align: center; padding: 4px; }",
		".outline { position: fixed; z-index: 2147482998; pointer-events: none; border: 2px solid #1f6feb;",
		"  background: rgba(31,111,235,.08); border-radius: 3px; display: none; }",
		".outline.show { display: block; }",
		".outline .label { position: absolute; left: 0; top: -20px; background: #1f6feb; color: #fff; padding: 1px 6px;",
		"  border-radius: 3px; white-space: nowrap; font-size: 11px; }",
		".toasts { position: fixed; z-index: 2147483001; right: 16px; bottom: 60px; display: flex; flex-direction: column; gap: 4px; pointer-events: none; }",
		".toast { background: #1f2328; color: #fff; padding: 4px 10px; border-radius: 4px; animation: fade 2.5s forwards; }",
		"@keyframes fade { 0% { opacity: 0; } 8% { opacity: 1; } 75% { opacity: 1; } 100% { opacity: 0; } }",
		".warning { position: fixed; z-index: 2147483002; left: 50%; top: 30%; transform: translateX(-50%); width: 340px;",
		"  background: #fff; border: 1px solid #c8ccd1; border-radius: 8px; box-shadow: 0 4px 16px rgba(0,0,0,.25); padding: 12px; display: none; }",
		".warning.open { display: block; }",
		".warning .buttons { display: flex; gap: 6px; justify-content: flex-end; margin-top: 10px; }",
		":host([data-mounted]) .bar, :host([data-mounted]) .drawer { position: static; }",
		":host([data-mounted]) .drawer { display: flex; width: auto; max-width: none; height: 100%; box-shadow: none; }"
	].join("\n");

	var MARKUP =
		"<div class=\"panel\">" +
			"<div class=\"bar\" part=\"bar\">" +
				"<span class=\"dot\" title=\"Recording\"></span>" +
				"<span class=\"count\" title=\"Recorded gestures\">0</span>" +
				"<button type=\"button\" data-act=\"check\" title=\"Check mode: click a field, a message or a grid to check it (Esc leaves)\">Check</button>" +
				"<button type=\"button\" data-act=\"note\" title=\"Add a comment to the script\">Note</button>" +
				"<button type=\"button\" data-act=\"stop\" title=\"Stop recording\">Stop</button>" +
				"<button type=\"button\" data-act=\"download\" class=\"primary\" hidden title=\"Download the script\">Download</button>" +
				"<button type=\"button\" data-act=\"expand\" title=\"Show the script\">&#8689;</button>" +
				"<span class=\"note\"><input type=\"text\" data-act=\"note-text\" placeholder=\"A note for the script\">" +
					"<button type=\"button\" data-act=\"note-add\">Add</button></span>" +
			"</div>" +
			"<div class=\"drawer\" part=\"drawer\">" +
				"<div class=\"head\"><span class=\"title\">SAIL recording</span>" +
					"<button type=\"button\" data-act=\"undo\" title=\"Undo the last gesture\">Undo</button>" +
					"<button type=\"button\" data-act=\"copy\" title=\"Copy the script\">Copy</button>" +
					"<button type=\"button\" data-act=\"download\" title=\"Download the script\">Download</button>" +
					"<button type=\"button\" data-act=\"collapse\" title=\"Hide the script\">&#8690;</button>" +
				"</div>" +
				"<div class=\"script\" data-act=\"script\"></div>" +
			"</div>" +
			"<div class=\"outline\"><span class=\"label\"></span></div>" +
			"<div class=\"toasts\"></div>" +
			"<div class=\"warning\"><div class=\"text\">Signing out ends the session, and the recording with it. Stop and download the script first?</div>" +
				"<div class=\"buttons\"><button type=\"button\" data-act=\"keep\">Keep recording</button>" +
				"<button type=\"button\" data-act=\"sign-out\">Sign out</button>" +
				"<button type=\"button\" data-act=\"stop-download\" class=\"primary\">Stop and download</button></div></div>" +
		"</div>";

	/** Events the panel keeps from the page, so that the page never sees an interaction with the panel. */
	var PANEL_EVENTS = ["mousedown", "mouseup", "click", "dblclick", "contextmenu", "pointerdown", "pointerup",
						"mousemove", "mouseover", "mouseout", "keydown", "keyup", "keypress", "wheel", "focusin", "focusout"];
	/** Pointer events Check mode keeps from the page. */
	var CHECK_EVENTS = ["mousedown", "mouseup", "click", "dblclick", "contextmenu", "pointerdown", "pointerup"];

	/**
	 * Creates the recorder for a renderer.
	 *
	 * @param adapter	the renderer's part: transport, attach, flush, describe, connected, control, checkTarget,
	 * 					isSignOut, signOut, reset, pageLoad (records the page's load, through pushPageLoad) (see
	 * 					recorder-sc.js); and optionally where the bar starts: corner
	 * 					("bottom-right", the default, or "bottom-left") and cornerInset (the pixels between the bar and
	 * 					that side of the window, 12 by default)
	 * @return the recorder
	 */
	function create(adapter) {
		var active = false;
		var stopped = false;
		var pageId = newPageId();
		var sequence = 0;
		/** This window's id and whether this page is a new window's first page (see windowIdentity()). */
		var windowInfo = null;
		/** True once the page has been shown again from the back/forward cache (see onPageShow()). */
		var restored = false;
		var token = null;
		var queue = [];
		var inFlight = 0;
		var holds = 0;
		var gesture = null;
		var detach = [];
		var drained = [];
		var connecting = false;
		var gestures = 0;
		var total = 0;
		var checkMode = false;
		var ui = loadUiState();

		// panel elements
		var host = null;
		var root = null;
		var bar = null;
		var drawer = null;
		var script = null;
		var outline = null;
		var warning = null;
		var loadedFrom = 0;
		var loadedCount = 0;
		var signOutTarget = null;

		/** Requests to the endpoint that have not returned. */
		var requests = 0;

		/** Calls the endpoint through the adapter's transport, counting the requests in flight. */
		function call(method, url, body, callback) {
			requests++;
			adapter.transport(method, url, body, function (status, text) {
				requests--;
				callback(status, text);
			});
		}

		function newPageId() {
			return Date.now().toString(36) + Math.random().toString(36).substring(2, 8);
		}

		// ---------------------------------------------------------------- steps and sending

		/** Records a step, after the adapter writes any pending data entry. Returns the step. */
		function record(step) {
			adapter.flush();
			return push(step);
		}

		/** Queues a step as it is, and sends it. */
		function push(step) {
			step.id = pageId + "-" + (++sequence);
			if (windowInfo && (step.windowId === undefined)) {
				step.windowId = windowInfo.id;
			}
			if (step.view === undefined) {
				var where = adapter.describe();
				step.view = where.view;
				if (step.screen === undefined) {
					step.screen = where.screen;
				}
				if ((step.title === undefined) && where.title) {
					// the view's title, which a later step may reuse (a capture; see SailRecordingCaptures)
					step.title = where.title;
				}
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

		function answer(text) {
			try {
				return JSON.parse(text);
			} catch (e) {
				return null;
			}
		}

		/** Posts a batch and calls back with the server's answer, or null if it failed. */
		function post(body, callback) {
			body._csrf = token;
			call("POST", ENDPOINT, JSON.stringify(body), function (status, text) {
				if (status === 403) {
					// the session no longer records (stopped elsewhere, signed out, or not allowed)
					stop(false);
					callback(null);
				} else {
					callback((status === 200) ? answer(text) : null);
				}
			});
		}

		/**
		 * Sends the steps not yet sent, one request at a time, up to any step still waiting for the user's answer to a
		 * confirmation, or while an action that may ask one runs.
		 */
		function send() {
			if (inFlight || holds) {
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
			post({ steps: sendable(queue.slice(0, count)) }, function (state) {
				inFlight = 0;
				if (state) {
					queue.splice(0, count);
					if (state.limitReached) {
						stop(false);
						warn("The SAIL recording has reached its limit of " + state.maximumSteps +
								" steps and records nothing more. Download it now.");
						return;
					}
					changed(state);
					send();
				} else {
					// kept: sent again with the next step, or from the page's beacon as it unloads
					notifyDrained();
				}
			});
		}

		/** Sends what is left as the page unloads; the server adds a step it has already received once. */
		function onPageHide() {
			adapter.flush();
			if (queue.length && (token != null) && navigator.sendBeacon) {
				navigator.sendBeacon(ENDPOINT, new Blob([JSON.stringify({ _csrf: token, steps: sendable(queue) })], { type: "application/json" }));
			}
		}

		// ---------------------------------------------------------------- the panel's view of the script

		/**
		 * The server's recording changed: show its count, and its script if the panel is expanded (unless the panel
		 * already shows it, as when a page brought it).
		 */
		function changed(state, shown) {
			if (state && (state.gestures != null)) {
				gestures = state.gestures;
				total = (state.steps != null) ? state.steps : total;
			}
			if (bar) {
				bar.querySelector(".count").textContent = String(gestures);
			}
			if (drawer && drawer.classList.contains("open") && !shown) {
				loadScript(false);
			}
		}

		/** Loads the last entries into the expanded panel, or with {@code earlier}, the entries before those shown. */
		function loadScript(earlier) {
			var count = Math.max(RANGE, loadedCount);
			var from = earlier ? Math.max(0, loadedFrom - RANGE) : Math.max(0, total - count);
			if (earlier) {
				count = loadedFrom - from + loadedCount;
			}
			call("GET", ENDPOINT + "?from=" + from + "&count=" + count, null, function (status, text) {
				var range = (status === 200) ? answer(text) : null;
				if (range) {
					total = range.total;
					gestures = range.gestures;
					bar.querySelector(".count").textContent = String(gestures);
					showScript(range, earlier);
				}
			});
		}

		function showScript(range, earlier) {
			var atBottom = (script.scrollTop + script.clientHeight >= script.scrollHeight - 4);
			var previousHeight = script.scrollHeight;
			loadedFrom = range.from;
			loadedCount = range.entries.length;
			// built with DOM nodes and text, never markup: a step's text is whatever the page or the user gave
			var element = function (tag, className, text) {
				var result = document.createElement(tag);
				if (className) {
					result.className = className;
				}
				if (text != null) {
					result.textContent = text;
				}
				return result;
			};
			var nodes = document.createDocumentFragment();
			if (loadedFrom > 0) {
				nodes.appendChild(element("div", "more", Number(loadedFrom) + " earlier entries: scroll up for more"));
			}
			range.entries.forEach(function (entry) {
				var div = element("div", "entry");
				div.setAttribute("data-id", String(entry.id || ""));
				var pre = element("pre");
				(entry.lines || []).forEach(function (line, i) {
					if (i > 0) {
						pre.appendChild(document.createTextNode("\n"));
					}
					pre.appendChild(element("span", (String(line).indexOf("<!--") === 0) ? "comment" : "step", String(line)));
				});
				div.appendChild(pre);
				if (!entry.context && !stopped && entry.id) {
					var button = element("button", null, "\u2715");
					button.type = "button";
					button.setAttribute("data-act", "delete");
					button.setAttribute("data-id", String(entry.id));
					button.title = "Delete";
					div.appendChild(button);
				}
				nodes.appendChild(div);
			});
			script.textContent = "";
			script.appendChild(nodes);
			if (earlier) {
				script.scrollTop = script.scrollHeight - previousHeight;
			} else if (atBottom || (ui.scroll == null)) {
				script.scrollTop = script.scrollHeight;
			}
		}

		// ---------------------------------------------------------------- panel actions

		/** Shows a short confirmation that fades by itself (a CSS animation, not a timer). */
		function toast(text) {
			if (!root) {
				return;
			}
			var element = document.createElement("div");
			element.className = "toast";
			element.textContent = text;
			element.addEventListener("animationend", function () {
				element.remove();
			});
			root.querySelector(".toasts").appendChild(element);
		}

		/** Shows a message in the panel that stays until the next one. */
		function warn(text) {
			toast(text);
			if (bar) {
				bar.title = text;
			}
		}

		function setCheckMode(on) {
			checkMode = on && active;
			if (bar) {
				bar.querySelector("[data-act=check]").classList.toggle("on", checkMode);
			}
			if (!checkMode) {
				hideOutline();
			}
		}

		/** Adds a check step for what the adapter found under the pointer. */
		function addCheck(target) {
			if (!target || !target.step) {
				toast("Nothing to check here");
				return;
			}
			record(target.step);
			toast("Added " + target.label);
		}

		function addNote() {
			var input = root.querySelector("[data-act=note-text]");
			var text = input.value.trim();
			if (text) {
				record({ op: "comment", text: text });
				toast("Added a note");
			}
			input.value = "";
			root.querySelector(".note").classList.remove("open");
		}

		function deleteEntry(id) {
			whenSent(function () {
				post({ delete: id }, function (state) {
					if (state) {
						toast("Deleted");
						changed(state);
					}
				});
			});
		}

		function undo() {
			adapter.flush();
			whenSent(function () {
				post({ undo: true }, function (state) {
					if (state) {
						toast("Undone");
						changed(state);
					}
				});
			});
		}

		function downloadUrl() {
			return ENDPOINT + "?format=xml&download=true";
		}

		/** Saves the script as a file: a link the browser downloads, which does not leave the page. */
		function download() {
			var link = document.createElement("a");
			link.href = downloadUrl();
			link.download = "";
			root.appendChild(link);
			link.click();
			link.remove();
		}

		function copy() {
			call("GET", ENDPOINT + "?format=xml", null, function (status, text) {
				if (status !== 200) {
					toast("Could not copy the script");
					return;
				}
				var done = function () {
					toast("Copied the script");
				};
				if (navigator.clipboard && navigator.clipboard.writeText) {
					navigator.clipboard.writeText(text).then(done, function () {
						copyBySelection(text);
						done();
					});
				} else {
					copyBySelection(text);
					done();
				}
			});
		}

		function copyBySelection(text) {
			var area = document.createElement("textarea");
			area.value = text;
			root.appendChild(area);
			area.select();
			document.execCommand("copy");
			area.remove();
		}

		/** Stops recording from the panel: sends every step, stops on the server, and offers the download. */
		function stopRecording(thenDownload) {
			adapter.flush();
			whenSent(function () {
				post({ stop: true }, function (state) {
					if (state && state.stopped) {
						changed(state);
						stop(true);
						if (thenDownload) {
							download();
						}
					}
				});
			});
		}

		function setExpanded(expanded, shown) {
			if (!drawer) {
				return;
			}
			drawer.classList.toggle("open", expanded);
			ui.expanded = expanded;
			saveUiState(ui);
			if (expanded && !shown) {
				loadScript(false);
			}
		}

		function act(name, element) {
			switch (name) {
				case "check":
					setCheckMode(!checkMode);
					break;
				case "note":
					root.querySelector(".note").classList.toggle("open");
					root.querySelector("[data-act=note-text]").focus();
					break;
				case "note-add":
					addNote();
					break;
				case "stop":
					stopRecording(false);
					break;
				case "download":
					download();
					break;
				case "expand":
					setExpanded(true);
					break;
				case "collapse":
					setExpanded(false);
					break;
				case "undo":
					undo();
					break;
				case "copy":
					copy();
					break;
				case "delete":
					deleteEntry(element.getAttribute("data-id"));
					break;
				case "keep":
					warning.classList.remove("open");
					break;
				case "sign-out":
					warning.classList.remove("open");
					adapter.signOut(signOutTarget);
					break;
				case "stop-download":
					warning.classList.remove("open");
					stopRecording(true);
					break;
				default:
					break;
			}
		}

		/** Creates the panel in a Shadow DOM root, in the container or the page's body. */
		function mount(container) {
			if (!host) {
				host = document.createElement("div");
				host.id = "skyve-sail-recorder";
				root = host.attachShadow({ mode: "open" });
				root.innerHTML = "<style>" + CSS + "</style>" + MARKUP;
				bar = root.querySelector(".bar");
				drawer = root.querySelector(".drawer");
				script = root.querySelector(".script");
				outline = root.querySelector(".outline");
				warning = root.querySelector(".warning");
				// the page never sees an interaction with the panel
				PANEL_EVENTS.forEach(function (type) {
					host.addEventListener(type, function (event) {
						event.stopPropagation();
					});
				});
				root.addEventListener("click", function (event) {
					var element = event.target.closest ? event.target.closest("[data-act]") : null;
					if (element && (element.tagName === "BUTTON")) {
						act(element.getAttribute("data-act"), element);
					}
				});
				root.addEventListener("keydown", function (event) {
					if ((event.key === "Enter") && event.target.getAttribute && (event.target.getAttribute("data-act") === "note-text")) {
						addNote();
					}
				});
				script.addEventListener("scroll", function () {
					ui.scroll = script.scrollTop;
					saveUiState(ui);
					if ((script.scrollTop === 0) && (loadedFrom > 0)) {
						loadScript(true);
					}
				});
				dragBar();
			}
			if (container) {
				host.setAttribute("data-mounted", "true");
				container.appendChild(host);
			} else {
				host.removeAttribute("data-mounted");
				document.body.appendChild(host);
				placeBar();
			}
			bar.querySelector(".count").textContent = String(gestures);
			// a page that brought the recording's state shows it without a request (see start)
			setExpanded(!!ui.expanded, true);
			return host;
		}

		function placeBar() {
			var x = ui.x;
			var y = ui.y;
			if ((x == null) || (y == null) || (x > window.innerWidth - 40) || (y > window.innerHeight - 20) || (x < 0) || (y < 0)) {
				// the adapter's corner and inset keep clear of the renderer's own corner controls
				var left = (adapter.corner === "bottom-left");
				var inset = (adapter.cornerInset || 12) + "px";
				bar.style.right = left ? "" : inset;
				bar.style.left = left ? inset : "";
				bar.style.bottom = "12px";
				bar.style.top = "";
			} else {
				bar.style.left = x + "px";
				bar.style.top = y + "px";
				bar.style.right = "";
				bar.style.bottom = "";
			}
		}

		/** The bar can be dragged by its background; its place is kept for the session. */
		function dragBar() {
			var start = null;
			bar.addEventListener("pointerdown", function (event) {
				if (event.target !== bar && !event.target.classList.contains("dot") && !event.target.classList.contains("count")) {
					return;
				}
				var rect = bar.getBoundingClientRect();
				start = { dx: event.clientX - rect.left, dy: event.clientY - rect.top };
				bar.setPointerCapture(event.pointerId);
			});
			bar.addEventListener("pointermove", function (event) {
				if (start) {
					ui.x = Math.max(0, event.clientX - start.dx);
					ui.y = Math.max(0, event.clientY - start.dy);
					placeBar();
				}
			});
			bar.addEventListener("pointerup", function () {
				if (start) {
					start = null;
					saveUiState(ui);
				}
			});
		}

		// ---------------------------------------------------------------- Check mode and the page's events

		/** True if the event happened in the panel. */
		function isPanelEvent(event) {
			return !!host && (event.composedPath ? event.composedPath().indexOf(host) >= 0 : host.contains(event.target));
		}

		function hideOutline() {
			if (outline) {
				outline.classList.remove("show");
			}
		}

		function showOutline(target) {
			if (!target || !target.rect) {
				hideOutline();
				return;
			}
			var r = target.rect;
			outline.style.left = (r.left - 2) + "px";
			outline.style.top = (r.top - 2) + "px";
			outline.style.width = r.width + "px";
			outline.style.height = r.height + "px";
			outline.querySelector(".label").textContent = target.label || "";
			outline.classList.add("show");
		}

		function checking(event) {
			return (checkMode || event.altKey) && !isPanelEvent(event);
		}

		/** Check mode: outline what a click would check. */
		function onMouseMove(event) {
			if (checking(event)) {
				showOutline(adapter.checkTarget(event.target, event));
			} else if (outline && outline.classList.contains("show")) {
				hideOutline();
			}
		}

		/** Check mode (or an alt-click): the page never sees the click; the click adds the check. */
		function onCheckEvent(event) {
			if (adapter.isSignOut(event.target) && !isPanelEvent(event)) {
				event.stopImmediatePropagation();
				event.preventDefault();
				if (event.type === "click") {
					signOutTarget = event.target;
					warning.classList.add("open");
				}
				return;
			}
			if (!checking(event)) {
				return;
			}
			event.stopImmediatePropagation();
			event.preventDefault();
			if (event.type === "click") {
				addCheck(adapter.checkTarget(event.target, event));
			}
		}

		function onKeyDown(event) {
			// Esc leaves Check mode wherever the focus is, but for the note being typed
			var inNote = isPanelEvent(event) && root && (root.activeElement === root.querySelector("[data-act=note-text]"));
			if (checkMode && (event.key === "Escape") && !inNote) {
				event.stopImmediatePropagation();
				event.preventDefault();
				setCheckMode(false);
			}
		}

		// ---------------------------------------------------------------- gestures

		/** A mouse button or key went down: the page handles what it does until it comes up. */
		function startGesture(event) {
			gesture = isPanelEvent(event) ? null : { target: event.target, steps: 0 };
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

		/** A click that recorded nothing, on a control, is a gesture the recorder cannot map: an unmapped comment. */
		function clicked(event) {
			var g = gesture;
			gesture = null;
			if (!active || !g || (g.steps > 0) || isPanelEvent(event)) {
				return;
			}
			var link = (event.target && event.target.closest) ? event.target.closest("a[href]") : null;
			if (link && opensNewWindow(link) && !event.defaultPrevented) {
				newWindowLink(link, link.href);
				return;
			}
			var description = adapter.control(event.target);
			if (description) {
				unmapped("click on " + description);
			}
		}

		// ---------------------------------------------------------------- windows

		/**
		 * Records a link that opens a new window, as navigateLink window="new", or an unmapped comment if the step
		 * cannot name it. The window remembers that it opened one, to say when it is the window in front again.
		 */
		function newWindowLink(link, url) {
			var step = linkStep(link, url);
			if (step) {
				record(step);
				setSessionValue(OPENER_KEY, "true");
			} else {
				unmapped("click on a link that opens a new window (\"" + linkText(link) + "\"), which navigateLink cannot name");
			}
		}

		/**
		 * window.open from a link the user clicked (a javascript: link, or a handler of the link) opens a new window as
		 * a target _blank link does. Another window.open (from a button) records nothing: a Skyve page in the new
		 * window is recorded as a window the script opens by its URL.
		 */
		function openWindow(original) {
			return function (url) {
				var link = (gesture && (gesture.steps === 0) && gesture.target && gesture.target.closest) ? gesture.target.closest("a") : null;
				if (active && link && (url != null) && (String(url) !== "")) {
					var absolute = null;
					try {
						absolute = new URL(String(url), location.href).href;
					} catch (e) {
						absolute = null;
					}
					if (absolute) {
						newWindowLink(link, absolute);
					}
				}
				// the new window gets a copy of this sessionStorage: mark it as a copy there (see windowIdentity())
				setSessionValue(COPY_KEY, "true");
				try {
					return original.apply(window, arguments);
				} finally {
					setSessionValue(COPY_KEY, null);
				}
			};
		}

		/**
		 * The window is in front again (its tab shows, or it has the focus): if it opened a new window, says so, so that
		 * the windows it opened are closed in the script before its next step.
		 */
		/**
		 * The page is shown again from the back/forward cache: a move through the history that loads nothing, so the
		 * adapter records it as it records a page load. Skyve's pages are not kept in that cache (their responses are
		 * Cache-Control: no-store, and SmartClient has an unload listener), so this is a guard.
		 */
		function onPageShow(event) {
			if (active && event.persisted) {
				restored = true;
				adapter.pageLoad();
			}
		}

		function onFront() {
			if (active && (document.visibilityState !== "hidden") && (sessionValue(OPENER_KEY) === "true")) {
				setSessionValue(OPENER_KEY, null);
				push({ op: "context", focus: "true" });
			}
		}

		// ---------------------------------------------------------------- attaching

		function listen(target, type, handler, capture) {
			target.addEventListener(type, handler, capture);
			detach.push(function () {
				target.removeEventListener(type, handler, capture);
			});
		}

		/** Replaces owner[name] with wrapper(original) until recording stops. */
		function wrap(owner, name, wrapper) {
			var original = owner[name];
			owner[name] = wrapper(original);
			detach.push(function () {
				owner[name] = original;
			});
		}

		// ---------------------------------------------------------------- uploads

		/** The parameters of a URL's query string. */
		function queryParams(search) {
			var result = {};
			String(search || "").replace(/^\?/, "").split("&").forEach(function (pair) {
				var equals = pair.indexOf("=");
				if (equals > 0) {
					result[decodeURIComponent(pair.substring(0, equals))] = decodeURIComponent(pair.substring(equals + 1).replace(/\+/g, " "));
				}
			});
			return result;
		}

		/** The refusal an upload page shows: its first error growl, error message or problem, shortened, or "". */
		function refusalIn(doc) {
			var texts = [];
			doc.querySelectorAll(".ui-growl-item-container.ui-growl-error .ui-growl-message, .ui-messages-error, #problems tbody tr").forEach(function (e) {
				var text = String(e.textContent || "").replace(/\s+/g, " ").trim();
				if (text && texts.indexOf(text) < 0) {
					texts.push(text);
				}
			});
			var result = texts[0] || "";
			return (result.length > 120) ? result.substring(0, 120) + "..." : result;
		}

		/**
		 * An iframe loaded: if it is the upload page, records each upload it makes as an upload step, naming the
		 * content binding (_n) or the upload action (_a) from the page's URL and the chosen file's name, then notes the
		 * refusal the page shows (errors: the step's replay may be refused).
		 */
		function onFrameLoad(event) {
			var frame = event.target;
			if (!active || !frame || (frame.tagName !== "IFRAME")) {
				return;
			}
			var win = null;
			var doc = null;
			try {
				win = frame.contentWindow;
				doc = frame.contentDocument;
			} catch (e) {
				return; // another origin
			}
			if (!win || !doc || !win.jQuery || !/\/upload\.xhtml$/.test(win.location.pathname)) {
				return;
			}
			var params = queryParams(win.location.search);
			var target = (params._u === "action") ? { action: params._a } : { binding: params._n };
			if (!target.action && !target.binding) {
				return;
			}
			var file = null;
			win.jQuery(doc).on("fileuploadsend", "#upload", function (e, data) {
				var chosen = data && data.files && data.files[0];
				file = chosen ? chosen.name : null;
			});
			win.jQuery(doc).on("fileuploadalways", "#upload", function () {
				if (!active || !file) {
					return;
				}
				var step = { op: "upload", file: file };
				if (target.action) {
					step.action = target.action;
				} else {
					step.binding = target.binding;
				}
				file = null;
				record(step);
				var refusal = refusalIn(doc);
				if (refusal) {
					push({ op: "context", text: "shown: upload refused \"" + refusal + "\"", errors: "true" });
				}
			});
		}

		function attach() {
			listen(document, "load", onFrameLoad, true);
			// on window, in the capture phase, so that Check mode and the sign out warning act before the page
			CHECK_EVENTS.forEach(function (type) {
				listen(window, type, onCheckEvent, true);
			});
			listen(window, "mousemove", onMouseMove, true);
			listen(window, "keydown", onKeyDown, true);
			listen(document, "mousedown", startGesture, true);
			listen(document, "keydown", startGesture, true);
			listen(window, "mouseup", endGesture, false);
			listen(window, "keyup", endKeyGesture, false);
			listen(window, "click", clicked, false);
			listen(window, "pagehide", onPageHide, false);
			listen(window, "pageshow", onPageShow, false);
			listen(document, "visibilitychange", onFront, false);
			listen(window, "focus", onFront, false);
			wrap(window, "open", openWindow);
			adapter.attach(recorder);
		}

		/** Asks the endpoint whether this session records, and for the recording's token. */
		function connect() {
			connecting = true;
			call("GET", ENDPOINT, null, function (status, text) {
				var state = (status === 200) ? answer(text) : null;
				connecting = false;
				if (state && state.recording) {
					token = state._csrf;
					total = state.steps || 0;
					gestures = (state.gestures != null) ? state.gestures : gestures;
					adapter.connected();
					changed(null);
					send();
				} else {
					stop(false);
				}
			});
		}

		/**
		 * Starts recording in this page.
		 *
		 * @param initial	optional state the page already has, {gestures, steps}, so that the panel shows it at once
		 */
		function start(initial) {
			if (active) {
				return;
			}
			active = true;
			stopped = false;
			queue = [];
			token = null;
			pageId = newPageId();
			sequence = 0;
			if (!windowInfo) {
				windowInfo = windowIdentity(newPageId());
			}
			var range = null;
			if (initial) {
				gestures = initial.gestures || 0;
				total = initial.steps || 0;
				if (initial.entries) {
					range = { total: total, gestures: gestures, from: initial.from || 0, entries: initial.entries };
				}
			}
			attach();
			mount(host && host.parentNode && host.hasAttribute("data-mounted") ? host.parentNode : null);
			bar.classList.remove("stopped");
			bar.querySelector("[data-act=stop]").hidden = false;
			bar.querySelector("[data-act=check]").hidden = false;
			bar.querySelector("[data-act=note]").hidden = false;
			bar.querySelector("[data-act=download]").hidden = true;
			if (initial && initial._csrf) {
				// the page brought the recording's state: no request is needed to start, or to show the panel
				token = initial._csrf;
				adapter.connected();
				if (range && drawer.classList.contains("open")) {
					showScript(range, false);
				}
				changed(null, true);
				send();
			} else {
				connect();
			}
		}

		/**
		 * Stops recording in this page: detaches from the page and leaves the panel showing the script, with its
		 * download.
		 *
		 * @param onServer	true if the server has stopped the recording (the script can be downloaded)
		 */
		function stop(onServer) {
			if (!active) {
				return;
			}
			active = false;
			stopped = true;
			setCheckMode(false);
			while (detach.length) {
				detach.pop()();
			}
			adapter.reset();
			queue = [];
			gesture = null;
			token = null;
			connecting = false;
			if (bar) {
				bar.classList.add("stopped");
				bar.querySelector("[data-act=stop]").hidden = true;
				bar.querySelector("[data-act=check]").hidden = true;
				bar.querySelector("[data-act=note]").hidden = true;
				root.querySelector(".note").classList.remove("open");
				bar.querySelector("[data-act=download]").hidden = !onServer;
				if (onServer) {
					toast("Recording stopped: download the script");
				}
			}
			notifyDrained();
		}

		var recorder = {
			// for the adapter
			record: record,
			push: push,
			unmapped: unmapped,
			listen: listen,
			wrap: wrap,
			/** Calls the function when recording stops in this page, to undo what the adapter attached. */
			onDetach: function (undo) {
				detach.push(undo);
			},
			whenSent: whenSent,
			/** Holds sending while an action that may ask a confirmation runs; endHold() sends. */
			beginHold: function () {
				holds++;
			},
			endHold: function () {
				holds--;
				send();
			},
			send: send,
			/** Marks the gesture being handled as recorded, though it has no step of its own yet (it records one later). */
			handled: function () {
				if (gesture) {
					gesture.steps++;
				}
			},
			lastQueued: function () {
				return queue.length ? queue[queue.length - 1] : null;
			},
			/**
			 * How the page was loaded, for its page load step: { load, opened, pageHref, url } where load is reload,
			 * back, forward or traverse (absent for an ordinary load), opened is "true" on the first page of a new
			 * window, with pageHref its URL as navigateLink href takes it, and url is the page's URL.
			 */
			/**
			 * Pushes the page load's step (null if the page names no view), with how the page was loaded: load (reload,
			 * back, forward or traverse; absent for an ordinary load), and on the first page of a new window opened
			 * "true", with its URL as navigateLink href takes it (pageHref), its referrer, the pages its history holds and
			 * when. A page that names no view is a context entry with these, when it was a reload, Back or Forward, or a
			 * new window's first page; otherwise nothing.
			 */
			pushPageLoad: function (step) {
				var how = { url: location.href };
				var kind = loadKind(restored);
				if (kind !== "navigate") {
					how.load = kind;
				}
				if (windowInfo && windowInfo.first && !restored) {
					how.opened = "true";
					how.pageHref = scriptUrl(location.href);
					how.referrer = withoutFragment(document.referrer);
					how.windowPages = String(history.length);
					how.at = String(Date.now());
				}
				var result = step;
				if (!result) {
					if (!how.load && !how.opened) {
						return;
					}
					result = { op: "context", view: "" };
				}
				Object.keys(how).forEach(function (key) {
					result[key] = how[key];
				});
				push(result);
			},
			/** The element the gesture being handled started on, or null outside a gesture. */
			gestureTarget: function () {
				return gesture ? gesture.target : null;
			},
			isPanelEvent: isPanelEvent,
			toast: toast,
			warn: warn,
			// for the page and its tests
			start: start,
			stop: stop,
			mount: mount,
			/** Stops recording on the server, as the panel's Stop does. */
			stopRecording: stopRecording,
			/** True while recording. */
			active: function () {
				return active;
			},
			/** True once the endpoint has confirmed the session records and every step so far has reached it. */
			ready: function () {
				return active && (token != null) && !inFlight && !holds && (requests === 0) && (queue.length === 0) && adapter.idle();
			},
			/** True while a request to the endpoint has not returned. */
			busy: function () {
				return requests > 0;
			},
			/** True while Check mode is on. */
			checking: function () {
				return checkMode;
			},
			/** The recorded gestures the server has confirmed. */
			gestures: function () {
				return gestures;
			},
			/** The steps not yet acknowledged by the server, for diagnosis. */
			pending: function () {
				return queue.slice();
			}
		};
		return recorder;
	}

	SKYVE.SailRecorderCore = { create: create };
})();
