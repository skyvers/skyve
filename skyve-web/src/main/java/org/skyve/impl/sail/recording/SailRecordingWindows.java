package org.skyve.impl.sail.recording;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Makes the window, back and refresh steps of a recording from what its pages said: each entry's browser window
 * ({@value #WINDOW}), a page load's kind ({@value #LOAD}) and whether it is a new window's first page
 * ({@value #OPENED}), and a window's "in front again" marker ({@value #FOCUS}). See docs/sail.md, "What is recorded".
 *
 * <p>The windows are a stack, as the executors keep them: the window the recording starts in, then each window opened
 * from the one above it. The rules, per entry, in the order the server received them:
 * <ul>
 * <li>{@code navigateLink window="new"} opens a window that has no id yet: the next new window whose first page is
 * the link's URL ({@value #URL}), or whose referrer ({@value #REFERRER}) is the page the link was on ({@value #FROM}),
 * as after a redirect, is that window. Windows send their entries independently, so a new window's first page can
 * reach the server before the link's entry (sent from its window behind an earlier request): such a link, matched the
 * same way and made no later than the page ({@value #AT}), is taken where the page is, and skipped where it arrives. A
 * window that never records (a PDF) stays open until a step comes from a window under it.</li>
 * <li>A new window's first page that no recorded link opened (a typed URL, a link opened in a new tab with the browser's
 * menu, a window a button opened) is {@code navigateLink href window="new"} with the page's URL, opened from the
 * window in front.</li>
 * <li>Either way, the new window's first page is not loaded again: its navigation becomes the push of its context
 * ({@code pushEditContext} or {@code pushListContext}), as a script must push one in a new window; unless the link's
 * window showed other pages first ({@value #WINDOW_PAGES}, as a PDF that leads to a Skyve page), when the page is
 * opened by its URL.</li>
 * <li>An entry from a window under the one in front (the user closed the window in front, or came back to the
 * window under it) closes each window above it first: {@code closeWindow}.</li>
 * <li>An entry from a window that is not on the stack (one the script closed, or one the recording did not see opened)
 * cannot be expressed, as SAIL cannot switch windows: an unmapped comment says so, once per switch, and the entries
 * follow in the window in front.</li>
 * <li>A page load that was a reload is {@code refresh}, and one that went back {@code navigateBack}, each followed by
 * the push of the page's context (the executors leave the context as it was, or without a document). Forward, or a
 * move through the history in a direction the browser did not tell, is an unmapped comment before the page's
 * navigation by its URL.</li>
 * </ul>
 * Focus markers and page load entries that name no view are dropped once applied. Entries without a window (from a
 * page that has not said, or made on the server) are kept as they are.
 *
 * <p>Threading: not thread-safe; use one instance per pass.
 */
final class SailRecordingWindows {
	/** The key of an entry's browser window id. */
	static final String WINDOW = "windowId";
	/** The key of a page load's kind: reload, back, forward or traverse. */
	static final String LOAD = "load";
	/** The key that marks a new window's first page. */
	static final String OPENED = "opened";
	/** The key that marks a window in front again. */
	static final String FOCUS = "focus";
	/** The key of a page's or a link's absolute URL. */
	static final String URL = "url";
	/** The key of a new window's first page's URL as navigateLink href takes it (relative to the application). */
	static final String PAGE_HREF = "pageHref";
	/** The key of a new window's first page's referrer. */
	static final String REFERRER = "referrer";
	/** The key of the number of pages a new window's history holds, at its first page that records. */
	static final String WINDOW_PAGES = "windowPages";
	/** The key of the URL of the page a link into a new window was on. */
	static final String FROM = "from";
	/** The key of when (milliseconds) a link into a new window was clicked, or a new window's first page loaded. */
	static final String AT = "at";

	private static final String OP = SailRecordingBuilder.OP;
	private static final String TEXT = "text";
	private static final String TRUE = "true";
	/** navigateLink's attribute that opens the link in a new window. */
	private static final String LINK_WINDOW = "window";
	private static final String CLOSE_WINDOW = "closeWindow";
	private static final Set<String> URL_NAVIGATIONS = Set.of("navigateEdit", "navigateList", "navigateTree", "navigateMap", "navigateCalendar");

	/**
	 * A window of the stack: its id, null until a window that is the link's opens (or forever, for a PDF); for a link's
	 * window, the link's URL and the page it was on.
	 */
	private static final class Frame {
		private String id;
		private final String url;
		private final String from;

		private Frame(@Nullable String id, @Nullable Map<String, String> link) {
			this.id = id;
			this.url = (link == null) ? null : link.get(URL);
			this.from = (link == null) ? null : link.get(FROM);
		}
	}

	private final List<? extends Map<String, String>> entries;
	/** The indexes of the links taken early, where their window's first page was (see {@link #lateLink(Map, int)}). */
	private final Set<Integer> taken = new HashSet<>();
	private final List<Frame> stack = new ArrayList<>();
	private final Set<String> closed = new HashSet<>();
	/** The window of the last entry that had one, to note a switch to an unknown window once. */
	private String lastWindow = null;

	private SailRecordingWindows(@Nonnull List<? extends Map<String, String>> entries) {
		this.entries = entries;
	}

	/**
	 * Returns, for each recorded entry, the entries it becomes: itself, or the window, back and refresh steps it
	 * implies with it. The entries given are not changed.
	 *
	 * <p>Complexity: O(n * w) in the entries and the depth of the windows.
	 *
	 * @param entries	the recorded entries, in order
	 * @return one list per entry, in order, each possibly empty
	 */
	static @Nonnull List<List<Map<String, String>>> grouped(@Nonnull List<? extends Map<String, String>> entries) {
		SailRecordingWindows pass = new SailRecordingWindows(entries);
		List<List<Map<String, String>>> result = new ArrayList<>(entries.size());
		for (int i = 0, l = entries.size(); i < l; i++) {
			result.add(pass.apply(i));
		}
		return result;
	}

	/** The entries the recorded entry at an index becomes. */
	private @Nonnull List<Map<String, String>> apply(int index) {
		Map<String, String> entry = entries.get(index);
		List<Map<String, String>> result = new ArrayList<>(3);
		String window = entry.get(WINDOW);
		if (taken.contains(Integer.valueOf(index))) {
			return result; // a link taken where its window's first page was
		}
		if (window == null) {
			result.add(entry);
			return result;
		}
		if (stack.isEmpty()) {
			stack.add(new Frame(window, null));
		}
		if (TRUE.equals(entry.get(OPENED)) && (indexOf(window) < 0) && (! closed.contains(window))) {
			opened(entry, index, window, result);
		}
		else {
			closeAbove(window, result);
			unknown(window, result);
			loaded(entry, result);
		}
		lastWindow = window;
		if (newWindowLink(entry)) {
			stack.add(new Frame(null, entry));
		}
		return result;
	}

	/**
	 * Notes, once per switch, entries from a window that is not on the stack (one the script closed, or one the
	 * recording did not see open): SAIL cannot switch windows.
	 */
	private void unknown(@Nonnull String window, @Nonnull List<Map<String, String>> result) {
		if ((indexOf(window) < 0) && (! window.equals(lastWindow))) {
			result.add(unmapped(closed.contains(window) ?
									"the steps below were made in a window that the script has closed; SAIL cannot switch windows, so they run in the window in front" :
									"the steps below were made in another browser window, which the recording did not see open; SAIL cannot switch windows, so they run in the window in front"));
		}
	}

	private static boolean newWindowLink(@Nonnull Map<String, String> entry) {
		return "navigateLink".equals(entry.get(OP)) && "new".equals(entry.get(LINK_WINDOW));
	}

	/** Closes the windows above a window of the stack (none if it is not on the stack), each a closeWindow step. */
	private void closeAbove(@Nonnull String window, @Nonnull List<Map<String, String>> result) {
		int index = indexOf(window);
		if (index >= 0) {
			while (stack.size() > index + 1) {
				close();
				result.add(step(CLOSE_WINDOW));
			}
		}
	}

	/**
	 * A new window's first page: the window a recorded link opened, if one is waiting for its window and the page is
	 * its (see {@link #isLinks(Map, String, String)}), or a link that arrives later but was clicked first; otherwise a
	 * window opened by its URL from the window in front.
	 */
	private void opened(@Nonnull Map<String, String> entry, int index, @Nonnull String window, @Nonnull List<Map<String, String>> result) {
		Frame top = stack.get(stack.size() - 1);
		String url = entry.get(URL);
		boolean linked = true;
		if ((top.id == null) && isLinks(entry, top.url, top.from)) {
			top.id = window;
		}
		else if (! lateLink(entry, index, window, result)) {
			linked = false;
			stack.add(new Frame(window, null));
			if (url == null) {
				result.add(unmapped("a new window opened, at a URL the page did not say"));
			}
			else {
				Map<String, String> link = step("navigateLink");
				link.put("href", entry.getOrDefault(PAGE_HREF, url));
				link.put(LINK_WINDOW, "new");
				result.add(link);
			}
		}
		// a link's window that showed other pages first (a PDF that led here) is not at this page: open it by its URL
		Map<String, String> push = (linked && (pages(entry) > 1)) ? null : contextPush(entry);
		if (push != null) {
			result.add(push);
		}
		else if (! marker(entry)) {
			result.add(without(entry, OPENED));
		}
	}

	/**
	 * True if a new window's first page is a link's window: it is at the link's URL, or its referrer is the page the
	 * link was on (a redirect, or a link that opens another URL than its own, keeps the referrer).
	 */
	private static boolean isLinks(@Nonnull Map<String, String> page, @Nullable String linkUrl, @Nullable String linkFrom) {
		String referrer = page.get(REFERRER);
		return sameUrl(linkUrl, page.get(URL)) || ((referrer != null) && (! referrer.isEmpty()) && sameUrl(linkFrom, referrer));
	}

	/**
	 * Takes, for a new window's first page that no waiting link explains, the first later link into a new window that
	 * does (see {@link #isLinks(Map, String, String)}), made in a window of the stack no later than the page: its entry
	 * was sent after the page's. The link's step goes here, after the windows its window closes, and its entry is skipped
	 * where it arrives.
	 *
	 * @return true if a link was taken
	 */
	private boolean lateLink(@Nonnull Map<String, String> page, int index, @Nonnull String window, @Nonnull List<Map<String, String>> result) {
		long pageAt = number(page.get(AT), Long.MAX_VALUE);
		for (int i = index + 1, l = entries.size(); i < l; i++) {
			Map<String, String> link = entries.get(i);
			String linkWindow = link.get(WINDOW);
			if (newWindowLink(link) && (! taken.contains(Integer.valueOf(i))) && (linkWindow != null) && (indexOf(linkWindow) >= 0) &&
					(number(link.get(AT), Long.MIN_VALUE) <= pageAt) && isLinks(page, link.get(URL), link.get(FROM))) {
				taken.add(Integer.valueOf(i));
				closeAbove(linkWindow, result);
				result.add(link);
				Frame frame = new Frame(window, link);
				stack.add(frame);
				return true;
			}
		}
		return false;
	}

	/** The pages a new window's history held at its first page that records; 1 if the page did not say. */
	private static long pages(@Nonnull Map<String, String> page) {
		return number(page.get(WINDOW_PAGES), 1);
	}

	private static long number(@Nullable String value, long otherwise) {
		try {
			return (value == null) ? otherwise : Long.parseLong(value.trim());
		}
		catch (@SuppressWarnings("unused") NumberFormatException e) {
			return otherwise;
		}
	}

	/** True for a context entry that only carries keys for this pass (no text to show, no values shown). */
	private static boolean marker(@Nonnull Map<String, String> entry) {
		return SailRecordingBuilder.CONTEXT.equals(entry.get(OP)) && (entry.get(TEXT) == null) && (entry.get(SailRecordingCaptures.SHOWN) == null);
	}

	/** A page load: a reload or Back with the push of its context, Forward with a comment, or the entry as it is. */
	private static void loaded(@Nonnull Map<String, String> entry, @Nonnull List<Map<String, String>> result) {
		String load = entry.get(LOAD);
		boolean marker = marker(entry);
		if ("reload".equals(load) || "back".equals(load)) {
			result.add(step("reload".equals(load) ? "refresh" : "navigateBack"));
			Map<String, String> push = contextPush(entry);
			if (push != null) {
				result.add(push);
			}
			else if (! marker) {
				result.add(without(entry, LOAD));
			}
		}
		else if (load != null) {
			result.add(unmapped(("forward".equals(load) ? "went forward with the browser's Forward button" : "went through the browser's history") +
									", which SAIL has no step for: the page is opened by its URL instead"));
			if (! marker) {
				result.add(without(entry, LOAD));
			}
		}
		else if (! marker) {
			result.add(entry);
		}
	}

	/**
	 * The push of the context a page loaded by its URL shows, as its navigation would push it: pushEditContext for an
	 * edit view, pushListContext for a list (or a tree, map or calendar); null for any other entry.
	 */
	private static @Nullable Map<String, String> contextPush(@Nonnull Map<String, String> entry) {
		String op = entry.get(OP);
		if (! URL_NAVIGATIONS.contains(op)) {
			return null;
		}
		Map<String, String> result = without(entry, LOAD);
		result.remove(OPENED);
		result.remove(PAGE_HREF);
		result.remove(URL);
		result.remove(REFERRER);
		result.remove(WINDOW_PAGES);
		result.remove(AT);
		result.remove("bizId");
		result.remove("geometryBinding");
		result.put(OP, "navigateEdit".equals(op) ? "pushEditContext" : "pushListContext");
		return result;
	}

	private int indexOf(@Nonnull String window) {
		for (int i = stack.size() - 1; i >= 0; i--) {
			if (window.equals(stack.get(i).id)) {
				return i;
			}
		}
		return -1;
	}

	private void close() {
		Frame frame = stack.remove(stack.size() - 1);
		if (frame.id != null) {
			closed.add(frame.id);
		}
	}

	/** True if two absolute URLs name the same page: equal without their fragments. */
	private static boolean sameUrl(@Nullable String a, @Nullable String b) {
		return (a != null) && (b != null) && a.replaceFirst("#.*$", "").equals(b.replaceFirst("#.*$", ""));
	}

	private static @Nonnull Map<String, String> step(@Nonnull String op) {
		Map<String, String> result = new LinkedHashMap<>();
		result.put(OP, op);
		return result;
	}

	private static @Nonnull Map<String, String> unmapped(@Nonnull String text) {
		Map<String, String> result = step(SailRecordingBuilder.UNMAPPED);
		result.put(TEXT, text);
		return result;
	}

	private static @Nonnull Map<String, String> without(@Nonnull Map<String, String> entry, @Nonnull String key) {
		Map<String, String> result = new LinkedHashMap<>(entry);
		result.remove(key);
		return result;
	}
}
