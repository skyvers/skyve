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
 * the link's URL ({@value #URL}) is that window. A window that never records (a PDF) stays open until a step comes from
 * a window under it.</li>
 * <li>A new window's first page that no recorded link opened (a typed URL, a link opened in a new tab with the browser's
 * menu, a window a button opened) is {@code navigateLink href window="new"} with the page's URL, opened from the
 * window in front.</li>
 * <li>Either way, the new window's first page is not loaded again: its navigation becomes the push of its context
 * ({@code pushEditContext} or {@code pushListContext}), as a script must push one in a new window.</li>
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

	private static final String OP = SailRecordingBuilder.OP;
	private static final String TEXT = "text";
	private static final String TRUE = "true";
	private static final Set<String> URL_NAVIGATIONS = Set.of("navigateEdit", "navigateList", "navigateTree", "navigateMap", "navigateCalendar");

	/** A window of the stack: its id, null until a window that is the link's opens (or forever, for a PDF). */
	private static final class Frame {
		private String id;
		private final String url;

		private Frame(@Nullable String id, @Nullable String url) {
			this.id = id;
			this.url = url;
		}
	}

	private final List<Frame> stack = new ArrayList<>();
	private final Set<String> closed = new HashSet<>();
	/** The window of the last entry that had one, to note a switch to an unknown window once. */
	private String lastWindow = null;

	private SailRecordingWindows() {
		// one instance per pass over a recording's entries
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
		SailRecordingWindows pass = new SailRecordingWindows();
		List<List<Map<String, String>>> result = new ArrayList<>(entries.size());
		for (Map<String, String> entry : entries) {
			result.add(pass.apply(entry));
		}
		return result;
	}

	/** The entries one recorded entry becomes. */
	private @Nonnull List<Map<String, String>> apply(@Nonnull Map<String, String> entry) {
		List<Map<String, String>> result = new ArrayList<>(3);
		String window = entry.get(WINDOW);
		if (window == null) {
			result.add(entry);
			return result;
		}
		if (stack.isEmpty()) {
			stack.add(new Frame(window, null));
		}
		if (TRUE.equals(entry.get(OPENED)) && (indexOf(window) < 0) && (! closed.contains(window))) {
			opened(entry, window, result);
		}
		else {
			int index = indexOf(window);
			if (index >= 0) {
				while (stack.size() > index + 1) {
					close();
					result.add(step("closeWindow"));
				}
			}
			else if (! window.equals(lastWindow)) {
				result.add(unmapped(closed.contains(window) ?
										"the steps below were made in a window that the script has closed; SAIL cannot switch windows, so they run in the window in front" :
										"the steps below were made in another browser window, which the recording did not see open; SAIL cannot switch windows, so they run in the window in front"));
			}
			loaded(entry, result);
		}
		lastWindow = window;
		if ("navigateLink".equals(entry.get(OP)) && "new".equals(entry.get(LINK_WINDOW))) {
			stack.add(new Frame(null, entry.get(URL)));
		}
		return result;
	}

	/** navigateLink's attribute that opens the link in a new window. */
	private static final String LINK_WINDOW = "window";

	/**
	 * A new window's first page: the window a recorded link opened, if it is waiting for one and the page is the link's
	 * URL, otherwise a window opened by its URL from the window in front.
	 */
	private void opened(@Nonnull Map<String, String> entry, @Nonnull String window, @Nonnull List<Map<String, String>> result) {
		Frame top = stack.get(stack.size() - 1);
		String url = entry.get(URL);
		if ((top.id == null) && sameUrl(top.url, url)) {
			top.id = window;
		}
		else {
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
		Map<String, String> push = contextPush(entry);
		if (push != null) {
			result.add(push);
		}
		else if (! marker(entry)) {
			result.add(without(entry, OPENED));
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
