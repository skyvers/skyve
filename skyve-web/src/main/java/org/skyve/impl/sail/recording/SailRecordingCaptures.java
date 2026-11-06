package org.skyve.impl.sail.recording;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.skyve.util.JSON;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Finds the values a recording reuses that the application made, and has the script capture them where they were
 * shown: a value the user types later, or a row a later step finds by its text, that equals a value the application
 * showed earlier and the user did not type, becomes a {@code {var:name}} in the later step, with a capture step where
 * the value was shown (see docs/sail.md, "The run token and variables").
 *
 * <p>What the recorders send for this, besides the steps:
 * <ul>
 * <li>{@value #SHOWN} on a context entry: a JSON object of the bound values the screen showed (a view that opened, or
 * the values a response changed), {@code {"binding": "value"}}. A capture goes straight after the entry, as
 * {@code <captureValue binding variable/>}.</li>
 * <li>{@value #CELLS} on a step that names a grid row (a zoom or a select): a JSON object of the row's cells,
 * {@code {"column": "value"}}. A capture goes straight before the step, as {@code <captureCell column variable/>}
 * with the step's grid and row.</li>
 * <li>{@value #TITLE} on any step: the title the view showed when the step was made. A capture goes straight before
 * the first step that carried it, as {@code <captureTitle variable/>}.</li>
 * </ul>
 *
 * <p>The heuristic, kept simple on purpose: a value is a candidate when it was shown after the recording's first
 * navigation and not on the Control Panel, is not {@code true} or {@code false}, has a digit or at least 4 characters
 * (a generated number can be a single digit), and the user had not typed or chosen it before it was shown (a
 * data entry, a filter value, a lookup's search or pick). A use is a
 * {@code dataEnter} value, a {@code listGridFilter} value, a lookup's {@code search} or {@code pick}, or any
 * {@code rowContaining}, that equals a candidate exactly (both trimmed); the latest candidate shown before the use
 * wins. The variable is named after the binding or column (its last segment), or {@code title}, with a number added
 * when a name is taken by another value. No {@code pattern} is written: the capture keeps the whole text.
 *
 * <p>Threading: not thread-safe; use one instance per build.
 */
final class SailRecordingCaptures {
	/** The key of a context entry's shown bound values (JSON). */
	static final String SHOWN = "shown";
	/** The key of a row step's cells (JSON). */
	static final String CELLS = "cells";
	/** The key of a step's view title. */
	static final String TITLE = "title";

	private static final String OP = SailRecordingBuilder.OP;
	private static final String VALUE = "value";
	private static final String ROW_CONTAINING = "rowContaining";
	private static final String ROW = "row";
	private static final String BINDING = "binding";
	private static final String VARIABLE = "variable";
	/** The keys a captureCell takes from the step that names the row: its grid, then its row by text. */
	private static final List<String> CELL_KEYS = List.of(BINDING, "module", "document", "query", "model", ROW_CONTAINING);

	/** A value the application showed: where, and the capture step that reads it. */
	private record Candidate(@Nonnull String value, int index, boolean before, @Nonnull Map<String, String> capture, @Nonnull String name) {
	}

	private SailRecordingCaptures() {
		// static only
	}

	/**
	 * Returns the entries with the captures added and the reused values replaced by {@code {var:name}}. The entries
	 * given are not changed: a changed entry is a copy.
	 *
	 * <p>Complexity: O(n * c) in the entries and the candidate values.
	 *
	 * @param entries	the recorded entries, in order
	 * @return the entries to build, with the capture entries (op-codes {@code captureValue}, {@code captureCell} and
	 * 			{@code captureTitle}) in place
	 */
	static @Nonnull List<Map<String, String>> apply(@Nonnull List<? extends Map<String, String>> entries) {
		return new Pass().run(entries);
	}

	/** One pass over a recording's entries: the candidates so far, and the captures the uses need. */
	private static final class Pass {
		private final List<Candidate> candidates = new ArrayList<>();
		private final Set<String> typed = new HashSet<>();
		private final Set<String> titles = new HashSet<>();
		/** Captures to add, by the index of the entry they go before. */
		private final Map<Integer, List<Map<String, String>>> before = new HashMap<>();
		/** Captures to add, by the index of the entry they go after. */
		private final Map<Integer, List<Map<String, String>>> after = new HashMap<>();
		/** Variable name to value. */
		private final Map<String, String> names = new HashMap<>();
		/** Candidate to variable name. */
		private final Map<Candidate, String> used = new HashMap<>();
		private boolean navigated = false;

		@Nonnull List<Map<String, String>> run(@Nonnull List<? extends Map<String, String>> entries) {
			List<Map<String, String>> result = new ArrayList<>(entries.size() + 4);
			for (int i = 0, l = entries.size(); i < l; i++) {
				Map<String, String> entry = entries.get(i);
				// the uses first: a value shown by this entry cannot be reused by the same entry
				result.add(reused(entry));
				noteTyped(entry);
				// a value shown before the first navigation (on the Control Panel, where recording starts) is not on a
				// page the script opens
				String op = entry.get(OP);
				if ((op != null) && op.startsWith("navigate")) {
					navigated = true;
				}
				if (navigated && (! SailRecordingBuilder.CONTROL_PANEL.equals(entry.get(SailRecordingBuilder.VIEW)))) {
					addCandidates(entry, i, candidates, typed, titles);
				}
			}
			return (before.isEmpty() && after.isEmpty()) ? result : withCaptures(result);
		}

		/** A copy of the entry with each value that reuses a candidate replaced by its variable. */
		private @Nonnull Map<String, String> reused(@Nonnull Map<String, String> entry) {
			Map<String, String> result = new LinkedHashMap<>(entry);
			for (String key : useKeys(entry)) {
				String value = trimmed(entry.get(key));
				Candidate candidate = (value == null) ? null : latest(candidates, value);
				if (candidate != null) {
					result.put(key, "{var:" + used.computeIfAbsent(candidate, this::capture) + "}");
				}
			}
			return result;
		}

		/** Plans the capture of a candidate, where it was shown; returns its variable's name. */
		private @Nonnull String capture(@Nonnull Candidate candidate) {
			String variable = unique(candidate.name(), candidate.value(), names);
			Map<String, String> capture = new LinkedHashMap<>(candidate.capture());
			capture.put(VARIABLE, variable);
			(candidate.before() ? before : after).computeIfAbsent(Integer.valueOf(candidate.index()), k -> new ArrayList<>(1)).add(capture);
			return variable;
		}

		/** Notes what the user typed or chose: a data entry, a filter, a lookup's search and pick. */
		private void noteTyped(@Nonnull Map<String, String> entry) {
			for (String key : typedKeys(entry)) {
				String value = trimmed(entry.get(key));
				if (value != null) {
					typed.add(value);
				}
			}
		}

		/** The entries with the captures before and after the entries they go with. */
		private @Nonnull List<Map<String, String>> withCaptures(@Nonnull List<Map<String, String>> entries) {
			List<Map<String, String>> result = new ArrayList<>(entries.size() + before.size() + after.size());
			for (int i = 0, l = entries.size(); i < l; i++) {
				Map<String, String> entry = entries.get(i);
				for (Map<String, String> capture : before.getOrDefault(Integer.valueOf(i), List.of())) {
					result.add(where(capture, entry));
				}
				result.add(entry);
				for (Map<String, String> capture : after.getOrDefault(Integer.valueOf(i), List.of())) {
					result.add(where(capture, entry));
				}
			}
			return result;
		}
	}


	/** The keys of an entry whose values may reuse a shown value. */
	private static @Nonnull List<String> useKeys(@Nonnull Map<String, String> entry) {
		String op = entry.get(OP);
		List<String> result = new ArrayList<>(2);
		if ("dataEnter".equals(op) || "listGridFilter".equals(op)) {
			result.add(VALUE);
		}
		else if ("lookupDescriptionAutoComplete".equals(op)) {
			result.add("search");
			result.add("pick");
		}
		if (entry.containsKey(ROW_CONTAINING)) {
			result.add(ROW_CONTAINING);
		}
		return result;
	}

	/** The keys of an entry whose values the user typed or chose. */
	private static @Nonnull List<String> typedKeys(@Nonnull Map<String, String> entry) {
		String op = entry.get(OP);
		if ("dataEnter".equals(op) || "listGridFilter".equals(op)) {
			return List.of(VALUE);
		}
		if ("lookupDescriptionAutoComplete".equals(op)) {
			return List.of("search", "pick");
		}
		if ("lookupDescriptionPick".equals(op)) {
			return List.of(ROW_CONTAINING);
		}
		return List.of();
	}

	private static void addCandidates(@Nonnull Map<String, String> entry,
										int index,
										@Nonnull List<Candidate> candidates,
										@Nonnull Set<String> typed,
										@Nonnull Set<String> titles) {
		for (Map.Entry<String, String> shown : object(entry.get(SHOWN)).entrySet()) {
			String value = candidateValue(shown.getValue(), typed);
			if (value != null) {
				Map<String, String> capture = new LinkedHashMap<>();
				capture.put(OP, "captureValue");
				capture.put(BINDING, shown.getKey());
				candidates.add(new Candidate(value, index, false, capture, shown.getKey()));
			}
		}
		if ((entry.get(ROW_CONTAINING) != null) || (entry.get(ROW) != null)) {
			for (Map.Entry<String, String> cell : object(entry.get(CELLS)).entrySet()) {
				String value = candidateValue(cell.getValue(), typed);
				if (value != null) {
					candidates.add(new Candidate(value, index, true, cellCapture(entry, cell.getKey()), cell.getKey()));
				}
			}
		}
		String title = candidateValue(entry.get(TITLE), typed);
		if ((title != null) && titles.add(title)) {
			Map<String, String> capture = new LinkedHashMap<>();
			capture.put(OP, "captureTitle");
			candidates.add(new Candidate(title, index, true, capture, TITLE));
		}
	}

	/** The captureCell of a column of the row a step names, in the grid it names. */
	private static @Nonnull Map<String, String> cellCapture(@Nonnull Map<String, String> entry, @Nonnull String column) {
		Map<String, String> result = new LinkedHashMap<>();
		result.put(OP, "captureCell");
		for (String key : CELL_KEYS) {
			if (entry.get(key) != null) {
				result.put(key, entry.get(key));
			}
		}
		if (entry.get(ROW_CONTAINING) == null) {
			result.put(ROW, entry.get(ROW));
		}
		result.put("column", column);
		return result;
	}


	/** The latest candidate with the value, or null. */
	private static @Nullable Candidate latest(@Nonnull List<Candidate> candidates, @Nonnull String value) {
		for (int i = candidates.size() - 1; i >= 0; i--) {
			Candidate candidate = candidates.get(i);
			if (candidate.value().equals(value)) {
				return candidate;
			}
		}
		return null;
	}

	/** The value, trimmed, if it can be a candidate: see the class comment. */
	private static @Nullable String candidateValue(@Nullable String value, @Nonnull Set<String> typed) {
		String result = trimmed(value);
		if ((result == null) || "true".equalsIgnoreCase(result) || "false".equalsIgnoreCase(result) || typed.contains(result)) {
			return null;
		}
		if ((result.length() < 4) && (! result.chars().anyMatch(Character::isDigit))) {
			return null;
		}
		return result;
	}

	/** A variable name for the value: the binding's or column's last segment, made unique among other values. */
	private static @Nonnull String unique(@Nonnull String source, @Nonnull String value, @Nonnull Map<String, String> names) {
		String last = source.substring(source.lastIndexOf('.') + 1);
		StringBuilder base = new StringBuilder(last.length());
		for (char c : last.toCharArray()) {
			if (Character.isLetterOrDigit(c) || (c == '_')) {
				base.append(c);
			}
		}
		if (base.isEmpty() || (! Character.isLetter(base.charAt(0)))) {
			base.insert(0, VALUE);
		}
		String result = base.toString();
		for (int n = 2; names.containsKey(result) && (! value.equals(names.get(result))); n++) {
			result = base.toString() + n;
		}
		names.put(result, value);
		return result;
	}

	/** The capture entry, made where the entry it goes next to was made. */
	private static @Nonnull Map<String, String> where(@Nonnull Map<String, String> capture, @Nonnull Map<String, String> entry) {
		Map<String, String> result = new LinkedHashMap<>(capture);
		for (String key : List.of(SailRecordingBuilder.VIEW, SailRecordingBuilder.SCREEN)) {
			if (entry.get(key) != null) {
				result.put(key, entry.get(key));
			}
		}
		return result;
	}

	private static @Nullable String trimmed(@Nullable String value) {
		if (value == null) {
			return null;
		}
		String result = value.trim();
		return result.isEmpty() ? null : result;
	}

	/** A JSON object of strings, or an empty map if there is none or it cannot be read. */
	private static @Nonnull Map<String, String> object(@Nullable String json) {
		if ((json == null) || json.isBlank()) {
			return Map.of();
		}
		try {
			Object parsed = JSON.unmarshall(json);
			Map<String, String> result = new LinkedHashMap<>();
			if (parsed instanceof Map<?, ?> map) {
				for (Map.Entry<?, ?> e : map.entrySet()) {
					if ((e.getKey() != null) && (e.getValue() != null)) {
						result.put(String.valueOf(e.getKey()), String.valueOf(e.getValue()));
					}
				}
			}
			return result;
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return Map.of();
		}
	}
}
