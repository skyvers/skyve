package org.skyve.impl.sail.execution.sc;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Represents a UI element locator used in SmartClient SAIL automation.
 * <p>
 * A locator is either a plain {@code isc.AutoTest} locator (starting with {@code //}) or a
 * <em>SAIL locator</em>: {@value #SAIL_PREFIX} followed by a JSON specification and an optional
 * AutoTest suffix, for example
 * {@code sail:{"view":"admin.Contact","kind":"item","name":"name"}/element}.
 * The SAIL harness resolves the specification in the browser against the view that is showing
 * (the top-most window, or the harness's current view), turns the widget it finds into an
 * AutoTest locator and appends the suffix. SAIL locators therefore do not depend on SmartClient's
 * generated component IDs, window numbers or the order of containers in the view.
 * <p>
 * The {@code inputType} is optional and should be provided when the input type context
 * is needed to correctly manipulate the locator during interactions.
 *
 * @author simeonsolomou
 */
public class Locator {
	/**
	 * Prefix that marks a SAIL locator.
	 */
	public static final String SAIL_PREFIX = "sail:";

	private final String locator;
	private final InputType inputType;

	/**
	 * Categorises supported SmartClient input widgets for locator interaction handling.
	 */
	public enum InputType {
		/** A checkbox, clicked until its state matches. */
		CHECKBOX,
		/** A select item; data entry values are zero-based option indexes. */
		COMBO,
		/** A lookup description; data entry values are search text. */
		LOOKUP_DESCRIPTION,
		/** A text input: text, numbers, dates, times, colours, geometry, passwords. */
		TEXT,
		/** A radio group; data entry values are zero-based option indexes. */
		RADIO,
		/** A rich text editor's editable area. */
		RICH_TEXT,
		/** A slider; data entry values are numbers. */
		SLIDER,
		/** A list membership; data entry values are the members' descriptions, separated by {@code |}. */
		MEMBERSHIP,
		/** An HTML editor (an edit button and an editor window); data entry values are HTML or text. */
		HTML
	}

	/**
	 * The kinds of widget a SAIL locator can describe.
	 */
	public enum Kind {
		/** A form item, by its name (the sanitised binding). */
		item,
		/** An inline data grid editor cell: {@code name} is the grid binding, {@code field} the column. */
		cell,
		/** An action button (toolbar or form), by action name. */
		button,
		/** A zoom in button, by sanitised binding. */
		zoomIn,
		/** A tab, by its title. */
		tab,
		/** A data grid or data repeater, by sanitised binding, with an optional part. */
		dataGrid,
		/** A list grid, tree grid or list repeater in an edit view, by its zero-based ordinal, with an optional part. */
		listGrid,
		/** The list grid of a list view, with an optional part. */
		list,
		/** A button on the SmartClient dialog: ok, yes, no or cancel. */
		dialog,
		/** A list membership, by sanitised binding. */
		membership
	}

	/**
	 * Creates a locator without input type hints.
	 *
	 * @param locator	an AutoTest or SAIL locator
	 */
	public Locator(@Nonnull String locator) {
		this.locator = locator;
		this.inputType = null;
	}

	/**
	 * Creates a locator with optional input type metadata.
	 *
	 * @param locator	an AutoTest or SAIL locator
	 * @param inputType	how data is entered into the widget, or {@code null} if it is not an input
	 */
	public Locator(@Nonnull String locator, @Nullable InputType inputType) {
		this.locator = locator;
		this.inputType = inputType;
	}

	/**
	 * Returns the concrete selector string used by automation steps.
	 *
	 * @return the AutoTest or SAIL locator
	 */
	public @Nonnull String getLocator() {
		return locator;
	}

	/**
	 * Returns the optional input type hint for this locator.
	 *
	 * @return the input type, or {@code null}
	 */
	public @Nullable InputType getInputType() {
		return inputType;
	}

	/**
	 * Returns whether this is a SAIL locator, resolved in the browser.
	 *
	 * @return {@code true} if the locator starts with {@value #SAIL_PREFIX}
	 */
	public boolean isSail() {
		return locator.startsWith(SAIL_PREFIX);
	}

	@Override
	public String toString() {
		return (inputType == null) ? locator : locator + " (" + inputType + ')';
	}

	/**
	 * Builds a SAIL locator string.
	 *
	 * @param view		the expected view as {@code module.Document}, or {@code null} to accept any view
	 * @param kind		the kind of widget
	 * @param name		the widget's name, binding, action name or title, or {@code null}
	 * @param field		the column of a {@link Kind#cell}, or {@code null}
	 * @param ordinal	the ordinal of a {@link Kind#listGrid}, or {@code null}
	 * @param part		the part of a grid: {@code new}, {@code zoom}, {@code edit}, {@code remove} or {@code rows}; or {@code null}
	 * @param suffix	an AutoTest suffix appended to the resolved widget's locator, or {@code null}
	 * @return the SAIL locator
	 */
	@SuppressWarnings("java:S107") // a flat builder keeps the JSON shape visible
	public static @Nonnull String sail(@Nullable String view,
										@Nonnull Kind kind,
										@Nullable String name,
										@Nullable String field,
										@Nullable Integer ordinal,
										@Nullable String part,
										@Nullable String suffix) {
		StringBuilder result = new StringBuilder(128);
		result.append(SAIL_PREFIX).append('{');
		if (view != null) {
			json(result, "view", view).append(',');
		}
		json(result, "kind", kind.name());
		if (name != null) {
			json(result.append(','), "name", name);
		}
		if (field != null) {
			json(result.append(','), "field", field);
		}
		if (ordinal != null) {
			result.append(",\"ordinal\":").append(ordinal.intValue());
		}
		if (part != null) {
			json(result.append(','), "part", part);
		}
		result.append('}');
		if (suffix != null) {
			result.append(suffix);
		}
		return result.toString();
	}

	/**
	 * Builds a locator for a form item.
	 *
	 * @param view		the expected view as {@code module.Document}
	 * @param name		the item name (the sanitised binding)
	 * @param suffix	the AutoTest suffix to the input element, or {@code null} for the item itself
	 * @param inputType	how data is entered, or {@code null} for a widget that only displays or uploads
	 * @return the locator
	 */
	public static @Nonnull Locator item(@Nonnull String view,
											@Nonnull String name,
											@Nullable String suffix,
											@Nullable InputType inputType) {
		return new Locator(sail(view, Kind.item, name, null, null, null, suffix), inputType);
	}

	/**
	 * Builds a locator for an inline data grid editor cell.
	 *
	 * @param view		the expected view as {@code module.Document}
	 * @param grid		the sanitised grid binding
	 * @param field		the column's field name
	 * @param suffix	the AutoTest suffix to the input element, or {@code null}
	 * @param inputType	how data is entered
	 * @return the locator
	 */
	public static @Nonnull Locator cell(@Nonnull String view,
											@Nonnull String grid,
											@Nonnull String field,
											@Nullable String suffix,
											@Nonnull InputType inputType) {
		return new Locator(sail(view, Kind.cell, grid, field, null, null, suffix), inputType);
	}

	/**
	 * Builds a locator for a widget that is not an input.
	 *
	 * @param view		the expected view as {@code module.Document}, or {@code null}
	 * @param kind		the kind of widget
	 * @param name		the name, binding, action name or title, or {@code null}
	 * @param ordinal	the ordinal of a list grid, or {@code null}
	 * @param part		the part of a grid, or {@code null}
	 * @param suffix	the AutoTest suffix, or {@code null}
	 * @return the locator
	 */
	public static @Nonnull Locator widget(@Nullable String view,
											@Nonnull Kind kind,
											@Nullable String name,
											@Nullable Integer ordinal,
											@Nullable String part,
											@Nullable String suffix) {
		return new Locator(sail(view, kind, name, null, ordinal, part, suffix));
	}

	private static @Nonnull StringBuilder json(@Nonnull StringBuilder builder, @Nonnull String key, @Nonnull String value) {
		builder.append('"').append(key).append("\":\"");
		for (int i = 0, l = value.length(); i < l; i++) {
			char c = value.charAt(i);
			switch (c) {
				case '"' -> builder.append("\\\"");
				case '\\' -> builder.append("\\\\");
				case '\n' -> builder.append("\\n");
				case '\r' -> builder.append("\\r");
				case '\t' -> builder.append("\\t");
				default -> {
					if (c < 0x20) {
						builder.append(String.format("\\u%04x", Integer.valueOf(c)));
					}
					else {
						builder.append(c);
					}
				}
			}
		}
		return builder.append('"');
	}
}
