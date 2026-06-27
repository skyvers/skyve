package org.skyve.metadata.view;

import org.skyve.metadata.FormatterName;

/**
 * Mixin interface for view elements that support declarative Skyve formatting.
 *
 * <p>A built-in {@link FormatterName} or a custom formatter class name may be
 * specified to control how the element's bound value is converted to a display
 * string. Only one of the two should be non-{@code null}; the custom formatter
 * takes precedence when both are set.
 *
 * @see org.skyve.metadata.FormatterName
 */
public interface FormattedText {
	/**
	 * Returns the built-in Skyve formatter name to apply to this element's value.
	 *
	 * @return the formatter name, or {@code null} if no built-in formatter is selected
	 */
	FormatterName getFormatterName();

	/**
	 * Returns the fully-qualified name of a custom formatter class to apply.
	 *
	 * <p>Custom formatter names reference classes implementing the Skyve
	 * {@code Converter} or display-formatting SPI.
	 *
	 * @return the custom formatter class name, or {@code null} if none
	 */
	String getCustomFormatterName();
}
