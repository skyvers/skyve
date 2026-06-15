package org.skyve.domain.types.converters;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.formatters.Formatter;
import org.skyve.metadata.model.Attribute.AttributeType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Bidirectional converter between a domain type {@code T} and its user-facing string
 * representation.
 *
 * <p>A {@code Converter} is declared in document attribute metadata to control how a
 * typed attribute value is formatted for display and how a user-entered string is parsed
 * back into the domain type. The framework applies converters in the view binding pipeline,
 * in REST serialisation, and during import/export operations.
 *
 * <p>{@link #toDisplayValue(T)} (inherited from {@link org.skyve.domain.types.formatters.Formatter})
 * and {@link #fromDisplayValue(String)} are symmetric: calling them in either order on a
 * valid value should round-trip without loss.
 *
 * <p>Optional components:
 * <ul>
 *   <li>{@link #getFormat()} — a mask and text-case constraint applied on top of the raw
 *       conversion, e.g. to enforce an input pattern like {@code ###-###}.
 *   <li>{@link #getValidator()} — a semantic validator invoked after
 *       {@link #fromDisplayValue} succeeds, allowing domain-level validation such as
 *       range checks.
 *   <li>{@link #getFormatPattern()} — the pattern string for {@link java.text.Format}
 *       subclasses (e.g. number or date format patterns).
 * </ul>
 *
 * @param <T> the domain type this converter handles
 * @see Format
 * @see Validator
 * @see org.skyve.domain.types.formatters.Formatter
 */
public interface Converter<T> extends Formatter<T> {
	/**
	 * Converts a display string (as entered by a user or read from an import source) to
	 * an instance of {@code T}.
	 *
	 * <p>This method is symmetric with
	 * {@link org.skyve.domain.types.formatters.Formatter#toDisplayValue}: for any valid
	 * value {@code v}, {@code fromDisplayValue(toDisplayValue(v))} should equal {@code v}.
	 *
	 * @param displayValue the user-facing string; must not be {@code null}
	 * @return the parsed domain value; never {@code null}
	 * @throws ConversionException if {@code displayValue} cannot be parsed into {@code T}
	 */
	@Nonnull T fromDisplayValue(@Nonnull String displayValue) throws ConversionException;

	/**
	 * Returns the Skyve attribute type that this converter is applicable to.
	 *
	 * <p>The framework uses this to find a compatible converter when one is not specified
	 * explicitly in metadata, and to validate converter-attribute type compatibility
	 * during deployment.
	 *
	 * @return the applicable attribute type; never {@code null}
	 */
	@Nonnull AttributeType getAttributeType();
	
	/**
	 * Returns the {@link Format} definition that constrains the display representation,
	 * or {@code null} if no mask or text-case constraint applies.
	 *
	 * @return the format definition, or {@code null}
	 */
	@Nullable Format<T> getFormat();
	
	/**
	 * Returns the {@link Validator} that is invoked after a successful
	 * {@link #fromDisplayValue} call to perform semantic validation, or {@code null}
	 * if no post-conversion validation is required.
	 *
	 * @return the validator, or {@code null}
	 */
	@Nullable Validator<T> getValidator();
	
	/**
	 * Returns the format pattern string used by a {@link java.text.Format} subclass
	 * (e.g. a {@link java.text.SimpleDateFormat} or {@link java.text.DecimalFormat}
	 * pattern), or {@code null} if this converter does not use pattern-based formatting.
	 *
	 * @return the format pattern, or {@code null}
	 */
	@Nullable String getFormatPattern();
}
