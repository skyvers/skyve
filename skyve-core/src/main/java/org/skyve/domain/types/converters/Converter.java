package org.skyve.domain.types.converters;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.formatters.Formatter;
import org.skyve.metadata.model.Attribute.AttributeType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public interface Converter<T> extends Formatter<T> {
	/**
	 * Convert a display value to an instance of T.
	 * This method should be symmetric with {@link #toDisplayValue(T)}.
	 * @param displayValue	The display value.
	 * @return	An instance of T.
	 * @throws ConversionException
	 */
	@Nullable T fromDisplayValue(@Nonnull String displayValue) throws ConversionException;

	/**
	 * The attribute type this converter is applicable to.
	 * @return	The applicable attribute type.
	 */
	@Nonnull AttributeType getAttributeType();
	
	/**
	 * The format definition to use to format the value for this converter.
	 * @return	The format definition.
	 */
	@Nullable Format<T> getFormat();
	
	/**
	 * The validator to use after running fromDisplayValue()
	 * @return	The validator.
	 */
	@Nullable Validator<T> getValidator();
	
	/**
	 * The pattern used in java.text.Format subclasses, if any.
	 * @return	The pattern to apply or null if there is no pattern used.
	 */
	@Nullable String getFormatPattern();
}
