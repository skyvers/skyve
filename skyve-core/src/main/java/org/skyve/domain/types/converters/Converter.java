package org.skyve.domain.types.converters;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.skyve.domain.messages.ConversionException;
import org.skyve.metadata.model.Attribute.AttributeType;

public interface Converter<T> {
	/**
	 * Convert a display value to an instance of T.
	 * This method should be symmetric with {@link #toDisplayValue(Object)}.
	 * @param displayValue	The display value.
	 * @return	An instance of T.
	 * @throws ConversionException
	 */
	@Nullable T fromDisplayValue(@Nonnull String displayValue) throws ConversionException;

	/**
	 * Convert an instance of T to a display value.
	 * This method should be symmetric with {@link #fromDisplayValue(String)}
	 * @param value	An instance of T.
	 * @return	The display value.
	 * @throws ConversionException
	 */
	@Nonnull String toDisplayValue(@Nonnull T value) throws ConversionException;
	
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
}
