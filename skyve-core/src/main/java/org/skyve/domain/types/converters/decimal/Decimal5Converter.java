package org.skyve.domain.types.converters.decimal;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Converts {@link Decimal5} values using plain string formatting/parsing.
 */
public class Decimal5Converter implements Converter<Decimal5> {
	/**
	 * Returns the value type handled by this converter.
	 * @return the result value
	 */
	@Override
	public @Nonnull Class<Decimal5> getValueType() {
		return Decimal5.class;
	}
	
	/**
	 * Converts a display representation to its domain value.
	 * @param displayValue the display value
	 * @return the result value
	 */
	@Override
	public @Nonnull Decimal5 fromDisplayValue(@Nonnull String displayValue) throws ConversionException {
		try {
			return new Decimal5(displayValue);
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.DECIMAL_5_CONVERTER_KEY, e);
		}
	}

	/**
	 * Converts a domain value to its display representation.
	 * @param value the value
	 * @return the result value
	 */
	@Override
	public @Nonnull String toDisplayValue(@Nonnull Decimal5 value) throws ConversionException {
		try {
			return value.toString();
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.DECIMAL_5_CONVERTER_KEY, e);
		}
	}

	/**
	 * Returns the attribute type supported by this converter.
	 * @return the result value
	 */
	@Override
	public @Nonnull AttributeType getAttributeType() {
		return AttributeType.decimal5;
	}

	/**
	 * Returns the optional format descriptor for this converter.
	 * @return the result value
	 */
	@Override
	public @Nullable Format<Decimal5> getFormat() {
		return null;
	}

	/**
	 * Returns the optional validator descriptor for this converter.
	 * @return the result value
	 */
	@Override
	public @Nullable Validator<Decimal5> getValidator() {
		return null;
	}
	
	/**
	 * Returns the optional format pattern for this converter.
	 * @return the result value
	 */
	@Override
	public @Nullable String getFormatPattern() {
		return null;
	}
}
