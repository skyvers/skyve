package org.skyve.domain.types.converters.decimal;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Converts {@link Decimal10} values using plain string formatting/parsing.
 */
public class Decimal10Converter implements Converter<Decimal10> {
	/**
	 * Returns the value type handled by this converter.
	 * @return the result value
	 */
	@Override
	public @Nonnull Class<Decimal10> getValueType() {
		return Decimal10.class;
	}
	
	/**
	 * Converts a display representation to its domain value.
	 * @param displayValue the display value
	 * @return the result value
	 */
	@Override
	public @Nonnull Decimal10 fromDisplayValue(@Nonnull String displayValue) throws ConversionException {
		try {
			return new Decimal10(displayValue);
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.DECIMAL_10_CONVERTER_KEY, e);
		}
	}

	/**
	 * Converts a domain value to its display representation.
	 * @param value the value
	 * @return the result value
	 */
	@Override
	public @Nonnull String toDisplayValue(@Nonnull Decimal10 value) throws ConversionException {
		try {
			return value.toString();
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.DECIMAL_10_CONVERTER_KEY, e);
		}
	}

	/**
	 * Returns the attribute type supported by this converter.
	 * @return the result value
	 */
	@Override
	public @Nonnull AttributeType getAttributeType() {
		return AttributeType.decimal10;
	}

	/**
	 * Returns the optional format descriptor for this converter.
	 * @return the result value
	 */
	@Override
	public @Nullable Format<Decimal10> getFormat() {
		return null;
	}

	/**
	 * Returns the optional validator descriptor for this converter.
	 * @return the result value
	 */
	@Override
	public @Nullable Validator<Decimal10> getValidator() {
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
