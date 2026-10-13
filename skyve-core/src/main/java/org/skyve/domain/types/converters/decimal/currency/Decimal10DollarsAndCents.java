package org.skyve.domain.types.converters.decimal.currency;

import org.skyve.CORE;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.math.BigDecimal;
import java.text.DecimalFormat;

/**
 * Converts {@link Decimal10} values using dollars-and-cents formatting.
 */
public class Decimal10DollarsAndCents implements Converter<Decimal10> {
	private static final String PATTERN = "###,###,###,##0.00";

	/**
	 * Returns the value type handled by this converter.
	 * @return the result value
	 */
	@Override
	public @Nonnull Class<Decimal10> getValueType() {
		return Decimal10.class;
	}

	/**
	 * Converts a domain value to its display representation.
	 * @param value the value
	 * @return the result value
	 */
	@Override
	public @Nonnull String toDisplayValue(@Nonnull Decimal10 value) throws ConversionException {
		try {
			DecimalFormat df = CORE.getDecimalFormat(PATTERN);
			df.setParseBigDecimal(true);
			return df.format(value.bigDecimalValue());
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.DECIMAL_10_DOLLARS_AND_CENTS_KEY, e);
		}
	}

	/**
	 * Converts a display representation to its domain value.
	 * @param displayValue the display value
	 * @return the result value
	 */
	@Override
	public @Nonnull Decimal10 fromDisplayValue(@Nonnull String displayValue) throws ConversionException {
		try {
			String numberValue = displayValue;
			if (displayValue.startsWith("$")) {
				numberValue = displayValue.substring(1).trim();
			}
			DecimalFormat df = CORE.getDecimalFormat(PATTERN);
			df.setParseBigDecimal(true);
		
			return new Decimal10((BigDecimal) df.parse(numberValue));
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.DECIMAL_10_DOLLARS_AND_CENTS_KEY, e);
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
	public @Nonnull String getFormatPattern() {
		return PATTERN;
	}
}
