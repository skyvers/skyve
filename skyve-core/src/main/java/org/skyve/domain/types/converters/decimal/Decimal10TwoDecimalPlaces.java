package org.skyve.domain.types.converters.decimal;

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
 * Converts {@link Decimal10} values using a two-decimal display format.
 */
public class Decimal10TwoDecimalPlaces implements Converter<Decimal10> {
	private static final String PATTERN = "###,###,###,##0.00";

	@Nonnull
	/**
	 * Returns the value type handled by this converter.
	 * @return the result value
	 */
	@Override
	public Class<Decimal10> getValueType() {
		return Decimal10.class;
	}

	/**
	 * Converts a domain value to its display representation.
	 * @param value the value
	 * @return the result value
	 */
	@Override
	public String toDisplayValue(Decimal10 value) {
		try {
			DecimalFormat df = CORE.getDecimalFormat(PATTERN);
			df.setParseBigDecimal(true);
			return df.format(value.bigDecimalValue());
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.DECIMAL_10_TWO_DECIMAL_PLACES_KEY, e);
		}
	}

	/**
	 * Converts a display representation to its domain value.
	 * @param displayValue the display value
	 * @return the result value
	 */
	@Override
	public Decimal10 fromDisplayValue(String displayValue) {
		try {
			DecimalFormat df = CORE.getDecimalFormat(PATTERN);
			df.setParseBigDecimal(true);
			return new Decimal10((BigDecimal) df.parse(displayValue));
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.DECIMAL_10_TWO_DECIMAL_PLACES_KEY, e);
		}
	}

	/**
	 * Returns the attribute type supported by this converter.
	 * @return the result value
	 */
	@Override
	public AttributeType getAttributeType() {
		return AttributeType.decimal10;
	}

	/**
	 * Returns the optional format descriptor for this converter.
	 * @return the result value
	 */
	@Override
	public Format<Decimal10> getFormat() {
		return null;
	}

	/**
	 * Returns the optional validator descriptor for this converter.
	 * @return the result value
	 */
	@Override
	public Validator<Decimal10> getValidator() {
		return null;
	}

	@Nullable
	/**
	 * Returns the optional format pattern for this converter.
	 * @return the result value
	 */
	@Override
	public String getFormatPattern() {
		return PATTERN;
	}
}
