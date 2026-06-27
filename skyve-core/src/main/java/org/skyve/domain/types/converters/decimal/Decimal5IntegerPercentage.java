package org.skyve.domain.types.converters.decimal;

import java.math.BigDecimal;
import java.text.DecimalFormat;

import org.skyve.CORE;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

/**
 * Converts {@link Decimal5} values as integer percentage display values.
 */
public class Decimal5IntegerPercentage implements Converter<Decimal5> {
	private static final String PATTERN = "###,###,###,##0";

	/**
	 * Returns the value type handled by this converter.
	 * @return the result value
	 */
	@Override
	public Class<Decimal5> getValueType() {
		return Decimal5.class;
	}
	
	/**
	 * Converts a domain value to its display representation.
	 * @param value the value
	 * @return the result value
	 */
	@Override
	public String toDisplayValue(Decimal5 value) throws ConversionException {
		try {
			DecimalFormat df = CORE.getDecimalFormat(PATTERN);
			df.setParseBigDecimal(true);
			return df.format(value.bigDecimalValue().multiply(new BigDecimal(100))) + "%";
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.DECIMAL_5_INTEGER_PERCENTAGE_KEY, e);
		}
	}

	/**
	 * Converts a display representation to its domain value.
	 * @param displayValue the display value
	 * @return the result value
	 */
	@Override
	public Decimal5 fromDisplayValue(String displayValue) throws ConversionException {
		try {
			DecimalFormat df = CORE.getDecimalFormat(PATTERN);
			df.setParseBigDecimal(true);
			String noPercentage = displayValue.replace("%", "");

			return new Decimal5(((BigDecimal) df.parse(noPercentage)).divide(new BigDecimal(100)));
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.DECIMAL_5_INTEGER_PERCENTAGE_KEY, e);
		}
	}

	/**
	 * Returns the attribute type supported by this converter.
	 * @return the result value
	 */
	@Override
	public AttributeType getAttributeType() {
		return AttributeType.decimal5;
	}

	/**
	 * Returns the optional format descriptor for this converter.
	 * @return the result value
	 */
	@Override
	public Format<Decimal5> getFormat() {
		return null;
	}

	/**
	 * Returns the optional validator descriptor for this converter.
	 * @return the result value
	 */
	@Override
	public Validator<Decimal5> getValidator() {
		return null;
	}
	
	/**
	 * Returns the optional format pattern for this converter.
	 * @return the result value
	 */
	@Override
	public String getFormatPattern() {
		return PATTERN;
	}
}
