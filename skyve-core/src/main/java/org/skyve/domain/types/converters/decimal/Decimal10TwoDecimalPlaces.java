package org.skyve.domain.types.converters.decimal;

import org.skyve.CORE;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.math.BigDecimal;
import java.text.DecimalFormat;

public class Decimal10TwoDecimalPlaces implements Converter<Decimal10> {
	private static final String PATTERN = "###,###,###,##0.00";

	@Nonnull
	@Override
	public Class<Decimal10> getValueType() {
		return Decimal10.class;
	}

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

	@Override
	public AttributeType getAttributeType() {
		return AttributeType.decimal10;
	}

	@Override
	public Format<Decimal10> getFormat() {
		return null;
	}

	@Override
	public Validator<Decimal10> getValidator() {
		return null;
	}

	@Nullable
	@Override
	public String getFormatPattern() {
		return PATTERN;
	}
}
