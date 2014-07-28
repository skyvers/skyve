package org.skyve.domain.types.converters.decimal;

import java.math.BigDecimal;
import java.text.DecimalFormat;

import org.skyve.CORE;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

public class Decimal5TwoDecimalPlacesPercentage implements Converter<Decimal5> {
	private static final String PATTERN = "###,###,###,##0.00";

	@Override
	public String toDisplayValue(Decimal5 value) throws Exception {
		DecimalFormat df = CORE.getDecimalFormat(PATTERN);
		df.setParseBigDecimal(true);
		return df.format(value.bigDecimalValue().multiply(new BigDecimal(100))) + "%";
	}

	@Override
	public Decimal5 fromDisplayValue(String displayValue) throws Exception {
		DecimalFormat df = CORE.getDecimalFormat(PATTERN);
		df.setParseBigDecimal(true);
		displayValue.replace("%", "");
		return new Decimal5(((BigDecimal) df.parse(displayValue)).divide(new BigDecimal(100)));
	}

	@Override
	public AttributeType getAttributeType() {
		return AttributeType.decimal5;
	}

	@Override
	public Format<Decimal5> getFormat() {
		return null;
	}

	@Override
	public Validator<Decimal5> getValidator() {
		return null;
	}
}
