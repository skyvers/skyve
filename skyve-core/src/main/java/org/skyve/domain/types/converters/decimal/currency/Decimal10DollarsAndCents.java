package org.skyve.domain.types.converters.decimal.currency;

import org.skyve.CORE;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

import java.math.BigDecimal;
import java.text.DecimalFormat;

public class Decimal10DollarsAndCents implements Converter<Decimal10> {
	private static final String PATTERN = "###,###,###,##0.00";

	@Override
	public String toDisplayValue(Decimal10 value) {
		DecimalFormat df = CORE.getDecimalFormat(PATTERN);
		df.setParseBigDecimal(true);
		return df.format(value.bigDecimalValue());
	}

	@Override
	public Decimal10 fromDisplayValue(String displayValue) throws Exception {
		String numberValue = displayValue;
		if(displayValue.startsWith("$")){
			numberValue = displayValue.substring(1).trim();
		}
		DecimalFormat df = CORE.getDecimalFormat(PATTERN);
		df.setParseBigDecimal(true);
		return new Decimal10((BigDecimal) df.parse(numberValue));
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
}
