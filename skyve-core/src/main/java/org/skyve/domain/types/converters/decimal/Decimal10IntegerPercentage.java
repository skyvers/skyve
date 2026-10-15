package org.skyve.domain.types.converters.decimal;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal10;
import org.skyve.metadata.model.Attribute.AttributeType;

/**
 * Converts {@link Decimal10} values as integer percentage display values.
 */
public class Decimal10IntegerPercentage extends AbstractDecimalPercentageConverter<Decimal10> {
	/**
	 * Creates the converter.
	 */
	public Decimal10IntegerPercentage() {
		super(Decimal10.class, AttributeType.decimal10, Decimal10::new, INTEGER_PATTERN, ConversionException.DECIMAL_10_INTEGER_PERCENTAGE_KEY);
	}
}
