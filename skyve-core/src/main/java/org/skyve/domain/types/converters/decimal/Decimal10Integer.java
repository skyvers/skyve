package org.skyve.domain.types.converters.decimal;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal10;
import org.skyve.metadata.model.Attribute.AttributeType;

/**
 * Converts {@link Decimal10} values as integer display values.
 */
public class Decimal10Integer extends AbstractDecimalConverter<Decimal10> {
	/**
	 * Creates the converter.
	 */
	public Decimal10Integer() {
		super(Decimal10.class, AttributeType.decimal10, Decimal10::new, INTEGER_PATTERN, ConversionException.DECIMAL_10_INTEGER_KEY);
	}
}
