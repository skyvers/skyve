package org.skyve.domain.types.converters.decimal;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal5;
import org.skyve.metadata.model.Attribute.AttributeType;

/**
 * Converts {@link Decimal5} values as integer display values.
 */
public class Decimal5Integer extends AbstractDecimalConverter<Decimal5> {
	/**
	 * Creates the converter.
	 */
	public Decimal5Integer() {
		super(Decimal5.class, AttributeType.decimal5, Decimal5::new, INTEGER_PATTERN, ConversionException.DECIMAL_5_INTEGER_KEY);
	}
}
