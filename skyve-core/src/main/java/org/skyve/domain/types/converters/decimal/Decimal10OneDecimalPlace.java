package org.skyve.domain.types.converters.decimal;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal10;
import org.skyve.metadata.model.Attribute.AttributeType;

/**
 * Converts {@link Decimal10} values using a one-decimal display format.
 */
public class Decimal10OneDecimalPlace extends AbstractDecimalConverter<Decimal10> {
	/**
	 * Creates the converter.
	 */
	public Decimal10OneDecimalPlace() {
		super(Decimal10.class, AttributeType.decimal10, Decimal10::new, ONE_DECIMAL_PLACE_PATTERN, ConversionException.DECIMAL_10_ONE_DECIMAL_PLACE_KEY);
	}
}
