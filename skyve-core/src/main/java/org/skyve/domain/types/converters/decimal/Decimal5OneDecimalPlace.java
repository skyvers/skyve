package org.skyve.domain.types.converters.decimal;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal5;
import org.skyve.metadata.model.Attribute.AttributeType;

/**
 * Converts {@link Decimal5} values using a one-decimal display format.
 */
public class Decimal5OneDecimalPlace extends AbstractDecimalConverter<Decimal5> {
	/**
	 * Creates the converter.
	 */
	public Decimal5OneDecimalPlace() {
		super(Decimal5.class, AttributeType.decimal5, Decimal5::new, ONE_DECIMAL_PLACE_PATTERN, ConversionException.DECIMAL_5_ONE_DECIMAL_PLACE_KEY);
	}
}
