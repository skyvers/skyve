package org.skyve.domain.types.converters.decimal;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal5;
import org.skyve.metadata.model.Attribute.AttributeType;

/**
 * Converts {@link Decimal5} values using a two-decimal display format.
 */
public class Decimal5TwoDecimalPlaces extends AbstractDecimalConverter<Decimal5> {
	/**
	 * Creates the converter.
	 */
	public Decimal5TwoDecimalPlaces() {
		super(Decimal5.class, AttributeType.decimal5, Decimal5::new, TWO_DECIMAL_PLACES_PATTERN, ConversionException.DECIMAL_5_TWO_DECIMAL_PLACES_KEY);
	}
}
