package org.skyve.domain.types.converters.decimal;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal10;
import org.skyve.metadata.model.Attribute.AttributeType;

/**
 * Converts {@link Decimal10} values using a two-decimal display format.
 */
public class Decimal10TwoDecimalPlaces extends AbstractDecimalConverter<Decimal10> {
	/**
	 * Creates the converter.
	 */
	public Decimal10TwoDecimalPlaces() {
		super(Decimal10.class, AttributeType.decimal10, Decimal10::new, TWO_DECIMAL_PLACES_PATTERN, ConversionException.DECIMAL_10_TWO_DECIMAL_PLACES_KEY);
	}
}
