package org.skyve.domain.types.converters.decimal;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal10;
import org.skyve.metadata.model.Attribute.AttributeType;

/**
 * Converts {@link Decimal10} values as percentage values with two decimals.
 */
public class Decimal10TwoDecimalPlacesPercentage extends AbstractDecimalPercentageConverter<Decimal10> {
	/**
	 * Creates the converter.
	 */
	public Decimal10TwoDecimalPlacesPercentage() {
		super(Decimal10.class, AttributeType.decimal10, Decimal10::new, TWO_DECIMAL_PLACES_PATTERN, ConversionException.DECIMAL_10_TWO_DECIMAL_PLACES_PERCENTAGE_KEY);
	}
}
