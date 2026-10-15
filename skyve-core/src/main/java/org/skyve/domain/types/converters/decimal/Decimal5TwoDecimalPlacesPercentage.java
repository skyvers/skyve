package org.skyve.domain.types.converters.decimal;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal5;
import org.skyve.metadata.model.Attribute.AttributeType;

/**
 * Converts {@link Decimal5} values as percentage values with two decimals.
 */
public class Decimal5TwoDecimalPlacesPercentage extends AbstractDecimalPercentageConverter<Decimal5> {
	/**
	 * Creates the converter.
	 */
	public Decimal5TwoDecimalPlacesPercentage() {
		super(Decimal5.class, AttributeType.decimal5, Decimal5::new, TWO_DECIMAL_PLACES_PATTERN, ConversionException.DECIMAL_5_TWO_DECIMAL_PLACES_PERCENTAGE_KEY);
	}
}
