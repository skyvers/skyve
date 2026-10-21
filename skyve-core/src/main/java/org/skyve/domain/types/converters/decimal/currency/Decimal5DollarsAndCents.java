package org.skyve.domain.types.converters.decimal.currency;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal5;
import org.skyve.metadata.model.Attribute.AttributeType;

/**
 * Converts {@link Decimal5} values using dollars-and-cents formatting.
 */
public class Decimal5DollarsAndCents extends AbstractDollarsAndCentsConverter<Decimal5> {
	/**
	 * Creates the converter.
	 */
	public Decimal5DollarsAndCents() {
		super(Decimal5.class, AttributeType.decimal5, Decimal5::new, ConversionException.DECIMAL_5_DOLLARS_AND_CENTS_KEY);
	}
}
