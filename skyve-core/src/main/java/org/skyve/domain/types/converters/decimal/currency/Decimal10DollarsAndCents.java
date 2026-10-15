package org.skyve.domain.types.converters.decimal.currency;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal10;
import org.skyve.metadata.model.Attribute.AttributeType;

/**
 * Converts {@link Decimal10} values using dollars-and-cents formatting.
 */
public class Decimal10DollarsAndCents extends AbstractDollarsAndCentsConverter<Decimal10> {
	/**
	 * Creates the converter.
	 */
	public Decimal10DollarsAndCents() {
		super(Decimal10.class, AttributeType.decimal10, Decimal10::new, ConversionException.DECIMAL_10_DOLLARS_AND_CENTS_KEY);
	}
}
