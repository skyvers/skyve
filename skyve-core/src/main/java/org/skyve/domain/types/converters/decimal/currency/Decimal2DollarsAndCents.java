package org.skyve.domain.types.converters.decimal.currency;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.converters.decimal.AbstractDecimalConverter;
import org.skyve.metadata.model.Attribute.AttributeType;

/**
 * Converts {@link Decimal2} values using dollars-and-cents formatting.
 */
public class Decimal2DollarsAndCents extends AbstractDollarsAndCentsConverter<Decimal2> {
	/** The display pattern, kept for callers that reference it. */
	public static final String PATTERN = AbstractDecimalConverter.TWO_DECIMAL_PLACES_PATTERN;

	/**
	 * Creates the converter.
	 */
	public Decimal2DollarsAndCents() {
		super(Decimal2.class, AttributeType.decimal2, Decimal2::new, ConversionException.DECIMAL_2_DOLLARS_AND_CENTS_KEY);
	}
}
