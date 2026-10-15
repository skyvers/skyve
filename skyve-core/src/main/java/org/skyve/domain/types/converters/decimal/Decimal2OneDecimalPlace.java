package org.skyve.domain.types.converters.decimal;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal2;
import org.skyve.metadata.model.Attribute.AttributeType;

/**
 * Converts {@link Decimal2} values using a one-decimal display format.
 */
public class Decimal2OneDecimalPlace extends AbstractDecimalConverter<Decimal2> {
	/** The display pattern, kept for callers that reference it. */
	public static final String PATTERN = ONE_DECIMAL_PLACE_PATTERN;

	/**
	 * Creates the converter.
	 */
	public Decimal2OneDecimalPlace() {
		super(Decimal2.class, AttributeType.decimal2, Decimal2::new, ONE_DECIMAL_PLACE_PATTERN, ConversionException.DECIMAL_2_ONE_DECIMAL_PLACE_KEY);
	}
}
