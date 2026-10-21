package org.skyve.domain.types.converters.decimal;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal2;
import org.skyve.metadata.model.Attribute.AttributeType;

/**
 * Converts {@link Decimal2} values as integer percentage display values.
 */
public class Decimal2IntegerPercentage extends AbstractDecimalPercentageConverter<Decimal2> {
	/** The display pattern, kept for callers that reference it. */
	public static final String PATTERN = INTEGER_PATTERN;

	/**
	 * Creates the converter.
	 */
	public Decimal2IntegerPercentage() {
		super(Decimal2.class, AttributeType.decimal2, Decimal2::new, INTEGER_PATTERN, ConversionException.DECIMAL_2_INTEGER_PERCENTAGE_KEY);
	}
}
