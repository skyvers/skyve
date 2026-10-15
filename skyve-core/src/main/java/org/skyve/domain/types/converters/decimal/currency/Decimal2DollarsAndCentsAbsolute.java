package org.skyve.domain.types.converters.decimal.currency;

import java.math.BigDecimal;
import java.text.DecimalFormat;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.converters.decimal.AbstractDecimalConverter;
import org.skyve.metadata.model.Attribute.AttributeType;

import jakarta.annotation.Nonnull;

/**
 * Converts {@link Decimal2} values using absolute dollars-and-cents
 * formatting.
 */
public class Decimal2DollarsAndCentsAbsolute extends AbstractDollarsAndCentsConverter<Decimal2> {
	/** The display pattern, kept for callers that reference it. */
	public static final String PATTERN = AbstractDecimalConverter.TWO_DECIMAL_PLACES_PATTERN;

	/**
	 * Creates the converter.
	 */
	public Decimal2DollarsAndCentsAbsolute() {
		super(Decimal2.class, AttributeType.decimal2, Decimal2::new, ConversionException.DECIMAL_2_DOLLARS_AND_CENTS_ABSOLUTE_KEY);
	}

	/**
	 * Formats the magnitude of an amount, so negative amounts display without a minus sign.
	 * @param format the format from {@link #newDecimalFormat()}
	 * @param value the amount to format
	 * @return the display value
	 */
	@Override
	protected @Nonnull String format(@Nonnull DecimalFormat format, @Nonnull BigDecimal value) {
		return super.format(format, value.abs());
	}
}
