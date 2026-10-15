package org.skyve.domain.types.converters.decimal;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.util.function.Function;

import org.skyve.domain.types.Decimal;
import org.skyve.metadata.model.Attribute.AttributeType;

import jakarta.annotation.Nonnull;

/**
 * Base converter for {@link Decimal} attributes holding a fraction that is
 * displayed as a percentage, so {@code 0.125} displays as {@code 12.5%} at one
 * decimal place.
 *
 * <p>Parsing accepts the value with or without a {@code %} sign and rounds it
 * {@link RoundingMode#HALF_UP} to the precision displayed, so entering
 * {@code 12.5%} into an integer percentage stores {@code 0.13}. This matches the
 * SmartClient percentage editors.
 *
 * @param <T> the decimal type converted
 */
public abstract class AbstractDecimalPercentageConverter<T extends Decimal> extends AbstractDecimalConverter<T> {
	/**
	 * Creates a percentage converter for one decimal type and display pattern.
	 *
	 * @param valueType the decimal type converted
	 * @param attributeType the attribute type the converter applies to
	 * @param factory creates a {@code T} from a parsed value, applying the type's scale
	 * @param pattern the {@link DecimalFormat} pattern for the percentage, without the {@code %} sign
	 * @param i18nKey the i18n key of the conversion error message
	 */
	protected AbstractDecimalPercentageConverter(@Nonnull Class<T> valueType,
													@Nonnull AttributeType attributeType,
													@Nonnull Function<BigDecimal, T> factory,
													@Nonnull String pattern,
													@Nonnull String i18nKey) {
		super(valueType, attributeType, factory, pattern, i18nKey);
	}

	/**
	 * Formats a fraction as a percentage followed by a {@code %} sign.
	 * @param format the format from {@link #newDecimalFormat()}
	 * @param value the fraction to format
	 * @return the display value
	 */
	@Override
	protected @Nonnull String format(@Nonnull DecimalFormat format, @Nonnull BigDecimal value) {
		return super.format(format, value.movePointRight(2)) + "%";
	}

	/**
	 * Parses a percentage, rounded to the displayed precision, into a fraction.
	 * @param format the format from {@link #newDecimalFormat()}
	 * @param displayValue the percentage, with or without a {@code %} sign
	 * @return the fraction
	 * @throws ParseException if the display value is not a decimal number
	 */
	@Override
	protected @Nonnull BigDecimal parse(@Nonnull DecimalFormat format, @Nonnull String displayValue)
	throws ParseException {
		BigDecimal percentage = super.parse(format, displayValue.replace("%", ""));
		return percentage.setScale(format.getMaximumFractionDigits(), RoundingMode.HALF_UP).movePointLeft(2);
	}
}
