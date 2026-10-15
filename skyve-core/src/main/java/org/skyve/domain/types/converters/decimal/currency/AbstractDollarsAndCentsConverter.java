package org.skyve.domain.types.converters.decimal.currency;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.util.function.Function;

import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.converters.decimal.AbstractDecimalConverter;
import org.skyve.metadata.model.Attribute.AttributeType;

import jakarta.annotation.Nonnull;

/**
 * Base converter for {@link Decimal} attributes displayed as dollars and cents,
 * with two decimal places and no currency symbol.
 *
 * <p>Parsing accepts the value with or without a leading {@code $} sign.
 *
 * @param <T> the decimal type converted
 */
public abstract class AbstractDollarsAndCentsConverter<T extends Decimal> extends AbstractDecimalConverter<T> {
	/**
	 * Creates a dollars-and-cents converter for one decimal type.
	 *
	 * @param valueType the decimal type converted
	 * @param attributeType the attribute type the converter applies to
	 * @param factory creates a {@code T} from a parsed value, applying the type's scale
	 * @param i18nKey the i18n key of the conversion error message
	 */
	protected AbstractDollarsAndCentsConverter(@Nonnull Class<T> valueType,
												@Nonnull AttributeType attributeType,
												@Nonnull Function<BigDecimal, T> factory,
												@Nonnull String i18nKey) {
		super(valueType, attributeType, factory, TWO_DECIMAL_PLACES_PATTERN, i18nKey);
	}

	/**
	 * Parses an amount, ignoring a leading {@code $} sign.
	 * @param format the format from {@link #newDecimalFormat()}
	 * @param displayValue the amount, with or without a leading {@code $} sign
	 * @return the amount
	 * @throws ParseException if the display value is not a decimal number
	 */
	@Override
	protected @Nonnull BigDecimal parse(@Nonnull DecimalFormat format, @Nonnull String displayValue)
	throws ParseException {
		String numberValue = displayValue.startsWith("$") ? displayValue.substring(1).trim() : displayValue;
		return super.parse(format, numberValue);
	}
}
