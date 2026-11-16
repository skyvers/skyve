package org.skyve.domain.types.converters.decimal;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.util.function.Function;

import org.skyve.CORE;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Base converter for {@link Decimal} attributes that display through a
 * {@link DecimalFormat} pattern.
 *
 * <p>Display values are rounded {@link RoundingMode#HALF_UP}, matching the
 * rounding of the {@link Decimal} types and the SmartClient and Vue renderers.
 * Any failure to format or parse, including a non-numeric parse result such as
 * {@code NaN}, is reported as a {@link ConversionException} carrying the
 * converter's i18n key.
 *
 * <p>Threading: thread-safe, because a new {@link DecimalFormat} is obtained
 * per operation.
 *
 * @param <T> the decimal type converted
 */
public abstract class AbstractDecimalConverter<T extends Decimal> implements Converter<T> {
	/** Pattern for whole-number display values. */
	public static final String INTEGER_PATTERN = "###,###,###,##0";
	/** Pattern for display values with one decimal place. */
	public static final String ONE_DECIMAL_PLACE_PATTERN = "###,###,###,##0.0";
	/** Pattern for display values with two decimal places. */
	public static final String TWO_DECIMAL_PLACES_PATTERN = "###,###,###,##0.00";

	private final Class<T> valueType;
	private final AttributeType attributeType;
	private final Function<BigDecimal, T> factory;
	private final String pattern;
	private final String i18nKey;

	/**
	 * Creates a converter for one decimal type and display pattern.
	 *
	 * @param valueType the decimal type converted
	 * @param attributeType the attribute type the converter applies to
	 * @param factory creates a {@code T} from a parsed value, applying the type's scale
	 * @param pattern the {@link DecimalFormat} pattern used for display and parsing
	 * @param i18nKey the i18n key of the conversion error message
	 */
	protected AbstractDecimalConverter(@Nonnull Class<T> valueType,
										@Nonnull AttributeType attributeType,
										@Nonnull Function<BigDecimal, T> factory,
										@Nonnull String pattern,
										@Nonnull String i18nKey) {
		this.valueType = valueType;
		this.attributeType = attributeType;
		this.factory = factory;
		this.pattern = pattern;
		this.i18nKey = i18nKey;
	}

	/**
	 * Returns the value type handled by this converter.
	 * @return the decimal type converted
	 */
	@Override
	public @Nonnull Class<T> getValueType() {
		return valueType;
	}

	/**
	 * Returns the attribute type supported by this converter.
	 * @return the decimal attribute type
	 */
	@Override
	public @Nonnull AttributeType getAttributeType() {
		return attributeType;
	}

	/**
	 * Returns the optional format descriptor for this converter.
	 * @return {@code null}; these converters have no input mask
	 */
	@Override
	public @Nullable Format<T> getFormat() {
		return null;
	}

	/**
	 * Returns the optional validator descriptor for this converter.
	 * @return {@code null}; these converters add no validation
	 */
	@Override
	public @Nullable Validator<T> getValidator() {
		return null;
	}

	/**
	 * Returns the {@link DecimalFormat} pattern used for display and parsing.
	 * @return the pattern; never {@code null}
	 */
	@Override
	public @Nonnull String getFormatPattern() {
		return pattern;
	}

	/**
	 * Converts a domain value to its display representation.
	 * @param value the value to format
	 * @return the formatted value
	 * @throws ConversionException if the value cannot be formatted
	 */
	@Override
	public @Nonnull String toDisplayValue(@Nonnull T value) throws ConversionException {
		try {
			return format(newDecimalFormat(), value.bigDecimalValue());
		}
		catch (Exception e) {
			throw new ConversionException(i18nKey, e);
		}
	}

	/**
	 * Converts a display representation to its domain value.
	 * @param displayValue the display value to parse
	 * @return the parsed value, scaled by the decimal type
	 * @throws ConversionException if the display value cannot be parsed
	 */
	@Override
	public @Nonnull T fromDisplayValue(@Nonnull String displayValue) throws ConversionException {
		try {
			return factory.apply(parse(newDecimalFormat(), displayValue));
		}
		catch (Exception e) {
			throw new ConversionException(i18nKey, e);
		}
	}

	/**
	 * Creates the format used for one conversion: the pattern, parsing to
	 * {@link BigDecimal} and {@link RoundingMode#HALF_UP} rounding.
	 * Subclasses override this to adjust symbols such as the negative prefix.
	 * @return a new format instance
	 */
	protected @Nonnull DecimalFormat newDecimalFormat() {
		DecimalFormat result = CORE.getDecimalFormat(pattern);
		result.setParseBigDecimal(true);
		result.setRoundingMode(RoundingMode.HALF_UP);
		return result;
	}

	/**
	 * Formats a decimal value for display.
	 * @param format the format from {@link #newDecimalFormat()}
	 * @param value the value to format
	 * @return the display value
	 */
	@SuppressWarnings("static-method")
	protected @Nonnull String format(@Nonnull DecimalFormat format, @Nonnull BigDecimal value) {
		return format.format(value);
	}

	/**
	 * Parses a display value into a decimal value.
	 * @param format the format from {@link #newDecimalFormat()}
	 * @param displayValue the display value to parse
	 * @return the parsed value
	 * @throws ParseException if the display value is not a decimal number
	 */
	@SuppressWarnings("static-method")
	protected @Nonnull BigDecimal parse(@Nonnull DecimalFormat format, @Nonnull String displayValue)
	throws ParseException {
		Number result = format.parse(displayValue);
		if (result instanceof BigDecimal decimal) {
			return decimal;
		}
		// DecimalFormat yields a Double for NaN and infinity, even when parsing to BigDecimal
		throw new ParseException(displayValue, 0);
	}
}
