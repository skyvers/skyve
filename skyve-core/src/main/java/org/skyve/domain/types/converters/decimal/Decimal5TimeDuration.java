package org.skyve.domain.types.converters.decimal;

import java.util.regex.Pattern;

import org.skyve.CORE;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.FormatterName;
import org.skyve.metadata.model.Attribute.AttributeType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Converts {@link Decimal5} values representing time durations, in hours, displayed as hours and minutes (h:mm).
 * <p>
 * It reads hours, a colon, then minutes, each a decimal number (with an optional sign and spaces around it), as
 * SmartClient's bizTimeDuration does: 1:30 is 1.5 hours, 1:75 is 2.25, 0:29.4996 is 0.49166, and -0:30 is -0.5 (the
 * sign is the hours' text's). Anything else, such as 1.5 (no colon), is not a duration. It shows a value as
 * {@link org.skyve.domain.types.formatters.TimeDurationFormatter} does: the minutes rounded once, half up.
 */
public class Decimal5TimeDuration implements Converter<Decimal5> {
	/** The hours or the minutes: a decimal number with an optional sign (no exponent). */
	private static final Pattern NUMBER = Pattern.compile("[-+]?(?:\\d+(?:\\.\\d*)?|\\.\\d+)");

	/**
	 * Returns the value type handled by this converter.
	 * @return the result value
	 */
	@Override
	public @Nonnull Class<Decimal5> getValueType() {
		return Decimal5.class;
	}
	
	/**
	 * Converts a display representation to its domain value.
	 * @param displayValue the display value
	 * @return the result value
	 */
	@Override
	public @Nonnull Decimal5 fromDisplayValue(@Nonnull String displayValue) throws ConversionException {
		try {
			int colonIndex = displayValue.indexOf(':');
			if (colonIndex < 0) {
				throw new ConversionException(ConversionException.DECIMAL_5_TIME_DURATION_KEY);
			}
	
			String hoursValue = displayValue.substring(0, colonIndex).trim();
			String minutesValue = displayValue.substring(colonIndex + 1).trim();
			if ((! NUMBER.matcher(hoursValue).matches()) || (! NUMBER.matcher(minutesValue).matches())) {
				throw new ConversionException(ConversionException.DECIMAL_5_TIME_DURATION_KEY);
			}
			Decimal5 hours = new Decimal5(hoursValue);
			Decimal5 minutes = new Decimal5(minutesValue);
			minutes = minutes.divide(Decimal5.SIXTY);
	
			// The sign is the hours' text's: -0:30 is negative, although its hours are zero
			Decimal5 returnValue;
			if (! hoursValue.startsWith("-")) {
				returnValue = hours.add(minutes);
			}
			else {
				returnValue = hours.subtract(minutes);
			}
	
			return returnValue;
		}
		catch (ConversionException e) {
			throw e;
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.DECIMAL_5_TIME_DURATION_KEY, e);
		}
	}

	/**
	 * Converts a domain value to its display representation.
	 * @param value the value
	 * @return the result value
	 */
	@Override
	public @Nonnull String toDisplayValue(@Nonnull Decimal5 value) throws ConversionException {
		try {
			return CORE.format(FormatterName.TimeDuration, value);
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.DECIMAL_5_TIME_DURATION_KEY, e);
		}
	}

	/**
	 * Returns the attribute type supported by this converter.
	 * @return the result value
	 */
	@Override
	public @Nonnull AttributeType getAttributeType() {
		return AttributeType.decimal5;
	}

	/**
	 * Returns the optional format descriptor for this converter.
	 * @return the result value
	 */
	@Override
	public @Nullable Format<Decimal5> getFormat() {
		return null;
	}

	/**
	 * Returns the optional validator descriptor for this converter.
	 * @return the result value
	 */
	@Override
	public @Nullable Validator<Decimal5> getValidator() {
		return null;
	}
	
	/**
	 * Returns the optional format pattern for this converter.
	 * @return the result value
	 */
	@Override
	public @Nullable String getFormatPattern() {
		return null;
	}
}
