package org.skyve.domain.types.converters.timestamp;

import org.skyve.CORE;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Timestamp;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

import jakarta.annotation.Nonnull;

/**
 * Base converter for {@link Timestamp} attributes using a configured
 * timestamp pattern and i18n conversion error key.
 *
 * <p>Threading: thread-safe when used with {@link CORE} format factories,
 * because formatter instances are obtained per operation.
 */
public abstract class AbstractTimestampConverter implements Converter<Timestamp> {
	/**
	 * Returns the attribute type supported by this converter.
	 * @return the result value
	 */
	@Override
	public final AttributeType getAttributeType() {
		return AttributeType.timestamp;
	}

	/**
	 * Returns the value type handled by this converter.
	 * @return the result value
	 */
	@Override
	public Class<Timestamp> getValueType() {
		return Timestamp.class;
	}
	
	/**
	 * Returns the optional format descriptor for this converter.
	 * @return the result value
	 */
	@Override
	public Format<Timestamp> getFormat() {
		return null;
	}

	/**
	 * Returns the optional validator descriptor for this converter.
	 * @return the result value
	 */
	@Override
	public Validator<Timestamp> getValidator() {
		return null;
	}

	/**
	 * Overridden to yield non-null results.
	 */
	@Override
	public abstract @Nonnull String getFormatPattern();

	/**
	 * The i18n key used to represent an error in conversion using this converter
	 * @return	The i18n key.
	 */
	protected abstract String getI18nKey();
	
	/**
	 * Converts a display representation to its domain value.
	 * @param displayValue the display value
	 * @return the result value
	 */
	@Override
	public Timestamp fromDisplayValue(String displayValue) throws ConversionException {
		try {
			return new Timestamp(CORE.getDateFormat(getFormatPattern()).parse(displayValue).getTime());
		}
		catch (Exception e) {
			throw new ConversionException(getI18nKey(), e);
		}
	}

	/**
	 * Converts a domain value to its display representation.
	 * @param value the value
	 * @return the result value
	 */
	@Override
	public String toDisplayValue(Timestamp value) throws ConversionException {
		try {
			return CORE.getDateFormat(getFormatPattern()).format(value);
		}
		catch (Exception e) {
			throw new ConversionException(getI18nKey(), e);
		}
	}
}
