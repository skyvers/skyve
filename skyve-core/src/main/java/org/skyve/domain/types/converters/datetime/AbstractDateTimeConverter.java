package org.skyve.domain.types.converters.datetime;

import org.skyve.CORE;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

public abstract class AbstractDateTimeConverter implements Converter<DateTime> {
	@Override
	public final AttributeType getAttributeType() {
		return AttributeType.dateTime;
	}

	@Override
	public Format<DateTime> getFormat() {
		return null;
	}

	@Override
	public Validator<DateTime> getValidator() {
		return null;
	}

	/**
	 * The i18n key used to represent an error in conversion using this converter
	 * @return	The i18n key.
	 */
	protected abstract String getI18nKey();
	
	@Override
	public DateTime fromDisplayValue(String displayValue) throws ConversionException {
		try {
			return new DateTime(CORE.getDateFormat(getFormatPattern()).parse(displayValue).getTime());
		}
		catch (Exception e) {
			throw new ConversionException(getI18nKey(), e);
		}
	}

	@Override
	public String toDisplayValue(DateTime value) throws ConversionException {
		try {
			return CORE.getDateFormat(getFormatPattern()).format(value);
		}
		catch (Exception e) {
			throw new ConversionException(getI18nKey(), e);
		}
	}
}
