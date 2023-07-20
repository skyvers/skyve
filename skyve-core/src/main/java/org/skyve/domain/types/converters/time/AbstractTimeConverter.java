package org.skyve.domain.types.converters.time;

import org.skyve.CORE;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

public abstract class AbstractTimeConverter implements Converter<TimeOnly> {
	@Override
	public final AttributeType getAttributeType() {
		return AttributeType.time;
	}

	@Override
	public Class<TimeOnly> getValueType() {
		return TimeOnly.class;
	}
	
	@Override
	public Format<TimeOnly> getFormat() {
		return null;
	}

	@Override
	public Validator<TimeOnly> getValidator() {
		return null;
	}

	/**
	 * The i18n key used to represent an error in conversion using this converter
	 * @return	The i18n key.
	 */
	protected abstract String getI18nKey();
	
	@Override
	public TimeOnly fromDisplayValue(String displayValue) throws ConversionException {
		try {
			return new TimeOnly(CORE.getDateFormat(getFormatPattern()).parse(displayValue).getTime());
		}
		catch (Exception e) {
			throw new ConversionException(getI18nKey(), e);
		}
	}

	@Override
	public String toDisplayValue(TimeOnly value) throws ConversionException {
		try {
			return CORE.getDateFormat(getFormatPattern()).format(value);
		}
		catch (Exception e) {
			throw new ConversionException(getI18nKey(), e);
		}
	}
}
