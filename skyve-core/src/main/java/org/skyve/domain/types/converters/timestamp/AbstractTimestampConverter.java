package org.skyve.domain.types.converters.timestamp;

import org.skyve.CORE;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Timestamp;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

public abstract class AbstractTimestampConverter implements Converter<Timestamp> {
	@Override
	public final AttributeType getAttributeType() {
		return AttributeType.timestamp;
	}

	@Override
	public Format<Timestamp> getFormat() {
		return null;
	}

	@Override
	public Validator<Timestamp> getValidator() {
		return null;
	}

	/**
	 * The pattern for this Timestamp converter
	 * 
	 * @return Timestamp format String pattern
	 */
	protected abstract String getPattern();

	protected abstract String getI18nKey();
	
	@Override
	public Timestamp fromDisplayValue(String displayValue) throws ConversionException {
		try {
			return new Timestamp(CORE.getDateFormat(getPattern()).parse(displayValue).getTime());
		}
		catch (Exception e) {
			throw new ConversionException(getI18nKey(), e);
		}
	}

	@Override
	public String toDisplayValue(Timestamp value) throws ConversionException {
		try {
			return CORE.getDateFormat(getPattern()).format(value);
		}
		catch (Exception e) {
			throw new ConversionException(getI18nKey(), e);
		}
	}
}
