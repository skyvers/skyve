package org.skyve.domain.types.converters.decimal;

import org.skyve.CORE;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.FormatterName;
import org.skyve.metadata.model.Attribute.AttributeType;

public class Decimal5TimeDuration implements Converter<Decimal5> {
	@Override
	public Class<Decimal5> getValueType() {
		return Decimal5.class;
	}
	
	@Override
	public Decimal5 fromDisplayValue(String displayValue) throws ConversionException {
		try {
			int colonIndex = displayValue.indexOf(':');
			if (colonIndex < 0) {
				throw new ConversionException(ConversionException.DECIMAL_5_TIME_DURATION_KEY);
			}
	
			Decimal5 hours = new Decimal5(displayValue.substring(0, colonIndex));
			Decimal5 minutes = new Decimal5(displayValue.substring(colonIndex + 1));
			minutes = minutes.divide(Decimal5.SIXTY);
	
			Decimal5 returnValue;
			if (hours.compareTo(Decimal5.ZERO) >= 0) {
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

	@Override
	public String toDisplayValue(Decimal5 value) throws ConversionException {
		try {
			return CORE.format(FormatterName.TimeDuration, value);
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.DECIMAL_5_TIME_DURATION_KEY, e);
		}
	}

	@Override
	public AttributeType getAttributeType() {
		return AttributeType.decimal5;
	}

	@Override
	public Format<Decimal5> getFormat() {
		return null;
	}

	@Override
	public Validator<Decimal5> getValidator() {
		return null;
	}
	
	@Override
	public String getFormatPattern() {
		return null;
	}
}
