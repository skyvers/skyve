package org.skyve.domain.types.converters.enumeration;

import java.util.List;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.util.Util;

/**
 * Used to convert dynamic enumeration code/display without using Java method invocations.
 */
public class DynamicEnumerationConverter implements Converter<String> {
	private List<EnumeratedValue> values;
	
	public DynamicEnumerationConverter(Enumeration enumeration) {
		values = enumeration.getTarget().getValues();
	}
	
	@Override
	public String toDisplayValue(String value) throws ConversionException {
		try {
			// check code first
			for (EnumeratedValue enumValue : values) {
				String code = enumValue.getCode();
				String display = Util.i18n(enumValue.getDescription());
				if (display == null) {
					display = code;
				}
				if (value.equals(code)) {
					return display;
				}
			}
			// check name next
			for (EnumeratedValue enumValue : values) {
				String display = Util.i18n(enumValue.getDescription());
				if (display == null) {
					display = enumValue.getCode();
				}
				if (value.equals(enumValue.getName())) {
					return display;
				}
			}
			// check description last
			for (EnumeratedValue enumValue : values) {
				String localisedDescription = Util.i18n(enumValue.getDescription());
				String display = localisedDescription;
				if (display == null) {
					display = enumValue.getCode();
				}
				if (value.equals(localisedDescription)) {
					return display;
				}
			}
			return "";
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.DYNAMIC_ENUMERATION_CONVERTER_KEY, e);
		}
	}

	@Override
	public String fromDisplayValue(String displayValue) throws ConversionException {
		try {
			// check description first
			for (EnumeratedValue enumValue : values) {
				String code = enumValue.getCode();
				if (displayValue.equals(Util.i18n(enumValue.getDescription()))) {
					return code;
				}
			}
			// check code next
			for (EnumeratedValue enumValue : values) {
				String code = enumValue.getCode();
				if (displayValue.equals(code)) {
					return code;
				}
			}
			// check name last
			for (EnumeratedValue enumValue : values) {
				String code = enumValue.getCode();
				if (displayValue.equals(enumValue.getName())) {
					return code;
				}
			}
			return null;
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.DYNAMIC_ENUMERATION_CONVERTER_KEY, e);
		}
	}

	@Override
	public AttributeType getAttributeType() {
		return AttributeType.enumeration;
	}

	@Override
	public Format<String> getFormat() {
		return null;
	}

	@Override
	public Validator<String> getValidator() {
		return null;
	}
	
	@Override
	public String getFormatPattern() {
		return null;
	}
}
