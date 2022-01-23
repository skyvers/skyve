package org.skyve.domain.types.converters.enumeration;

import java.util.List;

import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;
import org.skyve.metadata.model.Attribute.AttributeType;

/**
 * Used to convert dynamic enumeration code/display without using Java method invocations.
 */
public class DynamicEnumerationConverter implements Converter<String> {
	private List<EnumeratedValue> values;
	
	public DynamicEnumerationConverter(Enumeration enumeration) {
		values = enumeration.getTarget().getValues();
	}
	
	@Override
	public String toDisplayValue(String value) throws Exception {
		if (value != null) {
			// check code first
			for (EnumeratedValue enumValue : values) {
				String code = enumValue.getCode();
				String display = enumValue.getDescription();
				if (display == null) {
					display = code;
				}
				if (value.equals(code)) {
					return display;
				}
			}
			// check name next
			for (EnumeratedValue enumValue : values) {
				String display = enumValue.getDescription();
				if (display == null) {
					display = enumValue.getCode();
				}
				if (value.equals(enumValue.getName())) {
					return display;
				}
			}
			// check description last
			for (EnumeratedValue enumValue : values) {
				String description = enumValue.getDescription();
				String display = description;
				if (display == null) {
					display = enumValue.getCode();
				}
				if (value.equals(description)) {
					return display;
				}
			}
		}
		return "";
	}

	@Override
	public String fromDisplayValue(String displayValue) throws Exception {
		if (displayValue != null) {
			// check description first
			for (EnumeratedValue enumValue : values) {
				String code = enumValue.getCode();
				if (displayValue.equals(enumValue.getDescription())) {
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
		}
		return null;
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
}
