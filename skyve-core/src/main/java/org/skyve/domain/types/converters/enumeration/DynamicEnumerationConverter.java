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
	
	/**
	 * Creates a converter backed by the dynamic enumeration target values.
	 *
	 * @param enumeration the enumeration metadata supplying runtime values
	 */
	public DynamicEnumerationConverter(Enumeration enumeration) {
		values = enumeration.getTarget().getValues();
	}
	
	/**
	 * Returns the value type handled by this converter.
	 *
	 * @return {@link String}
	 */
	@Override
	public Class<String> getValueType() {
		return String.class;
	}
	
	/**
	 * Converts an enumeration code, name, or description into the display text.
	 *
	 * @param value the source value to convert
	 * @return the localised display value, or an empty string when no match is found
	 * @throws ConversionException when conversion fails unexpectedly
	 */
	@Override
	@SuppressWarnings("java:S3776") // Complexity OK
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

	/**
	 * Converts a display value back to the underlying enumeration code.
	 *
	 * @param displayValue the display value to convert
	 * @return the matching enumeration code
	 * @throws ConversionException when the value cannot be resolved
	 */
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
			throw new IllegalArgumentException(displayValue + " cannot be converted from a dynamic enumeration");
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.DYNAMIC_ENUMERATION_CONVERTER_KEY, e);
		}
	}

	/**
	 * Returns the attribute type supported by this converter.
	 *
	 * @return {@link AttributeType#enumeration}
	 */
	@Override
	public AttributeType getAttributeType() {
		return AttributeType.enumeration;
	}

	/**
	 * Returns an optional format descriptor.
	 *
	 * @return {@code null}, as this converter has no custom format descriptor
	 */
	@Override
	public Format<String> getFormat() {
		return null;
	}

	/**
	 * Returns an optional validator descriptor.
	 *
	 * @return {@code null}, as this converter has no custom validator descriptor
	 */
	@Override
	public Validator<String> getValidator() {
		return null;
	}
	
	/**
	 * Returns an optional format pattern.
	 *
	 * @return {@code null}, as this converter does not use a format pattern
	 */
	@Override
	public String getFormatPattern() {
		return null;
	}
}
