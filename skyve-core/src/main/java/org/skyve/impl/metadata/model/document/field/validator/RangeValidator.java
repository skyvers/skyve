package org.skyve.impl.metadata.model.document.field.validator;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.user.User;
import org.skyve.util.BeanValidator;
import org.skyve.util.Util;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public abstract class RangeValidator<T> extends FieldValidator<T> {
	private static final long serialVersionUID = 1703003606474596114L;

	private String xmlMin;
	private String xmlMax;
	private T min;
	private T max;
	
	public String getXmlMin() {
		return xmlMin;
	}

	@XmlAttribute(name = "min")
	public void setXmlMin(String xmlMin) {
		this.xmlMin = xmlMin;
	}

	public String getXmlMax() {
		return xmlMax;
	}

	@XmlAttribute(name = "max")
	public void setXmlMax(String xmlMax) {
		this.xmlMax = xmlMax;
	}

	public T getMin() {
		return min;
	}
	@XmlTransient
	public void setMin(T min) {
		this.min = min;
	}

	public T getMax() {
		return max;
	}
	@XmlTransient
	public void setMax(T max) {
		this.max = max;
	}

	@Override
	public String constructMessage(User user, String localisedDisplayName, Converter<T> converter) {
		String result = getLocalisedValidationMessage();
		if (result == null) {
			try {
				if (min != null) {
					String minDisplay = (converter == null) ? min.toString() : converter.toDisplayValue(min);
					if (max == null) {
						if (min instanceof Number) {
							result = Util.i18n(BeanValidator.VALIDATION_RANGE_LESS_KEY, localisedDisplayName, minDisplay);
						}
						else {
							result = Util.i18n(BeanValidator.VALIDATION_RANGE_BEFORE_KEY, localisedDisplayName, minDisplay);
						}
					}
					else {
						String maxDisplay = (converter == null) ? max.toString() : converter.toDisplayValue(max);
						result = Util.i18n(BeanValidator.VALIDATION_RANGE_BETWEEN_KEY, localisedDisplayName, minDisplay, maxDisplay);
					}
				}
				else if (max != null) {
					String maxDisplay = (converter == null) ? max.toString() : converter.toDisplayValue(max);
					if (max instanceof Number) {
						result = Util.i18n(BeanValidator.VALIDATION_RANGE_GREATER_KEY, localisedDisplayName, maxDisplay);
					}
					else {
						result = Util.i18n(BeanValidator.VALIDATION_RANGE_AFTER_KEY, localisedDisplayName, maxDisplay);
					}
				}
			}
			catch (Exception e) {
				throw new IllegalStateException("Could not call RangeValidator.constructMessage()", e);
			}
		}

		return result;
	}
}
