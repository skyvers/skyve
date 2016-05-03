package org.skyve.impl.metadata.model.document.field.validator;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.util.XMLUtil;

@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
public abstract class RangeValidator<T> extends FieldValidator<T> {
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
	public String constructMessage(String displayName, Converter<T> converter) {
		String message = getValidationMessage();
		if (message == null) {
			StringBuilder sb = new StringBuilder(128);
			try {
				if (min != null) {
					sb.append(displayName).append(" must not be ").append((min instanceof Number) ? "less than " : "before ");
					if (converter == null) {
						sb.append(min);
					}
					else {
						sb.append(converter.toDisplayValue(min));
					}
				}
				if (max != null) {
					if (min != null) {
						sb.append(" or ");
					}
					else {
						sb.append(displayName).append(" must not be ");
					}
				
					sb.append((max instanceof Number) ? "greater than " : "after ");
					if (converter == null) {
						sb.append(max);
					}
					else {
						sb.append(converter.toDisplayValue(max));
					}
				}
			}
			catch (Exception e) {
				throw new IllegalStateException("Could not call RangeValidator.constructMessage()", e);
			}

			return sb.toString();
		}

		return message;
	}
}
