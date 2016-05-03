package org.skyve.impl.metadata.model.document.field;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.model.document.field.validator.DateValidator;
import org.skyve.impl.util.XMLUtil;
import org.skyve.impl.metadata.model.document.field.ConvertableField;

@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
public class Time extends ConvertableField {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 5704150703623236506L;

	private DateValidator validator;
	
	public Time() {
		setAttributeType(AttributeType.time);
	}
	
	public DateValidator getValidator() {
		return validator;
	}

	@XmlElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
	public void setValidator(DateValidator validator) {
		this.validator = validator;
	}
}
