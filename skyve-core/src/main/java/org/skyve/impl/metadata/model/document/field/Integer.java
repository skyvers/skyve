package org.skyve.impl.metadata.model.document.field;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.model.document.field.validator.IntegerValidator;
import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class Integer extends ConvertableField {
	private static final long serialVersionUID = 884934642737849696L;

	private IntegerValidator validator;
	
	public Integer() {
		setAttributeType(AttributeType.integer);
	}
	
	public IntegerValidator getValidator() {
		return validator;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setValidator(IntegerValidator validator) {
		this.validator = validator;
	}
}
