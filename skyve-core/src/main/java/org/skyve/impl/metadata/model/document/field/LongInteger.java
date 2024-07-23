package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.metadata.model.document.field.validator.LongValidator;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class LongInteger extends ConvertableField {
	private static final long serialVersionUID = -8825125438350207039L;

	private LongValidator validator;
	
	public LongInteger() {
		setAttributeType(AttributeType.longInteger);
	}
	
	public LongValidator getValidator() {
		return validator;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setValidator(LongValidator validator) {
		this.validator = validator;
	}
}
