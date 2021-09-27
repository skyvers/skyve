package org.skyve.impl.metadata.model.document.field;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.model.document.field.validator.LongValidator;
import org.skyve.impl.util.XMLMetaData;

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
