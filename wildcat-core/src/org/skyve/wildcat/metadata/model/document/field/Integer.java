package org.skyve.wildcat.metadata.model.document.field;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.metadata.model.document.field.validator.IntegerValidator;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
public class Integer extends ConvertableField {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 884934642737849696L;

	private IntegerValidator validator;
	
	public Integer() {
		setAttributeType(AttributeType.integer);
	}
	
	public IntegerValidator getValidator() {
		return validator;
	}

	@XmlElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
	public void setValidator(IntegerValidator validator) {
		this.validator = validator;
	}
}
