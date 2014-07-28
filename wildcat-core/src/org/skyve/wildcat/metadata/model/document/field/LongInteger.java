package org.skyve.wildcat.metadata.model.document.field;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.metadata.model.document.field.validator.LongValidator;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
public class LongInteger extends ConvertableField {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -8825125438350207039L;

	private LongValidator validator;
	
	public LongInteger() {
		setAttributeType(AttributeType.longInteger);
	}
	
	public LongValidator getValidator() {
		return validator;
	}

	@XmlElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
	public void setValidator(LongValidator validator) {
		this.validator = validator;
	}
}
