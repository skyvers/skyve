package org.skyve.wildcat.metadata.model.document.field;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.metadata.model.document.field.validator.DecimalValidator;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
public class Decimal5 extends ConvertableField {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 1470285354635442754L;

	private DecimalValidator validator;
	
	public Decimal5() {
		setAttributeType(AttributeType.decimal5);
	}
	
	public DecimalValidator getValidator() {
		return validator;
	}

	@XmlElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
	public void setValidator(DecimalValidator validator) {
		this.validator = validator;
	}

}
