package org.skyve.impl.metadata.model.document.field;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.model.document.field.validator.DecimalValidator;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.metadata.model.document.field.ConvertableField;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class Decimal5 extends ConvertableField {
	private static final long serialVersionUID = 1470285354635442754L;

	private DecimalValidator validator;
	
	public Decimal5() {
		setAttributeType(AttributeType.decimal5);
	}
	
	public DecimalValidator getValidator() {
		return validator;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setValidator(DecimalValidator validator) {
		this.validator = validator;
	}

}
