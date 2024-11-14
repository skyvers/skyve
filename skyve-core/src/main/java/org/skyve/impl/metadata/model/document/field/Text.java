package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.metadata.model.document.field.validator.TextValidator;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE,
			propOrder = {"length", "format", "validator"})
public class Text extends ConvertibleField implements LengthField {
	private static final long serialVersionUID = 7128775778521517080L;

	private int length;
	private TextFormat format;
	private TextValidator validator;
	
	public Text() {
		setAttributeType(AttributeType.text);
	}
	
	@Override
	public int getLength() {
		return length;
	}

	@Override
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, required = true)
	public void setLength(int length) {
		this.length = length;
	}
	
	public TextFormat getFormat() {
		return format;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setFormat(TextFormat format) {
		this.format = format;
	}

	public TextValidator getValidator() {
		return validator;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setValidator(TextValidator validator) {
		this.validator = validator;
	}
}
