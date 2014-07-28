package org.skyve.wildcat.metadata.model.document.field;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.metadata.model.document.field.validator.TextValidator;
import org.skyve.wildcat.util.XMLUtil;

@XmlRootElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE,
			propOrder = {"length", "format", "validator"})
public class Text extends ConvertableField implements LengthField {
	/**
	 * For Serialization
	 */
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
	@XmlElement(namespace = XMLUtil.DOCUMENT_NAMESPACE, required = true)
	public void setLength(int length) {
		this.length = length;
	}
	
	public TextFormat getFormat() {
		return format;
	}

	@XmlElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
	public void setFormat(TextFormat format) {
		this.format = format;
	}

	public TextValidator getValidator() {
		return validator;
	}

	@XmlElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
	public void setValidator(TextValidator validator) {
		this.validator = validator;
	}
}
