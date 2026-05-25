package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.metadata.model.document.field.validator.TextValidator;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Document field type for bounded-length Unicode text.
 *
 * <p>Stored as a {@code VARCHAR(length)} column.  Extends {@link ConvertibleField}
 * with a maximum character {@code length} (required) and an optional
 * {@link TextFormat} mask.  A length of 255 or less maps to a standard column;
 * longer lengths may map to {@code TEXT} on some databases.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see ConvertibleField
 * @see LengthField
 */
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
