package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.metadata.model.document.field.validator.DateValidator;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Document field type for combined date and time values (without sub-second precision).
 *
 * <p>Stored as a {@code DATETIME} or {@code TIMESTAMP} column.  The domain type is
 * {@link org.skyve.domain.types.DateTime}.  Extends {@link ConvertibleField}
 * for locale-aware display formatting.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see ConvertibleField
 * @see org.skyve.domain.types.DateTime
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class DateTime extends ConvertibleField {
	private static final long serialVersionUID = 5225280371209879680L;

	private DateValidator validator;

	public DateTime() {
		setAttributeType(AttributeType.dateTime);
	}
	
	public DateValidator getValidator() {
		return validator;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setValidator(DateValidator validator) {
		this.validator = validator;
	}
}
