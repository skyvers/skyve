package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.metadata.model.document.field.validator.DateValidator;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Document field type for audit-timestamp values with millisecond precision.
 *
 * <p>Stored as a {@code DATETIME} or {@code TIMESTAMP} column with millisecond
 * resolution.  The domain type is {@link org.skyve.domain.types.Timestamp}.
 * Typically used for {@code bizCreatedDateTime} and {@code bizUpdatedDateTime}
 * audit columns.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see ConvertibleField
 * @see org.skyve.domain.types.Timestamp
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class Timestamp extends ConvertibleField {
	private static final long serialVersionUID = -5008213051812011630L;

	private DateValidator validator;
	
	public Timestamp() {
		setAttributeType(AttributeType.timestamp);
	}
	
	public DateValidator getValidator() {
		return validator;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setValidator(DateValidator validator) {
		this.validator = validator;
	}
}
