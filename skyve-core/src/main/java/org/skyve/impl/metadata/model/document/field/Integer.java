package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.metadata.model.document.field.validator.IntegerValidator;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Document field type for 32-bit integer values.
 *
 * <p>Stored as an {@code INT} or {@code INTEGER} column.  The domain type is
 * {@link java.lang.Integer}.  Extends {@link ConvertibleField} to allow display
 * formatting (e.g., thousands separators).
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see ConvertibleField
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
public class Integer extends ConvertibleField {
	private static final long serialVersionUID = 884934642737849696L;

	private IntegerValidator validator;
	
	public Integer() {
		setAttributeType(AttributeType.integer);
	}
	
	public IntegerValidator getValidator() {
		return validator;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setValidator(IntegerValidator validator) {
		this.validator = validator;
	}
}
