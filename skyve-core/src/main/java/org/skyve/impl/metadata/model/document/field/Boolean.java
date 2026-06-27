package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Document field type for boolean (true/false) values.
 *
 * <p>Stored as a {@code BOOLEAN} or {@code BIT} column depending on the
 * database dialect.  The domain type is {@link java.lang.Boolean}.
 * Extends {@link ConvertibleField} to allow custom display labels
 * (e.g., "Yes/No" instead of "true/false").
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see ConvertibleField
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
public class Boolean extends ConvertibleField {
	private static final long serialVersionUID = 4887475952064140008L;

	public Boolean() {
		setAttributeType(AttributeType.bool);
	}
}
