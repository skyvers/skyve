package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Document field type for CSS hex colour values (e.g., {@code #RRGGBB}).
 *
 * <p>Stored as a fixed-length {@code CHAR(7)} column.  The domain type is
 * {@link java.lang.String}.  Rendered via a colour-picker widget in the UI.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see ConvertibleField
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
public class Colour extends ConvertibleField {
	private static final long serialVersionUID = 2815253897974678949L;

	public Colour() {
		setAttributeType(AttributeType.colour);
	}
}
