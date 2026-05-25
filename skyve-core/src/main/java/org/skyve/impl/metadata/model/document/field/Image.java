package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Document field type for image content managed by the content repository.
 *
 * <p>Extends {@link Content} with a mandatory pixel width ({@code pixelWidth}) and
 * optional height constraint.  The image is stored via the
 * {@link org.skyve.content.ContentManager} and referenced by a UUID column.
 * Rendered via an image widget that scales to the declared pixel dimensions.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see Content
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class Image extends Content {
	private static final long serialVersionUID = 1207840577273060855L;

	public Image() {
		setAttributeType(AttributeType.image);
	}
}
