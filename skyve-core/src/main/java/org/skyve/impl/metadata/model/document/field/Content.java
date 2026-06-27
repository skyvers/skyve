package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Document field type for binary file content managed by the content repository.
 *
 * <p>Stores a reference UUID that points to a file in the configured
 * {@link org.skyve.content.ContentManager}.  The column itself holds only the
 * identifier string; the actual bytes live outside the relational database.
 * Deletion of the bean triggers removal of the associated content unless
 * the content is shared.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see Image
 * @see ConstrainableField
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class Content extends ConstrainableField {
	private static final long serialVersionUID = -167211573965135996L;

	public Content() {
		setAttributeType(AttributeType.content);
	}
}
