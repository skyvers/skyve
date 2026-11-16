package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.util.XMLMetaData;

import jakarta.annotation.Nullable;
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

	/**
	 * Determines whether an attachment should be included in the textual content index.
	 * Content and image attributes default to textual indexing when no index type is configured.
	 *
	 * @param indexType the configured index type, or {@code null} when it is undefined
	 * @return {@code true} when textual content extraction is enabled
	 */
	public static boolean isTextuallyIndexed(@Nullable IndexType indexType) {
		return (indexType == null) || IndexType.textual.equals(indexType) || IndexType.both.equals(indexType);
	}
}
