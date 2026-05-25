package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Document field type for rich HTML markup (sanitised HTML).
 *
 * <p>Extends {@link Memo} with a maximum character length for the HTML content.
 * The framework sanitises HTML input against a whitelist of permitted tags
 * and attributes to prevent XSS.  Rendered via a rich-text editor widget.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see Memo
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class Markup extends Memo {
	private static final long serialVersionUID = -4849631120596097936L;

	public Markup() {
		setAttributeType(AttributeType.markup);
	}
}
