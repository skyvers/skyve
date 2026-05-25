package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Document field type for long-form plain text (no length limit in storage).
 *
 * <p>Stored as a {@code TEXT}, {@code CLOB}, or equivalent column depending on
 * the database dialect.  Extends {@link ConstrainableField} to allow
 * regex-based and length-based validators even on large text values.
 * Rendered via a multi-line text area widget.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see Markup
 * @see ConstrainableField
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class Memo extends ConstrainableField {
	private static final long serialVersionUID = 6729451150523368478L;

	public Memo() {
		setAttributeType(AttributeType.memo);
	}
}
