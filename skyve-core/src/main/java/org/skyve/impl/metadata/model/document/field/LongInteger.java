package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.metadata.model.document.field.validator.LongValidator;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Document field type for 64-bit integer values.
 *
 * <p>Stored as a {@code BIGINT} column.  The domain type is
 * {@link java.lang.Long}.  Extends {@link ConvertibleField} to allow display
 * formatting.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see ConvertibleField
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class LongInteger extends ConvertibleField {
	private static final long serialVersionUID = -8825125438350207039L;

	private LongValidator validator;
	
	public LongInteger() {
		setAttributeType(AttributeType.longInteger);
	}
	
	public LongValidator getValidator() {
		return validator;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setValidator(LongValidator validator) {
		this.validator = validator;
	}
}
