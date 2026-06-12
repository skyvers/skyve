package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.metadata.model.document.field.validator.DecimalValidator;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Document field type for fixed-point decimal values with 10 decimal places.
 *
 * <p>Stored as a {@code DECIMAL(30,10)} column.  The domain type is
 * {@link org.skyve.domain.types.Decimal10}.  Used for high-precision calculations.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see ConvertibleField
 * @see org.skyve.domain.types.Decimal10
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
public class Decimal10 extends ConvertibleField {
	private static final long serialVersionUID = 7235405478552339257L;

	private DecimalValidator validator;
	
	public Decimal10() {
		setAttributeType(AttributeType.decimal10);
	}
	
	public DecimalValidator getValidator() {
		return validator;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setValidator(DecimalValidator validator) {
		this.validator = validator;
	}
}
