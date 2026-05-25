package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.metadata.model.document.field.validator.DecimalValidator;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Document field type for fixed-point decimal values with 2 decimal places.
 *
 * <p>Stored as a {@code DECIMAL(20,2)} column.  The domain type is
 * {@link org.skyve.domain.types.Decimal2}.  Commonly used for currency and
 * other two-decimal financial figures.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see ConvertibleField
 * @see org.skyve.domain.types.Decimal2
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class Decimal2 extends ConvertibleField {
	private static final long serialVersionUID = -4736160451221243314L;

	DecimalValidator validator;
	
	public Decimal2() {
		setAttributeType(AttributeType.decimal2);
	}
	
	public DecimalValidator getValidator() {
		return validator;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setValidator(DecimalValidator validator) {
		this.validator = validator;
	}
}
