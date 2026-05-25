package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.metadata.model.document.field.validator.DecimalValidator;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Document field type for fixed-point decimal values with 5 decimal places.
 *
 * <p>Stored as a {@code DECIMAL(20,5)} column.  The domain type is
 * {@link org.skyve.domain.types.Decimal5}.  Used for scientific measurements
 * and other five-decimal precision figures.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see ConvertibleField
 * @see org.skyve.domain.types.Decimal5
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class Decimal5 extends ConvertibleField {
	private static final long serialVersionUID = 1470285354635442754L;

	private DecimalValidator validator;
	
	public Decimal5() {
		setAttributeType(AttributeType.decimal5);
	}
	
	public DecimalValidator getValidator() {
		return validator;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setValidator(DecimalValidator validator) {
		this.validator = validator;
	}

}
