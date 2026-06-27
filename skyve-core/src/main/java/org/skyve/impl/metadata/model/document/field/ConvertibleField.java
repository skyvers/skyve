package org.skyve.impl.metadata.model.document.field;

import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.ConverterName;
import org.skyve.metadata.customer.Customer;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;

/**
 * A {@link ConstrainableField} that may declare an optional type converter.
 *
 * <p>The converter transforms the raw stored value to a human-readable display
 * string and parses user input back to the domain type.  When {@code converterName}
 * is {@code null} the default converter for the field's domain type is used.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see ConstrainableField
 * @see org.skyve.domain.types.converters.Converter
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class ConvertibleField extends ConstrainableField {
	private static final long serialVersionUID = 7758386077394530922L;

	private ConverterName converterName;
	private Converter<?> converter;
	
	public ConverterName getConverterName() {
		return converterName;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setConverterName(ConverterName converterName) {
		this.converterName = converterName;
	}

	public Converter<?> getConverter() {
		return converter;
	}

	@XmlTransient
	public void setConverter(Converter<?> converter) {
		this.converter = converter;
	}

	
	public Converter<?> getConverterForCustomer(Customer customer) {
		Converter<?> result = converter;
		
		if (result == null) {
			AttributeType type = getAttributeType();
			if (type == AttributeType.date) {
				result = customer.getDefaultDateConverter();
			}
			else if (type == AttributeType.time) {
				result = customer.getDefaultTimeConverter();
			}
			else if (type == AttributeType.dateTime) {
				result = customer.getDefaultDateTimeConverter();
			}
			else if (type == AttributeType.timestamp) {
				result = customer.getDefaultTimestampConverter();
			}
		}

		return result;
	}
}
