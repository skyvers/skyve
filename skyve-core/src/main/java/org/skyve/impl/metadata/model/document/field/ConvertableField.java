package org.skyve.impl.metadata.model.document.field;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.ConverterName;
import org.skyve.metadata.customer.Customer;
import org.skyve.impl.metadata.model.document.field.ConstrainableField;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class ConvertableField extends ConstrainableField {
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
			if (getAttributeType() == AttributeType.date) {
				result = customer.getDefaultDateConverter();
			}
			else if (getAttributeType() == AttributeType.time) {
				result = customer.getDefaultTimeConverter();
			}
			else if (getAttributeType() == AttributeType.dateTime) {
				result = customer.getDefaultDateTimeConverter();
			}
			else if (getAttributeType() == AttributeType.timestamp) {
				result = customer.getDefaultTimestampConverter();
			}
		}

		return result;
	}
}
