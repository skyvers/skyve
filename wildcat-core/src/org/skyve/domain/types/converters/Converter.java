package org.skyve.domain.types.converters;

import org.skyve.metadata.model.Attribute.AttributeType;

public interface Converter<T> {
	/**
	 * 
	 * @param displayValue
	 * @return
	 * @throws Exception
	 */
	public T fromDisplayValue(String displayValue) throws Exception;

	/**
	 * 
	 * @param value
	 * @return
	 * @throws Exception
	 */
	public String toDisplayValue(T value) throws Exception;
	
	public AttributeType getAttributeType();
	
	/**
	 * A format definition to use to format the value for this converter.
	 * @return
	 */
	public Format<T> getFormat();
	
	/**
	 * A validator to use after running fromDisplayValue()
	 * @return
	 */
	public Validator<T> getValidator();
}
