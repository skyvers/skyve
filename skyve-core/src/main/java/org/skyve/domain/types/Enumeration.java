package org.skyve.domain.types;

import org.skyve.metadata.SerializableMetaData;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

/**
 * The Skyve Enumeration interface.
 */
public interface Enumeration extends SerializableMetaData {
	/**
	 * The toCode method name.
	 */
	public static final String TO_CODE_METHOD_NAME = "toCode";
	
	/**
	 * The fromCode method name.
	 */
	public static final String FROM_CODE_METHOD_NAME = "fromCode";
	
	/**
	 * The fromDescription method name.
	 */
	public static final String FROM_DESCRIPTION_METHOD_NAME = "fromDescription";

	/**
	 * The fromDescription method name.
	 */
	public static final String TO_DOMAIN_VALUES_METHOD_NAME = "toDomainValues";

	/**
	 * The code of the enumerated value.
	 * The code is persisted in the data stores.
	 * @return	the code of the enumerated value.
	 */
	public String toCode();
	
	/**
	 * The description of the enumerated value.
	 * The description is displayed in UIs.
	 * @return	the description of the enumerated value.
	 */
	public String toDescription();
	
	/**
	 * Return a domain value representation of this enumerated value.
	 * @return	The enumerated value as a domain value.
	 */
	public DomainValue toDomainValue();
}
