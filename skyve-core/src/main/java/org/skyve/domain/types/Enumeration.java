package org.skyve.domain.types;

import java.util.Comparator;

import org.skyve.metadata.SerializableMetaData;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

import jakarta.annotation.Nonnull;

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
	 * The fromLocalisedDescription method name.
	 */
	public static final String FROM_LOCALISED_DESCRIPTION_METHOD_NAME = "fromLocalisedDescription";

	/**
	 * The toDomainValues method name.
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
	public String toLocalisedDescription();
	
	/**
	 * Return a domain value representation of this enumerated value.
	 * @return	The enumerated value as a domain value.
	 */
	public DomainValue toDomainValue();
	
	/**
	 * Comparator to allow sorting of domain values by code
	 */
	public static class DomainValueSortByCode implements Comparator<DomainValue> {
		@Override
		public int compare(@Nonnull DomainValue d1, @Nonnull DomainValue d2) {
			return d1.getCode().compareTo(d2.getCode());
		}
	}

	/**
	 * Comparator to allow sorting of domain values by description
	 */
	public static class DomainValueSortByDescription implements Comparator<DomainValue> {
		@Override
		public int compare(@Nonnull DomainValue d1, @Nonnull DomainValue d2) {
			return d1.getLocalisedDescription().compareTo(d2.getLocalisedDescription());
		}
	}
}
