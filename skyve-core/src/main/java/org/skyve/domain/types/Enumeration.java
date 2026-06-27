package org.skyve.domain.types;

import java.util.Comparator;

import org.skyve.metadata.SerializableMetaData;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

import jakarta.annotation.Nonnull;

/**
 * Contract implemented by every Skyve-generated enum type.
 *
 * <p>Skyve enumerations differ from plain Java enums in two important ways:
 * <ol>
 *   <li><b>Code vs. description</b> — {@link #toCode()} returns the stable identifier
 *       that is persisted in the data store and compared in queries. The code must not
 *       change across releases once data has been saved.
 *       {@link #toLocalisedDescription()} returns the label displayed in the UI, which
 *       may be localised and may change between releases.
 *   <li><b>Domain values</b> — {@link #toDomainValue()} converts a single member to a
 *       {@link org.skyve.metadata.model.document.Bizlet.DomainValue} for use in pickers
 *       and drop-down lists.
 * </ol>
 *
 * <p>The method-name constants ({@link #TO_CODE_METHOD_NAME}, {@link #FROM_CODE_METHOD_NAME},
 * etc.) are used by the framework for reflective access when deserialising enum values
 * from the database, REST payloads, and Bizlet domain-value lists.
 *
 * <p>The inner {@link DomainValueSortByCode} and {@link DomainValueSortByDescription}
 * comparators are utilities for sorting lists of domain values in picker overlays.
 *
 * @see org.skyve.metadata.model.document.Bizlet.DomainValue
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
	 * Comparator that orders {@link org.skyve.metadata.model.document.Bizlet.DomainValue}
	 * instances alphabetically by their code.
	 */
	public static class DomainValueSortByCode implements Comparator<DomainValue> {
		/**
		 * Executes compare.
		 * @param d1 the d1
		 * @param d2 the d2
		 * @return the result
		 */
		@Override
		public int compare(@Nonnull DomainValue d1, @Nonnull DomainValue d2) {
			return d1.getCode().compareTo(d2.getCode());
		}
	}

	/**
	 * Comparator that orders {@link org.skyve.metadata.model.document.Bizlet.DomainValue}
	 * instances alphabetically by their localised description.
	 */
	public static class DomainValueSortByDescription implements Comparator<DomainValue> {
		/**
		 * Executes compare.
		 * @param d1 the d1
		 * @param d2 the d2
		 * @return the result
		 */
		@Override
		public int compare(@Nonnull DomainValue d1, @Nonnull DomainValue d2) {
			return d1.getLocalisedDescription().compareTo(d2.getLocalisedDescription());
		}
	}
}
