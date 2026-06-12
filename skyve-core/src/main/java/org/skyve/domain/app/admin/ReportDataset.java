package org.skyve.domain.app.admin;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.beanutils.DynaBean;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.Enumeration;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlEnum;

/**
 * Domain contract for report dataset definitions used by report templates.
 */
public interface ReportDataset extends PersistentBean {
	/**
	 * Dataset Type
	 * <br/>
	 * The type of this dataset; if it is a BizQL or SQL query, a fixed constant value or a dataset Class.
	 **/
	@XmlEnum
	@SuppressWarnings("java:S115") // Enum names are stable persisted domain codes.
	public static enum DatasetType implements Enumeration {
		/**
		 * Dataset rows are produced by executing a BizQL query.
		 */
		bizQL("BizQL", "BizQL"),
		/**
		 * Dataset rows are produced by executing a native SQL query.
		 */
		SQL("SQL", "SQL"),
		/**
		 * Dataset provides a fixed constant value payload.
		 */
		constant("Constant", "Constant"),
		/**
		 * Dataset rows are produced by a class-based provider implementation.
		 */
		classValue("Class", "Class");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(DatasetType::toDomainValue).collect(Collectors.toUnmodifiableList());

		private DatasetType(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		/**
		 * Returns the stable code persisted for this dataset type.
		 *
		 * @return the persisted dataset type code
		 */
		@Override
		public String toCode() {
			return code;
		}

		/**
		 * Returns the localised display description for this dataset type.
		 *
		 * @return the localised description
		 */
		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		/**
		 * Returns this enum value as a domain value suitable for UI/domain lists.
		 *
		 * @return the corresponding domain value
		 */
		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		/**
		 * Resolves the enum value from its persisted code.
		 *
		 * @param code the persisted code to match
		 * @return the matching enum value, or {@code null} when no value matches
		 */
		public static DatasetType fromCode(String code) {
			DatasetType result = null;

			for (DatasetType value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		/**
		 * Resolves the enum value from a localised description.
		 *
		 * @param description the localised description to match
		 * @return the matching enum value, or {@code null} when no value matches
		 */
		public static DatasetType fromLocalisedDescription(String description) {
			DatasetType result = null;

			for (DatasetType value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		/**
		 * Returns the domain values for all enum constants.
		 *
		 * @return immutable domain values for this enum
		 */
		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}

	/**
	 * Returns the dataset source type.
	 *
	 * @return the dataset type
	 */
	DatasetType getDatasetType();

	/**
	 * Returns the logical dataset name used by the report engine.
	 *
	 * @return the dataset name
	 */
	String getDatasetName();

	/**
	 * Returns the configured BizQL/SQL query text when the dataset type is query based.
	 *
	 * @return the configured query text, or {@code null}
	 */
	String getQuery();

	/**
	 * Executes the configured BizQL query dataset.
	 *
	 * @return query result rows as Skyve beans
	 * @throws Exception if query execution fails
	 */
	List<Bean> executeQuery() throws Exception;

	/**
	 * Executes the configured SQL query dataset.
	 *
	 * @return query result rows as dynamic beans
	 * @throws Exception if query execution fails
	 */
	List<DynaBean> executeSQLQuery() throws Exception;

	/**
	 * Executes the configured class-backed dataset provider.
	 *
	 * @return dataset rows as dynamic beans
	 */
	List<DynaBean> executeClass();
}
