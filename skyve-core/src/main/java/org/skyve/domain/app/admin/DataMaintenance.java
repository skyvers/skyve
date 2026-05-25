package org.skyve.domain.app.admin;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.Enumeration;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlEnum;

/**
 * Domain contract for data maintenance operations and settings exposed by the
 * admin module.
 */
public interface DataMaintenance extends PersistentBean {
	
	/**
	 * Sensitivity
	 * <br/>
	 * Determines which attributes are redacted in backup. Attributes with greater than or equal to sensitivity level selected are redacted.
	 * <br/>
	 * Determines which attributes are redacted during an ad-hoc backup.
	 **/
	@XmlEnum
	public static enum DataSensitivity implements Enumeration {
		/**
		 * No redaction threshold; values marked at any sensitivity remain visible.
		 */
		none("none", "None"),
		/**
		 * Internal-only data threshold.
		 */
		internal("internal", "Internal"),
		/**
		 * Confidential data threshold.
		 */
		confidential("confidential", "Confidential"),
		/**
		 * Restricted data threshold.
		 */
		restricted("restricted", "Restricted"),
		/**
		 * Personal data threshold.
		 */
		personal("personal", "Personal"),
		/**
		 * Secret data threshold.
		 */
		secret("secret", "Secret");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(DataSensitivity::toDomainValue).collect(Collectors.toUnmodifiableList());

		private DataSensitivity(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		/**
		 * Returns the stable code persisted for this sensitivity value.
		 *
		 * @return the persisted sensitivity code
		 */
		@Override
		public String toCode() {
			return code;
		}

		/**
		 * Returns the localised display description for this sensitivity value.
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
		public static DataSensitivity fromCode(String code) {
			DataSensitivity result = null;

			for (DataSensitivity value : values()) {
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
		public static DataSensitivity fromLocalisedDescription(String description) {
			DataSensitivity result = null;

			for (DataSensitivity value : values()) {
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
	 * Returns whether the backup/export should include audit log records.
	 *
	 * @return {@code true} when audit logs are included
	 */
	Boolean getIncludeAuditLog();

	/**
	 * Returns whether binary/content payloads should be included.
	 *
	 * @return {@code true} when content should be included
	 */
	Boolean getIncludeContent();

	/**
	 * Returns the sensitivity threshold used for ad-hoc backup redaction.
	 *
	 * @return the configured sensitivity threshold
	 */
	DataSensitivity getDataSensitivity();
}
