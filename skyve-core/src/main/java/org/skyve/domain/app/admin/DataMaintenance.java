package org.skyve.domain.app.admin;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.Enumeration;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlEnum;

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
		none("none", "None"),
		internal("internal", "Internal"),
		confidential("confidential", "Confidential"),
		restricted("restricted", "Restricted"),
		personal("personal", "Personal"),
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

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

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

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}
	
	Boolean getIncludeAuditLog();
	Boolean getIncludeContent();
	DataSensitivity getDataSensitivity();
}
