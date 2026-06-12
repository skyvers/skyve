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
 * Domain contract for report parameter definitions supplied to report runs.
 */
public interface ReportParameter extends PersistentBean {
	/**
	 * Parameter Type
	 * <br/>
	 * The data type of this parameter
	 **/
	@XmlEnum
	@SuppressWarnings("java:S115") // Enum names are stable persisted domain codes.
	public static enum Type implements Enumeration {
		/**
		 * Free-form textual input parameter.
		 */
		text("text", "Text"),
		/**
		 * 32-bit integer numeric parameter.
		 */
		integer("integer", "Integer"),
		/**
		 * 64-bit integer numeric parameter.
		 */
		longInteger("long", "Long Integer"),
		/**
		 * Calendar date parameter.
		 */
		date("date", "Date");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(Type::toDomainValue).collect(Collectors.toUnmodifiableList());

		private Type(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		/**
		 * Returns the stable code persisted for this report parameter type.
		 *
		 * @return the persisted parameter type code
		 */
		@Override
		public String toCode() {
			return code;
		}

		/**
		 * Returns the localised display description for this parameter type.
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
		public static Type fromCode(String code) {
			Type result = null;

			for (Type value : values()) {
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
		public static Type fromLocalisedDescription(String description) {
			Type result = null;

			for (Type value : values()) {
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
	 * Returns the report input parameter name.
	 *
	 * @return the parameter name
	 */
	String getName();

	/**
	 * Returns the declared parameter data type.
	 *
	 * @return the parameter type
	 */
	Type getType();
	
	/**
	 * Applies an input value represented as text to this parameter.
	 *
	 * @param reportInputValue the input value supplied by report execution
	 */
	void setReportInputValue(String reportInputValue);
}
