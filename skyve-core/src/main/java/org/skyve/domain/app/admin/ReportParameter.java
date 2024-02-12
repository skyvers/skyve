package org.skyve.domain.app.admin;

import java.util.ArrayList;
import java.util.List;

import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.Enumeration;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlEnum;

public interface ReportParameter extends PersistentBean {
	/**
	 * Parameter Type
	 * <br/>
	 * The data type of this parameter
	 **/
	@XmlEnum
	public static enum Type implements Enumeration {
		text("text", "Text"),
		integer("integer", "Integer"),
		longInteger("long", "Long Integer"),
		date("date", "Date");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private Type(String code, String description) {
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

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				Type[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (Type value : values) {
					domainValues.add(value.domainValue);
				}
			}

			return domainValues;
		}
	}

	String getName();
	Type getType();
	
	void setReportInputValue(String reportInputValue);
}
