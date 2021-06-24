package org.skyve.domain.app.admin;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlEnum;

import org.apache.commons.beanutils.DynaBean;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.Enumeration;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

public interface ReportDataset extends PersistentBean {
	public static final String MODULE_NAME = "admin";
	public static final String DOCUMENT_NAME = "ReportDataset";

	/**
	 * Dataset Type
	 * <br/>
	 * The type of this dataset; if it is a BizQL or SQL query, a fixed constant value or a dataset Class.
	 **/
	@XmlEnum
	public static enum DatasetType implements Enumeration {
		bizQL("BizQL", "BizQL"),
		SQL("SQL", "SQL"),
		constant("Constant", "Constant"),
		classValue("Class", "Class");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private DatasetType(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toDescription() {
			return description;
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

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

		public static DatasetType fromDescription(String description) {
			DatasetType result = null;

			for (DatasetType value : values()) {
				if (value.description.equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				DatasetType[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (DatasetType value : values) {
					domainValues.add(value.domainValue);
				}
			}

			return domainValues;
		}
	}

	DatasetType getDatasetType();
	String getDatasetName();

	String getQuery();
	List<Bean> executeQuery() throws Exception;
	List<DynaBean> executeSQLQuery() throws Exception;
	List<DynaBean> executeClass();
}
