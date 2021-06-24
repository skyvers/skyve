package org.skyve.domain.app.admin;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlEnum;

import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Enumeration;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

public interface Communication extends PersistentBean {
	public static final String MODULE_NAME = "admin";
	public static final String DOCUMENT_NAME = "Communication";
	
	public static final String descriptionPropertyName = "description";
	public static final String resultsPropertyName = "results";
	public static final String moduleNamePropertyName = "moduleName";
	public static final String documentNamePropertyName = "documentName";
	public static final String tagPropertyName = "tag";
		
	/**
	 * Format
	 **/
	@XmlEnum
	public static enum FormatType implements Enumeration {
		email("email", "email");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private FormatType(String code, String description) {
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

		public static FormatType fromCode(String code) {
			FormatType result = null;

			for (FormatType value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static FormatType fromDescription(String description) {
			FormatType result = null;

			for (FormatType value : values()) {
				if (value.description.equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				FormatType[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (FormatType value : values) {
					domainValues.add(value.domainValue);
				}
			}

			return domainValues;
		}
	}

	@XmlEnum
	public static enum ActionType implements Enumeration {
		saveForBulkSend("save", "Save for bulk send"),
		sendImmediately("send", "Send Immediately"),
		testBindingsAndOutput("test", "Test bindings and output");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private ActionType(String code, String description) {
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

		public static ActionType fromCode(String code) {
			ActionType result = null;

			for (ActionType value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static ActionType fromDescription(String description) {
			ActionType result = null;

			for (ActionType value : values()) {
				if (value.description.equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				ActionType[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (ActionType value : values) {
					domainValues.add(value.domainValue);
				}
			}

			return domainValues;
		}
	}

	String getSendFrom();
	String getSendTo();
	String getSendToOverride();
	String getCcTo();
	String getCcToOverride();
	Boolean getMonitorBcc();
	String getSubject();
	String getBody();
	CommunicationTemplate getTemplate();
	Boolean getIncludeCalendar();
	String getBasePath();
	String getAttachment1();
	String getAttachmentFileName1();
	String getAttachment2();
	String getAttachmentFileName2();
	String getAttachment3();
	String getAttachmentFileName3();
	String getCalendarTitleExpression();
	String getCalendarDescriptionExpression();
	DateTime getCalendarStartTime();
	DateTime getCalendarEndTime();
	String getBatch();
	FormatType getFormatType();
	String getModuleName();
	String getDocumentName();
	Tag getTag();
	ActionType getActionType();
	
	void setDescription(String description);
	void setFormatType(FormatType formatType);
	void setSystemUse(Boolean systemUse);
	void setSendFrom(String sendFrom);
	void setSendTo(String sendTo);
	void setCcTo(String ccTo);
	void setSubject(String subject);
	void setBody(String body);
	void setResults(String results);
}
