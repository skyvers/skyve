package org.skyve.domain.app.admin;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Enumeration;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlEnum;

/**
 * Domain contract for queued or immediate communications sent by the admin
 * module (email and related outbound channels).
 */
public interface Communication extends PersistentBean {
	/**
	 * Enumerates outbound communication formats supported by the admin module.
	 *
	 * <p>The persisted code is stored on Communication records and must remain
	 * stable for backward compatibility across generated domain and metadata
	 * consumers.
	 */
	@XmlEnum
	@SuppressWarnings("java:S115") // Enum names are stable persisted domain codes.
	public static enum FormatType implements Enumeration {
		/**
		 * Email-based outbound communication format.
		 */
		email("email", "email");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(FormatType::toDomainValue).collect(Collectors.toUnmodifiableList());

		private FormatType(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		/**
		 * Returns the stable code persisted for this format type.
		 *
		 * @return the persisted format code
		 */
		@Override
		public String toCode() {
			return code;
		}

		/**
		 * Returns the localised display description for this format type.
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

		/**
		 * Resolves the enum value from a localised description.
		 *
		 * @param description the localised description to match
		 * @return the matching enum value, or {@code null} when no value matches
		 */
		public static FormatType fromLocalisedDescription(String description) {
			FormatType result = null;

			for (FormatType value : values()) {
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
	 * Enumerates communication processing actions available to the UI/workflow.
	 */
	@XmlEnum
	@SuppressWarnings("java:S115") // Enum names are stable persisted domain codes.
	public static enum ActionType implements Enumeration {
		/**
		 * Saves the communication for later batch/bulk sending.
		 */
		saveForBulkSend("save", "Save for bulk send"),
		/**
		 * Sends the communication immediately.
		 */
		sendImmediately("send", "Send Immediately"),
		/**
		 * Executes a dry run to validate bindings and generated output.
		 */
		testBindingsAndOutput("test", "Test bindings and output");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(ActionType::toDomainValue).collect(Collectors.toUnmodifiableList());

		private ActionType(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		/**
		 * Returns the stable code persisted for this action type.
		 *
		 * @return the persisted action code
		 */
		@Override
		public String toCode() {
			return code;
		}

		/**
		 * Returns the localised display description for this action type.
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

		/**
		 * Resolves the enum value from a localised description.
		 *
		 * @param description the localised description to match
		 * @return the matching enum value, or {@code null} when no value matches
		 */
		public static ActionType fromLocalisedDescription(String description) {
			ActionType result = null;

			for (ActionType value : values()) {
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
	 * Returns the sender address expression.
	 *
	 * @return the sender address expression
	 */
	String getSendFrom();

	/**
	 * Returns the primary recipient address expression.
	 *
	 * @return the recipient address expression
	 */
	String getSendTo();

	/**
	 * Returns an optional override recipient address.
	 *
	 * @return the override recipient address, or {@code null}
	 */
	String getSendToOverride();

	/**
	 * Returns the CC recipient address expression.
	 *
	 * @return the CC recipient address expression
	 */
	String getCcTo();

	/**
	 * Returns an optional override CC recipient address.
	 *
	 * @return the override CC recipient address, or {@code null}
	 */
	String getCcToOverride();

	/**
	 * Returns whether monitor BCC recipients should be included.
	 *
	 * @return {@code true} when monitor BCC should be included
	 */
	Boolean getMonitorBcc();

	/**
	 * Returns the communication subject expression.
	 *
	 * @return the communication subject expression
	 */
	String getSubject();

	/**
	 * Returns the communication body template.
	 *
	 * @return the communication body template
	 */
	String getBody();

	/**
	 * Returns the template used to build the communication.
	 *
	 * @return the communication template, or {@code null}
	 */
	CommunicationTemplate getTemplate();

	/**
	 * Returns whether a calendar attachment should be generated.
	 *
	 * @return {@code true} when a calendar attachment should be included
	 */
	Boolean getIncludeCalendar();

	/**
	 * Returns the base path used when resolving attachments and links.
	 *
	 * @return the base path
	 */
	String getBasePath();

	/**
	 * Returns the first attachment binding/expression.
	 *
	 * @return the first attachment binding/expression
	 */
	String getAttachment1();

	/**
	 * Returns the first attachment file name expression.
	 *
	 * @return the first attachment file name expression
	 */
	String getAttachmentFileName1();

	/**
	 * Returns the second attachment binding/expression.
	 *
	 * @return the second attachment binding/expression
	 */
	String getAttachment2();

	/**
	 * Returns the second attachment file name expression.
	 *
	 * @return the second attachment file name expression
	 */
	String getAttachmentFileName2();

	/**
	 * Returns the third attachment binding/expression.
	 *
	 * @return the third attachment binding/expression
	 */
	String getAttachment3();

	/**
	 * Returns the third attachment file name expression.
	 *
	 * @return the third attachment file name expression
	 */
	String getAttachmentFileName3();

	/**
	 * Returns the calendar title expression.
	 *
	 * @return the calendar title expression
	 */
	String getCalendarTitleExpression();

	/**
	 * Returns the calendar description expression.
	 *
	 * @return the calendar description expression
	 */
	String getCalendarDescriptionExpression();

	/**
	 * Returns the calendar event start time.
	 *
	 * @return the calendar event start time
	 */
	DateTime getCalendarStartTime();

	/**
	 * Returns the calendar event end time.
	 *
	 * @return the calendar event end time
	 */
	DateTime getCalendarEndTime();

	/**
	 * Returns the communication batch identifier.
	 *
	 * @return the communication batch identifier
	 */
	String getBatch();

	/**
	 * Returns the communication format type.
	 *
	 * @return the communication format type
	 */
	FormatType getFormatType();

	/**
	 * Returns the target module name.
	 *
	 * @return the target module name
	 */
	String getModuleName();

	/**
	 * Returns the target document name.
	 *
	 * @return the target document name
	 */
	String getDocumentName();

	/**
	 * Returns the tag used to categorise this communication.
	 *
	 * @return the communication tag, or {@code null}
	 */
	Tag getTag();

	/**
	 * Returns the action type to apply when processing this communication.
	 *
	 * @return the action type
	 */
	ActionType getActionType();

	/**
	 * Sets the action type for processing this communication.
	 *
	 * @param actionType the action type to set
	 */
	void setActionType(ActionType actionType);

	/**
	 * Sets the communication description.
	 *
	 * @param description the description to set
	 */
	void setDescription(String description);

	/**
	 * Sets the communication format type.
	 *
	 * @param formatType the format type to set
	 */
	void setFormatType(FormatType formatType);

	/**
	 * Sets whether this communication is for system use.
	 *
	 * @param systemUse {@code true} when system use is enabled
	 */
	void setSystemUse(Boolean systemUse);

	/**
	 * Sets the sender address expression.
	 *
	 * @param sendFrom the sender address expression
	 */
	void setSendFrom(String sendFrom);

	/**
	 * Sets the recipient address expression.
	 *
	 * @param sendTo the recipient address expression
	 */
	void setSendTo(String sendTo);

	/**
	 * Sets the override recipient address.
	 *
	 * @param sendToOverride the override recipient address
	 */
	void setSendToOverride(String sendToOverride);

	/**
	 * Sets the CC recipient address expression.
	 *
	 * @param ccTo the CC recipient address expression
	 */
	void setCcTo(String ccTo);

	/**
	 * Sets the override CC recipient address.
	 *
	 * @param ccToOverride the override CC recipient address
	 */
	void setCcToOverride(String ccToOverride);

	/**
	 * Sets whether monitor BCC recipients should be included.
	 *
	 * @param monitorBcc {@code true} to include monitor BCC recipients
	 */
	void setMonitorBcc(Boolean monitorBcc);

	/**
	 * Sets the communication subject expression.
	 *
	 * @param subject the communication subject expression
	 */
	void setSubject(String subject);

	/**
	 * Sets the communication body template.
	 *
	 * @param body the communication body template
	 */
	void setBody(String body);

	/**
	 * Sets execution results text for this communication.
	 *
	 * @param results the execution results text
	 */
	void setResults(String results);

	/**
	 * Sets the first attachment binding/expression.
	 *
	 * @param attachment1 the first attachment binding/expression
	 */
	void setAttachment1(String attachment1);

	/**
	 * Sets the first attachment file name expression.
	 *
	 * @param attachmentFileName1 the first attachment file name expression
	 */
	void setAttachmentFileName1(String attachmentFileName1);

	/**
	 * Sets the second attachment binding/expression.
	 *
	 * @param attachment2 the second attachment binding/expression
	 */
	void setAttachment2(String attachment2);

	/**
	 * Sets the second attachment file name expression.
	 *
	 * @param attachmentFileName2 the second attachment file name expression
	 */
	void setAttachmentFileName2(String attachmentFileName2);

	/**
	 * Sets the third attachment binding/expression.
	 *
	 * @param attachment3 the third attachment binding/expression
	 */
	void setAttachment3(String attachment3);

	/**
	 * Sets the third attachment file name expression.
	 *
	 * @param attachmentFileName3 the third attachment file name expression
	 */
	void setAttachmentFileName3(String attachmentFileName3);

	/**
	 * Sets the unsubscribe URL expression.
	 *
	 * @param unsubscribeUrl the unsubscribe URL expression
	 */
	void setUnsubscribeUrl(String unsubscribeUrl);

	/**
	 * Sets whether a calendar attachment should be generated.
	 *
	 * @param includeCalendar {@code true} to include a calendar attachment
	 */
	void setIncludeCalendar(Boolean includeCalendar);

	/**
	 * Sets the calendar title expression.
	 *
	 * @param calendarTitleExpression the calendar title expression
	 */
	void setCalendarTitleExpression(String calendarTitleExpression);

	/**
	 * Sets the calendar event start time.
	 *
	 * @param calendarStartTime the calendar event start time
	 */
	void setCalendarStartTime(DateTime calendarStartTime);

	/**
	 * Sets the calendar event end time.
	 *
	 * @param calendarEndTime the calendar event end time
	 */
	void setCalendarEndTime(DateTime calendarEndTime);

	/**
	 * Sets the calendar description expression.
	 *
	 * @param calendarDescriptionExpression the calendar description expression
	 */
	void setCalendarDescriptionExpression(String calendarDescriptionExpression);

	/**
	 * Sets the template used to build this communication.
	 *
	 * @param communicationTemplate the template to set
	 */
	void setTemplate(CommunicationTemplate communicationTemplate);
}
