package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlEnum;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import modules.admin.UserProxy.UserProxyExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Enumeration;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.types.jaxb.TimestampMapper;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.ExpressionEvaluator;
import org.skyve.util.Util;

/**
 * Security Log
 * <br/>
 * The Security Log consists of a list of records of {@link SecurityException}, {@link AccessExceptions}, so that intrusions can be detected in near real time.
			<br/>
			If email is configured, emails are sent to the support email address specified in the project JSON for each event raised.
 * 
 * @depend - - - ExceptionType
 * @navhas n loggedInUser 1 UserProxy
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class SecurityLog extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "SecurityLog";

	/** @hidden */
	public static final String timestampPropertyName = "timestamp";

	/** @hidden */
	public static final String threadIDPropertyName = "threadID";

	/** @hidden */
	public static final String loggedInUserPropertyName = "loggedInUser";

	/** @hidden */
	public static final String exceptionTypePropertyName = "exceptionType";

	/** @hidden */
	public static final String exceptionMessagePropertyName = "exceptionMessage";

	/** @hidden */
	public static final String sourceIPPropertyName = "sourceIP";

	/** @hidden */
	public static final String provenancePropertyName = "provenance";

	/**
	 * Exception Type
	 * <br/>
	 * Type of exception raised
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum ExceptionType implements Enumeration {
		securityException("Security Exception", "Security Exception"),
		accessException("Access Exception", "Access Exception");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(ExceptionType::toDomainValue).collect(Collectors.toUnmodifiableList());

		private ExceptionType(String code, String description) {
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

		public static ExceptionType fromCode(String code) {
			ExceptionType result = null;

			for (ExceptionType value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static ExceptionType fromLocalisedDescription(String description) {
			ExceptionType result = null;

			for (ExceptionType value : values()) {
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

	/**
	 * Timestamp
	 * <br/>
	 * When the exception was raised
	 **/
	private Timestamp timestamp = (Timestamp) ExpressionEvaluator.evaluate("{TIMESTAMP}", this);

	/**
	 * Thread ID
	 * <br/>
	 * Thread ID when the exception was raised
	 **/
	private String threadID;

	/**
	 * Logged In User
	 * <br/>
	 * Logged in user when exception was raised
	 **/
	private UserProxyExtension loggedInUser = null;

	/**
	 * Exception Type
	 * <br/>
	 * Type of exception raised
	 **/
	private ExceptionType exceptionType;

	/**
	 * Exception Message
	 * <br/>
	 * Exception message from exception raised
	 **/
	private String exceptionMessage;

	/**
	 * Source IP
	 * <br/>
	 * Source Ip when exception was raised
	 **/
	private String sourceIP;

	/**
	 * Provenance
	 * <br/>
	 * First line of the stack trace of exception raised
	 **/
	private String provenance;

	@Override
	@XmlTransient
	public String getBizModule() {
		return SecurityLog.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return SecurityLog.DOCUMENT_NAME;
	}

	public static SecurityLog newInstance() {
		try {
			return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
		}
		catch (RuntimeException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage("Security Log - {timestamp}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof SecurityLog) && 
					this.getBizId().equals(((SecurityLog) o).getBizId()));
	}

	/**
	 * {@link #timestamp} accessor.
	 * @return	The value.
	 **/
	public Timestamp getTimestamp() {
		return timestamp;
	}

	/**
	 * {@link #timestamp} mutator.
	 * @param timestamp	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setTimestamp(Timestamp timestamp) {
		preset(timestampPropertyName, timestamp);
		this.timestamp = timestamp;
	}

	/**
	 * {@link #threadID} accessor.
	 * @return	The value.
	 **/
	public String getThreadID() {
		return threadID;
	}

	/**
	 * {@link #threadID} mutator.
	 * @param threadID	The new value.
	 **/
	@XmlElement
	public void setThreadID(String threadID) {
		preset(threadIDPropertyName, threadID);
		this.threadID = threadID;
	}

	/**
	 * {@link #loggedInUser} accessor.
	 * @return	The value.
	 **/
	public UserProxyExtension getLoggedInUser() {
		return loggedInUser;
	}

	/**
	 * {@link #loggedInUser} mutator.
	 * @param loggedInUser	The new value.
	 **/
	@XmlElement
	public void setLoggedInUser(UserProxyExtension loggedInUser) {
		if (this.loggedInUser != loggedInUser) {
			preset(loggedInUserPropertyName, loggedInUser);
			this.loggedInUser = loggedInUser;
		}
	}

	/**
	 * {@link #exceptionType} accessor.
	 * @return	The value.
	 **/
	public ExceptionType getExceptionType() {
		return exceptionType;
	}

	/**
	 * {@link #exceptionType} mutator.
	 * @param exceptionType	The new value.
	 **/
	@XmlElement
	public void setExceptionType(ExceptionType exceptionType) {
		preset(exceptionTypePropertyName, exceptionType);
		this.exceptionType = exceptionType;
	}

	/**
	 * {@link #exceptionMessage} accessor.
	 * @return	The value.
	 **/
	public String getExceptionMessage() {
		return exceptionMessage;
	}

	/**
	 * {@link #exceptionMessage} mutator.
	 * @param exceptionMessage	The new value.
	 **/
	@XmlElement
	public void setExceptionMessage(String exceptionMessage) {
		preset(exceptionMessagePropertyName, exceptionMessage);
		this.exceptionMessage = exceptionMessage;
	}

	/**
	 * {@link #sourceIP} accessor.
	 * @return	The value.
	 **/
	public String getSourceIP() {
		return sourceIP;
	}

	/**
	 * {@link #sourceIP} mutator.
	 * @param sourceIP	The new value.
	 **/
	@XmlElement
	public void setSourceIP(String sourceIP) {
		preset(sourceIPPropertyName, sourceIP);
		this.sourceIP = sourceIP;
	}

	/**
	 * {@link #provenance} accessor.
	 * @return	The value.
	 **/
	public String getProvenance() {
		return provenance;
	}

	/**
	 * {@link #provenance} mutator.
	 * @param provenance	The new value.
	 **/
	@XmlElement
	public void setProvenance(String provenance) {
		preset(provenancePropertyName, provenance);
		this.provenance = provenance;
	}
}
