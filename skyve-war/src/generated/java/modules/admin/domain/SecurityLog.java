package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.types.jaxb.TimestampMapper;
import org.skyve.util.ExpressionEvaluator;

/**
 * Security Log
 * <br/>
 * The Security Log consists of a list of records of {@link SecurityException}, {@link AccessExceptions}, so that intrusions can be detected in near real time.
			<br/>
			If email is configured, emails are sent to the support email address specified in the project JSON for each event raised.
 * 
 * @depend - - - ExceptionType
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class SecurityLog extends AbstractPersistentBean implements org.skyve.domain.app.admin.SecurityLog {
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
	public static final String sourceIPPropertyName = "sourceIP";

	/** @hidden */
	public static final String usernamePropertyName = "username";

	/** @hidden */
	public static final String loggedInUserIdPropertyName = "loggedInUserId";

	/** @hidden */
	public static final String exceptionTypePropertyName = "exceptionType";

	/** @hidden */
	public static final String exceptionMessagePropertyName = "exceptionMessage";

	/** @hidden */
	public static final String provenancePropertyName = "provenance";

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
	private Long threadID;

	/**
	 * Source IP
	 * <br/>
	 * Source IP when exception was raised
	 **/
	private String sourceIP;

	/**
	 * Username
	 * <br/>
	 * The username the security exception was raised for
	 **/
	private String username;

	/**
	 * Logged In User ID
	 * <br/>
	 * Logged in user ID when exception was raised
	 **/
	private String loggedInUserId;

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
	public Long getThreadID() {
		return threadID;
	}

	/**
	 * {@link #threadID} mutator.
	 * @param threadID	The new value.
	 **/
	@XmlElement
	public void setThreadID(Long threadID) {
		preset(threadIDPropertyName, threadID);
		this.threadID = threadID;
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
	 * {@link #username} accessor.
	 * @return	The value.
	 **/
	public String getUsername() {
		return username;
	}

	/**
	 * {@link #username} mutator.
	 * @param username	The new value.
	 **/
	@XmlElement
	public void setUsername(String username) {
		preset(usernamePropertyName, username);
		this.username = username;
	}

	/**
	 * {@link #loggedInUserId} accessor.
	 * @return	The value.
	 **/
	public String getLoggedInUserId() {
		return loggedInUserId;
	}

	/**
	 * {@link #loggedInUserId} mutator.
	 * @param loggedInUserId	The new value.
	 **/
	@XmlElement
	public void setLoggedInUserId(String loggedInUserId) {
		preset(loggedInUserIdPropertyName, loggedInUserId);
		this.loggedInUserId = loggedInUserId;
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
