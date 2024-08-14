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
 * The Security Log consists of a list of records of security events in the system, so that intrusions can be detected in near real time.
			<br/>
			A SecurityLog instance is created automatically for every {@link AccessException} or {@link SecurityException} thrown.
			<br/>
			A SecurityLog instance can be manually created via <code>log</code> method in {@link SecurityUtil}.
			<br/>
			If email is configured, emails are sent to the support email address specified in the project JSON for each event raised.
 * 
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
	public static final String threadIdPropertyName = "threadId";

	/** @hidden */
	public static final String threadNamePropertyName = "threadName";

	/** @hidden */
	public static final String sourceIPPropertyName = "sourceIP";

	/** @hidden */
	public static final String usernamePropertyName = "username";

	/** @hidden */
	public static final String loggedInUserIdPropertyName = "loggedInUserId";

	/** @hidden */
	public static final String eventTypePropertyName = "eventType";

	/** @hidden */
	public static final String eventMessagePropertyName = "eventMessage";

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
	private Long threadId;

	/**
	 * Thread Name
	 * <br/>
	 * Thread name when the exception was raised
	 **/
	private String threadName;

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
	 * Event Type
	 * <br/>
	 * Type of security event raised. If raised with an exception, this is the exception name.
	 **/
	private String eventType;

	/**
	 * Event Message
	 * <br/>
	 * Details about the security event. If an exception, this is the message from raised exception
	 **/
	private String eventMessage;

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
	 * {@link #threadId} accessor.
	 * @return	The value.
	 **/
	public Long getThreadId() {
		return threadId;
	}

	/**
	 * {@link #threadId} mutator.
	 * @param threadId	The new value.
	 **/
	@XmlElement
	public void setThreadId(Long threadId) {
		preset(threadIdPropertyName, threadId);
		this.threadId = threadId;
	}

	/**
	 * {@link #threadName} accessor.
	 * @return	The value.
	 **/
	public String getThreadName() {
		return threadName;
	}

	/**
	 * {@link #threadName} mutator.
	 * @param threadName	The new value.
	 **/
	@XmlElement
	public void setThreadName(String threadName) {
		preset(threadNamePropertyName, threadName);
		this.threadName = threadName;
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
	 * {@link #eventType} accessor.
	 * @return	The value.
	 **/
	public String getEventType() {
		return eventType;
	}

	/**
	 * {@link #eventType} mutator.
	 * @param eventType	The new value.
	 **/
	@XmlElement
	public void setEventType(String eventType) {
		preset(eventTypePropertyName, eventType);
		this.eventType = eventType;
	}

	/**
	 * {@link #eventMessage} accessor.
	 * @return	The value.
	 **/
	public String getEventMessage() {
		return eventMessage;
	}

	/**
	 * {@link #eventMessage} mutator.
	 * @param eventMessage	The new value.
	 **/
	@XmlElement
	public void setEventMessage(String eventMessage) {
		preset(eventMessagePropertyName, eventMessage);
		this.eventMessage = eventMessage;
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
