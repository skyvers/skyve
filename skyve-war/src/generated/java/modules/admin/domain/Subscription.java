package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import modules.admin.Communication.CommunicationExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;

/**
 * Subscription
 * <br/>
 * A subscription Preference models the receiver's preference about how they wish to receive the communication type (or not).

		If the subscription is declined, the format type is not required. If the format type is supplied, then the communication
		is only declined for that format.
		
		If the subscription is not declined, the format type is required, as this specifies the format preference for the communication.		
		
		If the subscription is not declined and has no formatType, the subscription can be deleted as it holds no value.
		
		Subscriptions are user-scoped - The assumption is that it is up to the user whether they wish to 
		accept or decline receiving communications and in what manner they are delivered.
 * 
 * @navhas n communication 1 Communication
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class Subscription extends AbstractPersistentBean implements org.skyve.domain.app.admin.Subscription {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "Subscription";

	/** @hidden */
	public static final String communicationPropertyName = "communication";

	/** @hidden */
	public static final String receiverIdentifierPropertyName = "receiverIdentifier";

	/** @hidden */
	public static final String declinedPropertyName = "declined";

	/** @hidden */
	public static final String preferredReceiverIdentifierPropertyName = "preferredReceiverIdentifier";

	/**
	 * Communication
	 **/
	private CommunicationExtension communication = null;

	/**
	 * Receiver
	 * <br/>
	 * This could be an email or sms number, or any other identifier for a delivery method
	 **/
	private String receiverIdentifier;

	/**
	 * Declined
	 **/
	private Boolean declined;

	/**
	 * Redirect to
	 * <br/>
	 * This could be an email or sms number, or any other identifier for a delivery method
	 **/
	private String preferredReceiverIdentifier;

	@Override
	@XmlTransient
	public String getBizModule() {
		return Subscription.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Subscription.DOCUMENT_NAME;
	}

	public static Subscription newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{communication.description} for {receiverIdentifier}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Subscription) && 
					this.getBizId().equals(((Subscription) o).getBizId()));
	}

	/**
	 * {@link #communication} accessor.
	 * @return	The value.
	 **/
	public CommunicationExtension getCommunication() {
		return communication;
	}

	/**
	 * {@link #communication} mutator.
	 * @param communication	The new value.
	 **/
	@XmlElement
	public void setCommunication(CommunicationExtension communication) {
		if (this.communication != communication) {
			preset(communicationPropertyName, communication);
			this.communication = communication;
		}
	}

	/**
	 * {@link #receiverIdentifier} accessor.
	 * @return	The value.
	 **/
	public String getReceiverIdentifier() {
		return receiverIdentifier;
	}

	/**
	 * {@link #receiverIdentifier} mutator.
	 * @param receiverIdentifier	The new value.
	 **/
	@XmlElement
	public void setReceiverIdentifier(String receiverIdentifier) {
		preset(receiverIdentifierPropertyName, receiverIdentifier);
		this.receiverIdentifier = receiverIdentifier;
	}

	/**
	 * {@link #declined} accessor.
	 * @return	The value.
	 **/
	public Boolean getDeclined() {
		return declined;
	}

	/**
	 * {@link #declined} mutator.
	 * @param declined	The new value.
	 **/
	@XmlElement
	public void setDeclined(Boolean declined) {
		preset(declinedPropertyName, declined);
		this.declined = declined;
	}

	/**
	 * {@link #preferredReceiverIdentifier} accessor.
	 * @return	The value.
	 **/
	public String getPreferredReceiverIdentifier() {
		return preferredReceiverIdentifier;
	}

	/**
	 * {@link #preferredReceiverIdentifier} mutator.
	 * @param preferredReceiverIdentifier	The new value.
	 **/
	@XmlElement
	public void setPreferredReceiverIdentifier(String preferredReceiverIdentifier) {
		preset(preferredReceiverIdentifierPropertyName, preferredReceiverIdentifier);
		this.preferredReceiverIdentifier = preferredReceiverIdentifier;
	}
}
