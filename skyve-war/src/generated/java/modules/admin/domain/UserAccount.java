package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlEnum;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

/**
 * User Account
 * <br/>
 * Manages user sessions and two-factor authentication preferences.
 * 
 * @depend - - - SecondFactorPreferredMethod
 * @navcomposed n sessions 0..n Generic
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class UserAccount extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "UserAccount";

	/** @hidden */
	public static final String sessionsPropertyName = "sessions";

	/** @hidden */
	public static final String secondFactorPreferredMethodPropertyName = "secondFactorPreferredMethod";

	/**
	 * Second Factor Preferred Method
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum SecondFactorPreferredMethod implements Enumeration {
		authenticator("A", "Authenticator"),
		email("E", "Email"),
		SMSTextMessage("T", "SMS/Text message");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(SecondFactorPreferredMethod::toDomainValue).collect(Collectors.toUnmodifiableList());

		private SecondFactorPreferredMethod(String code, String description) {
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

		public static SecondFactorPreferredMethod fromCode(String code) {
			SecondFactorPreferredMethod result = null;

			for (SecondFactorPreferredMethod value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static SecondFactorPreferredMethod fromLocalisedDescription(String description) {
			SecondFactorPreferredMethod result = null;

			for (SecondFactorPreferredMethod value : values()) {
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
	 * Sessions
	 **/
	private List<Generic> sessions = new ChangeTrackingArrayList<>("sessions", this);

	/**
	 * Second Factor Preferred Method
	 **/
	private SecondFactorPreferredMethod secondFactorPreferredMethod;

	@Override
	@XmlTransient
	public String getBizModule() {
		return UserAccount.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return UserAccount.DOCUMENT_NAME;
	}

	public static UserAccount newInstance() {
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
			return org.skyve.util.Binder.formatMessage("User Account", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	/**
	 * {@link #sessions} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<Generic> getSessions() {
		return sessions;
	}

	/**
	 * {@link #sessions} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public Generic getSessionsElementById(String bizId) {
		return getElementById(sessions, bizId);
	}

	/**
	 * {@link #sessions} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setSessionsElementById(String bizId, Generic element) {
		setElementById(sessions, element);
	}

	/**
	 * {@link #sessions} add.
	 * @param element	The element to add.
	 **/
	public boolean addSessionsElement(Generic element) {
		return sessions.add(element);
	}

	/**
	 * {@link #sessions} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addSessionsElement(int index, Generic element) {
		sessions.add(index, element);
	}

	/**
	 * {@link #sessions} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeSessionsElement(Generic element) {
		return sessions.remove(element);
	}

	/**
	 * {@link #sessions} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public Generic removeSessionsElement(int index) {
		return sessions.remove(index);
	}

	/**
	 * {@link #secondFactorPreferredMethod} accessor.
	 * @return	The value.
	 **/
	public SecondFactorPreferredMethod getSecondFactorPreferredMethod() {
		return secondFactorPreferredMethod;
	}

	/**
	 * {@link #secondFactorPreferredMethod} mutator.
	 * @param secondFactorPreferredMethod	The new value.
	 **/
	@XmlElement
	public void setSecondFactorPreferredMethod(SecondFactorPreferredMethod secondFactorPreferredMethod) {
		preset(secondFactorPreferredMethodPropertyName, secondFactorPreferredMethod);
		this.secondFactorPreferredMethod = secondFactorPreferredMethod;
	}
}
