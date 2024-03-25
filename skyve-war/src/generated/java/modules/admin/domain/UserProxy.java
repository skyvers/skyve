package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import modules.admin.UserProxy.UserProxyExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateTime;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.types.jaxb.DateTimeMapper;

/**
 * User
 * <br/>
 * A proxy version of the admin.User (without roles and groups etc) used for referencing.
 * 
 * @navhas n contact 1 Contact
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public abstract class UserProxy extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "UserProxy";

	/** @hidden */
	public static final String userNamePropertyName = "userName";

	/** @hidden */
	public static final String createdDateTimePropertyName = "createdDateTime";

	/** @hidden */
	public static final String contactPropertyName = "contact";

	/** @hidden */
	public static final String inactivePropertyName = "inactive";

	/**
	 * User Name
	 * <br/>
	 * Length is derived from the maximum email address length from RFC 5321
	 **/
	private String userName;

	/**
	 * Created
	 * <br/>
	 * The time and date when this user account was created.
	 **/
	private DateTime createdDateTime;

	/**
	 * Contact
	 * <br/>
	 * The contact details for the user.
	 **/
	private Contact contact = null;

	/**
	 * Inactive
	 * <br/>
	 * Flag to indicate that this account has been marked as inactive and no longer in use.
	 **/
	private Boolean inactive;

	@Override
	@XmlTransient
	public String getBizModule() {
		return UserProxy.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return UserProxy.DOCUMENT_NAME;
	}

	public static UserProxyExtension newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{userName} - {contact.bizKey}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof UserProxy) && 
					this.getBizId().equals(((UserProxy) o).getBizId()));
	}

	/**
	 * {@link #userName} accessor.
	 * @return	The value.
	 **/
	public String getUserName() {
		return userName;
	}

	/**
	 * {@link #userName} mutator.
	 * @param userName	The new value.
	 **/
	@XmlElement
	public void setUserName(String userName) {
		preset(userNamePropertyName, userName);
		this.userName = userName;
	}

	/**
	 * {@link #createdDateTime} accessor.
	 * @return	The value.
	 **/
	public DateTime getCreatedDateTime() {
		return createdDateTime;
	}

	/**
	 * {@link #createdDateTime} mutator.
	 * @param createdDateTime	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setCreatedDateTime(DateTime createdDateTime) {
		preset(createdDateTimePropertyName, createdDateTime);
		this.createdDateTime = createdDateTime;
	}

	/**
	 * {@link #contact} accessor.
	 * @return	The value.
	 **/
	public Contact getContact() {
		return contact;
	}

	/**
	 * {@link #contact} mutator.
	 * @param contact	The new value.
	 **/
	@XmlElement
	public void setContact(Contact contact) {
		if (this.contact != contact) {
			preset(contactPropertyName, contact);
			this.contact = contact;
		}
	}

	/**
	 * {@link #inactive} accessor.
	 * @return	The value.
	 **/
	public Boolean getInactive() {
		return inactive;
	}

	/**
	 * {@link #inactive} mutator.
	 * @param inactive	The new value.
	 **/
	@XmlElement
	public void setInactive(Boolean inactive) {
		preset(inactivePropertyName, inactive);
		this.inactive = inactive;
	}
}
