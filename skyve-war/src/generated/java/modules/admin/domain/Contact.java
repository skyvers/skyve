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
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

/**
 * Contact
 * 
 * @depend - - - ContactType
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class Contact extends AbstractPersistentBean implements org.skyve.domain.app.admin.Contact {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "Contact";

	/** @hidden */
	public static final String namePropertyName = "name";

	/** @hidden */
	public static final String contactTypePropertyName = "contactType";

	/** @hidden */
	public static final String email1PropertyName = "email1";

	/** @hidden */
	public static final String mobilePropertyName = "mobile";

	/** @hidden */
	public static final String imagePropertyName = "image";

	/**
	 * Contact Type
	 * <br/>
	 * Whether this contact is a person or an organisation.
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum ContactType implements Enumeration {
		person("Person", "Person"),
		organisation("Organisation", "Organisation");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(ContactType::toDomainValue).collect(Collectors.toUnmodifiableList());

		private ContactType(String code, String description) {
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

		public static ContactType fromCode(String code) {
			ContactType result = null;

			for (ContactType value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static ContactType fromLocalisedDescription(String description) {
			ContactType result = null;

			for (ContactType value : values()) {
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
	 * Name
	 **/
	private String name;

	/**
	 * Contact Type
	 * <br/>
	 * Whether this contact is a person or an organisation.
	 **/
	private ContactType contactType;

	/**
	 * Email
	 **/
	private String email1;

	/**
	 * Mobile
	 **/
	private String mobile;

	/**
	 * Image
	 **/
	private String image;

	@Override
	@XmlTransient
	public String getBizModule() {
		return Contact.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Contact.DOCUMENT_NAME;
	}

	public static Contact newInstance() {
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
return modules.admin.Contact.ContactBizlet.bizKey(this);
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Contact) && 
					this.getBizId().equals(((Contact) o).getBizId()));
	}

	/**
	 * {@link #name} accessor.
	 * @return	The value.
	 **/
	public String getName() {
		return name;
	}

	/**
	 * {@link #name} mutator.
	 * @param name	The new value.
	 **/
	@XmlElement
	public void setName(String name) {
		preset(namePropertyName, name);
		this.name = name;
	}

	/**
	 * {@link #contactType} accessor.
	 * @return	The value.
	 **/
	public ContactType getContactType() {
		return contactType;
	}

	/**
	 * {@link #contactType} mutator.
	 * @param contactType	The new value.
	 **/
	@XmlElement
	public void setContactType(ContactType contactType) {
		preset(contactTypePropertyName, contactType);
		this.contactType = contactType;
	}

	/**
	 * {@link #email1} accessor.
	 * @return	The value.
	 **/
	public String getEmail1() {
		return email1;
	}

	/**
	 * {@link #email1} mutator.
	 * @param email1	The new value.
	 **/
	@XmlElement
	public void setEmail1(String email1) {
		preset(email1PropertyName, email1);
		this.email1 = email1;
	}

	/**
	 * {@link #mobile} accessor.
	 * @return	The value.
	 **/
	public String getMobile() {
		return mobile;
	}

	/**
	 * {@link #mobile} mutator.
	 * @param mobile	The new value.
	 **/
	@XmlElement
	public void setMobile(String mobile) {
		preset(mobilePropertyName, mobile);
		this.mobile = mobile;
	}

	/**
	 * {@link #image} accessor.
	 * @return	The value.
	 **/
	public String getImage() {
		return image;
	}

	/**
	 * {@link #image} mutator.
	 * @param image	The new value.
	 **/
	@XmlElement
	public void setImage(String image) {
		preset(imagePropertyName, image);
		this.image = image;
	}
}
