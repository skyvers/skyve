package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.wildcat.domain.AbstractTransientBean;

/**
 * MyContact
 * 
 * @navhas n myContact 0..1 Contact
 * @stereotype "transient"
 */
@XmlType
public class MyContact extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "MyContact";

	/** @hidden */
	public static final String myContactPropertyName = "myContact";

	private Contact myContact = null;

	@Override
	@XmlTransient
	public String getBizModule() {
		return MyContact.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return MyContact.DOCUMENT_NAME;
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof MyContact) && 
					this.getBizId().equals(((MyContact) o).getBizId()));
	}

	/**
	 * {@link #myContact} accessor.
	 **/
	public Contact getMyContact() {
		return myContact;
	}

	/**
	 * {@link #myContact} mutator.
	 **/
	@XmlElement
	public void setMyContact(Contact myContact) {
		preset(myContactPropertyName, myContact);
		this.myContact = myContact;
	}

	@XmlTransient
	public boolean isExists() {
		return (getMyContact()!=null);
	}

	public boolean isNotExists() {
		return (! isExists());
	}
}
