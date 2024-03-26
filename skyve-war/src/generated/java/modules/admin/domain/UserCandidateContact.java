package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import modules.admin.User.UserExtension;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractTransientBean;

/**
 * User Candidate Contact
 * <br/>
 * User Candidate Contact is a transient child of User, and holds the potential matching contacts
			when a search is conducted during the Create User wizard.
			<br/>
			When creating a new user, the wizard offers the opportunity to establish if the new user account
			corresponds to an existing contact, via a basic search facility (name and/or email).
			<br/>
			Possible (i.e. candidate) matches (and their match scores) are presented via the wizard for selection
			or alternatively, a new contact is created if required.
 * 
 * @navhas n contact 0..1 Contact
 * @stereotype "transient child"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class UserCandidateContact extends AbstractTransientBean implements ChildBean<UserExtension> {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "UserCandidateContact";

	/** @hidden */
	public static final String contactPropertyName = "contact";

	/** @hidden */
	public static final String matchScorePropertyName = "matchScore";

	/**
	 * admin.userCandidateContact.assocation.contact.displayName
	 **/
	private Contact contact = null;

	/**
	 * Score
	 **/
	private Integer matchScore;

	private UserExtension parent;

	private Integer bizOrdinal;

	@Override
	@XmlTransient
	public String getBizModule() {
		return UserCandidateContact.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return UserCandidateContact.DOCUMENT_NAME;
	}

	public static UserCandidateContact newInstance() {
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
			return org.skyve.util.Binder.formatMessage("Contact {contact.bizKey}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof UserCandidateContact) && 
					this.getBizId().equals(((UserCandidateContact) o).getBizId()));
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
	 * {@link #matchScore} accessor.
	 * @return	The value.
	 **/
	public Integer getMatchScore() {
		return matchScore;
	}

	/**
	 * {@link #matchScore} mutator.
	 * @param matchScore	The new value.
	 **/
	@XmlElement
	public void setMatchScore(Integer matchScore) {
		preset(matchScorePropertyName, matchScore);
		this.matchScore = matchScore;
	}

	@Override
	public UserExtension getParent() {
		return parent;
	}

	@Override
	@XmlElement
	public void setParent(UserExtension parent) {
		if (this.parent != parent) {
			preset(ChildBean.PARENT_NAME, parent);
			this.parent = parent;
		}
	}

	@Override
	public Integer getBizOrdinal() {
		return bizOrdinal;
	}

	@Override
	@XmlElement
	public void setBizOrdinal(Integer bizOrdinal) {
		preset(Bean.ORDINAL_NAME, bizOrdinal);
		this.bizOrdinal =  bizOrdinal;
	}
}
