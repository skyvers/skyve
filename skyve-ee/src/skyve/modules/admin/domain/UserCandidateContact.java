package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import modules.admin.domain.User;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
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
public class UserCandidateContact extends AbstractTransientBean implements ChildBean<User> {
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
	 * Contact
	 **/
	private Contact contact = null;
	/**
	 * Score
	 **/
	private Integer matchScore;
	private User parent;

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

	public static UserCandidateContact newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
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
		preset(contactPropertyName, contact);
		this.contact = contact;
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
	public User getParent() {
		return parent;
	}

	@Override
	@XmlElement
	public void setParent(User parent) {
		preset(ChildBean.PARENT_NAME, parent);
		this.parent =  parent;
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
