package modules.admin.domain;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Enumeration;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.wildcat.domain.AbstractPersistentBean;
import org.skyve.wildcat.domain.types.jaxb.DateTimeMapper;

/**
 * User
 * 
 * @depend - - - WizardState
 * @navcomposed 1 candidateContacts 0..n UserCandidateContact
 * @navcomposed 1 roles 0..n UserRole
 * @navhas n contact 1 Contact
 * @navhas n groups 0..n Group
 * @navhas n dataGroup 0..1 DataGroup
 * @stereotype "persistent"
 */
@XmlType
public class User extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "User";

	/** @hidden */
	public static final String userNamePropertyName = "userName";
	/** @hidden */
	public static final String passwordPropertyName = "password";
	/** @hidden */
	public static final String createdDateTimePropertyName = "createdDateTime";
	/** @hidden */
	public static final String homeModulePropertyName = "homeModule";
	/** @hidden */
	public static final String newPasswordPropertyName = "newPassword";
	/** @hidden */
	public static final String confirmPasswordPropertyName = "confirmPassword";
	/** @hidden */
	public static final String generatedPropertyName = "generated";
	/** @hidden */
	public static final String legacyIdPropertyName = "legacyId";
	/** @hidden */
	public static final String clearTextPasswordPropertyName = "clearTextPassword";
	/** @hidden */
	public static final String passwordExpiredPropertyName = "passwordExpired";
	/** @hidden */
	public static final String passwordLastChangedPropertyName = "passwordLastChanged";
	/** @hidden */
	public static final String contactPropertyName = "contact";
	/** @hidden */
	public static final String dataGroupPropertyName = "dataGroup";
	/** @hidden */
	public static final String groupsPropertyName = "groups";
	/** @hidden */
	public static final String rolesPropertyName = "roles";
	/** @hidden */
	public static final String wizardStatePropertyName = "wizardState";
	/** @hidden */
	public static final String searchContactNamePropertyName = "searchContactName";
	/** @hidden */
	public static final String searchEmailPropertyName = "searchEmail";
	/** @hidden */
	public static final String candidateContactsPropertyName = "candidateContacts";
	/** @hidden */
	public static final String contactSelectedPropertyName = "contactSelected";

	/**
	 * The create user wizard is staged into the following states which roughly follow in order.
			Either an existing contact is confirmed as that of the new user,
			OR
			A new contact is created for the new user.
			Once the identity of the new user is established, the wizard moves on
			to confirm the new user name and password and membership of groups.
	 **/
	@XmlEnum
	public static enum WizardState implements Enumeration {
		confirmContact("confirmContact", "confirmContact"),
		createContact("createContact", "createContact"),
		confirmUserNameAndPassword("confirmUserNameAndPassword", "confirmUserNameAndPassword"),
		confirmGroupMemberships("confirmGroupMemberships", "confirmGroupMemberships");

		private String code;
		private String description;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private WizardState(String code, String description) {
			this.code = code;
			this.description = description;
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toDescription() {
			return description;
		}

		public static WizardState fromCode(String code) {
			WizardState result = null;

			for (WizardState value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static WizardState fromDescription(String description) {
			WizardState result = null;

			for (WizardState value : values()) {
				if (value.description.equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				WizardState[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (WizardState value : values) {
					domainValues.add(new DomainValue(value.code, value.description));
				}
			}

			return domainValues;
		}
	}

	/**
	 * Login name of the user.
	 **/
	private String userName;
	/**
	 * Check Password Complexity settings for minimum required strength.
	 **/
	private String password;
	/**
	 * The time and date when this user account was created.
	 **/
	private DateTime createdDateTime;
	/**
	 * The module displayed when the user first logs in.
	 **/
	private String homeModule;
	/**
	 * Check Password Complexity settings for minimum required strength.
	 **/
	private String newPassword;
	/**
	 * Check Password Complexity settings for minimum required strength.
	 **/
	private String confirmPassword;
	/**
	 * Whether the User was generated or manually entered.
	 **/
	private Boolean generated;
	/**
	 * Legacy ID value when imported from legacy System using the conversion tool.
	 **/
	private String legacyId;
	/**
	 * The newly assigned password - this password must be changed at your next login.
	 **/
	private String clearTextPassword;
	/**
	 * Whether the password must be changed
	 **/
	private Boolean passwordExpired;
	/**
	 * Date and Time the users password was last changed
	 **/
	private DateTime passwordLastChanged;
	/**
	 * The contact details for the user.
	 **/
	private Contact contact = null;
	/**
	 * The group that constrains what information this user can see.
	 **/
	private DataGroup dataGroup = null;
	/**
	 * The collection of groups that this user belongs to.
	 **/
	private List<Group> groups = new ArrayList<>();
	/**
	 * Typically users are assigned membership of groups, which define sets of roles, 
			corresponding to business roles within an organisation.
			<br/>
			However user may also have specific roles assigned in addition to the roles
			which are implied from the groups to which they belong.
	 **/
	private List<UserRole> roles = new ArrayList<>();
	/**
	 * The create user wizard is staged into the following states which roughly follow in order.
			Either an existing contact is confirmed as that of the new user,
			OR
			A new contact is created for the new user.
			Once the identity of the new user is established, the wizard moves on
			to confirm the new user name and password and membership of groups.
	 **/
	private WizardState wizardState;
	/**
	 * This is used to determine if you are on the system already
	 **/
	private String searchContactName;
	/**
	 * The email address to use to search existing contacts.
	 **/
	private String searchEmail;
	/**
	 * The contacts who possibly match the search criteria.
	 **/
	private List<UserCandidateContact> candidateContacts = new ArrayList<>();
	private Boolean contactSelected = new Boolean(false);

	@Override
	@XmlTransient
	public String getBizModule() {
		return User.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return User.DOCUMENT_NAME;
	}

	@Override
	@XmlTransient
	public String getBizKey() {
return (getContact() == null) ?
					getUserName() : 
					getUserName() + " - " + getContact().getBizKey();
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof User) && 
					this.getBizId().equals(((User) o).getBizId()));
	}

	/**
	 * {@link #userName} accessor.
	 **/
	public String getUserName() {
		return userName;
	}

	/**
	 * {@link #userName} mutator.
	 * 
	 * @param userName	The new value to set.
	 **/
	@XmlElement
	public void setUserName(String userName) {
		preset(userNamePropertyName, userName);
		this.userName = userName;
	}

	/**
	 * {@link #password} accessor.
	 **/
	public String getPassword() {
		return password;
	}

	/**
	 * {@link #password} mutator.
	 * 
	 * @param password	The new value to set.
	 **/
	@XmlElement
	public void setPassword(String password) {
		preset(passwordPropertyName, password);
		this.password = password;
	}

	/**
	 * {@link #createdDateTime} accessor.
	 **/
	public DateTime getCreatedDateTime() {
		return createdDateTime;
	}

	/**
	 * {@link #createdDateTime} mutator.
	 * 
	 * @param createdDateTime	The new value to set.
	 **/
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	@XmlElement
	public void setCreatedDateTime(DateTime createdDateTime) {
		preset(createdDateTimePropertyName, createdDateTime);
		this.createdDateTime = createdDateTime;
	}

	/**
	 * {@link #homeModule} accessor.
	 **/
	public String getHomeModule() {
		return homeModule;
	}

	/**
	 * {@link #homeModule} mutator.
	 * 
	 * @param homeModule	The new value to set.
	 **/
	@XmlElement
	public void setHomeModule(String homeModule) {
		preset(homeModulePropertyName, homeModule);
		this.homeModule = homeModule;
	}

	/**
	 * {@link #newPassword} accessor.
	 **/
	public String getNewPassword() {
		return newPassword;
	}

	/**
	 * {@link #newPassword} mutator.
	 * 
	 * @param newPassword	The new value to set.
	 **/
	@XmlElement
	public void setNewPassword(String newPassword) {
		preset(newPasswordPropertyName, newPassword);
		this.newPassword = newPassword;
	}

	/**
	 * {@link #confirmPassword} accessor.
	 **/
	public String getConfirmPassword() {
		return confirmPassword;
	}

	/**
	 * {@link #confirmPassword} mutator.
	 * 
	 * @param confirmPassword	The new value to set.
	 **/
	@XmlElement
	public void setConfirmPassword(String confirmPassword) {
		preset(confirmPasswordPropertyName, confirmPassword);
		this.confirmPassword = confirmPassword;
	}

	/**
	 * {@link #generated} accessor.
	 **/
	public Boolean getGenerated() {
		return generated;
	}

	/**
	 * {@link #generated} mutator.
	 * 
	 * @param generated	The new value to set.
	 **/
	@XmlElement
	public void setGenerated(Boolean generated) {
		preset(generatedPropertyName, generated);
		this.generated = generated;
	}

	/**
	 * {@link #legacyId} accessor.
	 **/
	public String getLegacyId() {
		return legacyId;
	}

	/**
	 * {@link #legacyId} mutator.
	 * 
	 * @param legacyId	The new value to set.
	 **/
	@XmlElement
	public void setLegacyId(String legacyId) {
		preset(legacyIdPropertyName, legacyId);
		this.legacyId = legacyId;
	}

	/**
	 * {@link #clearTextPassword} accessor.
	 **/
	public String getClearTextPassword() {
		return clearTextPassword;
	}

	/**
	 * {@link #clearTextPassword} mutator.
	 * 
	 * @param clearTextPassword	The new value to set.
	 **/
	@XmlElement
	public void setClearTextPassword(String clearTextPassword) {
		preset(clearTextPasswordPropertyName, clearTextPassword);
		this.clearTextPassword = clearTextPassword;
	}

	/**
	 * {@link #passwordExpired} accessor.
	 **/
	public Boolean getPasswordExpired() {
		return passwordExpired;
	}

	/**
	 * {@link #passwordExpired} mutator.
	 * 
	 * @param passwordExpired	The new value to set.
	 **/
	@XmlElement
	public void setPasswordExpired(Boolean passwordExpired) {
		preset(passwordExpiredPropertyName, passwordExpired);
		this.passwordExpired = passwordExpired;
	}

	/**
	 * {@link #passwordLastChanged} accessor.
	 **/
	public DateTime getPasswordLastChanged() {
		return passwordLastChanged;
	}

	/**
	 * {@link #passwordLastChanged} mutator.
	 * 
	 * @param passwordLastChanged	The new value to set.
	 **/
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	@XmlElement
	public void setPasswordLastChanged(DateTime passwordLastChanged) {
		preset(passwordLastChangedPropertyName, passwordLastChanged);
		this.passwordLastChanged = passwordLastChanged;
	}

	/**
	 * {@link #contact} accessor.
	 **/
	public Contact getContact() {
		return contact;
	}

	/**
	 * {@link #contact} mutator.
	 * 
	 * @param contact	The new value to set.
	 **/
	@XmlElement
	public void setContact(Contact contact) {
		preset(contactPropertyName, contact);
		this.contact = contact;
	}

	/**
	 * {@link #dataGroup} accessor.
	 **/
	public DataGroup getDataGroup() {
		return dataGroup;
	}

	/**
	 * {@link #dataGroup} mutator.
	 * 
	 * @param dataGroup	The new value to set.
	 **/
	@XmlElement
	public void setDataGroup(DataGroup dataGroup) {
		preset(dataGroupPropertyName, dataGroup);
		this.dataGroup = dataGroup;
	}

	/**
	 * {@link #groups} accessor.
	 **/
	@XmlElement
	public List<Group> getGroups() {
		return groups;
	}

	/**
	 * {@link #groups} accessor.
	 * 
	 * @param bizId	The bizId of the element in the list.
	 **/
	public Group getGroupsElementById(String bizId) {
		return getElementById(groups, bizId);
	}

	/**
	 * {@link #groups} mutator.
	 * 
	 * @param bizId	The bizId of the element in the list.
	 * @param groups	The new value to set.
	 **/
	public void setGroupsElementById(@SuppressWarnings("unused") String bizId, Group element) {
		 setElementById(groups, element);
	}

	/**
	 * {@link #roles} accessor.
	 **/
	@XmlElement
	public List<UserRole> getRoles() {
		return roles;
	}

	/**
	 * {@link #roles} accessor.
	 * 
	 * @param bizId	The bizId of the element in the list.
	 **/
	public UserRole getRolesElementById(String bizId) {
		return getElementById(roles, bizId);
	}

	/**
	 * {@link #roles} mutator.
	 * 
	 * @param bizId	The bizId of the element in the list.
	 * @param roles	The new value to set.
	 **/
	public void setRolesElementById(@SuppressWarnings("unused") String bizId, UserRole element) {
		 setElementById(roles, element);
	}

	/**
	 * {@link #wizardState} accessor.
	 **/
	public WizardState getWizardState() {
		return wizardState;
	}

	/**
	 * {@link #wizardState} mutator.
	 * 
	 * @param wizardState	The new value to set.
	 **/
	@XmlElement
	public void setWizardState(WizardState wizardState) {
		preset(wizardStatePropertyName, wizardState);
		this.wizardState = wizardState;
	}

	/**
	 * {@link #searchContactName} accessor.
	 **/
	public String getSearchContactName() {
		return searchContactName;
	}

	/**
	 * {@link #searchContactName} mutator.
	 * 
	 * @param searchContactName	The new value to set.
	 **/
	@XmlElement
	public void setSearchContactName(String searchContactName) {
		preset(searchContactNamePropertyName, searchContactName);
		this.searchContactName = searchContactName;
	}

	/**
	 * {@link #searchEmail} accessor.
	 **/
	public String getSearchEmail() {
		return searchEmail;
	}

	/**
	 * {@link #searchEmail} mutator.
	 * 
	 * @param searchEmail	The new value to set.
	 **/
	@XmlElement
	public void setSearchEmail(String searchEmail) {
		preset(searchEmailPropertyName, searchEmail);
		this.searchEmail = searchEmail;
	}

	/**
	 * {@link #candidateContacts} accessor.
	 **/
	@XmlElement
	public List<UserCandidateContact> getCandidateContacts() {
		return candidateContacts;
	}

	/**
	 * {@link #candidateContacts} accessor.
	 * 
	 * @param bizId	The bizId of the element in the list.
	 **/
	public UserCandidateContact getCandidateContactsElementById(String bizId) {
		return getElementById(candidateContacts, bizId);
	}

	/**
	 * {@link #candidateContacts} mutator.
	 * 
	 * @param bizId	The bizId of the element in the list.
	 * @param candidateContacts	The new value to set.
	 **/
	public void setCandidateContactsElementById(@SuppressWarnings("unused") String bizId, UserCandidateContact element) {
		 setElementById(candidateContacts, element);
	}

	/**
	 * {@link #contactSelected} accessor.
	 **/
	public Boolean getContactSelected() {
		return contactSelected;
	}

	/**
	 * {@link #contactSelected} mutator.
	 * 
	 * @param contactSelected	The new value to set.
	 **/
	@XmlElement
	public void setContactSelected(Boolean contactSelected) {
		preset(contactSelectedPropertyName, contactSelected);
		this.contactSelected = contactSelected;
	}

	@XmlTransient
	public boolean isCandidateContactsEmpty() {
		return (candidateContacts.isEmpty());
	}

	public boolean isNotCandidateContactsEmpty() {
		return (! isCandidateContactsEmpty());
	}

	@XmlTransient
	public boolean isConfirmContact() {
		return (WizardState.confirmContact.equals(getWizardState()));
	}

	public boolean isNotConfirmContact() {
		return (! isConfirmContact());
	}

	@XmlTransient
	public boolean isConfirmGroupMemberships() {
		return (WizardState.confirmGroupMemberships.equals(getWizardState()));
	}

	public boolean isNotConfirmGroupMemberships() {
		return (! isConfirmGroupMemberships());
	}

	@XmlTransient
	public boolean isConfirmUserNameAndPassword() {
		return (WizardState.confirmUserNameAndPassword.equals(getWizardState()));
	}

	public boolean isNotConfirmUserNameAndPassword() {
		return (! isConfirmUserNameAndPassword());
	}

	@XmlTransient
	public boolean isCreateContact() {
		return (WizardState.createContact.equals(getWizardState()));
	}

	public boolean isNotCreateContact() {
		return (! isCreateContact());
	}

	@XmlTransient
	@Override
	public boolean isCreated() {
		return (isPersisted());
	}

	@Override
	public boolean isNotCreated() {
		return (! isCreated());
	}

	@XmlTransient
	public boolean isDesigner() {
		return (isUserInRole("design", "BizHubDesigner"));
	}

	public boolean isNotDesigner() {
		return (! isDesigner());
	}

	@XmlTransient
	public boolean isInDataGroup() {
		return (! isUserInDataGroup(null));
	}

	public boolean isNotInDataGroup() {
		return (! isInDataGroup());
	}

	@XmlTransient
	public boolean isSecurityAdministrator() {
		return (isUserInRole("admin","SecurityAdministrator"));
	}

	public boolean isNotSecurityAdministrator() {
		return (! isSecurityAdministrator());
	}

	@XmlTransient
	public boolean isShowNextButton() {
		return (isCreateContact() || isConfirmUserNameAndPassword());
	}

	public boolean isNotShowNextButton() {
		return (! isShowNextButton());
	}
}
