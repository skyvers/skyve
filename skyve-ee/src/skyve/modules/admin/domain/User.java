package modules.admin.domain;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.skyve.CORE;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.types.jaxb.DateTimeMapper;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

/**
 * User
 * 
 * @depend - - - WizardState
 * @navhas n dataGroup 0..1 DataGroup
 * @navhas n contact 1 Contact
 * @navcomposed 1 roles 0..n UserRole
 * @navhas n groups 0..n Group
 * @navcomposed 1 candidateContacts 0..n UserCandidateContact
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
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
	public static final String generatedPasswordPropertyName = "generatedPassword";
	/** @hidden */
	public static final String createdDateTimePropertyName = "createdDateTime";
	/** @hidden */
	public static final String homeModulePropertyName = "homeModule";
	/** @hidden */
	public static final String newPasswordPropertyName = "newPassword";
	/** @hidden */
	public static final String confirmPasswordPropertyName = "confirmPassword";
	/** @hidden */
	public static final String legacyIdPropertyName = "legacyId";
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
	/** @hidden */
	public static final String inactivePropertyName = "inactive";

	/**
	 * Wizard State
	 * <br/>
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
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private WizardState(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toDescription() {
			return description;
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
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
					domainValues.add(value.domainValue);
				}
			}

			return domainValues;
		}
	}

	/**
	 * User Name
	 * <br/>
	 * Length is derived from the maximum email address length from RFC 5321
	 **/
	private String userName;
	/**
	 * Password
	 * <br/>
	 * Check Password Complexity settings for minimum required strength.
	 **/
	private String password;
	/**
	 * Generated Password
	 * <br/>
	 * Used to temporarily hold generated passwords for further processing.
	 **/
	private String generatedPassword;
	/**
	 * Created
	 * <br/>
	 * The time and date when this user account was created.
	 **/
	private DateTime createdDateTime;
	/**
	 * Home Module
	 * <br/>
	 * The module displayed when the user first logs in.
	 **/
	private String homeModule;
	/**
	 * New Password
	 * <br/>
	 * Check Password Complexity settings for minimum required strength.
	 **/
	private String newPassword;
	/**
	 * Confirm Password
	 * <br/>
	 * Check Password Complexity settings for minimum required strength.
	 **/
	private String confirmPassword;
	/**
	 * Legacy Id
	 * <br/>
	 * Legacy ID value when imported from legacy System using the conversion tool.
	 **/
	private String legacyId;
	/**
	 * Must change password
	 * <br/>
	 * Whether the password must be changed
	 **/
	private Boolean passwordExpired;
	/**
	 * Last changed
	 * <br/>
	 * Date and Time the users password was last changed
	 **/
	private DateTime passwordLastChanged;
	/**
	 * Contact
	 * <br/>
	 * The contact details for the user.
	 **/
	private Contact contact = null;
	/**
	 * Data Group
	 * <br/>
	 * The group that constrains what information this user can see.
	 **/
	private DataGroup dataGroup = null;
	/**
	 * Groups
	 * <br/>
	 * The collection of groups that this user belongs to.
	 **/
	private List<Group> groups = new ArrayList<>();
	/**
	 * Roles
	 * <br/>
	 * Typically users are assigned membership of groups, which define sets of roles, 
			corresponding to business roles within an organisation.
			<br/>
			However user may also have specific roles assigned in addition to the roles
			which are implied from the groups to which they belong.
	 **/
	private List<UserRole> roles = new ArrayList<>();
	/**
	 * Wizard State
	 * <br/>
	 * The create user wizard is staged into the following states which roughly follow in order.
			Either an existing contact is confirmed as that of the new user,
			OR
			A new contact is created for the new user.
			Once the identity of the new user is established, the wizard moves on
			to confirm the new user name and password and membership of groups.
	 **/
	private WizardState wizardState;
	/**
	 * Full name
	 * <br/>
	 * This is used to determine if you are on the system already
	 **/
	private String searchContactName;
	/**
	 * Email
	 * <br/>
	 * The email address to use to search existing contacts.
	 **/
	private String searchEmail;
	/**
	 * Candidate Contacts
	 * <br/>
	 * The contacts who possibly match the search criteria.
	 **/
	private List<UserCandidateContact> candidateContacts = new ArrayList<>();
	/**
	 * The contact selected for this user.
	 **/
	private Boolean contactSelected = new Boolean(false);
	/**
	 * Inactive
	 **/
	private Boolean inactive;

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

	public static User newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	@XmlTransient
	public String getBizKey() {
return modules.admin.User.UserBizlet.bizKey(this);
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof User) && 
					this.getBizId().equals(((User) o).getBizId()));
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
	 * {@link #password} accessor.
	 * @return	The value.
	 **/
	public String getPassword() {
		return password;
	}

	/**
	 * {@link #password} mutator.
	 * @param password	The new value.
	 **/
	@XmlElement
	public void setPassword(String password) {
		preset(passwordPropertyName, password);
		this.password = password;
	}

	/**
	 * {@link #generatedPassword} accessor.
	 * @return	The value.
	 **/
	public String getGeneratedPassword() {
		return generatedPassword;
	}

	/**
	 * {@link #generatedPassword} mutator.
	 * @param generatedPassword	The new value.
	 **/
	@XmlElement
	public void setGeneratedPassword(String generatedPassword) {
		this.generatedPassword = generatedPassword;
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
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	@XmlElement
	public void setCreatedDateTime(DateTime createdDateTime) {
		preset(createdDateTimePropertyName, createdDateTime);
		this.createdDateTime = createdDateTime;
	}

	/**
	 * {@link #homeModule} accessor.
	 * @return	The value.
	 **/
	public String getHomeModule() {
		return homeModule;
	}

	/**
	 * {@link #homeModule} mutator.
	 * @param homeModule	The new value.
	 **/
	@XmlElement
	public void setHomeModule(String homeModule) {
		preset(homeModulePropertyName, homeModule);
		this.homeModule = homeModule;
	}

	/**
	 * {@link #newPassword} accessor.
	 * @return	The value.
	 **/
	public String getNewPassword() {
		return newPassword;
	}

	/**
	 * {@link #newPassword} mutator.
	 * @param newPassword	The new value.
	 **/
	@XmlElement
	public void setNewPassword(String newPassword) {
		preset(newPasswordPropertyName, newPassword);
		this.newPassword = newPassword;
	}

	/**
	 * {@link #confirmPassword} accessor.
	 * @return	The value.
	 **/
	public String getConfirmPassword() {
		return confirmPassword;
	}

	/**
	 * {@link #confirmPassword} mutator.
	 * @param confirmPassword	The new value.
	 **/
	@XmlElement
	public void setConfirmPassword(String confirmPassword) {
		preset(confirmPasswordPropertyName, confirmPassword);
		this.confirmPassword = confirmPassword;
	}

	/**
	 * {@link #legacyId} accessor.
	 * @return	The value.
	 **/
	public String getLegacyId() {
		return legacyId;
	}

	/**
	 * {@link #legacyId} mutator.
	 * @param legacyId	The new value.
	 **/
	@XmlElement
	public void setLegacyId(String legacyId) {
		preset(legacyIdPropertyName, legacyId);
		this.legacyId = legacyId;
	}

	/**
	 * {@link #passwordExpired} accessor.
	 * @return	The value.
	 **/
	public Boolean getPasswordExpired() {
		return passwordExpired;
	}

	/**
	 * {@link #passwordExpired} mutator.
	 * @param passwordExpired	The new value.
	 **/
	@XmlElement
	public void setPasswordExpired(Boolean passwordExpired) {
		preset(passwordExpiredPropertyName, passwordExpired);
		this.passwordExpired = passwordExpired;
	}

	/**
	 * {@link #passwordLastChanged} accessor.
	 * @return	The value.
	 **/
	public DateTime getPasswordLastChanged() {
		return passwordLastChanged;
	}

	/**
	 * {@link #passwordLastChanged} mutator.
	 * @param passwordLastChanged	The new value.
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
	 * {@link #dataGroup} accessor.
	 * @return	The value.
	 **/
	public DataGroup getDataGroup() {
		return dataGroup;
	}

	/**
	 * {@link #dataGroup} mutator.
	 * @param dataGroup	The new value.
	 **/
	@XmlElement
	public void setDataGroup(DataGroup dataGroup) {
		preset(dataGroupPropertyName, dataGroup);
		this.dataGroup = dataGroup;
	}

	/**
	 * {@link #groups} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<Group> getGroups() {
		return groups;
	}

	/**
	 * {@link #groups} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public Group getGroupsElementById(String bizId) {
		return getElementById(groups, bizId);
	}

	/**
	 * {@link #groups} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setGroupsElementById(@SuppressWarnings("unused") String bizId, Group element) {
		 setElementById(groups, element);
	}

	/**
	 * {@link #roles} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<UserRole> getRoles() {
		return roles;
	}

	/**
	 * {@link #roles} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public UserRole getRolesElementById(String bizId) {
		return getElementById(roles, bizId);
	}

	/**
	 * {@link #roles} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setRolesElementById(@SuppressWarnings("unused") String bizId, UserRole element) {
		 setElementById(roles, element);
	}

	/**
	 * {@link #wizardState} accessor.
	 * @return	The value.
	 **/
	public WizardState getWizardState() {
		return wizardState;
	}

	/**
	 * {@link #wizardState} mutator.
	 * @param wizardState	The new value.
	 **/
	@XmlElement
	public void setWizardState(WizardState wizardState) {
		preset(wizardStatePropertyName, wizardState);
		this.wizardState = wizardState;
	}

	/**
	 * {@link #searchContactName} accessor.
	 * @return	The value.
	 **/
	public String getSearchContactName() {
		return searchContactName;
	}

	/**
	 * {@link #searchContactName} mutator.
	 * @param searchContactName	The new value.
	 **/
	@XmlElement
	public void setSearchContactName(String searchContactName) {
		preset(searchContactNamePropertyName, searchContactName);
		this.searchContactName = searchContactName;
	}

	/**
	 * {@link #searchEmail} accessor.
	 * @return	The value.
	 **/
	public String getSearchEmail() {
		return searchEmail;
	}

	/**
	 * {@link #searchEmail} mutator.
	 * @param searchEmail	The new value.
	 **/
	@XmlElement
	public void setSearchEmail(String searchEmail) {
		preset(searchEmailPropertyName, searchEmail);
		this.searchEmail = searchEmail;
	}

	/**
	 * {@link #candidateContacts} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<UserCandidateContact> getCandidateContacts() {
		return candidateContacts;
	}

	/**
	 * {@link #candidateContacts} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public UserCandidateContact getCandidateContactsElementById(String bizId) {
		return getElementById(candidateContacts, bizId);
	}

	/**
	 * {@link #candidateContacts} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setCandidateContactsElementById(@SuppressWarnings("unused") String bizId, UserCandidateContact element) {
		 setElementById(candidateContacts, element);
	}

	/**
	 * {@link #contactSelected} accessor.
	 * @return	The value.
	 **/
	public Boolean getContactSelected() {
		return contactSelected;
	}

	/**
	 * {@link #contactSelected} mutator.
	 * @param contactSelected	The new value.
	 **/
	@XmlElement
	public void setContactSelected(Boolean contactSelected) {
		preset(contactSelectedPropertyName, contactSelected);
		this.contactSelected = contactSelected;
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

	/**
	 * Candidate Contacts is empty
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isCandidateContactsEmpty() {
		return (candidateContacts.isEmpty());
	}

	/**	 * {@link #isCandidateContactsEmpty} negation.

	 * @return	The negated condition

	 */
	public boolean isNotCandidateContactsEmpty() {
		return (! isCandidateContactsEmpty());
	}

	/**
	 * Confirm Contact step
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isConfirmContact() {
		return (WizardState.confirmContact.equals(getWizardState()));
	}

	/**	 * {@link #isConfirmContact} negation.

	 * @return	The negated condition

	 */
	public boolean isNotConfirmContact() {
		return (! isConfirmContact());
	}

	/**
	 * Confirm Group Memberships step
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isConfirmGroupMemberships() {
		return (WizardState.confirmGroupMemberships.equals(getWizardState()));
	}

	/**	 * {@link #isConfirmGroupMemberships} negation.

	 * @return	The negated condition

	 */
	public boolean isNotConfirmGroupMemberships() {
		return (! isConfirmGroupMemberships());
	}

	/**
	 * Confirm User Name and Password step
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isConfirmUserNameAndPassword() {
		return (WizardState.confirmUserNameAndPassword.equals(getWizardState()));
	}

	/**	 * {@link #isConfirmUserNameAndPassword} negation.

	 * @return	The negated condition

	 */
	public boolean isNotConfirmUserNameAndPassword() {
		return (! isConfirmUserNameAndPassword());
	}

	/**
	 * Create Contact step
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isCreateContact() {
		return (WizardState.createContact.equals(getWizardState()));
	}

	/**	 * {@link #isCreateContact} negation.

	 * @return	The negated condition

	 */
	public boolean isNotCreateContact() {
		return (! isCreateContact());
	}

	/**
	 * Created
	 * @return	The condition

	 */
	@XmlTransient
	@Override
	public boolean isCreated() {
		return (isPersisted());
	}

	/**	 * {@link #isCreated} negation.

	 * @return	The negated condition

	 */
	@Override
	public boolean isNotCreated() {
		return (! isCreated());
	}

	/**
	 * Designer
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isDesigner() {
		return (isUserInRole("design", "BizHubDesigner"));
	}

	/**	 * {@link #isDesigner} negation.

	 * @return	The negated condition

	 */
	public boolean isNotDesigner() {
		return (! isDesigner());
	}

	/**
	 * In Data Group
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isInDataGroup() {
		return (! isUserInDataGroup(null));
	}

	/**	 * {@link #isInDataGroup} negation.

	 * @return	The negated condition

	 */
	public boolean isNotInDataGroup() {
		return (! isInDataGroup());
	}

	/**
	 * Security Administrator
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isSecurityAdministrator() {
		return (isUserInRole("admin","SecurityAdministrator"));
	}

	/**	 * {@link #isSecurityAdministrator} negation.

	 * @return	The negated condition

	 */
	public boolean isNotSecurityAdministrator() {
		return (! isSecurityAdministrator());
	}

	/**
	 * Show Next Button
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isShowNextButton() {
		return (isCreateContact() || isConfirmUserNameAndPassword());
	}

	/**	 * {@link #isShowNextButton} negation.

	 * @return	The negated condition

	 */
	public boolean isNotShowNextButton() {
		return (! isShowNextButton());
	}
}
