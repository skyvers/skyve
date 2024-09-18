package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlEnum;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import modules.admin.Group.GroupExtension;
import modules.admin.User.UserExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Enumeration;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;
import org.skyve.impl.domain.types.jaxb.DateTimeMapper;
import org.skyve.impl.domain.types.jaxb.TimestampMapper;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

/**
 * User
 * 
 * @depend - - - WizardState
 * @depend - - - GroupSelection
 * @navhas n dataGroup 0..1 DataGroup
 * @navhas n assignedRoles 0..n UserRole
 * @navhas n contact 1 Contact
 * @navcomposed 1 roles 0..n UserRole
 * @navhas n groups 0..n Group
 * @navhas n newGroup 0..1 Group
 * @navcomposed 1 candidateContacts 0..n UserCandidateContact
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public abstract class User extends AbstractPersistentBean implements org.skyve.domain.app.admin.User {
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
	public static final String passwordLastChangedIPPropertyName = "passwordLastChangedIP";

	/** @hidden */
	public static final String passwordLastChangedCountryCodePropertyName = "passwordLastChangedCountryCode";

	/** @hidden */
	public static final String passwordLastChangedCountryNamePropertyName = "passwordLastChangedCountryName";

	/** @hidden */
	public static final String passwordResetTokenPropertyName = "passwordResetToken";

	/** @hidden */
	public static final String passwordResetTokenCreationTimestampPropertyName = "passwordResetTokenCreationTimestamp";

	/** @hidden */
	public static final String passwordHistoryPropertyName = "passwordHistory";

	/** @hidden */
	public static final String authenticationFailuresPropertyName = "authenticationFailures";

	/** @hidden */
	public static final String lastAuthenticationFailurePropertyName = "lastAuthenticationFailure";

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

	/** @hidden */
	public static final String groupSelectionPropertyName = "groupSelection";

	/** @hidden */
	public static final String groupsExistPropertyName = "groupsExist";

	/** @hidden */
	public static final String newGroupPropertyName = "newGroup";

	/** @hidden */
	public static final String assignedRolesPropertyName = "assignedRoles";

	/** @hidden */
	public static final String activatedPropertyName = "activated";

	/** @hidden */
	public static final String activationCodePropertyName = "activationCode";

	/** @hidden */
	public static final String activationCodeCreationDateTimePropertyName = "activationCodeCreationDateTime";

	/** @hidden */
	public static final String activateUrlPropertyName = "activateUrl";

	/** @hidden */
	public static final String twoFactorCodePropertyName = "twoFactorCode";

	/** @hidden */
	public static final String twoFactorCodeGeneratedTimestampPropertyName = "twoFactorCodeGeneratedTimestamp";

	/** @hidden */
	public static final String twoFactorTokenPropertyName = "twoFactorToken";

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
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
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
		private static List<DomainValue> domainValues = Stream.of(values()).map(WizardState::toDomainValue).collect(Collectors.toUnmodifiableList());

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
		public String toLocalisedDescription() {
			return Util.i18n(description);
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

		public static WizardState fromLocalisedDescription(String description) {
			WizardState result = null;

			for (WizardState value : values()) {
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
	 * Groups
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum GroupSelection implements Enumeration {
		existingGroups("existingGroups", "Existing groups"),
		newGroup("newGroup", "New group");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(GroupSelection::toDomainValue).collect(Collectors.toUnmodifiableList());

		private GroupSelection(String code, String description) {
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

		public static GroupSelection fromCode(String code) {
			GroupSelection result = null;

			for (GroupSelection value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static GroupSelection fromLocalisedDescription(String description) {
			GroupSelection result = null;

			for (GroupSelection value : values()) {
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
	 * The module displayed when the user first signs in.
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
	 * <br/>
	 * When password was last changed. Referenced in password change notification email contents.
	 **/
	private DateTime passwordLastChanged;

	/**
	 * Password Last Changed IP
	 * <br/>
	 * Source IP when password was last changed
	 * <br/>
	 * IP when password was last changed. Referenced in password change notification email contents.
	 **/
	private String passwordLastChangedIP;

	/**
	 * Password Last Changed Country Code
	 * <br/>
	 * Country Code where password was last changed
	 * <br/>
	 * 2-letter country-code where password was last changed. Referenced in password change notification email contents.
	 **/
	private String passwordLastChangedCountryCode;

	/**
	 * Password Last Changed Country
	 * <br/>
	 * Country where password was last changed
	 * <br/>
	 * User Locale Country Name where password was last changed.
					Referenced in password change notification email contents.
					The getter is overridden in the extension class.
	 **/
	private String passwordLastChangedCountryName;

	/**
	 * Password Reset Token
	 * <br/>
	 * The password reset token emailed to the user on pass reset request.
	 * <br/>
	 * This contains a token (UUID + time in millis) which when submitted by the user will enable them to reset their password.
	 **/
	private String passwordResetToken;

	/**
	 * Password Reset Token Creation Timestamp
	 * <br/>
	 * When the password reset token was created
	 * <br/>
	 * Used in WebUtil.resetPassword to evaluate token expiry.
	 **/
	private Timestamp passwordResetTokenCreationTimestamp;

	/**
	 * Password History
	 * <br/>
	 * A tab separated list of previous password hashes used
	 **/
	private String passwordHistory;

	/**
	 * Authentication Failures
	 * <br/>
	 * The number of authentication failures since the last successful authentication
	 * <br/>
	 * This value is zeroed on successful authentication.
	 **/
	private Integer authenticationFailures;

	/**
	 * Last Authentication Failure
	 * <br/>
	 * Time that last authentication failure occurred
	 **/
	private Timestamp lastAuthenticationFailure;

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
	 * Security Groups
	 * <br/>
	 * The collection of security groups that this user belongs to.
	 **/
	private List<GroupExtension> groups = new ChangeTrackingArrayList<>("groups", this);

	/**
	 * Roles
	 * <br/>
	 * Typically users are assigned membership of groups, which define sets of roles, corresponding to business roles within an organisation. 

However user may also have specific roles assigned in addition to the roles
which are implied from the groups to which they belong.
	 **/
	private List<UserRole> roles = new ChangeTrackingArrayList<>("roles", this);

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
	private List<UserCandidateContact> candidateContacts = new ChangeTrackingArrayList<>("candidateContacts", this);

	/**
	 * The contact selected for this user.
	 **/
	private Boolean contactSelected = Boolean.valueOf(false);

	/**
	 * Inactive
	 * <br/>
	 * Indicates that this account has been marked as inactive and no longer in use.
	 **/
	private Boolean inactive;

	/**
	 * Groups
	 **/
	private GroupSelection groupSelection;

	/**
	 * Groups Exist
	 **/
	private Boolean groupsExist;

	/**
	 * New Group
	 **/
	private GroupExtension newGroup = null;

	/**
	 * Roles
	 * <br/>
	 * The assigned roles through the groups, customer roles and module roles assigned.
	 **/
	private List<UserRole> assignedRoles = new ArrayList<>();

	/**
	 * Account Activated
	 * <br/>
	 * Whether this account has been activated or not. An account not activated means the user has not finished the activation process by clicking the link from their registration email.
	 * <br/>
	 * By default the account will be activated.
			For public users, we want them to activate the account manually so this will be set to false and the activationCode field will be populated.
	 **/
	private Boolean activated = Boolean.valueOf(true);

	/**
	 * Activation Code
	 * <br/>
	 * The activation code for this user account.
	 * <br/>
	 * This contains a code which when submitted by the user will activate their account.
	 **/
	private String activationCode;

	/**
	 * The date and time the activation code was created
	 * <br/>
	 * This setting is used to control expiry of activation codes.
	 **/
	private DateTime activationCodeCreationDateTime;

	/**
	 * Activation Url
	 **/
	private String activateUrl;

	/**
	 * Two Factor Code
	 * <br/>
	 * this is hashed
	 **/
	private String twoFactorCode;

	/**
	 * 2FA Code DateTime
	 * <br/>
	 * used to invalidate the 2fa code when X amount of time has passed. Not displayed to the user
	 **/
	private Timestamp twoFactorCodeGeneratedTimestamp;

	/**
	 * Two Factor Token
	 * <br/>
	 * Used to identify the user is in the same session for 2FA code entry, this is for the system
	 **/
	private String twoFactorToken;

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

	public static UserExtension newInstance() {
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
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
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
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setPasswordLastChanged(DateTime passwordLastChanged) {
		preset(passwordLastChangedPropertyName, passwordLastChanged);
		this.passwordLastChanged = passwordLastChanged;
	}

	/**
	 * {@link #passwordLastChangedIP} accessor.
	 * @return	The value.
	 **/
	public String getPasswordLastChangedIP() {
		return passwordLastChangedIP;
	}

	/**
	 * {@link #passwordLastChangedIP} mutator.
	 * @param passwordLastChangedIP	The new value.
	 **/
	@XmlElement
	public void setPasswordLastChangedIP(String passwordLastChangedIP) {
		preset(passwordLastChangedIPPropertyName, passwordLastChangedIP);
		this.passwordLastChangedIP = passwordLastChangedIP;
	}

	/**
	 * {@link #passwordLastChangedCountryCode} accessor.
	 * @return	The value.
	 **/
	public String getPasswordLastChangedCountryCode() {
		return passwordLastChangedCountryCode;
	}

	/**
	 * {@link #passwordLastChangedCountryCode} mutator.
	 * @param passwordLastChangedCountryCode	The new value.
	 **/
	@XmlElement
	public void setPasswordLastChangedCountryCode(String passwordLastChangedCountryCode) {
		preset(passwordLastChangedCountryCodePropertyName, passwordLastChangedCountryCode);
		this.passwordLastChangedCountryCode = passwordLastChangedCountryCode;
	}

	/**
	 * {@link #passwordLastChangedCountryName} accessor.
	 * @return	The value.
	 **/
	public String getPasswordLastChangedCountryName() {
		return passwordLastChangedCountryName;
	}

	/**
	 * {@link #passwordLastChangedCountryName} mutator.
	 * @param passwordLastChangedCountryName	The new value.
	 **/
	@XmlElement
	public void setPasswordLastChangedCountryName(String passwordLastChangedCountryName) {
		this.passwordLastChangedCountryName = passwordLastChangedCountryName;
	}

	/**
	 * {@link #passwordResetToken} accessor.
	 * @return	The value.
	 **/
	public String getPasswordResetToken() {
		return passwordResetToken;
	}

	/**
	 * {@link #passwordResetToken} mutator.
	 * @param passwordResetToken	The new value.
	 **/
	@XmlElement
	public void setPasswordResetToken(String passwordResetToken) {
		preset(passwordResetTokenPropertyName, passwordResetToken);
		this.passwordResetToken = passwordResetToken;
	}

	/**
	 * {@link #passwordResetTokenCreationTimestamp} accessor.
	 * @return	The value.
	 **/
	public Timestamp getPasswordResetTokenCreationTimestamp() {
		return passwordResetTokenCreationTimestamp;
	}

	/**
	 * {@link #passwordResetTokenCreationTimestamp} mutator.
	 * @param passwordResetTokenCreationTimestamp	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setPasswordResetTokenCreationTimestamp(Timestamp passwordResetTokenCreationTimestamp) {
		preset(passwordResetTokenCreationTimestampPropertyName, passwordResetTokenCreationTimestamp);
		this.passwordResetTokenCreationTimestamp = passwordResetTokenCreationTimestamp;
	}

	/**
	 * {@link #passwordHistory} accessor.
	 * @return	The value.
	 **/
	public String getPasswordHistory() {
		return passwordHistory;
	}

	/**
	 * {@link #passwordHistory} mutator.
	 * @param passwordHistory	The new value.
	 **/
	@XmlElement
	public void setPasswordHistory(String passwordHistory) {
		this.passwordHistory = passwordHistory;
	}

	/**
	 * {@link #authenticationFailures} accessor.
	 * @return	The value.
	 **/
	public Integer getAuthenticationFailures() {
		return authenticationFailures;
	}

	/**
	 * {@link #authenticationFailures} mutator.
	 * @param authenticationFailures	The new value.
	 **/
	@XmlElement
	public void setAuthenticationFailures(Integer authenticationFailures) {
		preset(authenticationFailuresPropertyName, authenticationFailures);
		this.authenticationFailures = authenticationFailures;
	}

	/**
	 * {@link #lastAuthenticationFailure} accessor.
	 * @return	The value.
	 **/
	public Timestamp getLastAuthenticationFailure() {
		return lastAuthenticationFailure;
	}

	/**
	 * {@link #lastAuthenticationFailure} mutator.
	 * @param lastAuthenticationFailure	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setLastAuthenticationFailure(Timestamp lastAuthenticationFailure) {
		preset(lastAuthenticationFailurePropertyName, lastAuthenticationFailure);
		this.lastAuthenticationFailure = lastAuthenticationFailure;
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
		if (this.dataGroup != dataGroup) {
			preset(dataGroupPropertyName, dataGroup);
			this.dataGroup = dataGroup;
		}
	}

	/**
	 * {@link #groups} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<GroupExtension> getGroups() {
		return groups;
	}

	/**
	 * {@link #groups} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public GroupExtension getGroupsElementById(String bizId) {
		return getElementById(groups, bizId);
	}

	/**
	 * {@link #groups} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setGroupsElementById(String bizId, GroupExtension element) {
		setElementById(groups, element);
	}

	/**
	 * {@link #groups} add.
	 * @param element	The element to add.
	 **/
	public boolean addGroupsElement(GroupExtension element) {
		return groups.add(element);
	}

	/**
	 * {@link #groups} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addGroupsElement(int index, GroupExtension element) {
		groups.add(index, element);
	}

	/**
	 * {@link #groups} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeGroupsElement(GroupExtension element) {
		return groups.remove(element);
	}

	/**
	 * {@link #groups} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public GroupExtension removeGroupsElement(int index) {
		return groups.remove(index);
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
	public void setRolesElementById(String bizId, UserRole element) {
		setElementById(roles, element);
	}

	/**
	 * {@link #roles} add.
	 * @param element	The element to add.
	 **/
	public boolean addRolesElement(UserRole element) {
		boolean result = roles.add(element);
		if (result) {
			element.setParent((UserExtension) this);
		}
		return result;
	}

	/**
	 * {@link #roles} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addRolesElement(int index, UserRole element) {
		roles.add(index, element);
		element.setParent((UserExtension) this);
	}

	/**
	 * {@link #roles} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeRolesElement(UserRole element) {
		boolean result = roles.remove(element);
		if (result) {
			element.setParent(null);
		}
		return result;
	}

	/**
	 * {@link #roles} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public UserRole removeRolesElement(int index) {
		UserRole result = roles.remove(index);
		result.setParent(null);
		return result;
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
	public void setCandidateContactsElementById(String bizId, UserCandidateContact element) {
		setElementById(candidateContacts, element);
	}

	/**
	 * {@link #candidateContacts} add.
	 * @param element	The element to add.
	 **/
	public boolean addCandidateContactsElement(UserCandidateContact element) {
		boolean result = candidateContacts.add(element);
		if (result) {
			element.setParent((UserExtension) this);
		}
		return result;
	}

	/**
	 * {@link #candidateContacts} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addCandidateContactsElement(int index, UserCandidateContact element) {
		candidateContacts.add(index, element);
		element.setParent((UserExtension) this);
	}

	/**
	 * {@link #candidateContacts} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeCandidateContactsElement(UserCandidateContact element) {
		boolean result = candidateContacts.remove(element);
		if (result) {
			element.setParent(null);
		}
		return result;
	}

	/**
	 * {@link #candidateContacts} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public UserCandidateContact removeCandidateContactsElement(int index) {
		UserCandidateContact result = candidateContacts.remove(index);
		result.setParent(null);
		return result;
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
	 * {@link #groupSelection} accessor.
	 * @return	The value.
	 **/
	public GroupSelection getGroupSelection() {
		return groupSelection;
	}

	/**
	 * {@link #groupSelection} mutator.
	 * @param groupSelection	The new value.
	 **/
	@XmlElement
	public void setGroupSelection(GroupSelection groupSelection) {
		this.groupSelection = groupSelection;
	}

	/**
	 * {@link #groupsExist} accessor.
	 * @return	The value.
	 **/
	public Boolean getGroupsExist() {
		return groupsExist;
	}

	/**
	 * {@link #groupsExist} mutator.
	 * @param groupsExist	The new value.
	 **/
	@XmlElement
	public void setGroupsExist(Boolean groupsExist) {
		this.groupsExist = groupsExist;
	}

	/**
	 * {@link #newGroup} accessor.
	 * @return	The value.
	 **/
	public GroupExtension getNewGroup() {
		return newGroup;
	}

	/**
	 * {@link #newGroup} mutator.
	 * @param newGroup	The new value.
	 **/
	@XmlElement
	public void setNewGroup(GroupExtension newGroup) {
		if (this.newGroup != newGroup) {
			this.newGroup = newGroup;
		}
	}

	/**
	 * {@link #assignedRoles} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<UserRole> getAssignedRoles() {
		return assignedRoles;
	}

	/**
	 * {@link #assignedRoles} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public UserRole getAssignedRolesElementById(String bizId) {
		return getElementById(assignedRoles, bizId);
	}

	/**
	 * {@link #assignedRoles} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setAssignedRolesElementById(String bizId, UserRole element) {
		setElementById(assignedRoles, element);
	}

	/**
	 * {@link #assignedRoles} add.
	 * @param element	The element to add.
	 **/
	public boolean addAssignedRolesElement(UserRole element) {
		return assignedRoles.add(element);
	}

	/**
	 * {@link #assignedRoles} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addAssignedRolesElement(int index, UserRole element) {
		assignedRoles.add(index, element);
	}

	/**
	 * {@link #assignedRoles} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeAssignedRolesElement(UserRole element) {
		return assignedRoles.remove(element);
	}

	/**
	 * {@link #assignedRoles} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public UserRole removeAssignedRolesElement(int index) {
		return assignedRoles.remove(index);
	}

	/**
	 * {@link #activated} accessor.
	 * @return	The value.
	 **/
	public Boolean getActivated() {
		return activated;
	}

	/**
	 * {@link #activated} mutator.
	 * @param activated	The new value.
	 **/
	@XmlElement
	public void setActivated(Boolean activated) {
		preset(activatedPropertyName, activated);
		this.activated = activated;
	}

	/**
	 * {@link #activationCode} accessor.
	 * @return	The value.
	 **/
	public String getActivationCode() {
		return activationCode;
	}

	/**
	 * {@link #activationCode} mutator.
	 * @param activationCode	The new value.
	 **/
	@XmlElement
	public void setActivationCode(String activationCode) {
		preset(activationCodePropertyName, activationCode);
		this.activationCode = activationCode;
	}

	/**
	 * {@link #activationCodeCreationDateTime} accessor.
	 * @return	The value.
	 **/
	public DateTime getActivationCodeCreationDateTime() {
		return activationCodeCreationDateTime;
	}

	/**
	 * {@link #activationCodeCreationDateTime} mutator.
	 * @param activationCodeCreationDateTime	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setActivationCodeCreationDateTime(DateTime activationCodeCreationDateTime) {
		preset(activationCodeCreationDateTimePropertyName, activationCodeCreationDateTime);
		this.activationCodeCreationDateTime = activationCodeCreationDateTime;
	}

	/**
	 * {@link #activateUrl} accessor.
	 * @return	The value.
	 **/
	public String getActivateUrl() {
		return activateUrl;
	}

	/**
	 * {@link #activateUrl} mutator.
	 * @param activateUrl	The new value.
	 **/
	@XmlElement
	public void setActivateUrl(String activateUrl) {
		this.activateUrl = activateUrl;
	}

	/**
	 * {@link #twoFactorCode} accessor.
	 * @return	The value.
	 **/
	public String getTwoFactorCode() {
		return twoFactorCode;
	}

	/**
	 * {@link #twoFactorCode} mutator.
	 * @param twoFactorCode	The new value.
	 **/
	@XmlElement
	public void setTwoFactorCode(String twoFactorCode) {
		preset(twoFactorCodePropertyName, twoFactorCode);
		this.twoFactorCode = twoFactorCode;
	}

	/**
	 * {@link #twoFactorCodeGeneratedTimestamp} accessor.
	 * @return	The value.
	 **/
	public Timestamp getTwoFactorCodeGeneratedTimestamp() {
		return twoFactorCodeGeneratedTimestamp;
	}

	/**
	 * {@link #twoFactorCodeGeneratedTimestamp} mutator.
	 * @param twoFactorCodeGeneratedTimestamp	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setTwoFactorCodeGeneratedTimestamp(Timestamp twoFactorCodeGeneratedTimestamp) {
		preset(twoFactorCodeGeneratedTimestampPropertyName, twoFactorCodeGeneratedTimestamp);
		this.twoFactorCodeGeneratedTimestamp = twoFactorCodeGeneratedTimestamp;
	}

	/**
	 * {@link #twoFactorToken} accessor.
	 * @return	The value.
	 **/
	public String getTwoFactorToken() {
		return twoFactorToken;
	}

	/**
	 * {@link #twoFactorToken} mutator.
	 * @param twoFactorToken	The new value.
	 **/
	@XmlElement
	public void setTwoFactorToken(String twoFactorToken) {
		preset(twoFactorTokenPropertyName, twoFactorToken);
		this.twoFactorToken = twoFactorToken;
	}

	/**
	 * Whether the current user is allowed to manage this user's details
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isAccessDenied() {
		return (!isOwningUser() && !isSecurityAdministrator());
	}

	/**
	 * {@link #isAccessDenied} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotAccessDenied() {
		return (! isAccessDenied());
	}

	/**
	 * Allows administrators to manually activate users when User Self-Registration is enabled.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isCanActivateUser() {
		return (isSecurityAdministrator() && isSelfRegistrationEnabled());
	}

	/**
	 * {@link #isCanActivateUser} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotCanActivateUser() {
		return (! isCanActivateUser());
	}

	/**
	 * Candidate Contacts is empty
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isCandidateContactsEmpty() {
		return (candidateContacts.isEmpty());
	}

	/**
	 * {@link #isCandidateContactsEmpty} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotCandidateContactsEmpty() {
		return (! isCandidateContactsEmpty());
	}

	/**
	 * Confirm Contact step
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isConfirmContact() {
		return (WizardState.confirmContact.equals(getWizardState()));
	}

	/**
	 * {@link #isConfirmContact} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotConfirmContact() {
		return (! isConfirmContact());
	}

	/**
	 * Confirm Group Memberships step
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isConfirmGroupMemberships() {
		return (WizardState.confirmGroupMemberships.equals(getWizardState()));
	}

	/**
	 * {@link #isConfirmGroupMemberships} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotConfirmGroupMemberships() {
		return (! isConfirmGroupMemberships());
	}

	/**
	 * Confirm User Name and Password step
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isConfirmUserNameAndPassword() {
		return (WizardState.confirmUserNameAndPassword.equals(getWizardState()));
	}

	/**
	 * {@link #isConfirmUserNameAndPassword} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotConfirmUserNameAndPassword() {
		return (! isConfirmUserNameAndPassword());
	}

	/**
	 * Create Contact step
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isCreateContact() {
		return (WizardState.createContact.equals(getWizardState()));
	}

	/**
	 * {@link #isCreateContact} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotCreateContact() {
		return (! isCreateContact());
	}

	/**
	 * Created
	 *
	 * @return The condition
	 */
	@XmlTransient
	@Override
	public boolean isCreated() {
		return (isPersisted());
	}

	/**
	 * {@link #isCreated} negation.
	 *
	 * @return The negated condition
	 */
	@Override
	public boolean isNotCreated() {
		return (! isCreated());
	}

	/**
	 * Designer
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isDesigner() {
		return (isUserInRole("design", "BizHubDesigner"));
	}

	/**
	 * {@link #isDesigner} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotDesigner() {
		return (! isDesigner());
	}

	/**
	 * In Data Group
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isInDataGroup() {
		return (! isUserInDataGroup(null));
	}

	/**
	 * {@link #isInDataGroup} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotInDataGroup() {
		return (! isInDataGroup());
	}

	/**
	 * Whether the current user is this user
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isOwningUser() {
		return (((UserExtension) this).owningUser());
	}

	/**
	 * {@link #isOwningUser} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotOwningUser() {
		return (! isOwningUser());
	}

	/**
	 * Security Administrator
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isSecurityAdministrator() {
		return (isUserInRole("admin","SecurityAdministrator"));
	}

	/**
	 * {@link #isSecurityAdministrator} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotSecurityAdministrator() {
		return (! isSecurityAdministrator());
	}

	/**
	 * True when User Self-Registration is enabled.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isSelfRegistrationEnabled() {
		return (org.skyve.impl.util.UtilImpl.ACCOUNT_ALLOW_SELF_REGISTRATION);
	}

	/**
	 * {@link #isSelfRegistrationEnabled} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotSelfRegistrationEnabled() {
		return (! isSelfRegistrationEnabled());
	}

	/**
	 * True when User Self-Registration is enabled and the User has not been activated.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isSelfRegistrationEnabledAndUserNotActivated() {
		return (isSelfRegistrationEnabled() && Boolean.FALSE.equals(getActivated()) && isSecurityAdministrator());
	}

	/**
	 * {@link #isSelfRegistrationEnabledAndUserNotActivated} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotSelfRegistrationEnabledAndUserNotActivated() {
		return (! isSelfRegistrationEnabledAndUserNotActivated());
	}

	/**
	 * showExistingGroups
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowExistingGroups() {
		return (Boolean.TRUE.equals(groupsExist));
	}

	/**
	 * {@link #isShowExistingGroups} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowExistingGroups() {
		return (! isShowExistingGroups());
	}

	/**
	 * showGroupCreator
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowGroupCreator() {
		return (GroupSelection.newGroup.equals(groupSelection));
	}

	/**
	 * {@link #isShowGroupCreator} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowGroupCreator() {
		return (! isShowGroupCreator());
	}

	/**
	 * Show Next Button
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowNextButton() {
		return (isCreateContact() || isConfirmUserNameAndPassword());
	}

	/**
	 * {@link #isShowNextButton} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowNextButton() {
		return (! isShowNextButton());
	}
}
