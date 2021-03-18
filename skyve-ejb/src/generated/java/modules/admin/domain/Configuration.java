package modules.admin.domain;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import modules.admin.Configuration.ConfigurationExtension;
import modules.admin.Group.GroupExtension;
import modules.admin.Startup.StartupExtension;
import modules.admin.UserProxy.UserProxyExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

/**
 * Setup
 * 
 * @depend - - - PasswordComplexityModel
 * @navhas n publicUser 0..1 UserProxy
 * @navhas n emailToContact 0..1 Contact
 * @navhas n startup 0..1 Startup
 * @navhas n userSelfRegistrationGroup 0..1 Group
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public abstract class Configuration extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "Configuration";

	/** @hidden */
	public static final String passwordMinLengthPropertyName = "passwordMinLength";

	/** @hidden */
	public static final String passwordRequireLowercasePropertyName = "passwordRequireLowercase";

	/** @hidden */
	public static final String passwordRequireUppercasePropertyName = "passwordRequireUppercase";

	/** @hidden */
	public static final String passwordRequireNumericPropertyName = "passwordRequireNumeric";

	/** @hidden */
	public static final String passwordRequireSpecialPropertyName = "passwordRequireSpecial";

	/** @hidden */
	public static final String passwordRuleDescriptionPropertyName = "passwordRuleDescription";

	/** @hidden */
	public static final String fromEmailPropertyName = "fromEmail";

	/** @hidden */
	public static final String passwordResetEmailSubjectPropertyName = "passwordResetEmailSubject";

	/** @hidden */
	public static final String passwordResetEmailBodyPropertyName = "passwordResetEmailBody";

	/** @hidden */
	public static final String userSelfRegistrationGroupPropertyName = "userSelfRegistrationGroup";

	/** @hidden */
	public static final String selfRegistrationActivationExpiryHoursPropertyName = "selfRegistrationActivationExpiryHours";

	/** @hidden */
	@Deprecated
	public static final String allowUserSelfRegistrationPropertyName = "allowUserSelfRegistration";

	/** @hidden */
	@Deprecated
	public static final String passwordComplexityModelPropertyName = "passwordComplexityModel";

	/** @hidden */
	public static final String publicUserPropertyName = "publicUser";

	/** @hidden */
	public static final String emailFromPropertyName = "emailFrom";

	/** @hidden */
	public static final String emailToPropertyName = "emailTo";

	/** @hidden */
	public static final String emailSubjectPropertyName = "emailSubject";

	/** @hidden */
	public static final String emailContentPropertyName = "emailContent";

	/** @hidden */
	public static final String passwordExpiryDaysPropertyName = "passwordExpiryDays";

	/** @hidden */
	public static final String passwordHistoryRetentionPropertyName = "passwordHistoryRetention";

	/** @hidden */
	public static final String passwordAccountLockoutThresholdPropertyName = "passwordAccountLockoutThreshold";

	/** @hidden */
	public static final String passwordAccountLockoutDurationPropertyName = "passwordAccountLockoutDuration";

	/** @hidden */
	public static final String emailToContactPropertyName = "emailToContact";

	/** @hidden */
	public static final String startupPropertyName = "startup";

	/**
	 * Password Complexity
	 * <br/>
	 * The security level/complexity model for user passwords
	 * <br/>
	 * Replaced by password length and complexity booleans. To be removed 
				in a future version of Skyve. Here for backwards compatibility during Restore.
	 **/
	@XmlEnum
	public static enum PasswordComplexityModel implements Enumeration {
		minimumMin6Chars("MINIMUM", "Minimum - min 6 chars"),
		mediumMin6CharsUpperLowerAndNumeric("MEDIUM", "Medium - min 6 chars, upper, lower and numeric"),
		goodMin8CharsUpperLowerNumericAndPunctuation("MAXIMUM", "Good - min 8 chars, upper, lower, numeric and punctuation"),
		strongMin10CharsUpperLowerNumericAndPunctuation("STRONG", "Strong - min 10 chars, upper, lower, numeric and punctuation");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private PasswordComplexityModel(String code, String description) {
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

		public static PasswordComplexityModel fromCode(String code) {
			PasswordComplexityModel result = null;

			for (PasswordComplexityModel value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static PasswordComplexityModel fromDescription(String description) {
			PasswordComplexityModel result = null;

			for (PasswordComplexityModel value : values()) {
				if (value.description.equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				PasswordComplexityModel[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (PasswordComplexityModel value : values) {
					domainValues.add(value.domainValue);
				}
			}

			return domainValues;
		}
	}

	/**
	 * Minumum Password Length
	 * <br/>
	 * The minimum number of characters for new passwords
	 **/
	private Integer passwordMinLength = Integer.valueOf(10);

	/**
	 * Requires Lowercase
	 * <br/>
	 * If new passwords should require at least one lowercase character
	 **/
	private Boolean passwordRequireLowercase;

	/**
	 * Requires Uppercase
	 * <br/>
	 * If new passwords should require at least one uppercase character
	 **/
	private Boolean passwordRequireUppercase;

	/**
	 * Requires Numeric Characters
	 * <br/>
	 * If new passwords should require at least one numeric character
	 **/
	private Boolean passwordRequireNumeric;

	/**
	 * Requires Special Characters
	 * <br/>
	 * If new passwords should require at least one special character
	 **/
	private Boolean passwordRequireSpecial;

	/**
	 * Password Rule Description
	 * <br/>
	 * A text description which can be shown to the user if their password does not comply
				with the system password complexity settings. This is a calculated field, see ConfigurationExtension.
	 **/
	private String passwordRuleDescription;

	/**
	 * Sender/From Email Address
	 * <br/>
	 * Email Address that all email's that the system sends will be sent from.
	 **/
	private String fromEmail;

	/**
	 * Password Reset Email Subject
	 * <br/>
	 * The subject of the password reset email to be sent to clients. Bindings are allowed relative to the User.
	 **/
	private String passwordResetEmailSubject;

	/**
	 * Password Reset Email Body
	 * <br/>
	 * The body of the password reset email to be sent to clients. Bindings are allowed relative to the User.
	 **/
	private String passwordResetEmailBody;

	/**
	 * User Self Registration Group
	 * <br/>
	 * Which group self-registering users will be assigned upon registration, which specifies the roles they will have access to. 
	 **/
	private GroupExtension userSelfRegistrationGroup = null;

	/**
	 * Number of hours to keep self-registration activation codes enabled
	 * <br/>
	 * Clear this setting to have codes that never expire.
	 **/
	private Integer selfRegistrationActivationExpiryHours;

	/**
	 * This option is now a startup property found in the project JSON file.
	 * <br/>
	 * Master switch to allow or disallow self registration.
	 **/
	@Deprecated
	private Boolean allowUserSelfRegistration;

	/**
	 * Password Complexity
	 * <br/>
	 * The security level/complexity model for user passwords
	 * <br/>
	 * Replaced by password length and complexity booleans. To be removed 
				in a future version of Skyve. Here for backwards compatibility during Restore.
	 **/
	@Deprecated
	private PasswordComplexityModel passwordComplexityModel;

	/**
	 * Anonymous Public User
	 * <br/>
	 * The anonymous public user asserted on all public pages.
	 **/
	private UserProxyExtension publicUser = null;

	/**
	 * Email From
	 **/
	private String emailFrom;

	/**
	 * Email To
	 **/
	private String emailTo;

	/**
	 * Email Subject
	 **/
	private String emailSubject;

	/**
	 * Email
	 **/
	private String emailContent;

	/**
	 * Password Expiry in Days
	 * <br/>
	 * Number of days until a password change is required. Blank indicates no password aging.
	 * <br/>
	 * Read from the application JSON file set at system startup.
	 **/
	private String passwordExpiryDays;

	/**
	 * Password History Retention
	 * <br/>
	 * Number of previous passwords to check for duplicates. Blank indicates no password history.
	 * <br/>
	 * Read from the application JSON file set at system startup.
	 **/
	private String passwordHistoryRetention;

	/**
	 * Account Lockout Threshold
	 * <br/>
	 * Number of sign in attempts until the user account is locked. Blank indicates no account lockout.
	 * <br/>
	 * Read from the application JSON file set at system startup.
	 **/
	private String passwordAccountLockoutThreshold;

	/**
	 * Account Lockout Duration
	 * <br/>
	 * admin.configuration.passwordAccoutnLockoutDuration.description
	 * <br/>
	 * Read from the application JSON file set at system startup.
	 **/
	private String passwordAccountLockoutDuration;

	/**
	 * Email To Contact
	 **/
	private Contact emailToContact = null;

	/**
	 * Startup
	 **/
	private StartupExtension startup = null;

	@Override
	@XmlTransient
	public String getBizModule() {
		return Configuration.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Configuration.DOCUMENT_NAME;
	}

	public static ConfigurationExtension newInstance() {
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
			return org.skyve.util.Binder.formatMessage("Admin Setup", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Configuration) && 
					this.getBizId().equals(((Configuration) o).getBizId()));
	}

	/**
	 * {@link #passwordMinLength} accessor.
	 * @return	The value.
	 **/
	public Integer getPasswordMinLength() {
		return passwordMinLength;
	}

	/**
	 * {@link #passwordMinLength} mutator.
	 * @param passwordMinLength	The new value.
	 **/
	@XmlElement
	public void setPasswordMinLength(Integer passwordMinLength) {
		preset(passwordMinLengthPropertyName, passwordMinLength);
		this.passwordMinLength = passwordMinLength;
	}

	/**
	 * {@link #passwordRequireLowercase} accessor.
	 * @return	The value.
	 **/
	public Boolean getPasswordRequireLowercase() {
		return passwordRequireLowercase;
	}

	/**
	 * {@link #passwordRequireLowercase} mutator.
	 * @param passwordRequireLowercase	The new value.
	 **/
	@XmlElement
	public void setPasswordRequireLowercase(Boolean passwordRequireLowercase) {
		preset(passwordRequireLowercasePropertyName, passwordRequireLowercase);
		this.passwordRequireLowercase = passwordRequireLowercase;
	}

	/**
	 * {@link #passwordRequireUppercase} accessor.
	 * @return	The value.
	 **/
	public Boolean getPasswordRequireUppercase() {
		return passwordRequireUppercase;
	}

	/**
	 * {@link #passwordRequireUppercase} mutator.
	 * @param passwordRequireUppercase	The new value.
	 **/
	@XmlElement
	public void setPasswordRequireUppercase(Boolean passwordRequireUppercase) {
		preset(passwordRequireUppercasePropertyName, passwordRequireUppercase);
		this.passwordRequireUppercase = passwordRequireUppercase;
	}

	/**
	 * {@link #passwordRequireNumeric} accessor.
	 * @return	The value.
	 **/
	public Boolean getPasswordRequireNumeric() {
		return passwordRequireNumeric;
	}

	/**
	 * {@link #passwordRequireNumeric} mutator.
	 * @param passwordRequireNumeric	The new value.
	 **/
	@XmlElement
	public void setPasswordRequireNumeric(Boolean passwordRequireNumeric) {
		preset(passwordRequireNumericPropertyName, passwordRequireNumeric);
		this.passwordRequireNumeric = passwordRequireNumeric;
	}

	/**
	 * {@link #passwordRequireSpecial} accessor.
	 * @return	The value.
	 **/
	public Boolean getPasswordRequireSpecial() {
		return passwordRequireSpecial;
	}

	/**
	 * {@link #passwordRequireSpecial} mutator.
	 * @param passwordRequireSpecial	The new value.
	 **/
	@XmlElement
	public void setPasswordRequireSpecial(Boolean passwordRequireSpecial) {
		preset(passwordRequireSpecialPropertyName, passwordRequireSpecial);
		this.passwordRequireSpecial = passwordRequireSpecial;
	}

	/**
	 * {@link #passwordRuleDescription} accessor.
	 * @return	The value.
	 **/
	public String getPasswordRuleDescription() {
		return passwordRuleDescription;
	}

	/**
	 * {@link #passwordRuleDescription} mutator.
	 * @param passwordRuleDescription	The new value.
	 **/
	@XmlElement
	public void setPasswordRuleDescription(String passwordRuleDescription) {
		this.passwordRuleDescription = passwordRuleDescription;
	}

	/**
	 * {@link #fromEmail} accessor.
	 * @return	The value.
	 **/
	public String getFromEmail() {
		return fromEmail;
	}

	/**
	 * {@link #fromEmail} mutator.
	 * @param fromEmail	The new value.
	 **/
	@XmlElement
	public void setFromEmail(String fromEmail) {
		preset(fromEmailPropertyName, fromEmail);
		this.fromEmail = fromEmail;
	}

	/**
	 * {@link #passwordResetEmailSubject} accessor.
	 * @return	The value.
	 **/
	public String getPasswordResetEmailSubject() {
		return passwordResetEmailSubject;
	}

	/**
	 * {@link #passwordResetEmailSubject} mutator.
	 * @param passwordResetEmailSubject	The new value.
	 **/
	@XmlElement
	public void setPasswordResetEmailSubject(String passwordResetEmailSubject) {
		preset(passwordResetEmailSubjectPropertyName, passwordResetEmailSubject);
		this.passwordResetEmailSubject = passwordResetEmailSubject;
	}

	/**
	 * {@link #passwordResetEmailBody} accessor.
	 * @return	The value.
	 **/
	public String getPasswordResetEmailBody() {
		return passwordResetEmailBody;
	}

	/**
	 * {@link #passwordResetEmailBody} mutator.
	 * @param passwordResetEmailBody	The new value.
	 **/
	@XmlElement
	public void setPasswordResetEmailBody(String passwordResetEmailBody) {
		preset(passwordResetEmailBodyPropertyName, passwordResetEmailBody);
		this.passwordResetEmailBody = passwordResetEmailBody;
	}

	/**
	 * {@link #userSelfRegistrationGroup} accessor.
	 * @return	The value.
	 **/
	public GroupExtension getUserSelfRegistrationGroup() {
		return userSelfRegistrationGroup;
	}

	/**
	 * {@link #userSelfRegistrationGroup} mutator.
	 * @param userSelfRegistrationGroup	The new value.
	 **/
	@XmlElement
	public void setUserSelfRegistrationGroup(GroupExtension userSelfRegistrationGroup) {
		if (this.userSelfRegistrationGroup != userSelfRegistrationGroup) {
			preset(userSelfRegistrationGroupPropertyName, userSelfRegistrationGroup);
			this.userSelfRegistrationGroup = userSelfRegistrationGroup;
		}
	}

	/**
	 * {@link #selfRegistrationActivationExpiryHours} accessor.
	 * @return	The value.
	 **/
	public Integer getSelfRegistrationActivationExpiryHours() {
		return selfRegistrationActivationExpiryHours;
	}

	/**
	 * {@link #selfRegistrationActivationExpiryHours} mutator.
	 * @param selfRegistrationActivationExpiryHours	The new value.
	 **/
	@XmlElement
	public void setSelfRegistrationActivationExpiryHours(Integer selfRegistrationActivationExpiryHours) {
		preset(selfRegistrationActivationExpiryHoursPropertyName, selfRegistrationActivationExpiryHours);
		this.selfRegistrationActivationExpiryHours = selfRegistrationActivationExpiryHours;
	}

	/**
	 * {@link #allowUserSelfRegistration} accessor.
	 * @return	The value.
	 **/
	@Deprecated
	public Boolean getAllowUserSelfRegistration() {
		return allowUserSelfRegistration;
	}

	/**
	 * {@link #allowUserSelfRegistration} mutator.
	 * @param allowUserSelfRegistration	The new value.
	 **/
	@Deprecated
	@XmlElement
	public void setAllowUserSelfRegistration(Boolean allowUserSelfRegistration) {
		preset(allowUserSelfRegistrationPropertyName, allowUserSelfRegistration);
		this.allowUserSelfRegistration = allowUserSelfRegistration;
	}

	/**
	 * {@link #passwordComplexityModel} accessor.
	 * @return	The value.
	 **/
	@Deprecated
	public PasswordComplexityModel getPasswordComplexityModel() {
		return passwordComplexityModel;
	}

	/**
	 * {@link #passwordComplexityModel} mutator.
	 * @param passwordComplexityModel	The new value.
	 **/
	@Deprecated
	@XmlElement
	public void setPasswordComplexityModel(PasswordComplexityModel passwordComplexityModel) {
		preset(passwordComplexityModelPropertyName, passwordComplexityModel);
		this.passwordComplexityModel = passwordComplexityModel;
	}

	/**
	 * {@link #publicUser} accessor.
	 * @return	The value.
	 **/
	public UserProxyExtension getPublicUser() {
		return publicUser;
	}

	/**
	 * {@link #publicUser} mutator.
	 * @param publicUser	The new value.
	 **/
	@XmlElement
	public void setPublicUser(UserProxyExtension publicUser) {
		if (this.publicUser != publicUser) {
			preset(publicUserPropertyName, publicUser);
			this.publicUser = publicUser;
		}
	}

	/**
	 * {@link #emailFrom} accessor.
	 * @return	The value.
	 **/
	public String getEmailFrom() {
		return emailFrom;
	}

	/**
	 * {@link #emailFrom} mutator.
	 * @param emailFrom	The new value.
	 **/
	@XmlElement
	public void setEmailFrom(String emailFrom) {
		preset(emailFromPropertyName, emailFrom);
		this.emailFrom = emailFrom;
	}

	/**
	 * {@link #emailTo} accessor.
	 * @return	The value.
	 **/
	public String getEmailTo() {
		return emailTo;
	}

	/**
	 * {@link #emailTo} mutator.
	 * @param emailTo	The new value.
	 **/
	@XmlElement
	public void setEmailTo(String emailTo) {
		preset(emailToPropertyName, emailTo);
		this.emailTo = emailTo;
	}

	/**
	 * {@link #emailSubject} accessor.
	 * @return	The value.
	 **/
	public String getEmailSubject() {
		return emailSubject;
	}

	/**
	 * {@link #emailSubject} mutator.
	 * @param emailSubject	The new value.
	 **/
	@XmlElement
	public void setEmailSubject(String emailSubject) {
		preset(emailSubjectPropertyName, emailSubject);
		this.emailSubject = emailSubject;
	}

	/**
	 * {@link #emailContent} accessor.
	 * @return	The value.
	 **/
	public String getEmailContent() {
		return emailContent;
	}

	/**
	 * {@link #emailContent} mutator.
	 * @param emailContent	The new value.
	 **/
	@XmlElement
	public void setEmailContent(String emailContent) {
		preset(emailContentPropertyName, emailContent);
		this.emailContent = emailContent;
	}

	/**
	 * {@link #passwordExpiryDays} accessor.
	 * @return	The value.
	 **/
	public String getPasswordExpiryDays() {
		return passwordExpiryDays;
	}

	/**
	 * {@link #passwordExpiryDays} mutator.
	 * @param passwordExpiryDays	The new value.
	 **/
	@XmlElement
	public void setPasswordExpiryDays(String passwordExpiryDays) {
		this.passwordExpiryDays = passwordExpiryDays;
	}

	/**
	 * {@link #passwordHistoryRetention} accessor.
	 * @return	The value.
	 **/
	public String getPasswordHistoryRetention() {
		return passwordHistoryRetention;
	}

	/**
	 * {@link #passwordHistoryRetention} mutator.
	 * @param passwordHistoryRetention	The new value.
	 **/
	@XmlElement
	public void setPasswordHistoryRetention(String passwordHistoryRetention) {
		this.passwordHistoryRetention = passwordHistoryRetention;
	}

	/**
	 * {@link #passwordAccountLockoutThreshold} accessor.
	 * @return	The value.
	 **/
	public String getPasswordAccountLockoutThreshold() {
		return passwordAccountLockoutThreshold;
	}

	/**
	 * {@link #passwordAccountLockoutThreshold} mutator.
	 * @param passwordAccountLockoutThreshold	The new value.
	 **/
	@XmlElement
	public void setPasswordAccountLockoutThreshold(String passwordAccountLockoutThreshold) {
		this.passwordAccountLockoutThreshold = passwordAccountLockoutThreshold;
	}

	/**
	 * {@link #passwordAccountLockoutDuration} accessor.
	 * @return	The value.
	 **/
	public String getPasswordAccountLockoutDuration() {
		return passwordAccountLockoutDuration;
	}

	/**
	 * {@link #passwordAccountLockoutDuration} mutator.
	 * @param passwordAccountLockoutDuration	The new value.
	 **/
	@XmlElement
	public void setPasswordAccountLockoutDuration(String passwordAccountLockoutDuration) {
		this.passwordAccountLockoutDuration = passwordAccountLockoutDuration;
	}

	/**
	 * {@link #emailToContact} accessor.
	 * @return	The value.
	 **/
	public Contact getEmailToContact() {
		return emailToContact;
	}

	/**
	 * {@link #emailToContact} mutator.
	 * @param emailToContact	The new value.
	 **/
	@XmlElement
	public void setEmailToContact(Contact emailToContact) {
		if (this.emailToContact != emailToContact) {
			preset(emailToContactPropertyName, emailToContact);
			this.emailToContact = emailToContact;
		}
	}

	/**
	 * {@link #startup} accessor.
	 * @return	The value.
	 **/
	public StartupExtension getStartup() {
		return startup;
	}

	/**
	 * {@link #startup} mutator.
	 * @param startup	The new value.
	 **/
	@XmlElement
	public void setStartup(StartupExtension startup) {
		if (this.startup != startup) {
			preset(startupPropertyName, startup);
			this.startup = startup;
		}
	}

	/**
	 * backupsConfigured
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isBackupsConfigured() {
		return (modules.admin.Configuration.ConfigurationExtension.validBackupConfiguration());
	}

	/**
	 * {@link #isBackupsConfigured} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotBackupsConfigured() {
		return (! isBackupsConfigured());
	}

	/**
	 * emailConfigured
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isEmailConfigured() {
		return (modules.admin.Configuration.ConfigurationExtension.validSMTPHost());
	}

	/**
	 * {@link #isEmailConfigured} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotEmailConfigured() {
		return (! isEmailConfigured());
	}

	/**
	 * True when the selected startup map type is Google Maps
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isMapTypeGmap() {
		return (getStartup() != null && getStartup().isMapTypeGmap());
	}

	/**
	 * {@link #isMapTypeGmap} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotMapTypeGmap() {
		return (! isMapTypeGmap());
	}

	/**
	 * selfRegistrationConfiguredEmailOrGroupNotConfigured
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isSelfRegistrationConfiguredEmailOrGroupNotConfigured() {
		return (startup.getAccountAllowUserSelfRegistration().equals(Boolean.TRUE) &&
					(!modules.admin.Configuration.ConfigurationExtension.validSMTPHost() || userSelfRegistrationGroup == null));
	}

	/**
	 * {@link #isSelfRegistrationConfiguredEmailOrGroupNotConfigured} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotSelfRegistrationConfiguredEmailOrGroupNotConfigured() {
		return (! isSelfRegistrationConfiguredEmailOrGroupNotConfigured());
	}

	/**
	 * True when this application has a default customer specified (is single tenant)
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isSingleTenant() {
		return (getStartup() != null && getStartup().isSingleTenant());
	}

	/**
	 * {@link #isSingleTenant} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotSingleTenant() {
		return (! isSingleTenant());
	}
}
