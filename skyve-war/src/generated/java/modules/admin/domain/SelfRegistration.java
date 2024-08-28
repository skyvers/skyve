package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import modules.admin.SelfRegistration.SelfRegistrationExtension;
import modules.admin.User.UserExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateTime;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.impl.domain.types.jaxb.DateTimeMapper;

/**
 * Self Registration
 * 
 * @navhas n user 1 User
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public abstract class SelfRegistration extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "SelfRegistration";

	/** @hidden */
	public static final String activateUrlPropertyName = "activateUrl";

	/** @hidden */
	public static final String confirmEmailPropertyName = "confirmEmail";

	/** @hidden */
	public static final String confirmPasswordPropertyName = "confirmPassword";

	/** @hidden */
	public static final String loginUrlPropertyName = "loginUrl";

	/** @hidden */
	public static final String loginMessagePropertyName = "loginMessage";

	/** @hidden */
	public static final String registrationDatePropertyName = "registrationDate";

	/** @hidden */
	public static final String passSilentlyPropertyName = "passSilently";

	/** @hidden */
	public static final String userPropertyName = "user";

	/** @hidden */
	public static final String previouslyAttemptedPasswordPropertyName = "previouslyAttemptedPassword";

	/**
	 * Activation Url
	 **/
	private String activateUrl;

	/**
	 * Confirm Email
	 **/
	private String confirmEmail;

	/**
	 * Confirm Password
	 * <br/>
	 * Confirm password checked during user registration to bypass change password
                validation in the UserBizlet.
	 **/
	private String confirmPassword;

	/**
	 * Sign In URL
	 **/
	private String loginUrl;

	/**
	 * Sign In Message
	 * <br/>
	 * Transient field to allow login message to be overridden
	 **/
	private String loginMessage;

	/**
	 * Registration Date
	 * <br/>
	 * The date and time that this self registration was completed.
	 **/
	private DateTime registrationDate;

	/**
	 * Silent Pass
	 * <br/>
	 * Flag used when registration should pass but not actually create a user. 
				Used for suspected bot registrations.
	 **/
	private Boolean passSilently = Boolean.valueOf(false);

	/**
	 * User
	 * <br/>
	 * The new user to create for this registration
	 **/
	private UserExtension user = null;

	/**
	 * Previously Attempted Password
	 * <br/>
	 * If checks for breached passwords are enabled, this field is used to track whether a warning has been displayed for this password.
				A user may use a 'breached' password once they confirm by re-submitting.
				Implementation is similar to that in changePassword.jsp & resetPassword.jsp.
	 **/
	private String previouslyAttemptedPassword;

	@Override
	@XmlTransient
	public String getBizModule() {
		return SelfRegistration.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return SelfRegistration.DOCUMENT_NAME;
	}

	public static SelfRegistrationExtension newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{user}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof SelfRegistration) && 
					this.getBizId().equals(((SelfRegistration) o).getBizId()));
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
	 * {@link #confirmEmail} accessor.
	 * @return	The value.
	 **/
	public String getConfirmEmail() {
		return confirmEmail;
	}

	/**
	 * {@link #confirmEmail} mutator.
	 * @param confirmEmail	The new value.
	 **/
	@XmlElement
	public void setConfirmEmail(String confirmEmail) {
		preset(confirmEmailPropertyName, confirmEmail);
		this.confirmEmail = confirmEmail;
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
	 * {@link #loginUrl} accessor.
	 * @return	The value.
	 **/
	public String getLoginUrl() {
		return loginUrl;
	}

	/**
	 * {@link #loginUrl} mutator.
	 * @param loginUrl	The new value.
	 **/
	@XmlElement
	public void setLoginUrl(String loginUrl) {
		this.loginUrl = loginUrl;
	}

	/**
	 * {@link #loginMessage} accessor.
	 * @return	The value.
	 **/
	public String getLoginMessage() {
		return loginMessage;
	}

	/**
	 * {@link #loginMessage} mutator.
	 * @param loginMessage	The new value.
	 **/
	@XmlElement
	public void setLoginMessage(String loginMessage) {
		this.loginMessage = loginMessage;
	}

	/**
	 * {@link #registrationDate} accessor.
	 * @return	The value.
	 **/
	public DateTime getRegistrationDate() {
		return registrationDate;
	}

	/**
	 * {@link #registrationDate} mutator.
	 * @param registrationDate	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setRegistrationDate(DateTime registrationDate) {
		preset(registrationDatePropertyName, registrationDate);
		this.registrationDate = registrationDate;
	}

	/**
	 * {@link #passSilently} accessor.
	 * @return	The value.
	 **/
	public Boolean getPassSilently() {
		return passSilently;
	}

	/**
	 * {@link #passSilently} mutator.
	 * @param passSilently	The new value.
	 **/
	@XmlElement
	public void setPassSilently(Boolean passSilently) {
		this.passSilently = passSilently;
	}

	/**
	 * {@link #user} accessor.
	 * @return	The value.
	 **/
	public UserExtension getUser() {
		return user;
	}

	/**
	 * {@link #user} mutator.
	 * @param user	The new value.
	 **/
	@XmlElement
	public void setUser(UserExtension user) {
		if (this.user != user) {
			preset(userPropertyName, user);
			this.user = user;
		}
	}

	/**
	 * {@link #previouslyAttemptedPassword} accessor.
	 * @return	The value.
	 **/
	public String getPreviouslyAttemptedPassword() {
		return previouslyAttemptedPassword;
	}

	/**
	 * {@link #previouslyAttemptedPassword} mutator.
	 * @param previouslyAttemptedPassword	The new value.
	 **/
	@XmlElement
	public void setPreviouslyAttemptedPassword(String previouslyAttemptedPassword) {
		this.previouslyAttemptedPassword = previouslyAttemptedPassword;
	}

	/**
	 * True if the entered confirm email address is invalid (does not conform to the regular expression).
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isConfirmEmailInvalid() {
		return (((SelfRegistrationExtension)this).confirmEmailInvalid());
	}

	/**
	 * {@link #isConfirmEmailInvalid} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotConfirmEmailInvalid() {
		return (! isConfirmEmailInvalid());
	}

	/**
	 * True if the entered email address is invalid (does not conform to the regular expression).
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isEmailInvalid() {
		return (((SelfRegistrationExtension)this).emailInvalid());
	}

	/**
	 * {@link #isEmailInvalid} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotEmailInvalid() {
		return (! isEmailInvalid());
	}

	/**
	 * Whether or not the registration process has finished.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isRegistrationComplete() {
		return (getUser() != null && getUser().isPersisted() || Boolean.TRUE.equals(passSilently));
	}

	/**
	 * {@link #isRegistrationComplete} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotRegistrationComplete() {
		return (! isRegistrationComplete());
	}

	/**
	 * Whether or not self registration is allowed.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isSelfRegistrationAllowed() {
		return (org.skyve.impl.util.UtilImpl.ACCOUNT_ALLOW_SELF_REGISTRATION);
	}

	/**
	 * {@link #isSelfRegistrationAllowed} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotSelfRegistrationAllowed() {
		return (! isSelfRegistrationAllowed());
	}

	/**
	 * Whether to show cloudflare turnstile.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowCloudflareTurnstile() {
		return (org.skyve.impl.util.UtilImpl.CLOUDFLARE_TURNSTILE_SITE_KEY != null);
	}

	/**
	 * {@link #isShowCloudflareTurnstile} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowCloudflareTurnstile() {
		return (! isShowCloudflareTurnstile());
	}

	/**
	 * Whether to show google recaptcha.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowGoogleRecaptcha() {
		return (org.skyve.impl.util.UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY != null);
	}

	/**
	 * {@link #isShowGoogleRecaptcha} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowGoogleRecaptcha() {
		return (! isShowGoogleRecaptcha());
	}
}
