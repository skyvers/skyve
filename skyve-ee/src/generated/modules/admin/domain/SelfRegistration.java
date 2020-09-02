package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import modules.admin.Configuration.ConfigurationExtension;
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
 * @navhas n configuration 1 Configuration
 * @navhas n user 1 User
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
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
	public static final String confirmPasswordPropertyName = "confirmPassword";
	/** @hidden */
	public static final String loginUrlPropertyName = "loginUrl";
	/** @hidden */
	public static final String loginMessagePropertyName = "loginMessage";
	/** @hidden */
	public static final String registrationEmailWasSentPropertyName = "registrationEmailWasSent";
	/** @hidden */
	public static final String registrationDatePropertyName = "registrationDate";
	/** @hidden */
	public static final String registrationQuestionPropertyName = "registrationQuestion";
	/** @hidden */
	public static final String registrationQuestionSubmittedPropertyName = "registrationQuestionSubmitted";
	/** @hidden */
	public static final String configurationPropertyName = "configuration";
	/** @hidden */
	public static final String userPropertyName = "user";

	/**
	 * Activation Url
	 **/
	private String activateUrl;
	/**
	 * Confirm Password
	 * <br/>
	 * Confirm password checked during user registration to bypass change password
                validation in the UserBizlet.
	 **/
	private String confirmPassword;
	/**
	 * Login Url
	 **/
	private String loginUrl;
	/**
	 * Login Message
	 * <br/>
	 * Transient field to allow login message to be overridden
	 **/
	private String loginMessage;
	/**
	 * Registration Email Was Sent
	 * <br/>
	 * Indicates if an email was sent to the public user or not.
	 **/
	private Boolean registrationEmailWasSent;
	/**
	 * Registration Date
	 * <br/>
	 * The date and time that this self registration was completed.
	 **/
	private DateTime registrationDate;
	/**
	 * What led you to search for and try Skyve Foundry today?
	 * <br/>
	 * Post-registration sign-up question to new users
	 **/
	private String registrationQuestion;
	/**
	 * Submitted
	 * <br/>
	 * True when the user has submitted a response
	 **/
	private Boolean registrationQuestionSubmitted = new Boolean(false);
	/**
	 * Configuration
	 * <br/>
	 * Settings for sending registration emails.
	 **/
	private ConfigurationExtension configuration = null;
	/**
	 * User
	 * <br/>
	 * The new user to register.
	 **/
	private UserExtension user = null;

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
			return org.skyve.util.Binder.formatMessage(org.skyve.CORE.getUser().getCustomer(),
														"{user}",
														this);
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
	 * {@link #registrationEmailWasSent} accessor.
	 * @return	The value.
	 **/
	public Boolean getRegistrationEmailWasSent() {
		return registrationEmailWasSent;
	}

	/**
	 * {@link #registrationEmailWasSent} mutator.
	 * @param registrationEmailWasSent	The new value.
	 **/
	@XmlElement
	public void setRegistrationEmailWasSent(Boolean registrationEmailWasSent) {
		preset(registrationEmailWasSentPropertyName, registrationEmailWasSent);
		this.registrationEmailWasSent = registrationEmailWasSent;
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
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	@XmlElement
	public void setRegistrationDate(DateTime registrationDate) {
		preset(registrationDatePropertyName, registrationDate);
		this.registrationDate = registrationDate;
	}

	/**
	 * {@link #registrationQuestion} accessor.
	 * @return	The value.
	 **/
	public String getRegistrationQuestion() {
		return registrationQuestion;
	}

	/**
	 * {@link #registrationQuestion} mutator.
	 * @param registrationQuestion	The new value.
	 **/
	@XmlElement
	public void setRegistrationQuestion(String registrationQuestion) {
		preset(registrationQuestionPropertyName, registrationQuestion);
		this.registrationQuestion = registrationQuestion;
	}

	/**
	 * {@link #registrationQuestionSubmitted} accessor.
	 * @return	The value.
	 **/
	public Boolean getRegistrationQuestionSubmitted() {
		return registrationQuestionSubmitted;
	}

	/**
	 * {@link #registrationQuestionSubmitted} mutator.
	 * @param registrationQuestionSubmitted	The new value.
	 **/
	@XmlElement
	public void setRegistrationQuestionSubmitted(Boolean registrationQuestionSubmitted) {
		preset(registrationQuestionSubmittedPropertyName, registrationQuestionSubmitted);
		this.registrationQuestionSubmitted = registrationQuestionSubmitted;
	}

	/**
	 * {@link #configuration} accessor.
	 * @return	The value.
	 **/
	public ConfigurationExtension getConfiguration() {
		return configuration;
	}

	/**
	 * {@link #configuration} mutator.
	 * @param configuration	The new value.
	 **/
	@XmlElement
	public void setConfiguration(ConfigurationExtension configuration) {
		if (this.configuration != configuration) {
			preset(configurationPropertyName, configuration);
			this.configuration = configuration;
		}
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
	 * Whether or not the registration process has finished.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isRegistrationComplete() {
		return (getUser() != null && getUser().isPersisted());
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
		return (((java.util.function.BooleanSupplier) () -> {
						throw new RuntimeException("Implemented in extension class.");
					}).getAsBoolean());
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
	 * True when the user has submitted a response to the registration question.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isSubmittedReponse() {
		return (Boolean.TRUE.equals(getRegistrationQuestionSubmitted()));
	}

	/**
	 * {@link #isSubmittedReponse} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotSubmittedReponse() {
		return (! isSubmittedReponse());
	}
}
