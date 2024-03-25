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
import modules.admin.SelfRegistrationActivation.SelfRegistrationActivationExtension;
import modules.admin.User.UserExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

/**
 * Self Registration Activation
 * 
 * @depend - - - Result
 * @navhas n user 0..1 User
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public abstract class SelfRegistrationActivation extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "SelfRegistrationActivation";

	/** @hidden */
	public static final String resultPropertyName = "result";

	/** @hidden */
	public static final String userPropertyName = "user";

	/** @hidden */
	public static final String loginUrlPropertyName = "loginUrl";

	/** @hidden */
	public static final String pleaseSignInPropertyName = "pleaseSignIn";

	/** @hidden */
	public static final String signInLinkPropertyName = "signInLink";

	/** @hidden */
	public static final String alreadyActivatedPropertyName = "alreadyActivated";

	/** @hidden */
	public static final String noLongerValidPropertyName = "noLongerValid";

	/** @hidden */
	public static final String notRecognisedPropertyName = "notRecognised";

	/**
	 * Activation Result
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum Result implements Enumeration {
		SUCCESS("SUCCESS", "SUCCESS"),
		ALREADYACTIVATED("ALREADY_ACTIVATED", "ALREADY_ACTIVATED"),
		EXPIRED("EXPIRED", "EXPIRED"),
		FAILURE("FAILURE", "FAILURE");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(Result::toDomainValue).collect(Collectors.toUnmodifiableList());

		private Result(String code, String description) {
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

		public static Result fromCode(String code) {
			Result result = null;

			for (Result value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static Result fromLocalisedDescription(String description) {
			Result result = null;

			for (Result value : values()) {
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
	 * Activation Result
	 **/
	private Result result;

	/**
	 * User
	 * <br/>
	 * The activated user.
	 **/
	private UserExtension user = null;

	/**
	 * Login Url
	 **/
	private String loginUrl;

	/**
	 * Please sign in message
	 * <br/>
	 * <p>Congratulations {0}! Your account is now active.</p><p>Please <a href="{1}">Sign in</a> with the email address {2}.</p>
	 **/
	private String pleaseSignIn;

	/**
	 * Sign in link
	 * <br/>
	 * <a href="{0}">Sign in</a>
	 **/
	private String signInLink;

	/**
	 * Account already activated
	 * <br/>
	 * <p>Welcome {0} - you have already activated your account.</p><p>Please <a href="{1}">Sign in</a>.</p>
	 **/
	private String alreadyActivated;

	/**
	 * No longer valid
	 * <br/>
	 * <p>Sorry, that code is no longer valid.</p><p>Please <a href="{0}">Sign in</a> or Register again to request a new activation email.</p>
	 **/
	private String noLongerValid;

	/**
	 * Not recognised
	 * <br/>
	 * <p>Sorry, that link is not recognised. Please check the link and try again.</p><p>Return to <a href="{0}">Sign in</a>.</p>
	 **/
	private String notRecognised;

	@Override
	@XmlTransient
	public String getBizModule() {
		return SelfRegistrationActivation.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return SelfRegistrationActivation.DOCUMENT_NAME;
	}

	public static SelfRegistrationActivationExtension newInstance() {
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
			return org.skyve.util.Binder.formatMessage("Activation", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof SelfRegistrationActivation) && 
					this.getBizId().equals(((SelfRegistrationActivation) o).getBizId()));
	}

	/**
	 * {@link #result} accessor.
	 * @return	The value.
	 **/
	public Result getResult() {
		return result;
	}

	/**
	 * {@link #result} mutator.
	 * @param result	The new value.
	 **/
	@XmlElement
	public void setResult(Result result) {
		preset(resultPropertyName, result);
		this.result = result;
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
	 * {@link #pleaseSignIn} accessor.
	 * @return	The value.
	 **/
	public String getPleaseSignIn() {
		return pleaseSignIn;
	}

	/**
	 * {@link #pleaseSignIn} mutator.
	 * @param pleaseSignIn	The new value.
	 **/
	@XmlElement
	public void setPleaseSignIn(String pleaseSignIn) {
		preset(pleaseSignInPropertyName, pleaseSignIn);
		this.pleaseSignIn = pleaseSignIn;
	}

	/**
	 * {@link #signInLink} accessor.
	 * @return	The value.
	 **/
	public String getSignInLink() {
		return signInLink;
	}

	/**
	 * {@link #signInLink} mutator.
	 * @param signInLink	The new value.
	 **/
	@XmlElement
	public void setSignInLink(String signInLink) {
		preset(signInLinkPropertyName, signInLink);
		this.signInLink = signInLink;
	}

	/**
	 * {@link #alreadyActivated} accessor.
	 * @return	The value.
	 **/
	public String getAlreadyActivated() {
		return alreadyActivated;
	}

	/**
	 * {@link #alreadyActivated} mutator.
	 * @param alreadyActivated	The new value.
	 **/
	@XmlElement
	public void setAlreadyActivated(String alreadyActivated) {
		preset(alreadyActivatedPropertyName, alreadyActivated);
		this.alreadyActivated = alreadyActivated;
	}

	/**
	 * {@link #noLongerValid} accessor.
	 * @return	The value.
	 **/
	public String getNoLongerValid() {
		return noLongerValid;
	}

	/**
	 * {@link #noLongerValid} mutator.
	 * @param noLongerValid	The new value.
	 **/
	@XmlElement
	public void setNoLongerValid(String noLongerValid) {
		preset(noLongerValidPropertyName, noLongerValid);
		this.noLongerValid = noLongerValid;
	}

	/**
	 * {@link #notRecognised} accessor.
	 * @return	The value.
	 **/
	public String getNotRecognised() {
		return notRecognised;
	}

	/**
	 * {@link #notRecognised} mutator.
	 * @param notRecognised	The new value.
	 **/
	@XmlElement
	public void setNotRecognised(String notRecognised) {
		preset(notRecognisedPropertyName, notRecognised);
		this.notRecognised = notRecognised;
	}

	/**
	 * showAlready
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowAlready() {
		return (result==Result.ALREADYACTIVATED);
	}

	/**
	 * {@link #isShowAlready} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowAlready() {
		return (! isShowAlready());
	}

	/**
	 * showExpired
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowExpired() {
		return (result==Result.EXPIRED);
	}

	/**
	 * {@link #isShowExpired} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowExpired() {
		return (! isShowExpired());
	}

	/**
	 * showFailure
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowFailure() {
		return (result==Result.FAILURE);
	}

	/**
	 * {@link #isShowFailure} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowFailure() {
		return (! isShowFailure());
	}

	/**
	 * showSuccess
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowSuccess() {
		return (result==Result.SUCCESS);
	}

	/**
	 * {@link #isShowSuccess} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowSuccess() {
		return (! isShowSuccess());
	}
}
