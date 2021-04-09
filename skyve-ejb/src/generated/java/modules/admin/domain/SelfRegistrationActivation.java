package modules.admin.domain;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import modules.admin.SelfRegistrationActivation.SelfRegistrationActivationExtension;
import modules.admin.User.UserExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

/**
 * Self Registration Activation
 * 
 * @depend - - - Result
 * @navhas n user 0..1 User
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
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

	/**
	 * Activation Result
	 **/
	@XmlEnum
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
		private static List<DomainValue> domainValues;

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
		public String toDescription() {
			return description;
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

		public static Result fromDescription(String description) {
			Result result = null;

			for (Result value : values()) {
				if (value.description.equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				Result[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (Result value : values) {
					domainValues.add(value.domainValue);
				}
			}

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
