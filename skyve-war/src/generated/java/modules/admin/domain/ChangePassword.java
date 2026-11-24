package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractTransientBean;

/**
 * Change Password
 * <br/>
 * Provides a secure form for users to change their passwords. Supports authenticated changes (logged-in users providing their current password) and self-service resets (when users reset password from resetPassword.jsp and cannot provide their old password).
 * 
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class ChangePassword extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "ChangePassword";

	/** @hidden */
	public static final String oldPasswordPropertyName = "oldPassword";

	/** @hidden */
	public static final String newPasswordPropertyName = "newPassword";

	/** @hidden */
	public static final String confirmPasswordPropertyName = "confirmPassword";

	/** @hidden */
	public static final String responsePropertyName = "response";

	/**
	 * Old Password
	 * <br/>
	 * Enter your old password
	 * <br/>
	 * The old password is required, and must be right, for every password change except a reset by token
					(resetPassword.jsp), which cannot ask for it. The action enforces this: the reset path marks itself
					in the persistence stash (AppConstants.PASSWORD_RESET_STASH_KEY) and nothing else can skip the check.
					The attribute is not marked required here only because the reset path leaves it empty;
					the edit view marks it required.
	 **/
	private String oldPassword;

	/**
	 * New Password
	 * <br/>
	 * New Password
	 **/
	private String newPassword;

	/**
	 * Confirm Password
	 * <br/>
	 * Re-enter your new password
	 **/
	private String confirmPassword;

	/**
	 * Response
	 **/
	private String response;

	@Override
	@XmlTransient
	public String getBizModule() {
		return ChangePassword.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return ChangePassword.DOCUMENT_NAME;
	}

	public static ChangePassword newInstance() {
		try {
			return AdminDocument.CHANGE_PASSWORD.toDocument(CORE.getUser().getCustomer()).newInstance(CORE.getUser());
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
			return org.skyve.util.Binder.formatMessage("Change Password", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	/**
	 * {@link #oldPassword} accessor.
	 * @return	The value.
	 **/
	public String getOldPassword() {
		return oldPassword;
	}

	/**
	 * {@link #oldPassword} mutator.
	 * @param oldPassword	The new value.
	 **/
	@XmlElement
	public void setOldPassword(String oldPassword) {
		preset(oldPasswordPropertyName, oldPassword);
		this.oldPassword = oldPassword;
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
	 * {@link #response} accessor.
	 * @return	The value.
	 **/
	public String getResponse() {
		return response;
	}

	/**
	 * {@link #response} mutator.
	 * @param response	The new value.
	 **/
	@XmlElement
	public void setResponse(String response) {
		preset(responsePropertyName, response);
		this.response = response;
	}

	/**
	 * passwordChanged
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isPasswordChanged() {
		return (response != null);
	}

	/**
	 * {@link #isPasswordChanged} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotPasswordChanged() {
		return (! isPasswordChanged());
	}
}
