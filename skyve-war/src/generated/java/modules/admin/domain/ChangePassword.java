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
	 * The old password is not always required to change the password as the
					self service reset password function cannot demand the user for their old password.
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
			return org.skyve.util.Binder.formatMessage("Change Password", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof ChangePassword) && 
					this.getBizId().equals(((ChangePassword) o).getBizId()));
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
}
