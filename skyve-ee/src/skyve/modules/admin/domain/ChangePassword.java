package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.impl.domain.AbstractTransientBean;

/**
 * Change Password
 * 
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
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
	public static final String newPasswordPropertyName = "newPassword";
	/** @hidden */
	public static final String confirmPasswordPropertyName = "confirmPassword";
	/** @hidden */
	public static final String responsePropertyName = "response";

	/**
	 * New Password
	 * <br/>
	 * Enter your new password
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

	public static ChangePassword newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof ChangePassword) && 
					this.getBizId().equals(((ChangePassword) o).getBizId()));
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
