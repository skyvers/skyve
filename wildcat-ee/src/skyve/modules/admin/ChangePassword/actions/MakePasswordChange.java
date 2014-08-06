package modules.admin.ChangePassword.actions;

import java.security.MessageDigest;

import modules.admin.AdminUtil;
import modules.admin.domain.ChangePassword;
import modules.admin.domain.Configuration;
import modules.admin.domain.Configuration.PasswordComplexityModel;

import org.apache.commons.codec.binary.Base64;

import org.skyve.wildcat.util.UtilImpl;
import org.skyve.CORE;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.types.DateTime;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.util.BeanValidator;
import org.skyve.web.WebContext;

public class MakePasswordChange implements ServerSideAction<ChangePassword> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -4317908281075686229L;

	@Override
	public ServerSideActionResult execute(ChangePassword bean, WebContext webContext) throws Exception {
		Persistence persistence = CORE.getPersistence();
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(ChangePassword.MODULE_NAME);
		Document changePasswordDocument = module.getDocument(customer, ChangePassword.DOCUMENT_NAME);
		BeanValidator.validateBeanAgainstDocument(changePasswordDocument, bean);

		String newPassword = bean.getNewPassword();
		String confirmPassword = bean.getConfirmPassword();

		// check for suitable complexity
		Configuration configuration = AdminUtil.getConfiguration();
		PasswordComplexityModel cm = configuration.getPasswordComplexityModel();

		if (!AdminUtil.validatePasswordComplexity(newPassword, cm)) {
			StringBuilder sb = new StringBuilder("The password you have entered is not sufficiently complex.\n");
			sb.append(AdminUtil.validatePasswordComplexityMessage(cm));
			sb.append("\nPlease re-enter and confirm the password.");

			Message message = new Message(ChangePassword.newPasswordPropertyName, sb.toString());
			throw new ValidationException(message);
		}

		if (! newPassword.equals(confirmPassword)) { // these 2 are mandatory in the document
			Message message = new Message(ChangePassword.newPasswordPropertyName,
					"You did not type the same password.  Please re-enter and confirm the password.");
			message.addBinding(ChangePassword.confirmPasswordPropertyName);
			throw new ValidationException(message);
		}

		Document userDocument = module.getDocument(customer, modules.admin.domain.User.DOCUMENT_NAME);
		modules.admin.domain.User userBean = persistence.retrieve(userDocument, user.getId(), true);

		MessageDigest md = MessageDigest.getInstance(UtilImpl.PASSWORD_HASHING_ALGORITHM);
		Base64 base64Codec = new Base64();
		String hashedPassword = new String(base64Codec.encode(md.digest(newPassword.getBytes())));

		userBean.setPassword(hashedPassword);

		// clear reset password details
		userBean.setPasswordExpired(Boolean.FALSE);
		userBean.setClearTextPassword(null);
		userBean.setPasswordLastChanged(new DateTime());

		userBean = persistence.save(userDocument, userBean);

		// clear out the passwords since the change was successful
		bean.setNewPassword(null);
		bean.setConfirmPassword(null);

		bean.setResponse("Your password has been changed.");

		return new ServerSideActionResult(bean); // stay on the same form
	}
}
