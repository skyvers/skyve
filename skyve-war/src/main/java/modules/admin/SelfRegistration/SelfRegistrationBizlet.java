package modules.admin.SelfRegistration;

import org.skyve.CORE;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.persistence.DocumentQuery;
import org.skyve.util.Util;

import modules.admin.User.UserExtension;
import modules.admin.domain.User;

public class SelfRegistrationBizlet extends Bizlet<SelfRegistrationExtension> {

	@Override
	public SelfRegistrationExtension newInstance(SelfRegistrationExtension bean) throws Exception {
		bean.setUser(User.newInstance());
		return super.newInstance(bean);
	}
	
	@Override
	public void validate(SelfRegistrationExtension bean, ValidationException e) throws Exception {
		// validate that this email address hasn't already been registered (prompts to login)
		validateUniqueEmail(bean.getUser().getContact().getEmail1(), e);
		
		super.validate(bean, e);
	}
	
	/**
	 * Checks that a {@link User} does not already exist in the system with the same username
	 * (email address) as the one trying to be registered.
	 * 
	 * @throws A {@link ValidationException} if the email address has already been registered.
	 */
	private static void validateUniqueEmail(final String emailAddress, final ValidationException e) {
		CORE.getPersistence().withDocumentPermissionScopes(DocumentPermissionScope.customer, p -> {
			DocumentQuery qUnique = CORE.getPersistence().newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME);
			qUnique.getFilter().addEquals(User.userNamePropertyName, emailAddress);
			UserExtension existingUser = qUnique.beanResult();

			if (existingUser != null) {
				final String baseHref = Util.getBaseUrl();
				final String loginUrl = baseHref;

				final String requestPasswordResetUrl = baseHref + "pages/requestPasswordReset.jsp";
				final String resendRegistrationEmailUrl = baseHref + "pages/resendRegistrationEmail.jsp?userId="
						+ existingUser.getBizId();

				// if the user exists but is not activated, prompt to resend the activation email
				if (Boolean.FALSE.equals(existingUser.getActivated())) {
					e.getMessages().add(
							new Message(
									// keep the message to user vague as we can't allow them to use this function to find out
									// if someone else has registered
									String.format("You cannot register with this email at this time. " +
											"Please activate then login <a href=\"%s\" title=\"Login\">here</a> " +
											"or <a href=\"%s\" title=\"Resend activation\">resend your activation email</a>.",
											loginUrl,
											resendRegistrationEmailUrl)));
					return;
				}

				// if the user exists and is activated, prompt to login or reset password
				e.getMessages().add(
						new Message(
								// keep the message to user vague as we can't allow them to use this function to find out
								// if someone else has registered
								String.format("You cannot register with this email at this time. " +
										"Please login <a href=\"%s\" title=\"Login\">here</a> " +
										"or <a href=\"%s\" title=\"Reset password\">reset your password</a>.",
										loginUrl,
										requestPasswordResetUrl)));
			}
		});
	}
}
