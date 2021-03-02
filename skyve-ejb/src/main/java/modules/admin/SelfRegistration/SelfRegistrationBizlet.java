package modules.admin.SelfRegistration;

import org.skyve.CORE;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Util;

import modules.admin.domain.User;

public class SelfRegistrationBizlet extends Bizlet<SelfRegistrationExtension> {

	private static final long serialVersionUID = -3270121906624275634L;
	
	@Override
	public SelfRegistrationExtension newInstance(SelfRegistrationExtension bean) throws Exception {
		bean.setUser(User.newInstance());
		return super.newInstance(bean);
	}
	
	
	@Override
	public void validate(SelfRegistrationExtension bean, ValidationException e) throws Exception {
		// validate that this email address hasn't already been registered (prompts to login)
		validateUniqueEmail(bean.getUser().getContact().getEmail1());
		
		super.validate(bean, e);
	}
	
	/**
	 * Checks that a {@link User} does not already exist in the system with the
	 * same username (email address) as the one trying to be registered. Throws
	 * a {@link ValidationException} if it has already been registered.
	 */
	private static void validateUniqueEmail(final String emailAddress) {
		Persistence pers = CORE.getPersistence();
		DocumentQuery q = pers.newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME);
		q.getFilter().addEquals(User.userNamePropertyName, emailAddress);
		User existingUser = q.beanResult();
		
		if (existingUser != null) {
			final String baseHref = Util.getSkyveContextUrl() + '/';
			final String loginUrl = baseHref;
			
			final String requestPasswordResetUrl = baseHref + "pages/requestPasswordReset.jsp";
			final String resendRegistrationEmailUrl = baseHref + "pages/resendRegistrationEmail.jsp?userId="
					+ existingUser.getBizId();

			// if the user exists but is not activated, prompt to resend the activation email
			if (Boolean.FALSE.equals(existingUser.getActivated())) {
				throw new ValidationException(
						new Message(
								String.format("This email address has already been registered but not yet activated. " +
										"Please activate then login <a href=\"%s\" title=\"Login\">here</a> " +
										"or <a href=\"%s\" title=\"Resend activation\">resend your activation email</a>.",
										loginUrl,
										resendRegistrationEmailUrl)));
			}

			// if the user exists and is activated, prompt to login or reset password
			throw new ValidationException(
					new Message(
							String.format("This email address has already been registered. " + 
											"Please login <a href=\"%s\" title=\"Login\">here</a> " +
									"or <a href=\"%s\" title=\"Reset password\">reset your password</a>.",
									loginUrl,
									requestPasswordResetUrl)));
		}
	}
}
