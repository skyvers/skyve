package modules.admin.SelfRegistration;

import java.util.List;

import org.h2.util.StringUtils;
import org.skyve.CORE;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Util;

import modules.admin.domain.Contact;
import modules.admin.domain.User;

public class SelfRegistrationBizlet extends Bizlet<SelfRegistrationExtension> {

	private static final long serialVersionUID = -3270121906624275634L;

	public static void generateActivationLink(SelfRegistrationExtension bean) {
		StringBuilder urlBuilder = new StringBuilder(Util.getSkyveContextUrl());
		urlBuilder.append("/?a=e&m=admin&d=SelfRegistrationActivation&code=");
		urlBuilder.append(bean.getUser().getActivationCode());
		bean.setActivateUrl(urlBuilder.toString());
	}
	
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
	 * Validates that a mobile phone or local or international telephone 
	 * number has been entered during the registration, at least 1 is required.
	 * @param contact The contact to validate the phone numbers of
	 * @throws {@link ValidationException} if both fields are blank
	 */
	private void validateMobileOrTelephone(Contact contact) {
		if (contact == null || (StringUtils.isNullOrEmpty(contact.getMobile()))) {
			throw new ValidationException(
					new Message(Contact.mobilePropertyName,
							"A mobile number is required"));
		}
	}
	
	/**
	 * Checks that a {@link User} does not already exist in the system with the
	 * same username (email address) as the one trying to be registered. Throws
	 * a {@link ValidationException} if it has already been registered.
	 */
	private void validateUniqueEmail(final String emailAddress) {
		Persistence pers = CORE.getPersistence();
		DocumentQuery q = pers.newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME);
		q.getFilter().addEquals(User.userNamePropertyName, emailAddress);
		List<User> otherUsers = q.beanResults();
		
		if (!otherUsers.isEmpty()) {
			final String baseHref = Util.getSkyveContextUrl() + '/';
			final String loginUrl = baseHref;
			
			final String requestPasswordResetUrl = baseHref + "pages/requestPasswordReset.jsp";
			throw new ValidationException(
					new Message(
							String.format("This email address has already been registered. " + 
											"Please login <a href=\"%s\" title=\"Login\">here</a> " +
											"or <a href=\"%s\" title=\"Reset password\">reset your password</a>",
									loginUrl,
									requestPasswordResetUrl)));
		}
	}
}
