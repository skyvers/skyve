package modules.admin.SelfRegistration;

import java.util.UUID;

import org.apache.commons.lang3.StringUtils;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateTime;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.Persistence;
import org.skyve.util.BeanVisitor;
import org.skyve.util.Binder;
import org.skyve.util.Util;

import modules.admin.Communication.CommunicationUtil;
import modules.admin.Communication.CommunicationUtil.ResponseMode;
import modules.admin.Configuration.ConfigurationExtension;
import modules.admin.User.UserExtension;
import modules.admin.domain.Configuration;
import modules.admin.domain.Contact;
import modules.admin.domain.SelfRegistration;
import modules.admin.domain.SelfRegistrationActivation;
import modules.admin.domain.User;

public class SelfRegistrationExtension extends SelfRegistration {

	private static final long serialVersionUID = 4437526012340020305L;

	public static final String SELF_REGISTRATION_COMMUNICATION = "SYSTEM Self Registration";
	public static final String SELF_REGISTRATION_SUBJECT = "Activate your account";
	public static final String SELF_REGISTRATION_HEADING = "Welcome!";
	public static final String SELF_REGISTRATION_BODY = String.format("<p>Hi <b>{%1$s}</b>,</p><br/>"
			+ "<p>Thank you for registering.</p>"
			+ "<p>To complete your account setup, please click the activation link below.</p>"
			+ "<p><a href=\"{%2$s}\">{%2$s}</a></p>"
			+ "<p>If you have any questions about your new account, contact us at <a href=\"mailto:%3$s\">%3$s</a>.</p>",
			Binder.createCompoundBinding(SelfRegistration.userPropertyName, User.contactPropertyName, Contact.namePropertyName),
			SelfRegistration.activateUrlPropertyName,
			Util.getSupportEmailAddress());

	static final String CONFIRM_PASSWORD_REQUIRED = "Confirm Password is required.";
	static final String PASSWORD_MISMATCH = "You did not type the same password.  Please re-enter the password again.";
	static final String PASSWORD_REQUIRED = "Password is required.";

	/**
	 * Generates the activation link for the email to send to the new user with the activation code.
	 */
	public void generateActivationLink() {
		StringBuilder urlBuilder = new StringBuilder();
		urlBuilder.append(Util.getDocumentUrl(SelfRegistrationActivation.MODULE_NAME, SelfRegistrationActivation.DOCUMENT_NAME))
				.append("&code=")
				.append(this.getUser().getActivationCode());
		setActivateUrl(urlBuilder.toString());
	}

	@Override
	public String getLoginUrl() {
		return Util.getSkyveContextUrl() + "/login";
	}

	@Override
	public String getLoginMessage() {
		return String.format("If you are an existing user, please <a href=\"%s\">Log in</a>", getLoginUrl());
	}

	/**
	 * Sends the activation email to the user who registered.
	 * 
	 * @throws Exception
	 */
	public void sendUserRegistrationEmail() throws Exception {
		Util.LOGGER.info("Sending registration email to " + this.getUser().getContact().getEmail1());
		CommunicationUtil.sendFailSafeSystemCommunication(SELF_REGISTRATION_COMMUNICATION,
				this.getUser().getContact().getEmail1(),
				null,
				SELF_REGISTRATION_SUBJECT,
				SELF_REGISTRATION_BODY,
				ResponseMode.EXPLICIT,
				null,
				this);
	}

	/**
	 * Validates that the password and confirmPassword entered during a SelfRegistration
	 * match. Throws a {@link ValidationException} if there are any problems.
	 * @throws Exception 
	 */
	public void validateConfirmPassword() {
		if (getUser() != null) {
			ValidationException ve = new ValidationException();
			if ((getUser().getPassword() == null) || StringUtils.isEmpty(getUser().getPassword())) {
				Message message = new Message(Binder.createCompoundBinding(SelfRegistration.userPropertyName, User.passwordPropertyName), PASSWORD_REQUIRED);
				ve.getMessages().add(message);
			}
			
			if ((getConfirmPassword() == null) || StringUtils.isEmpty(getConfirmPassword())) {
				Message message = new Message(SelfRegistration.confirmPasswordPropertyName, CONFIRM_PASSWORD_REQUIRED);
				ve.getMessages().add(message);
			}
			
			if (!ve.getMessages().isEmpty()) {
				throw ve;
			}
			
			// if both aren't null, check that they are the same
			if (!getUser().getPassword().equals(getConfirmPassword())) {
				Message message = new Message(SelfRegistration.confirmPasswordPropertyName, PASSWORD_MISMATCH);
				ve.getMessages().add(message);
			}
			
			if (!ve.getMessages().isEmpty()) {
				throw ve;
			}
			
			// if they are the same, check the password is suitably complex
			// check for suitable complexity
			ConfigurationExtension configuration = Configuration.newInstance();
			if (!configuration.meetsComplexity(getUser().getPassword())) {
				StringBuilder sb = new StringBuilder(64);
				sb.append("The password you have entered is not sufficiently complex.\n");
				sb.append(configuration.getPasswordRuleDescription());
				sb.append("\nPlease re-enter and confirm the password.");
				Message message = new Message(Binder.createCompoundBinding(SelfRegistration.userPropertyName, User.passwordPropertyName), sb.toString());
				ve.getMessages().add(message);
			}
			
			if(!ve.getMessages().isEmpty()) {
				throw ve;
			}
		}
	}

	/**
	 * Generates the activation code and link for this new user and upserts them
	 * into the datastore.
	 */
	public void generateActivationDetailsAndSave(final Persistence persistence) {
		// Set activation details
		getUser().setActivated(Boolean.FALSE);
		getUser().setActivationCode(UUID.randomUUID().toString());
		getUser().setActivationCodeCreationDateTime(new DateTime());

		getUser().setBizUserId(getBizId());

		// Save and set the user
		setUser(upsertUser(persistence, getUser()));

		// Generate link used for activation
		generateActivationLink();
	}

	/**
	 * Upsert the user into the database.
	 * <br />
	 * NOTE: We upsert the user rather than regularly calling {@link Persistence#save(org.skyve.domain.PersistentBean)} because
	 * calling save will automatically set the bizUserId to be the current logged in user.
	 * <br />
	 * When registering, we want the User we are just about to create to own the documents.
	 * 
	 * @param persistence skyve persistence to save the bean
	 * @param bean the user bean to register
	 * @return the saved user bean
	 */
	private UserExtension upsertUser(Persistence persistence, UserExtension bean) {
		persistence.begin();

		// update bizuser id on User and related objects
		org.skyve.metadata.user.User u = persistence.getUser();
		Customer c = u.getCustomer();
		Module am = c.getModule(bean.getBizModule());
		Document ad = am.getDocument(c, bean.getBizDocument());
		new UpdateBizUserVisitor(bean.getBizId()).visit(ad, bean, c);

		// upsert Contact, User and Roles
		persistence.upsertBeanTuple(bean.getContact());
		persistence.upsertBeanTuple(bean);
		persistence.upsertCollectionTuples(bean, User.groupsPropertyName);

		persistence.commit(false);
		return bean;
	}

	/**
	 * Visitor to update all beans related to a document to have a given owning bizUserId
	 */
	private class UpdateBizUserVisitor extends BeanVisitor {

		private String bizUserId;

		public UpdateBizUserVisitor(String bizUserId) {
			super(false, false, false);
			this.bizUserId = bizUserId;
		}

		@Override
		protected boolean accept(String binding, Document document, Document owningDocument, Relation owningRelation, Bean bean) {
			bean.setBizUserId(bizUserId);
			return true;
		}
	}
}
