package modules.admin.SelfRegistration.actions;

import java.util.UUID;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateTime;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.Persistence;
import org.skyve.util.BeanValidator;
import org.skyve.util.BeanVisitor;
import org.skyve.web.WebContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import modules.admin.Configuration.ConfigurationExtension;
import modules.admin.Group.GroupExtension;
import modules.admin.SelfRegistration.SelfRegistrationBizlet;
import modules.admin.SelfRegistration.SelfRegistrationExtension;
import modules.admin.User.UserExtension;
import modules.admin.domain.Contact;
import modules.admin.domain.User;

/**
 * Action to register a new user, giving them appropriate permissions and sending a registration email to confirm their user account.
 * 
 */
public class Register implements ServerSideAction<SelfRegistrationExtension> {

	private static final Logger LOGGER = LoggerFactory.getLogger(Register.class);

	private static final long serialVersionUID = 8763158382139036658L;

	@Override
	public ServerSideActionResult<SelfRegistrationExtension> execute(SelfRegistrationExtension bean, WebContext webContext) throws Exception {
		Persistence persistence = CORE.getPersistence();
		if (bean.getUser() != null) {
			try {
				// Update the username to be the same as the email
				bean.getUser().setUserName(bean.getUser().getContact().getEmail1());

				// Should be a person registering for an account
				bean.getUser().getContact().setContactType(Contact.ContactType.person);

				String unencodedPassword = bean.getUser().getPassword();
				try {
					// validate the password and confirm password match
					bean.validateConfirmPassword();

					// Encode password
					encodePassword(bean.getUser());

					final GroupExtension selfRegistrationGroup = bean.getConfiguration().getUserSelfRegistrationGroup();
					if (selfRegistrationGroup != null) {
						bean.getUser().getGroups().add(selfRegistrationGroup);
					} else {
						LOGGER.error("Self registration failed because the no self registration has been set in the configuration.");
						throw new ValidationException(new Message("Self registration cannot be completed at this time."));
					}

					// Validate
					// perform UserRegistration specific validations first
					BeanValidator.validateBeanAgainstBizlet(bean);

					BeanValidator.validateBeanAgainstDocument(bean.getUser().getContact());
					BeanValidator.validateBeanAgainstDocument(bean.getUser());
					BeanValidator.validateBeanAgainstBizlet(bean.getUser());
				} catch (ValidationException e) {
					// Reset roles
					bean.getUser().getGroups().clear();
					// Decode password
					bean.getUser().setPassword(unencodedPassword);
					// Rethrow validation exceptions
					throw e;
				}

				// Set activation details
				bean.getUser().setActivated(Boolean.FALSE);
				bean.getUser().setActivationCode(UUID.randomUUID().toString());
				bean.getUser().setActivationCodeCreationDateTime(new DateTime());

				bean.getUser().setBizUserId(bean.getBizId());

				// Save and set the user
				bean.setUser(upsertUser(persistence, bean.getUser()));

				// Generate link used for activation
				SelfRegistrationBizlet.generateActivationLink(bean);

				// Send registration email to the new user
				sendRegistrationEmail(bean);
				
				webContext.growl(MessageSeverity.info, "An activation email has been sent to your address." );
				
			} catch (ValidationException e) {
				// Catch and re-throw so that we do not send a slack message for ValidationExceptions.
				throw e;
			} catch (Exception e) {
				throw e;
			}
		}
		return new ServerSideActionResult<>(bean);
	}

	private static void encodePassword(User user) throws Exception {
		user.setPassword(EXT.hashPassword(user.getPassword()));
	}

	private static void sendRegistrationEmail(SelfRegistrationExtension bean) throws Exception {
		try {
			// Send the registration email if a generated password is set.
			final ConfigurationExtension configuration = bean.getConfiguration();

			CORE.getPersistence().begin();
			configuration.sendUserRegistrationEmail(bean);
			CORE.getPersistence().commit(false);

			// ensure that the public screen reacts to whether we sent an email or not
			bean.setRegistrationEmailWasSent(Boolean.TRUE);
		} catch (Exception e) {
			LOGGER.warn("Self Registration successful but email failed to send.", e);
		}
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
		
		// Update bizuser id on User and related objects
		org.skyve.metadata.user.User u = persistence.getUser();
		Customer c = u.getCustomer();
		Module am = c.getModule(bean.getBizModule());
		Document ad = am.getDocument(c, bean.getBizDocument());
		new UpdateBizUserVisitor(bean.getBizId()).visit(ad, bean, c);
		
		// Upsert Contact, User and Roles
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
