package modules.admin.User;

import java.sql.Connection;
import java.util.List;
import java.util.UUID;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.DateTime;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.Role;
import org.skyve.persistence.Persistence;
import org.skyve.util.BeanVisitor;
import org.skyve.util.Binder;
import org.skyve.util.CommunicationUtil;
import org.skyve.util.CommunicationUtil.ResponseMode;
import org.skyve.util.Util;

import modules.admin.UserProxy.UserProxyExtension;
import modules.admin.domain.Contact;
import modules.admin.domain.SelfRegistrationActivation;
import modules.admin.domain.User;
import modules.admin.domain.UserProxy;
import modules.admin.domain.UserRole;

public class UserExtension extends User {
	private static final long serialVersionUID = 3422968996147520436L;

	private boolean determinedRoles = false;

	public static final String SELF_REGISTRATION_COMMUNICATION = "SYSTEM Self Registration";
	public static final String SELF_REGISTRATION_SUBJECT = "Activate your account";
	public static final String SELF_REGISTRATION_HEADING = "Welcome!";
	public static final String SELF_REGISTRATION_BODY = String.format("<p>Hi <b>{%1$s}</b>,</p><br/>"
			+ "<p>Thank you for registering.</p>"
			+ "<p>To complete your account setup, please click the activation link below.</p>"
			+ "<p><a href=\"{%2$s}\">{%2$s}</a></p>"
			+ "<p>If you have any questions about your new account, contact us at <a href=\"mailto:%3$s\">%3$s</a>.</p>",
			Binder.createCompoundBinding(User.contactPropertyName, Contact.namePropertyName),
			activateUrlPropertyName,
			Util.getSupportEmailAddress());


	@Override
	public List<UserRole> getAssignedRoles() {
		List<UserRole> assignedRoles = super.getAssignedRoles();
		if (! determinedRoles) {
			determinedRoles = true;
			assignedRoles.clear();
			if (isPersisted()) {
				try {
					org.skyve.metadata.user.User metaDataUser = toMetaDataUser();

					// Add the assigned roles
					Customer c = metaDataUser.getCustomer();
					for (Module m : c.getModules()) {
						String moduleName = m.getName();
						for (Role role : m.getRoles()) {
							String roleName = role.getName();
							if (metaDataUser.isInRole(moduleName, roleName)) {
								UserRole assignedRole = UserRole.newInstance();
								assignedRole.setRoleName(moduleName + "." + roleName);
								assignedRole.originalValues().clear();
								assignedRoles.add(assignedRole);
							}
						}
					}
				}
				catch (@SuppressWarnings("unused") Exception e) {
					// assigned roles is already empty
				}
			}
		}

		return assignedRoles;
	}

	void clearAssignedRoles() {
		determinedRoles = false;
	}

	/**
	 * Return the metadata user for this {@link User}.
	 *
	 * @return the metadata user that is this user
	 */
	public org.skyve.metadata.user.User toMetaDataUser() {
		UserImpl result = null;
		
		if (isPersisted()) {
			// Populate the user using the persistence connection since it might have just been inserted and not committed yet
			result = ProvidedRepositoryFactory.setCustomerAndUserFromPrincipal((UtilImpl.CUSTOMER == null) ? getBizCustomer() + "/" + getUserName() : getUserName());
			if (result != null) {
				result.clearAllPermissionsAndMenus();
				
				// Use the current persistence connection, so do not close
				@SuppressWarnings("resource")
				Connection c = ((AbstractHibernatePersistence) CORE.getPersistence()).getConnection();
				ProvidedRepositoryFactory.get().populateUser(result, c);
			}
		}

		return result;
	}

	/**
	 * Generates the activation code and link for this new user and upserts them
	 * into the datastore.
	 */
	public void generateActivationDetailsAndSave(final Persistence persistence) {
		// Set activation details and generate link used for activation
		this.setActivated(Boolean.FALSE);
		this.setActivationCode(UUID.randomUUID().toString());
		this.setActivationCodeCreationDateTime(new DateTime());

		this.setBizUserId(getBizId());

		// Save and set the user
		this.upsertUser(persistence, this);
	}

	/**
	 * Generates the activation link for the email to send to the new user with the activation code.
	 */
	@Override
	public String getActivateUrl() {
		if (this.getActivationCode() == null) {
			return null;
		}
		StringBuilder urlBuilder = new StringBuilder();
		urlBuilder.append(Util.getDocumentUrl(SelfRegistrationActivation.MODULE_NAME, SelfRegistrationActivation.DOCUMENT_NAME))
				.append("&code=")
				.append(this.getActivationCode());
		return urlBuilder.toString();
	}
	
	/**
	 * Get the password last changed country name (for the current user's locale) for the country code.
	 */
	@Override
	public String getPasswordLastChangedCountryName() {
		String countryCode = getPasswordLastChangedCountryCode();
		return (countryCode == null) ? null : Util.countryNameFromCode(countryCode);
	}

	/**
	 * Sends the activation email to the user who registered.
	 *
	 * @throws Exception
	 */
	public void sendUserRegistrationEmail() throws Exception {
		Util.LOGGER.info("Sending registration email to " + this.getContact().getEmail1());
		CommunicationUtil.sendFailSafeSystemCommunication(SELF_REGISTRATION_COMMUNICATION,
															"{contact.email1}",
															null,
															SELF_REGISTRATION_SUBJECT,
															SELF_REGISTRATION_BODY,
															ResponseMode.EXPLICIT,
															null,
															this);
	}
	
	/**
	 * Whether the currently logged in {@link org.skyve.metadata.user.User} is this {@link User}.
	 * 
	 * @return true if the logged in user is this {@link User}
	 */
	public boolean owningUser() {
		return CORE.getPersistence().getUser().getId().equals(getBizId());
	}

	/**
	 * Return a {@link UserProxy} from this {@link User}, if persisted, or null if not persisted.
	 * 
	 * @return A user proxy representing this user
	 */
	public UserProxyExtension toUserProxy() {
		return CORE.getPersistence().retrieve(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME, getBizId());
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
