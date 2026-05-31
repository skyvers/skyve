package org.skyve.impl.metadata.user;

import java.util.Locale;
import java.util.Set;
import java.util.TreeSet;

import org.slf4j.Logger;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;
import org.skyve.util.logging.SkyveLoggerFactory;

/**
 * A {@link UserImpl} subclass with all document and action privileges granted.
 *
 * <p>Used for framework-internal background jobs, bootstrapping, and system
 * operations that must run outside a real authenticated user session. Granting
 * superuser access bypasses all document-level permission checks.
 *
 * <p>Side effects: any persistence operation performed under a {@code SuperUser}
 * context will not enforce customer or data-group scoping.
 *
 * @see UserImpl
 */
public class SuperUser extends UserImpl {
	private static final long serialVersionUID = -6233814867322594601L;
	private static final Logger LOGGER = SkyveLoggerFactory.getLogger(SuperUser.class);

	/**
	 * Creates a superuser with an English default locale.
	 */
	public SuperUser() {
		setWebLocale(Locale.ENGLISH);
	}
	
	/**
	 * Copies identity and context values from an existing user.
	 *
	 * @param user the source user whose identity/customer context is copied.
	 */
	public SuperUser(User user) {
		this();
    	setContactId(user.getContactId());
    	setContactName(user.getContactName());
    	setCustomerName(user.getCustomerName());
    	setDataGroupId(user.getDataGroupId());
    	setHomeModuleName(user.getHomeModuleName());
    	setId(user.getId());
    	setName(user.getName());
    	setPasswordHash(user.getPasswordHash());
    	setPasswordChangeRequired(user.isPasswordChangeRequired());
    	Locale locale = user.getLocale();
    	setLanguageTag(user.getLanguageTag());
    	setWebLocale((locale == null) ? Locale.ENGLISH : locale);
	}

	/**
	 * Indicates that superusers are considered members of every role.
	 *
	 * @param moduleName ignored for superusers.
	 * @param roleName ignored for superusers.
	 * @return always {@code true}.
	 */
	@Override
	public boolean isInRole(String moduleName, String roleName) {
		return true;
	}
	
	/**
	 * Returns customer-scope access when the base implementation would deny access.
	 *
	 * @param moduleName the module name being checked.
	 * @param documentName the document name being checked.
	 * @return the resolved scope, defaulting from {@code none} to {@code customer}.
	 */
	@Override
	public DocumentPermissionScope getScope(String moduleName, String documentName) {
		DocumentPermissionScope result = super.getScope(moduleName, documentName);
		if (DocumentPermissionScope.none.equals(result)) {
			result = DocumentPermissionScope.customer;
		}
		
		return result;
	}
	
	/**
	 * Allows read access to any bean regardless of module, ownership, or scoping values.
	 *
	 * @return always {@code true}.
	 */
	@Override
	public boolean canReadBean(String beanBizId,
								String beanBizModule,
								String beanBizDocument,
								String beanBizCustomer,
								String beanBizDataGroupId,
								String beanBizUserId) {
		return true;
	}

	/**
	 * Allows read access to any bean regardless of ownership fields.
	 *
	 * @return always {@code true}.
	 */
	@Override
	public boolean canAccessDocument(Document document) {
		return true;
	}

	/**
	 * Allows create access to every document type.
	 *
	 * @param document the target document metadata.
	 * @return always {@code true}.
	 */
	@Override
	public boolean canCreateDocument(Document document) {
		return true;
	}

	/**
	 * Allows delete access to every document type.
	 *
	 * @param document the target document metadata.
	 * @return always {@code true}.
	 */
	@Override
	public boolean canDeleteDocument(Document document) {
		return true;
	}

	/**
	 * Allows execution of all actions on all documents.
	 *
	 * @param document the target document metadata.
	 * @param actionName the action name.
	 * @return always {@code true}.
	 */
	@Override
	public boolean canExecuteAction(Document document, String actionName) {
		return true;
	}

	/**
	 * Allows use of record flagging features.
	 *
	 * @return always {@code true}.
	 */
	@Override
	public boolean canFlag() {
		return true;
	}

	/**
	 * Allows read access to every document type.
	 *
	 * @param document the target document metadata.
	 * @return always {@code true}.
	 */
	@Override
	public boolean canReadDocument(Document document) {
		return true;
	}

	/**
	 * Allows use of global text search features.
	 *
	 * @return always {@code true}.
	 */
	@Override
	public boolean canTextSearch() {
		return true;
	}

	/**
	 * Allows switching between available UI modes.
	 *
	 * @return always {@code true}.
	 */
	@Override
	public boolean canSwitchMode() {
		return true;
	}

	/**
	 * Allows update access to every document type.
	 *
	 * @param document the target document metadata.
	 * @return always {@code true}.
	 */
	@Override
	public boolean canUpdateDocument(Document document) {
		return true;
	}

	/**
	 * Returns all module names available to the active customer.
	 *
	 * @return module names, or {@code null} if customer metadata cannot be resolved.
	 */
	@Override
	public Set<String> getAccessibleModuleNames() {
		try {
			return new TreeSet<>(((CustomerImpl) getCustomer()).getModuleEntries().keySet());//Repository.get().getAllVanillaModuleNames());
		}
		catch (MetaDataException e) {
			LOGGER.error(e.getMessage(), e);
		}

		return null;
	}
}
