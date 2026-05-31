package org.skyve.impl.metadata.user;

import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;

import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.repository.module.ContentPermission;
import org.skyve.impl.metadata.repository.module.ContentRestriction;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.menu.Menu;
import org.skyve.metadata.user.DocumentPermission;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.persistence.Persistence;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;

import com.google.common.base.MoreObjects;

import jakarta.annotation.Nonnull;

/**
 * Runtime implementation of {@link org.skyve.metadata.user.User} carrying the
 * authenticated user's identity, resolved role privileges, and permission state.
 *
 * <p>Constructed by the persistence layer during login; populated with the user's
 * customer, data group, roles, and the derived privilege set (aggregated from all
 * assigned roles). The privilege set is cached on construction so that permission
 * checks during request processing do not require repeated metadata traversal.
 *
 * <p>Threading: thread-confined. One instance per request thread (held by
 * {@link org.skyve.impl.persistence.AbstractPersistence}).
 *
 * @see org.skyve.metadata.user.User
 * @see SuperUser
 */
public class UserImpl implements User {
	private static final long serialVersionUID = -8485741818564437957L;

	private static final String SECURITY_ADMINISTRATOR_ROLE = "admin.SecurityAdministrator";

    private static final Logger SECURITY_LOGGER = Category.SECURITY.logger();

	/**
	 * Represents a user that does not belong to a data group.
	 */
	public static final String DATA_ADMINISTRATOR_ROLE = "admin.DataAdministrator";

	private String id;
	private String sessionId;
	private String name;
	private String languageTag;
	private String passwordHash;
	private boolean passwordChangeRequired;
	private String contactId;
	private String contactName;
	private String contactImageId;
	private String customerName;
	private String dataGroupId;
	private String homeModuleName;
	
	/**
	 * Locale either derived from the language tag or from the Http Servlet Request.
	 * Note that this is not serialized.
	 */
	private transient Locale locale;

	/**
	 * User (session) attributes. Keep this small since the user is in the web session.
	 */
	private Map<String, Object> attributes = new TreeMap<>();

	/**
	 * To allow SuperUser to set SUPER Role
	 */
	private Set<String> roleNames = new TreeSet<>();

	/**
	 * Document Name -> CRUD permission
	 */
	private Map<String, DocumentPermission> documentPermissions = new TreeMap<>();

	/**
	 * module.document.action
	 */
	private Set<String> actions = new TreeSet<>();

	/**
	 * Module name -> Menu
	 */
	private Map<String, Menu> moduleMenuMap = new TreeMap<>();

	/**
	 * module.document.attribute
	 */
	private Set<String> contentRestrictions = new TreeSet<>();

	/**
	 * module.document.attribute
	 */
	private Set<String> contentPermissions = new TreeSet<>();

	/**
	 * A set of navigations in string form that are allowed, per UX/UI.
	 * ie UserAccess String -> UX/UIs (or null if valid for any UX/UI)
	 * This is derived from the metadata or defined in the router.
	 * It is thread safe as many threads can populate as it is populated on the fly in canAccess().
	 */
	private ConcurrentHashMap<String, Set<String>> accesses = new ConcurrentHashMap<>();
	
	/**
	 * Returns the authenticated user identifier.
	 *
	 * @return the user identifier.
	 */
	@Override
	public String getId() {
		return id;
	}

	/**
	 * Sets the authenticated user identifier.
	 *
	 * @param id the user identifier.
	 */
	@Override
	public void setId(String id) {
		this.id = id;
	}

	/**
	 * Returns the display name for this user.
	 *
	 * @return the user display name.
	 */
	@Override
	public String getName() {
		return name;
	}

	/**
	 * Sets the display name for this user.
	 *
	 * @param name the user display name.
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * Returns the active HTTP session identifier for this user.
	 *
	 * @return the session identifier.
	 */
	@Override
	public String getSessionId() {
		return sessionId;
	}

	/**
	 * Set the session ID of the session this user is in.
	 * @param sessionId	The session ID to set.
	 */
	public void setSessionId(@Nonnull String sessionId) {
		this.sessionId = sessionId;
	}

	/**
	 * Returns the preferred language tag for this user.
	 *
	 * @return the IETF BCP 47 language tag, or {@code null} when not set.
	 */
	@Override
	public String getLanguageTag() {
		return languageTag;
	}
	
	/**
	 * Sets the preferred language tag and derives the transient locale.
	 *
	 * @param languageTag the IETF BCP 47 language tag.
	 */
	public void setLanguageTag(String languageTag) {
		this.languageTag = languageTag;
		if (languageTag == null) {
			locale = null;
		}
		else {
			locale = Locale.forLanguageTag(languageTag);
		}
	}
	
    /**
     * Reinstate the transient locale value from the language tag after Serialization.
     */
    protected Object readResolve() {
        setLanguageTag(languageTag);
        return this;
    }

	/**
	 * Returns the resolved locale for this user.
	 *
	 * @return the locale derived from language tag or request context.
	 */
	@Override
	public Locale getLocale() {
		return locale;
	}

	/**
	 * Sets the web-request locale only when no explicit language tag exists.
	 *
	 * @param locale the request locale to cache for this user.
	 */
	public void setWebLocale(Locale locale) {
		if (languageTag == null) {
			this.locale = locale;
		}
	}
	
	/**
	 * Returns the stored password hash.
	 *
	 * @return the password hash.
	 */
	@Override
	public String getPasswordHash() {
		return passwordHash;
	}

	/**
	 * Sets the stored password hash.
	 *
	 * @param passwordHash the password hash.
	 */
	public void setPasswordHash(String passwordHash) {
		this.passwordHash = passwordHash;
	}

	/**
	 * Indicates whether this user must change their password.
	 *
	 * @return {@code true} when a password change is required.
	 */
	@Override
	public boolean isPasswordChangeRequired() {
		return passwordChangeRequired;
	}

	/**
	 * Sets whether this user must change their password.
	 *
	 * @param passwordChangeRequired {@code true} when a password change is required.
	 */
	public void setPasswordChangeRequired(boolean passwordChangeRequired) {
		this.passwordChangeRequired = passwordChangeRequired;
	}

	/**
	 * Returns the linked contact identifier for this user.
	 *
	 * @return the contact identifier.
	 */
	@Override
	public String getContactId() {
		return contactId;
	}

	/**
	 * Sets the linked contact identifier for this user.
	 *
	 * @param contactId the contact identifier.
	 */
	public void setContactId(String contactId) {
		this.contactId = contactId;
	}

	/**
	 * Returns the linked contact display name for this user.
	 *
	 * @return the contact display name.
	 */
	@Override
	public String getContactName() {
		return contactName;
	}

	/**
	 * Sets the linked contact display name for this user.
	 *
	 * @param contactName the contact display name.
	 */
	public void setContactName(String contactName) {
		this.contactName = contactName;
	}

	/**
	 * Returns the linked contact image content identifier.
	 *
	 * @return the contact image identifier.
	 */
	@Override
	public String getContactImageId() {
		return contactImageId;
	}

	/**
	 * Sets the linked contact image content identifier.
	 *
	 * @param contactImageId the contact image identifier.
	 */
	public void setContactImageId(String contactImageId) {
		this.contactImageId = contactImageId;
	}

	/**
	 * Builds a content endpoint URL for the user's contact image.
	 *
	 * @param width the requested image width.
	 * @param height the requested image height.
	 * @return the content URL, or {@code null} when no contact image is available.
	 */
	@Override
	public String getContactImageUrl(int width, int height) {
		if (contactImageId == null) {
			return null;
		}
		
		StringBuilder result = new StringBuilder(256);
		result.append("content?_n=").append(contactImageId).append("&_doc=admin.Contact&_b=image&_w=");
		result.append(width).append("&_h=").append(height);
		return result.toString();
	}

	/**
	 * Returns avatar initials derived from contact name or user name.
	 *
	 * @return one or two uppercase initials, or {@code "??"} when no name is available.
	 */
	@Override
	public String getContactAvatarInitials() {
		String n = (contactName == null) ? name : contactName;
		if (n == null) {
			return "??";
		}
		String[] tokens = n.split(" ");
		if (tokens.length == 0) {
			return "??";
		}
		if (tokens.length == 1) {
			return tokens[0].substring(0, 1).toUpperCase();
		}
		return tokens[0].substring(0, 1).toUpperCase() +
				tokens[tokens.length - 1].substring(0, 1).toUpperCase();
	}
	
	/**
	 * Hold other user (session) attributes for processing. 
	 * NOTE: Keep this small since the user is in the web session.
	 * 
	 * @return The attributes Map.
	 */
	@Override
	public Map<String, Object> getAttributes() {
		return attributes;
	}

	/**
	 * Resolves the customer's metadata object for this user context.
	 *
	 * @return the current customer metadata, or {@code null} if it cannot be resolved.
	 */
	@Override
	public Customer getCustomer() {
		return ProvidedRepositoryFactory.get().getCustomer(customerName);
	}

	/**
	 * Sets the customer name associated with this user context.
	 *
	 * @param customerName the customer name.
	 */
	@Override
	public void setCustomerName(String customerName) {
		this.customerName = customerName;
	}

	/**
	 * Returns the customer name associated with this user context.
	 *
	 * @return the customer name.
	 */
	@Override
	public String getCustomerName() {
		return customerName;
	}

	/**
	 * Returns the data-group identifier associated with this user.
	 *
	 * @return the data-group identifier, or {@code null} for ungrouped users.
	 */
	@Override
	public String getDataGroupId() {
		return dataGroupId;
	}

	/**
	 * Sets the data-group identifier associated with this user.
	 *
	 * @param dataGroupId the data-group identifier.
	 */
	@Override
	public void setDataGroupId(String dataGroupId) {
		this.dataGroupId = dataGroupId;
	}

	/**
	 * Returns the configured home module name for this user.
	 *
	 * @return the home module name.
	 */
	@Override
	public String getHomeModuleName() {
		return homeModuleName;
	}

	/**
	 * Sets the configured home module name for this user.
	 *
	 * @param homeModuleName the home module name.
	 */
	public void setHomeModuleName(String homeModuleName) {
		this.homeModuleName = homeModuleName;
	}

	/**
	 * Merges permissions from a role into this user's effective access state.
	 *
	 * <p>Side effects: updates document CRUD scope, executable actions, content
	 * restrictions, and content permissions. Duplicate role additions are ignored.
	 *
	 * @param role the role to merge into effective permissions.
	 */
	public void addRole(RoleImpl role) {
		String roleName = new StringBuilder(128).append(role.getOwningModule().getName()).append('.').append(role.getName()).toString();
		
		// Only continue if role hasn't been added already
		if (roleNames.add(roleName)) {
			String owningModuleName = role.getOwningModule().getName();
			for (Privilege privilege : role.getPrivileges()) {
				if (privilege instanceof DocumentPrivilege documentPrivilege) {
					DocumentPermission permission = documentPrivilege.getPermission();
					putDocumentPermission(owningModuleName, privilege.getName(), permission);
				}
				else if (privilege instanceof ActionPrivilege actionPrivilege) {
					addActionPermission(owningModuleName, actionPrivilege);
				}
			}
			
			for (ContentRestriction contentRestriction : role.getContentRestrictions()) {
				addContentRestriction(owningModuleName, contentRestriction);
			}
			
			for (ContentPermission contentPermission : role.getContentPermissions()) {
				addContentPermission(owningModuleName, contentPermission);
			}
		}
	}
	
	
	/**
	 * Add a action permission for this user.
	 * @param moduleName the module the action belongs to
	 * @param actionPrivilege the action
	 */
	protected void addActionPermission(String moduleName, ActionPrivilege actionPrivilege) {
		// will add to set if not already present
		StringBuilder sb = new StringBuilder();
		sb.append(moduleName).append('.');
		sb.append(actionPrivilege.getDocumentName()).append('.');
		sb.append(actionPrivilege.getName());
		actions.add(sb.toString());
	}
	
	/**
	 * Add a document permission for this user.
	 * @param moduleName the module the document belongs to
	 * @param documentName the document
	 * @param documentPermission the permission
	 */
	protected void putDocumentPermission(String moduleName, String documentName, DocumentPermission documentPermission) {
		DocumentPermission mergedPermission = documentPermission;
		
		String fullyQualifiedDocumentName = new StringBuilder(128).append(moduleName).append('.').append(documentName).toString();
		DocumentPermission existingDocumentPermission = documentPermissions.get(fullyQualifiedDocumentName);
		if (existingDocumentPermission != null) {
			mergedPermission = existingDocumentPermission.mergePermission(mergedPermission);
		}

		documentPermissions.put(fullyQualifiedDocumentName, mergedPermission);
	}
	
	
	/**
	 * Add a content restriction for this user.
	 * @param moduleName the module the content restriction belongs to
	 * @param contentRestriction the content restriction
	 */
	protected void addContentRestriction(String moduleName, ContentRestriction contentRestriction) {
		// will add to set if not already present
		StringBuilder key = new StringBuilder(196);
		key.append(moduleName).append('.');
		key.append(contentRestriction.getDocumentName()).append('.');
		key.append(contentRestriction.getAttributeName());
		contentRestrictions.add(key.toString());
	}
	
	
	/**
	 * Add a content permission for this user.
	 * @param moduleName the module the content permission belongs to
	 * @param contentPermission the content permission
	 */
	protected void addContentPermission(String moduleName, ContentPermission contentPermission) {
		// will add to set if not already present
		StringBuilder key = new StringBuilder(196);
		key.append(moduleName).append('.');
		key.append(contentPermission.getDocumentName()).append('.');
		key.append(contentPermission.getAttributeName());
		contentPermissions.add(key.toString());
	}

	/**
	 * Returns fully qualified document names for which this user has explicit permissions.
	 *
	 * @return the permission map key set in {@code <module>.<document>} form.
	 */

	public Set<String> getFullyQualifiedDocumentNames() {
		return documentPermissions.keySet();
	}

	/**
	 * Gets the accessible menu structure for a user and module.
	 * 
	 * @param moduleName
	 * @return
	 */
	public Menu getModuleMenu(String moduleName) {
		return moduleMenuMap.get(moduleName);
	}

	/**
	 * Put a menu in place for a given module
	 * @param moduleName the module
	 * @param menu the menu for that module
	 */
	public void putModuleMenu(Module module, Menu menu) {
		moduleMenuMap.put(module.getName(), menu);
	}

	/**
	 * Clear all module menus
	 */
	public void clearModuleMenus() {
		moduleMenuMap.clear();
		accesses.clear();
	}

	/**
	 * Returns module names that currently have at least one accessible menu item.
	 */
	@Override
	public Set<String> getAccessibleModuleNames() {
		Set<String> result = new TreeSet<>();

		// add module name if menu has accessible items
		for (Entry<String, Menu> entry : moduleMenuMap.entrySet()) {
			if (! entry.getValue().getItems().isEmpty()) {
				result.add(entry.getKey());
			}
		}

		return result;
	}

	/**
	 * Checks whether the user holds the specified role.
	 *
	 * <p>Also treats security administrators without a data group as data administrators.
	 *
	 * @param moduleName the role's owning module name.
	 * @param roleName the role name.
	 * @return {@code true} when the role is effectively held.
	 */
	@Override
	public boolean isInRole(String moduleName, String roleName) {
		String fullyQualifiedRoleName = new StringBuilder(128).append(moduleName).append('.').append(roleName).toString();
		return roleNames.contains(fullyQualifiedRoleName) || // user has the role
		// looking for data administrator role,
		// and user has no data group and has security admin role
		(DATA_ADMINISTRATOR_ROLE.equals(fullyQualifiedRoleName) && 
			(dataGroupId == null) && 
			roleNames.contains(SECURITY_ADMINISTRATOR_ROLE));
	}

	/**
	 * Returns the document permission scope for the specified module/document pair.
	 *
	 * @param moduleName the module name.
	 * @param documentName the document name.
	 * @return the configured scope, or {@link DocumentPermissionScope#none} when no
	 *         permission exists.
	 */
	@Override
	public DocumentPermissionScope getScope(String moduleName, String documentName) {
		DocumentPermissionScope result = DocumentPermissionScope.none;
		String key = new StringBuilder(128).append(moduleName).append('.').append(documentName).toString();
		DocumentPermission permission = documentPermissions.get(key);
		if (permission != null) {
			result = permission.getScope();
		}

		return result;
	}

	/**
	 * Determines whether this user can read a bean instance identified by biz fields.
	 *
	 * <p>Evaluates document permission scope (global/customer/data-group/user). If the
	 * document has no direct permission and is a child document, the decision falls back
	 * to parent-document readability.
	 *
	 * <p>Side effects: may execute a parent-join lookup for child documents.
	 *
	 * @param beanBizId the bean identifier.
	 * @param beanBizModule the owning module name.
	 * @param beanBizDocument the owning document name.
	 * @param beanBizCustomer the bean customer value.
	 * @param beanBizDataGroupId the bean data-group identifier.
	 * @param beanBizUserId the bean owning user identifier.
	 * @return {@code true} when read access is granted; otherwise {@code false}.
	 */
	@Override
	@SuppressWarnings("incomplete-switch")
	public boolean canReadBean(String beanBizId,
								String beanBizModule,
								String beanBizDocument,
								String beanBizCustomer,
								String beanBizDataGroupId,
								String beanBizUserId) {
		boolean result = false;
		
		String key = new StringBuilder(128).append(beanBizModule).append('.').append(beanBizDocument).toString();
		DocumentPermission permission = documentPermissions.get(key);
		if (permission != null) {
			if (permission.canRead()) {
				switch (permission.getScope()) {
				case global:
					result = true;
					break;
				case customer:
					result = customerName.equals(beanBizCustomer);
					break;
				case dataGroup:
					result = customerName.equals(beanBizCustomer);
					if (result && (dataGroupId != null)) {
						result = dataGroupId.equals(beanBizDataGroupId);
					}
					break;
				case user:
					result = customerName.equals(beanBizCustomer);
					if (result && (dataGroupId != null)) {
						result = dataGroupId.equals(beanBizDataGroupId);
					}
					if (result) {
						result = id.equals(beanBizUserId);
					}
				}
				if ((! result) && UtilImpl.SECURITY_TRACE) {
                    SECURITY_LOGGER.info("Security - {}.{}.{} denied - not in scope", beanBizModule, beanBizDocument, beanBizId);
				}
			}
			else {
				result = false;
				if (UtilImpl.SECURITY_TRACE) {
					SECURITY_LOGGER.info("Security - {}.{}.{} denied - no read permission", beanBizModule, beanBizDocument, beanBizId);
				}
			}
		}
		else { // no permission defined
			// if document is a child document, get the parent
			// if the parent bean can be read then allow
			Customer customer = getCustomer();
			Module module = customer.getModule(beanBizModule);
			Document document = module.getDocument(customer, beanBizDocument);
			Document parentDocument = document.getParentDocument(customer);
			if (parentDocument == null) { // document has no parent
				result = false;
                if (UtilImpl.SECURITY_TRACE) {
                    SECURITY_LOGGER.info("Security - {}.{}.{} denied - no permission", beanBizModule, beanBizDocument, beanBizId);
                }
			}
			else { // found a parent document
				if (! beanBizDocument.equals(parentDocument.getName())) { // exclude hierarchical documents
					if (document.isPersistable() && parentDocument.isPersistable()) {
						StringBuilder sb = new StringBuilder(256);
						sb.append("select p.").append(Bean.DOCUMENT_ID).append(", p.");
						sb.append(Bean.CUSTOMER_NAME).append(", p.");
						sb.append(Bean.DATA_GROUP_ID).append(", p.");
						sb.append(Bean.USER_ID);
						sb.append(" from ");
						Persistent persistent = parentDocument.getPersistent();
						sb.append((persistent == null) ? "N/A" : persistent.getPersistentIdentifier()); // work around compiler
						sb.append(" as p inner join ");
						persistent = document.getPersistent();
						sb.append((persistent == null) ? "N/A" : persistent.getPersistentIdentifier()); // work around compiler
						sb.append(" as c on p.").append(Bean.DOCUMENT_ID);
						sb.append(" = c.").append(ChildBean.PARENT_NAME);
						sb.append("_id where c.").append(Bean.DOCUMENT_ID).append(" = :");
						sb.append(Bean.DOCUMENT_ID);
						Persistence p = AbstractPersistence.get();
						List<Object[]> rows = p.newSQL(sb.toString()).putParameter(Bean.DOCUMENT_ID, beanBizId, false).tupleResults();
						if (rows.isEmpty()) { // bean is still transient - user hasn't saved
							result = true;
						}
						else {
							Object[] values = rows.get(0);
	
							// deny if user can't read parent document
							result = canReadBean((String) values[0], 
													parentDocument.getOwningModuleName(), 
													parentDocument.getName(),
													(String) values[1], 
													(String) values[2], 
													(String) values[3]);
							if ((! result) && (UtilImpl.SECURITY_TRACE)) {
			                    SECURITY_LOGGER.info("Security - {}.{}.{} denied - no read on parent", beanBizModule, beanBizDocument, beanBizId);
							}
						}
					}
				}
			}
		}

		return result;
	}

	/**
	 * Indicates if the user is able to search or view the content
	 * 
	 * @return <code>true</code> if access is allowed, otherwise <code>false</code>.
	 */
	@Override
	public boolean canAccessContent(String bizId,
										String bizModule,
										String bizDocument,
										String bizCustomer,
										String bizDataGroupId,
										String bizUserId,
										String attributeName) {
		String modocAndAttr = new StringBuilder(192).append(bizModule).append('.').append(bizDocument).append('.').append(attributeName).toString();
		boolean result = (! contentRestrictions.contains(modocAndAttr));
		if (result) {
			result = (contentPermissions.contains(modocAndAttr));
			if (! result) {
				// deny if user can't read owning document
				result = canReadBean(bizId, bizModule, bizDocument, bizCustomer, bizDataGroupId, bizUserId);
				if ((! result) && (UtilImpl.SECURITY_TRACE)) {
                    SECURITY_LOGGER.info(
                            "Security - {}.{}.{} denied - no read. Content permission can be explicitly granted for non-persistent document attribtues",
                            bizModule, bizDocument, bizId);
				}
			}
		}
		else {
			if (UtilImpl.SECURITY_TRACE) {
                SECURITY_LOGGER.info( "Security - {}.{}.{} denied - restricted content", bizModule, bizDocument, attributeName);
			}
		}

		return result;
	}
	
	/**
	 * Indicates whether any explicit document permission exists for the supplied document.
	 *
	 * @param document the document metadata.
	 * @return {@code true} when a document permission entry exists.
	 */
	@Override
	public boolean canAccessDocument(Document document) {
		String modoc = new StringBuilder(128).append(document.getOwningModuleName()).append('.').append(document.getName()).toString();
		return (documentPermissions.get(modoc) != null);
	}

	/**
	 * Indicates whether the user has create permission on the supplied document.
	 *
	 * @param document the document metadata.
	 * @return {@code true} when create permission is granted.
	 */
	@Override
	public boolean canCreateDocument(Document document) {
		boolean result = false;
		
		String modoc = new StringBuilder(128).append(document.getOwningModuleName()).append('.').append(document.getName()).toString();
		DocumentPermission permission = documentPermissions.get(modoc);
		if (permission != null) {
			result = permission.canCreate();
		}

		return result;
	}

	/**
	 * Indicates whether the user may use flagging features.
	 *
	 * @return {@code true} when the user has at least one configured flag role.
	 */
	@Override
	public boolean canFlag() {
		return ! Collections.disjoint(((CustomerImpl) getCustomer()).getFlagRoles(), roleNames);
	}

	/**
	 * Indicates whether the user has read permission on the supplied document.
	 *
	 * @param document the document metadata.
	 * @return {@code true} when read permission is granted.
	 */
	@Override
	public boolean canReadDocument(Document document) {
		boolean result = false;

		String modoc = new StringBuilder(128).append(document.getOwningModuleName()).append('.').append(document.getName()).toString();
		DocumentPermission permission = documentPermissions.get(modoc);
		if (permission != null) {
			result = permission.canRead();
		}

		return result;
	}

	/**
	 * Indicates whether the user may use text-search features.
	 *
	 * @return {@code true} when the user has at least one configured text-search role.
	 */
	@Override
	public boolean canTextSearch() {
		return ! Collections.disjoint(((CustomerImpl) getCustomer()).getTextSearchRoles(), roleNames);
	}

	/**
	 * Indicates whether the user may switch UI mode.
	 *
	 * @return {@code true} when the user has at least one configured switch-mode role.
	 */
	@Override
	public boolean canSwitchMode() {
		return ! Collections.disjoint(((CustomerImpl) getCustomer()).getSwitchModeRoles(), roleNames);
	}

	/**
	 * Indicates whether the user has update permission on the supplied document.
	 *
	 * @param document the document metadata.
	 * @return {@code true} when update permission is granted.
	 */
	@Override
	public boolean canUpdateDocument(Document document) {
		boolean result = false;

		String modoc = new StringBuilder(128).append(document.getOwningModuleName()).append('.').append(document.getName()).toString();
		DocumentPermission permission = documentPermissions.get(modoc);
		if (permission != null) {
			result = permission.canUpdate();
		}

		return result;
	}

	/**
	 * Indicates whether the user has delete permission on the supplied document.
	 *
	 * @param document the document metadata.
	 * @return {@code true} when delete permission is granted.
	 */
	@Override
	public boolean canDeleteDocument(Document document) {
		boolean result = false;

		String modoc = new StringBuilder(128).append(document.getOwningModuleName()).append('.').append(document.getName()).toString();
		DocumentPermission permission = documentPermissions.get(modoc);
		if (permission != null) {
			result = permission.canDelete();
		}

		return result;
	}

	/**
	 * Indicates whether the user can execute the named action on the supplied document.
	 *
	 * @param document the document metadata.
	 * @param actionName the action name.
	 * @return {@code true} when the fully qualified action is present in the user's action set.
	 */
	@Override
	public boolean canExecuteAction(Document document, String actionName) {
		StringBuilder sb = new StringBuilder(192);
		sb.append(document.getOwningModuleName()).append('.').append(document.getName());
		String fullyQualifiedActionName = sb.append('.').append(actionName).toString();
		return actions.contains(fullyQualifiedActionName);
	}

	/**
	 * Evaluates whether the user may navigate to the requested access target for a UX/UI.
	 *
	 * <p>When access control is disabled all access is allowed. Otherwise this method
	 * evaluates the lazily built access-control list and in development mode retries once
	 * after clearing stale ACL state.
	 * 
 	 * If ! UtilImpl.ACCESS_CONTROL, allow access.
	 * If access string key DNE then no access.
	 * If access string key exists and yields null, then access for every UX/UI.
	 * Otherwise, check access at UX/UI level.
	 */
	@Override
	public boolean canAccess(UserAccess access, String uxui) {
		// If access control is switched off then everything is accessible
		if (! UtilImpl.ACCESS_CONTROL) {
			return true;
		}

		boolean aclCreated = (! accesses.isEmpty());
		boolean result = canAccessWithDevMode(access, uxui);
		// If no access and we're in dev mode and the ACL was established before this call,
		// clear the ACL and retry just in case there was a UI, menu or router change that is picked up.
		if (UtilImpl.DEV_MODE && aclCreated && (! result)) { // dev mode and established ACL and no access
			accesses.clear(); // clear the ACL
			result = canAccessWithDevMode(access, uxui); // retry
		}

		return result;
	}

	/**
	 * Evaluates access against the built access-control map, including singular-access
	 * fallback to base-document access definitions.
	 *
	 * @param access the requested access vector.
	 * @param uxui the active UX/UI identifier.
	 * @return {@code true} when access is allowed for the supplied UX/UI.
	 */
	private boolean canAccessWithDevMode(UserAccess access, String uxui) {
		// Create the ACL if not already created
		if (accesses.isEmpty()) {
			new AccessProcessor(this, moduleMenuMap, accesses).process();
		}

		// Check access exists by key
		String accessString = access.toString();
		boolean foundAccess = accesses.containsKey(accessString);
		// If not found and a singular access, do polymorphic check for acccess
		// NB UserAccess used is closest to given document access
		if ((! foundAccess) && access.isSingular()) {
			// Check if we have access to a base document if one exists
			UserAccess baseAccess = access.determineSingularBaseDocument();
			while (baseAccess != null) { // a base document exists
				// Check access to the base document
				accessString = baseAccess.toString();
				foundAccess = accesses.containsKey(accessString);
				if (foundAccess) {
					// bug out next iteration
					baseAccess = null;
				}
				else {
					// Check if the base document has a base document
					baseAccess = baseAccess.determineSingularBaseDocument();
				}
			}
		}
		// If we found there is access defined somewhere, check the current UXUI has access
		if (foundAccess) {
			Set<String> uxuis = accesses.get(accessString);
			return (uxuis == UserAccess.ALL_UX_UIS) || uxuis.contains(uxui);
		}

		// No access found
		return false;
	}

	/**
	 * Creates a client-facing DTO representation of this user and their effective permissions.
	 * 
	 * @return a populated client user payload.
	 */
	public ClientUserData createClientUser() {
		ClientUserData result = new ClientUserData();

		result.setId(id);
		result.setName(name);
		result.setContactId(contactId);
		result.setContactName(contactName);
		result.setCustomerName(customerName);
		result.setDataGroupId(dataGroupId);
		result.setRoleNames(roleNames);

		Map<String, Map<String, Boolean>> newPermissions = new TreeMap<>();
		for (String documentName : documentPermissions.keySet()) {
			DocumentPermission documentPermission = documentPermissions.get(documentName);
			Map<String, Boolean> permission = new TreeMap<>();
			permission.put("create", documentPermission.canCreate() ? Boolean.TRUE : Boolean.FALSE);
			permission.put("delete", documentPermission.canDelete() ? Boolean.TRUE : Boolean.FALSE);
			permission.put("read", documentPermission.canRead() ? Boolean.TRUE : Boolean.FALSE);
			permission.put("update", documentPermission.canUpdate() ? Boolean.TRUE : Boolean.FALSE);

			newPermissions.put(documentName, permission);
		}
		result.setDocumentPermissions(newPermissions);
		result.setActions(actions);
		result.setModuleMenuMap(moduleMenuMap);
		// NB accessVectors not required for ClientUserData as these are only used by the server.

		return result;
	}
	
	/**
	 * Clear all the permissions and menus on this user.
	 * <br />
	 * This should generally used before re-populating that data against the user.
	 */
	public void clearAllPermissionsAndMenus() {
		roleNames.clear();
		documentPermissions.clear();
		actions.clear();
		contentRestrictions.clear();
		contentPermissions.clear();
		moduleMenuMap.clear();
		accesses.clear();
	}

	/**
	 * Returns a concise string representation of this user context.
	 *
	 * @return a string containing name, customer, and user identifier.
	 */
	@Override
	public String toString() {
		return MoreObjects.toStringHelper(this)
				.add("name", name)
				.add("customerName", customerName)
				.add("id", id)
				.toString();
	}
}
