package org.skyve.metadata.user;

import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.controller.Observer;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Represents an authenticated Skyve user within a customer (tenant) context.
 *
 * <p>A {@code User} is the central security principal in Skyve. It carries identity
 * information (id, name, contact), tenancy context (customer, data group), locale
 * preferences, and role membership. The security layer consults the user to determine
 * what documents, beans, actions, and resources the current principal may access.
 *
 * <p>Permission tests are split into two categories:
 * <ul>
 *   <li><strong>Document-level:</strong> {@link #canAccessDocument}, {@link #canCreateDocument},
 *       {@link #canReadDocument}, {@link #canUpdateDocument}, {@link #canDeleteDocument},
 *       {@link #canExecuteAction} — determined by the role-based
 *       {@link DocumentPermission}s assigned to the user.</li>
 *   <li><strong>Bean-level:</strong> {@link #canReadBean} — additionally considers the
 *       data-group and user-ownership scope when the permission scope is narrower than
 *       {@link DocumentPermissionScope#customer}.</li>
 * </ul>
 *
 * <p>Thread-confined: a {@code User} instance is bound to the web session and must
 * not be shared across threads without external synchronisation.
 *
 * @see DocumentPermission
 * @see DocumentPermissionScope
 * @see Role
 * @see UserAccess
 */
public interface User extends NamedMetaData {
	/**
	 * Returns the unique identifier for this user, typically the database primary key.
	 *
	 * @return the user id; never {@code null}
	 */
	@Nonnull String getId();
	void setId(@Nonnull String id);
	
	/**
	 * Get the session ID of the session this user is in.
	 * This can be used for customising per user session in tandem with an {#{@link Observer}.
	 * It is used by the session scoped meta-data implementation as a key.
	 * @return	The session ID.
	 */
	@Nullable String getSessionId();
	
	/**
	 * Returns the language tag (BCP 47) configured for this user, or {@code null} if
	 * no explicit per-user language is set.
	 *
	 * <p>Used by {@link #getLocale()} as the first preference in locale resolution.
	 *
	 * @return the user's language tag, or {@code null}
	 */
	String getLanguageTag();
	
	/**
	 * Determine the user's locale in the following way.
	 * If user's language tag is set, use this.
	 * If the customer's language tag is set, use this.
	 * Otherwise, use the locale of the browser.
	 * 
	 * @return	The appropriate locale.
	 */
	Locale getLocale();
	
	/**
	 * @return	The hashed password value as stored in the data store.
	 */
	String getPasswordHash();

	/**
	 * Does the password need to be changed before accessing the system.
	 * 
	 * @return <code>true</code> if a change of password is required, 
	 * 			otherwise <code>false</code>.
	 */
	boolean isPasswordChangeRequired();
	
	/**
	 * Returns the unique identifier of the contact record associated with this user,
	 * or {@code null} if no contact is linked.
	 *
	 * @return the contact bean id, or {@code null}
	 */
	String getContactId();
	
	/**
	 * Returns the display name of the contact linked to this user.
	 *
	 * @return the contact name, or {@code null} if no contact is linked
	 */
	String getContactName();
	
	/**
	 * Returns the content repository ID of the user's contact avatar image,
	 * or {@code null} if no image has been uploaded.
	 *
	 * @return the content id of the avatar image, or {@code null}
	 */
	String getContactImageId();
	
	/**
	 * Returns the URL for a thumbnail of the user's contact image at the given dimensions.
	 *
	 * <p>The URL targets the Skyve resource servlet and includes the width and height
	 * as parameters. Returns {@code null} if no contact image is set.
	 *
	 * @param width   the desired thumbnail width in pixels
	 * @param height  the desired thumbnail height in pixels
	 * @return the thumbnail URL, or {@code null}
	 */
	String getContactImageUrl(int width, int height);
	
	/**
	 * Returns a 2 Upper Case initials for the contact name if possible.
	 * @return	Avatar Initials
	 */
	String getContactAvatarInitials();

	/**
	 * Returns the resolved {@link Customer} for this user.
	 *
	 * <p>The customer provides access to modules, document metadata overrides, and
	 * tenant-specific configuration.
	 *
	 * @return the user's customer; never {@code null}
	 */
	Customer getCustomer();
	
	String getCustomerName();
	void setCustomerName(String customerName);
	
	/**
	 * Returns the data group identifier scoping this user's data access, or {@code null}
	 * for users not assigned to a data group.
	 *
	 * <p>When a data group is set, document permissions with
	 * {@link DocumentPermissionScope#dataGroup} scope restrict the user to instances
	 * owned by the same group.
	 *
	 * @return the data group id, or {@code null} if not applicable
	 */
	String getDataGroupId();
	void setDataGroupId(String dataGroupId);
	
	/**
	 * Returns the name of the module that is the default landing page for this user.
	 *
	 * @return the home module name; may be {@code null}
	 */
	String getHomeModuleName();

	/**
	 * Returns the names of all modules accessible to this user based on their role
	 * assignments.
	 *
	 * @return a non-{@code null} set of accessible module names
	 */
	Set<String> getAccessibleModuleNames();

	/**
	 * Returns {@code true} if this user has been assigned the specified role.
	 *
	 * @param moduleName  the module that declares the role; must not be {@code null}
	 * @param roleName    the role name within the module; must not be {@code null}
	 * @return {@code true} if the user holds the role
	 */
	boolean isInRole(String moduleName, String roleName);
	
	/**
	 * Returns the effective {@link DocumentPermissionScope} for the user on the given
	 * document, computed as the union of all assigned role permissions.
	 *
	 * @param moduleName    the module that owns the document; must not be {@code null}
	 * @param documentName  the document name; must not be {@code null}
	 * @return the effective scope; {@link DocumentPermissionScope#none} if no roles grant access
	 */
	DocumentPermissionScope getScope(String moduleName, String documentName);
	
	/**
	 * Returns {@code true} if this user can read the given bean, considering both the
	 * document-level permission scope and the bean's ownership attributes.
	 *
	 * <p>This check is performed on in-memory beans before any data-store operation.
	 * The ownership attributes ({@code bizCustomer}, {@code bizDataGroupId},
	 * {@code bizUserId}) are used to enforce {@link DocumentPermissionScope#dataGroup}
	 * and {@link DocumentPermissionScope#user} restrictions without reloading the bean.
	 *
	 * @param beanBizId           the bean's primary key
	 * @param beanBizModule       the module that owns the bean's document
	 * @param beanBizDocument     the document name
	 * @param beanBizCustomer     the customer that owns the bean
	 * @param beanBizDataGroupId  the data group that owns the bean, or {@code null}
	 * @param beanBizUserId       the user id that owns the bean, or {@code null}
	 * @return {@code true} if the user may read this bean
	 */
	boolean canReadBean(String beanBizId,
							String beanBizModule,
							String beanBizDocument,
							String beanBizCustomer,
							String beanBizDataGroupId,
							String beanBizUserId);
	
	/**
	 * Returns {@code true} if this user can search or view the given content attachment.
	 *
	 * <p>The check considers the document-level permission scope and the ownership
	 * attributes of the bean that holds the content.
	 *
	 * @param bizId           the bean's primary key
	 * @param bizModule       the module that owns the bean's document
	 * @param bizDocument     the document name
	 * @param bizCustomer     the customer that owns the bean
	 * @param bizDataGroupId  the data group that owns the bean, or {@code null}
	 * @param bizUserId       the user id that owns the bean, or {@code null}
	 * @param attributeName   the content attribute binding name
	 * @return {@code true} if access to the content is allowed
	 */
	boolean canAccessContent(String bizId,
								String bizModule,
								String bizDocument,
								String bizCustomer,
								String bizDataGroupId,
								String bizUserId,
								String attributeName);
	
	/**
	 * Returns {@code true} if this user has any form of access to the given document
	 * (at minimum, read permission at any scope).
	 *
	 * @param document  the document to test; must not be {@code null}
	 * @return {@code true} if the user can access the document
	 */
	boolean canAccessDocument(Document document);
	
	/**
	 * Returns {@code true} if this user may create new instances of the given document.
	 *
	 * @param document  the document to test; must not be {@code null}
	 * @return {@code true} if create permission is granted
	 */
	boolean canCreateDocument(Document document);
	
	/**
	 * Returns {@code true} if this user has permission to use the flag/notes feature.
	 *
	 * @return {@code true} if flagging is permitted
	 */
	boolean canFlag();
	
	/**
	 * Returns {@code true} if this user may read instances of the given document.
	 *
	 * @param document  the document to test; must not be {@code null}
	 * @return {@code true} if read permission is granted
	 */
	boolean canReadDocument(Document document);
	
	/**
	 * Returns {@code true} if this user has access to the full-text search feature.
	 *
	 * @return {@code true} if text search is permitted
	 */
	boolean canTextSearch();
	
	/**
	 * Returns {@code true} if this user may switch between power user and simple modes
	 * in the UI.
	 *
	 * @return {@code true} if mode switching is permitted
	 */
	boolean canSwitchMode();
	
	/**
	 * Returns {@code true} if this user may update existing instances of the given document.
	 *
	 * @param document  the document to test; must not be {@code null}
	 * @return {@code true} if update permission is granted
	 */
	boolean canUpdateDocument(Document document);
	
	/**
	 * Returns {@code true} if this user may delete instances of the given document.
	 *
	 * @param document  the document to test; must not be {@code null}
	 * @return {@code true} if delete permission is granted
	 */
	boolean canDeleteDocument(Document document);
	
	/**
	 * Returns {@code true} if this user may execute the named action on the given document.
	 *
	 * @param document    the document that declares the action; must not be {@code null}
	 * @param actionName  the name of the action to test; must not be {@code null}
	 * @return {@code true} if the action is permitted
	 */
	boolean canExecuteAction(Document document, String actionName);
	
	/**
	 * Does the given router UX/UI have access to the given UserAccess.
	 * @param access The user access to test.
	 * @param uxui	The UX/UI name to test for.
	 * 
	 * @return	true if the user is able to access the application in this way.
	 */
	boolean canAccess(@Nonnull UserAccess access, @Nonnull String uxui);
	
	/**
	 * User (session) attributes. Keep this small since the user is in the web session.
	 */
	public Map<String, Object> getAttributes();
}
