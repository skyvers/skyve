package org.skyve.impl.cdi;

import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.enterprise.inject.Alternative;

/**
 * Stateless CDI proxy for {@link User}.
 *
 * <p>Delegates to {@link CORE#getUser()} so user-context lookups continue to work
 * after session passivation without serializing the concrete runtime user object.
 */
@Alternative
public class UserInjectable implements User {
	private static final long serialVersionUID = 3550566480873232160L;

	/**
	 * Returns the display name for the current user.
	 *
	 * @return the display name.
	 */
	@Override
	public @Nullable String getName() {
		return CORE.getUser().getName();
	}

	/**
	 * Returns the unique user identifier.
	 *
	 * @return the user identifier.
	 */
	@Override
	public @Nonnull String getId() {
		return CORE.getUser().getId();
	}

	/**
	 * Sets the unique user identifier.
	 *
	 * @param id the user identifier.
	 */
	@Override
	public void setId(@Nonnull String id) {
		CORE.getUser().setId(id);
	}

	/**
	 * Returns the current session identifier.
	 *
	 * @return the session identifier.
	 */
	@Override
	public @Nullable String getSessionId() {
		return CORE.getUser().getSessionId();
	}

	/**
	 * Returns the user's language tag.
	 *
	 * @return the language tag.
	 */
	@Override
	public @Nullable String getLanguageTag() {
		return CORE.getUser().getLanguageTag();
	}

	/**
	 * Returns the user's locale.
	 *
	 * @return the locale.
	 */
	@Override
	public @Nullable Locale getLocale() {
		return CORE.getUser().getLocale();
	}

	/**
	 * Returns the user's password hash.
	 *
	 * @return the password hash.
	 */
	@Override
	public @Nullable String getPasswordHash() {
		return CORE.getUser().getPasswordHash();
	}

	/**
	 * Indicates whether the user must change password.
	 *
	 * @return true if a password change is required; otherwise false.
	 */
	@Override
	public boolean isPasswordChangeRequired() {
		return CORE.getUser().isPasswordChangeRequired();
	}

	/**
	 * Returns the linked contact identifier.
	 *
	 * @return the contact identifier.
	 */
	@Override
	public @Nullable String getContactId() {
		return CORE.getUser().getContactId();
	}

	/**
	 * Returns the linked contact name.
	 *
	 * @return the contact name.
	 */
	@Override
	public @Nullable String getContactName() {
		return CORE.getUser().getContactName();
	}

	/**
	 * Returns the linked contact image identifier.
	 *
	 * @return the contact image identifier.
	 */
	@Override
	public @Nullable String getContactImageId() {
		return CORE.getUser().getContactImageId();
	}

	/**
	 * Returns the contact image URL resized to the requested dimensions.
	 *
	 * @param width the requested image width in pixels.
	 * @param height the requested image height in pixels.
	 * @return the contact image URL.
	 */
	@Override
	public @Nullable String getContactImageUrl(int width, int height) {
		return CORE.getUser().getContactImageUrl(width, height);
	}
	
	/**
	 * Returns avatar initials for the linked contact.
	 *
	 * @return the contact avatar initials.
	 */
	@Override
	public @Nonnull String getContactAvatarInitials() {
		return CORE.getUser().getContactAvatarInitials();
	}
	
	/**
	 * Returns the active customer metadata for this user.
	 *
	 * @return the active customer.
	 */
	@Override
	public @Nonnull Customer getCustomer() {
		return CORE.getUser().getCustomer();
	}

	/**
	 * Returns the active customer name.
	 *
	 * @return the customer name.
	 */
	@Override
	public @Nullable String getCustomerName() {
		return CORE.getUser().getCustomerName();
	}

	/**
	 * Sets the active customer name.
	 *
	 * @param customerName the customer name.
	 */
	@Override
	public void setCustomerName(@Nullable String customerName) {
		CORE.getUser().setCustomerName(customerName);
	}

	/**
	 * Returns the active data group identifier.
	 *
	 * @return the data group identifier.
	 */
	@Override
	public @Nullable String getDataGroupId() {
		return CORE.getUser().getDataGroupId();
	}

	/**
	 * Sets the active data group identifier.
	 *
	 * @param dataGroupId the data group identifier.
	 */
	@Override
	public void setDataGroupId(@Nullable String dataGroupId) {
		CORE.getUser().setDataGroupId(dataGroupId);
	}

	/**
	 * Returns the user's home module name.
	 *
	 * @return the home module name.
	 */
	@Override
	public @Nullable String getHomeModuleName() {
		return CORE.getUser().getHomeModuleName();
	}

	/**
	 * Returns the user's legacy ID.
	 *
	 * @return the legacy ID, or {@code null} if the user has none.
	 */
	@Override
	public @Nullable String getLegacyId() {
		return CORE.getUser().getLegacyId();
	}

	/**
	 * Returns the set of module names this user can access.
	 *
	 * @return accessible module names.
	 */
	@Override
	public @Nonnull Set<String> getAccessibleModuleNames() {
		return CORE.getUser().getAccessibleModuleNames();
	}

	/**
	 * Indicates whether the user belongs to the specified role in a module.
	 *
	 * @param moduleName the module name.
	 * @param roleName the role name.
	 * @return true if the user is in the role; otherwise false.
	 */
	@Override
	public boolean isInRole(@Nonnull String moduleName, @Nonnull String roleName) {
		return CORE.getUser().isInRole(moduleName, roleName);
	}

	/**
	 * Returns document permission scope for a module and document.
	 *
	 * @param moduleName the module name.
	 * @param documentName the document name.
	 * @return the resolved permission scope.
	 */
	@Override
	public @Nonnull DocumentPermissionScope getScope(@Nonnull String moduleName, @Nonnull String documentName) {
		return CORE.getUser().getScope(moduleName, documentName);
	}

	/**
	 * Evaluates bean-level read access against module/document/customer/data-group ownership.
	 *
	 * @param beanBizId the bean identifier.
	 * @param beanBizModule the bean module name.
	 * @param beanBizDocument the bean document name.
	 * @param beanBizCustomer the bean customer name.
	 * @param beanBizDataGroupId the bean data group identifier.
	 * @param beanBizUserId the bean owning user identifier.
	 * @return true if the bean can be read; otherwise false.
	 */
	@Override
	public boolean canReadBean(@Nonnull String beanBizId,
								@Nonnull String beanBizModule,
								@Nonnull String beanBizDocument,
								@Nullable String beanBizCustomer,
								@Nullable String beanBizDataGroupId,
								@Nullable String beanBizUserId) {
		return CORE.getUser().canReadBean(beanBizId,
											beanBizModule,
											beanBizDocument,
											beanBizCustomer,
											beanBizDataGroupId,
											beanBizUserId);
	}

	/**
	 * Evaluates content-attribute access permissions for the supplied bean identity.
	 *
	 * @param bizId the bean identifier.
	 * @param bizModule the bean module name.
	 * @param bizDocument the bean document name.
	 * @param bizCustomer the bean customer name.
	 * @param bizDataGroupId the bean data group identifier.
	 * @param bizUserId the bean owning user identifier.
	 * @param attributeName the attribute name.
	 * @return true if content access is allowed; otherwise false.
	 */
	@Override
	public boolean canAccessContent(@Nonnull String bizId,
										@Nonnull String bizModule,
										@Nonnull String bizDocument,
										@Nullable String bizCustomer,
										@Nullable String bizDataGroupId,
										@Nullable String bizUserId,
										@Nonnull String attributeName) {
		return CORE.getUser().canAccessContent(bizId,
												bizModule,
												bizDocument,
												bizCustomer,
												bizDataGroupId,
												bizUserId,
												attributeName);
	}

	/**
	 * Indicates whether the user can access the supplied document.
	 *
	 * @param document the document metadata.
	 * @return true if access is allowed; otherwise false.
	 */
	@Override
	public boolean canAccessDocument(@Nonnull Document document) {
		return CORE.getUser().canAccessDocument(document);
	}

	/**
	 * Indicates whether the user can create instances of the supplied document.
	 *
	 * @param document the document metadata.
	 * @return true if create is allowed; otherwise false.
	 */
	@Override
	public boolean canCreateDocument(@Nonnull Document document) {
		return CORE.getUser().canCreateDocument(document);
	}

	/**
	 * Indicates whether the user can use record flagging features.
	 *
	 * @return true if flagging is allowed; otherwise false.
	 */
	@Override
	public boolean canFlag() {
		return CORE.getUser().canFlag();
	}

	/**
	 * Indicates whether the user can read instances of the supplied document.
	 *
	 * @param document the document metadata.
	 * @return true if read is allowed; otherwise false.
	 */
	@Override
	public boolean canReadDocument(@Nonnull Document document) {
		return CORE.getUser().canReadDocument(document);
	}

	/**
	 * Indicates whether the user can use text search features.
	 *
	 * @return true if text search is allowed; otherwise false.
	 */
	@Override
	public boolean canTextSearch() {
		return CORE.getUser().canTextSearch();
	}

	/**
	 * Indicates whether the user can switch runtime mode.
	 *
	 * @return true if mode switching is allowed; otherwise false.
	 */
	@Override
	public boolean canSwitchMode() {
		return CORE.getUser().canSwitchMode();
	}

	/**
	 * Indicates whether the user can update instances of the supplied document.
	 *
	 * @param document the document metadata.
	 * @return true if update is allowed; otherwise false.
	 */
	@Override
	public boolean canUpdateDocument(@Nonnull Document document) {
		return CORE.getUser().canUpdateDocument(document);
	}

	/**
	 * Indicates whether the user can delete instances of the supplied document.
	 *
	 * @param document the document metadata.
	 * @return true if delete is allowed; otherwise false.
	 */
	@Override
	public boolean canDeleteDocument(@Nonnull Document document) {
		return CORE.getUser().canDeleteDocument(document);
	}

	/**
	 * Indicates whether the user can execute a named action on a document.
	 *
	 * @param document the document metadata.
	 * @param actionName the action name.
	 * @return true if action execution is allowed; otherwise false.
	 */
	@Override
	public boolean canExecuteAction(@Nonnull Document document, @Nonnull String actionName) {
		return CORE.getUser().canExecuteAction(document, actionName);
	}

	/**
	 * Evaluates UI navigation access for the given target and UX/UI context.
	 *
	 * @param access the access target descriptor.
	 * @param uxui the UX/UI identifier.
	 * @return true if access is allowed; otherwise false.
	 */
	@Override
	public boolean canAccess(@Nonnull UserAccess access, @Nonnull String uxui) {
		return CORE.getUser().canAccess(access, uxui);
	}
	
	/**
	 * Returns mutable session-scoped user attributes.
	 *
	 * @return mutable user attributes.
	 */
	@Override
	public @Nonnull Map<String, Object> getAttributes() {
		return CORE.getUser().getAttributes();
	}
}
