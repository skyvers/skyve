package org.skyve.metadata.user;

import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.skyve.domain.messages.AccessException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;

import jakarta.annotation.Nonnull;

/**
 * 
 */
public interface User extends NamedMetaData {
	/**
	 * 
	 * @return
	 */
	String getId();
	void setId(String id);
	
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
	 * 
	 * @return
	 */
	String getContactId();
	
	/**
	 * 
	 * @return
	 */
	String getContactName();
	
	/**
	 * The content ID of the contant image or null if there is no image
	 */
	String getContactImageId();
	
	/**
	 * The URL to use to get a thumbnail of the image from the resource servlet.
	 */
	String getContactImageUrl(int width, int height);
	
	/**
	 * Returns a 2 Upper Case initials for the contact name if possible.
	 * @return	Avatar Initials
	 */
	String getContactAvatarInitials();

	/**
	 * 
	 * @return
	 */
	Customer getCustomer();
	
	String getCustomerName();
	void setCustomerName(String customerName);
	
	/**
	 * 
	 * @return
	 */
	String getDataGroupId();
	void setDataGroupId(String dataGroupId);
	
	/**
	 * 
	 * @return
	 */
	String getHomeModuleName();

	/**
	 * 
	 * @return
	 */
	Set<String> getAccessibleModuleNames();

	/**
	 * 
	 * @param moduleName
	 * @param roleName
	 * @return
	 */
	boolean isInRole(String moduleName, String roleName);
	
	/**
	 * 
	 * @param moduleName
	 * @param documentName
	 * @return
	 */
	DocumentPermissionScope getScope(String moduleName, String documentName);
	
	/**
	 * Determine if we can read the document bean given the document scope etc. 
	 * NB. Cannot select the bean from the data store in this method, coz it may be transient.
	 * 
	 * @param beanBizId
	 * @param beanBizModule
	 * @param beanBizDocument
	 * @param beanBizCustomer
	 * @param beanBizDataGroupId
	 * @param beanBizUserId
	 * @return
	 */
	boolean canReadBean(String beanBizId,
							String beanBizModule,
							String beanBizDocument,
							String beanBizCustomer,
							String beanBizDataGroupId,
							String beanBizUserId);
	
	/**
	 * Indicates if the user is able to search or view the content
	 * 
	 * @return <code>true</code> if access is allowed, otherwise <code>false</code>.
	 */
	boolean canAccessContent(String bizId,
								String bizModule,
								String bizDocument,
								String bizCustomer,
								String bizDataGroupId,
								String bizUserId,
								String attributeName);
	
	/**
	 * 
	 * @param document
	 * @return
	 */
	boolean canAccessDocument(Document document);
	
	/**
	 * 
	 * @param document
	 * @return
	 */
	boolean canCreateDocument(Document document);
	
	/**
	 * 
	 * @return
	 */
	boolean canFlag();
	
	/**
	 * 
	 * @param document
	 * @return
	 */
	boolean canReadDocument(Document document);
	
	/**
	 * 
	 * @return
	 */
	boolean canTextSearch();
	
	/**
	 * 
	 * @param document
	 * @return
	 */
	boolean canUpdateDocument(Document document);
	
	/**
	 * 
	 * @param document
	 * @return
	 */
	boolean canDeleteDocument(Document document);
	
	/**
	 * 
	 * @param document
	 * @param actionName
	 * @return
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
	
	default void checkAccess(@Nonnull UserAccess access, @Nonnull String uxui) {
		if (! canAccess(access, uxui)) {
			final String userName = getName();
			final String moduleName = access.getModuleName();
			final String documentName = access.getDocumentName();
			final String component = access.getComponent();
			final StringBuilder warning = new StringBuilder(256);
			final String resource;
			warning.append("User ").append(userName).append(" cannot access ");
			if (access.isContent()) {
				warning.append("content for document ").append(moduleName).append('.').append(documentName);
				warning.append(" with binding ").append(component);
				warning.append(" with UX/UI ").append(uxui);
				resource = "this content";
			}
			else if (access.isDocumentAggregate()) {
				warning.append("default query for document ").append(moduleName).append('.').append(component);
				warning.append(" with UX/UI ").append(uxui);
				resource = "this query";
			}
			else if (access.isDynamicImage()) {
				warning.append("dynamic image for document ").append(moduleName).append('.').append(documentName);
				warning.append(" with binding ").append(component);
				warning.append(" and UX/UI ").append(uxui);
				resource = "this dynamic image";
			}
			else if (access.isModelAggregate()) {
				warning.append("model for document ").append(moduleName).append('.').append(documentName);
				warning.append(" named ").append(component);
				warning.append(" with UX/UI ").append(uxui);
				resource = "this model";
			}
			else if (access.isPreviousComplete()) {
				warning.append("previous complete for document ").append(moduleName).append('.').append(documentName);
				warning.append(" with binding ").append(component);
				warning.append(" and UX/UI ").append(uxui);
				resource = "this previous data";
			}
			else if (access.isQueryAggregate()) {
				warning.append("query for module ").append(moduleName);
				warning.append(" named ").append(component);
				warning.append(" with UX/UI ").append(uxui);
				resource = "this query";
			}
			else if (access.isReport()) {
				warning.append("report for document ").append(moduleName).append('.').append(documentName);
				warning.append(" named ").append(component);
				warning.append(" with UX/UI ").append(uxui);
				resource = "this report";
			}
			else if (access.isSingular()) {
				warning.append("view for document ").append(moduleName).append('.').append(documentName);
				warning.append(" named ").append(component);
				warning.append(" with UX/UI ").append(uxui);
				resource = "this view";
			}
			else {
				throw new IllegalStateException(access.toString() + " not catered for");
			}

			UtilImpl.LOGGER.warning(warning.toString());
			UtilImpl.LOGGER.info("If this user already has a document or action privilege, check if they were navigated to this page/resource programatically or by means other than the menu or views and need to be granted access via an <accesses> stanza in the module or view XML.");
			throw new AccessException(resource, userName);
		}
	}
	
	/**
	 * User (session) attributes. Keep this small since the user is in the web session.
	 */
	public Map<String, Object> getAttributes();
}
