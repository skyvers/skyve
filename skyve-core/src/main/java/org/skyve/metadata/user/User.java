package org.skyve.metadata.user;

import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;

/**
 * 
 */
public interface User extends NamedMetaData {
	/**
	 * 
	 * @return
	 */
	public String getId();
	public void setId(String id);
	
	public String getLanguageTag();
	
	/**
	 * Determine the user's locale in the following way.
	 * If user's language tag is set, use this.
	 * If the customer's language tag is set, use this.
	 * Otherwise, use the locale of the browser.
	 * 
	 * @return	The appropriate locale.
	 */
	public Locale getLocale();
	
	/**
	 * @return	The hashed password value as stored in the data store.
	 */
	public String getPasswordHash();

	/**
	 * Does the password need to be changed before accessing the system.
	 * 
	 * @return <code>true</code> if a change of password is required, 
	 * 			otherwise <code>false</code>.
	 */
	public boolean isPasswordChangeRequired();
	
	/**
	 * 
	 * @return
	 */
	public String getContactId();
	
	/**
	 * 
	 * @return
	 */
	public String getContactName();
	
	/**
	 * The content ID of the contant image or null if there is no image
	 */
	public String getContactImageId();
	
	/**
	 * The URL to use to get a thumbnail of the image from the resource servlet.
	 */
	public String getContactImageUrl(int width, int height);
	
	/**
	 * 
	 * @return
	 */
	public Customer getCustomer();
	
	public String getCustomerName();
	public void setCustomerName(String customerName);
	
	/**
	 * 
	 * @return
	 */
	public String getDataGroupId();
	public void setDataGroupId(String dataGroupId);
	
	/**
	 * 
	 * @return
	 */
	public String getHomeModuleName();

	/**
	 * 
	 * @return
	 */
	public Set<String> getAccessibleModuleNames();

	/**
	 * 
	 * @param moduleName
	 * @param roleName
	 * @return
	 */
	public boolean isInRole(String moduleName, String roleName);
	
	/**
	 * 
	 * @param moduleName
	 * @param documentName
	 * @return
	 */
	public DocumentPermissionScope getScope(String moduleName, String documentName);
	
	/**
	 * Determine if we can read the document bean given the document scope etc. 
	 * NB. Cannot select the bean from bizhub data store in this method, coz it may be transient.
	 * 
	 * @param beanBizId
	 * @param beanBizModule
	 * @param beanBizDocument
	 * @param beanBizCustomer
	 * @param beanBizDataGroupId
	 * @param beanBizUserId
	 * @return
	 */
	public boolean canReadBean(String beanBizId,
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
	public boolean canAccessContent(String bizId,
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
	public boolean canAccessDocument(Document document);
	
	/**
	 * 
	 * @param document
	 * @return
	 */
	public boolean canCreateDocument(Document document);
	
	/**
	 * 
	 * @param document
	 * @return
	 */
	public boolean canReadDocument(Document document);
	
	/**
	 * 
	 * @param document
	 * @return
	 */
	public boolean canUpdateDocument(Document document);
	
	/**
	 * 
	 * @param document
	 * @return
	 */
	public boolean canDeleteDocument(Document document);
	
	/**
	 * 
	 * @param document
	 * @param actionName
	 * @return
	 */
	public boolean canExecuteAction(Document document, String actionName);
	
	/**
	 * User (session) attributes. Keep this small since the user is in the web session.
	 */
	public Map<String, Object> getAttributes();
}
