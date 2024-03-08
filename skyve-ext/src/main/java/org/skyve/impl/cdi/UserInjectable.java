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

import jakarta.enterprise.inject.Alternative;

/**
 * A proxy that can be Serialized with no state and continue to work after deserialization.
 * 
 * @author mike
 */
@Alternative
public class UserInjectable implements User {
	private static final long serialVersionUID = 3550566480873232160L;

	@Override
	public String getName() {
		return CORE.getUser().getName();
	}

	@Override
	public String getId() {
		return CORE.getUser().getId();
	}

	@Override
	public void setId(String id) {
		CORE.getUser().setId(id);
	}

	@Override
	public String getLanguageTag() {
		return CORE.getUser().getLanguageTag();
	}

	@Override
	public Locale getLocale() {
		return CORE.getUser().getLocale();
	}

	@Override
	public String getPasswordHash() {
		return CORE.getUser().getPasswordHash();
	}

	@Override
	public boolean isPasswordChangeRequired() {
		return CORE.getUser().isPasswordChangeRequired();
	}

	@Override
	public String getContactId() {
		return CORE.getUser().getContactId();
	}

	@Override
	public String getContactName() {
		return CORE.getUser().getContactName();
	}

	@Override
	public String getContactImageId() {
		return CORE.getUser().getContactImageId();
	}

	@Override
	public String getContactImageUrl(int width, int height) {
		return CORE.getUser().getContactImageUrl(width, height);
	}
	
	@Override
	public String getContactAvatarInitials() {
		return CORE.getUser().getContactAvatarInitials();
	}
	
	@Override
	public Customer getCustomer() {
		return CORE.getUser().getCustomer();
	}

	@Override
	public String getCustomerName() {
		return CORE.getUser().getCustomerName();
	}

	@Override
	public void setCustomerName(String customerName) {
		CORE.getUser().setCustomerName(customerName);
	}

	@Override
	public String getDataGroupId() {
		return CORE.getUser().getDataGroupId();
	}

	@Override
	public void setDataGroupId(String dataGroupId) {
		CORE.getUser().setDataGroupId(dataGroupId);
	}

	@Override
	public String getHomeModuleName() {
		return CORE.getUser().getHomeModuleName();
	}

	@Override
	public Set<String> getAccessibleModuleNames() {
		return CORE.getUser().getAccessibleModuleNames();
	}

	@Override
	public boolean isInRole(String moduleName, String roleName) {
		return CORE.getUser().isInRole(moduleName, roleName);
	}

	@Override
	public DocumentPermissionScope getScope(String moduleName, String documentName) {
		return CORE.getUser().getScope(moduleName, documentName);
	}

	@Override
	public boolean canReadBean(String beanBizId,
								String beanBizModule,
								String beanBizDocument,
								String beanBizCustomer,
								String beanBizDataGroupId,
								String beanBizUserId) {
		return CORE.getUser().canReadBean(beanBizId,
											beanBizModule,
											beanBizDocument,
											beanBizCustomer,
											beanBizDataGroupId,
											beanBizUserId);
	}

	@Override
	public boolean canAccessContent(String bizId,
										String bizModule,
										String bizDocument,
										String bizCustomer,
										String bizDataGroupId,
										String bizUserId,
										String attributeName) {
		return CORE.getUser().canAccessContent(bizId,
												bizModule,
												bizDocument,
												bizCustomer,
												bizDataGroupId,
												bizUserId,
												attributeName);
	}

	@Override
	public boolean canAccessDocument(Document document) {
		return CORE.getUser().canAccessDocument(document);
	}

	@Override
	public boolean canCreateDocument(Document document) {
		return CORE.getUser().canCreateDocument(document);
	}

	@Override
	public boolean canReadDocument(Document document) {
		return CORE.getUser().canReadDocument(document);
	}
	
	@Override
	public boolean canTextSearch() {
		return CORE.getUser().canTextSearch();
	}

	@Override
	public boolean canUpdateDocument(Document document) {
		return CORE.getUser().canUpdateDocument(document);
	}

	@Override
	public boolean canDeleteDocument(Document document) {
		return CORE.getUser().canDeleteDocument(document);
	}

	@Override
	public boolean canExecuteAction(Document document, String actionName) {
		return CORE.getUser().canExecuteAction(document, actionName);
	}

	@Override
	public boolean canAccess(UserAccess access, String uxui) {
		return CORE.getUser().canAccess(access, uxui);
	}
	
	@Override
	public Map<String, Object> getAttributes() {
		return CORE.getUser().getAttributes();
	}
}
