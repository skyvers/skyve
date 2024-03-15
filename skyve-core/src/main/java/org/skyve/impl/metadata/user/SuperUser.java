package org.skyve.impl.metadata.user;

import java.util.Locale;
import java.util.Set;
import java.util.TreeSet;

import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;

public class SuperUser extends UserImpl {
	private static final long serialVersionUID = -6233814867322594601L;

	public SuperUser() {
		setWebLocale(Locale.ENGLISH);
	}
	
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

	@Override
	public boolean isInRole(String moduleName, String roleName) {
		return true;
	}
	
	@Override
	public DocumentPermissionScope getScope(String moduleName, String documentName) {
		DocumentPermissionScope result = super.getScope(moduleName, documentName);
		if (DocumentPermissionScope.none.equals(result)) {
			result = DocumentPermissionScope.customer;
		}
		
		return result;
	}
	
	@Override
	public boolean canReadBean(String beanBizId,
								String beanBizModule,
								String beanBizDocument,
								String beanBizCustomer,
								String beanBizDataGroupId,
								String beanBizUserId) {
		return true;
	}

	@Override
	public boolean canAccessDocument(Document document) {
		return true;
	}

	@Override
	public boolean canCreateDocument(Document document) {
		return true;
	}

	@Override
	public boolean canDeleteDocument(Document document) {
		return true;
	}

	@Override
	public boolean canExecuteAction(Document document, String actionName) {
		return true;
	}
	
	@Override
	public boolean canFlag() {
		return true;
	}

	@Override
	public boolean canReadDocument(Document document) {
		return true;
	}
	
	@Override
	public boolean canTextSearch() {
		return true;
	}
	
	@Override
	public boolean canSwitchMode() {
		return true;
	}

	@Override
	public boolean canUpdateDocument(Document document) {
		return true;
	}

	@Override
	public Set<String> getAccessibleModuleNames() {
		try {
			return new TreeSet<>(((CustomerImpl) getCustomer()).getModuleEntries().keySet());//Repository.get().getAllVanillaModuleNames());
		}
		catch (MetaDataException e) {
			e.printStackTrace();
		}

		return null;
	}
}
