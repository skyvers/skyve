package org.skyve.impl.metadata.user;

import java.util.Set;
import java.util.TreeSet;

import javax.enterprise.inject.Alternative;

import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.User;
import org.skyve.impl.metadata.user.UserImpl;

@Alternative
public class SuperUser extends UserImpl {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -6233814867322594601L;

	public SuperUser() {
		roleNames.add(SUPER_ROLE);
	}
	
	public SuperUser(User user) {
		this();
    	setContactId(user.getContactId());
    	setContactName(user.getContactName());
    	setCustomerName(user.getCustomerName());
    	setDataGroupId(user.getDataGroupId());
    	setHomeModuleName(user.getHomeModuleName());
    	setId(user.getId());
    	setLanguageTag(user.getLanguageTag());
    	setName(user.getName());
    	setPasswordHash(user.getPasswordHash());
    	setPasswordChangeRequired(user.isPasswordChangeRequired());
    	setWebLocale(user.getLocale());
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
	public boolean canReadDocument(Document document) {
		return true;
	}

	@Override
	public boolean canUpdateDocument(Document document) {
		return true;
	}

	@Override
	public Set<String> getAccessibleModuleNames() {
		try {
			return new TreeSet<>(((CustomerImpl) getCustomer()).getModuleNames());//Repository.get().getAllVanillaModuleNames());
		}
		catch (MetaDataException e) {
			e.printStackTrace();
		}

		return null;
	}
}
