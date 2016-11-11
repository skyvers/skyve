package org.skyve.impl.metadata.user;

import java.util.Set;
import java.util.TreeSet;

import org.skyve.domain.messages.DomainException;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.document.Document;
import org.skyve.impl.metadata.user.UserImpl;

public class SuperUser extends UserImpl {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -6233814867322594601L;

	public SuperUser() {
		roleNames.add(SUPER_ROLE);
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
