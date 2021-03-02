package modules.admin.UserProxy;

import org.skyve.CORE;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.util.SQLMetaDataUtil;
import org.skyve.impl.util.UtilImpl;

import modules.admin.domain.UserProxy;

public class UserProxyExtension extends UserProxy {
	private static final long serialVersionUID = 6917022687678513883L;

	/**
	 * Return the metadata user that is this user
	 * 
	 * @return the metadata user that is this user
	 */
	public org.skyve.metadata.user.User toMetaDataUser() {
		UserImpl metaDataUser = null;
		if(isPersisted()) {
			// Populate the user using the persistence connection since it might have just been inserted and not committed yet
			metaDataUser = AbstractRepository.setCustomerAndUserFromPrincipal((UtilImpl.CUSTOMER == null) ? getBizCustomer() + "/" + getUserName() : getUserName());
			metaDataUser.clearAllPermissionsAndMenus();
			SQLMetaDataUtil.populateUser(metaDataUser, ((AbstractHibernatePersistence) CORE.getPersistence()).getConnection());
		}
		
		return metaDataUser;
	}
}
