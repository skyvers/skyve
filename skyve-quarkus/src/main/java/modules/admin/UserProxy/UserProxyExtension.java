package modules.admin.UserProxy;

import java.sql.Connection;

import org.skyve.CORE;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.util.UtilImpl;

import modules.admin.User.UserExtension;
import modules.admin.domain.User;
import modules.admin.domain.UserProxy;

public class UserProxyExtension extends UserProxy {
	private static final long serialVersionUID = 6917022687678513883L;

	/**
	 * Return the metadata user that is this user
	 * 
	 * @return the metadata user that is this user
	 */
	public org.skyve.metadata.user.User toMetaDataUser() {
		UserImpl result = null;
		if (isPersisted()) {
			// Populate the user using the persistence connection since it might have just been inserted and not committed yet
			result = ProvidedRepositoryFactory.setCustomerAndUserFromPrincipal((UtilImpl.CUSTOMER == null) ?
																					getBizCustomer() + "/" + getUserName() : 
																					getUserName());
			if (result != null) {
				result.clearAllPermissionsAndMenus();
				@SuppressWarnings("resource")
				Connection connection = ((AbstractHibernatePersistence) CORE.getPersistence()).getConnection();
				ProvidedRepositoryFactory.get().populateUser(result, connection);
			}
		}

		return result;
	}

	/**
	 * Return the user from this user proxy, if persisted, or null if not persisted.
	 * 
	 * @return User
	 */
	public UserExtension toUser() {
		return CORE.getPersistence().retrieve(User.MODULE_NAME, User.DOCUMENT_NAME, getBizId());
	}
}
