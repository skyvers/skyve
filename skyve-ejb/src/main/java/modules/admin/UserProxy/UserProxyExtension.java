package modules.admin.UserProxy;

import java.sql.Connection;

import org.skyve.CORE;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.util.SQLMetaDataUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.persistence.DocumentQuery;

import modules.admin.User.UserExtension;
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
		if (isPersisted()) {
			// Populate the user using the persistence connection since it might have just been inserted and not committed yet
			metaDataUser = ProvidedRepositoryFactory.setCustomerAndUserFromPrincipal(
					(UtilImpl.CUSTOMER == null) ? getBizCustomer() + "/" + getUserName() : getUserName());
			metaDataUser.clearAllPermissionsAndMenus();
			@SuppressWarnings("resource")
			Connection connection = ((AbstractHibernatePersistence) CORE.getPersistence()).getConnection();
			SQLMetaDataUtil.populateUser(metaDataUser, connection);
		}

		return metaDataUser;
	}

	/**
	 * Return the user from the user proxy
	 * 
	 * @param userProxy
	 * @return
	 * @throws Exception
	 */
	public UserExtension userFromUserProxy() throws Exception {
		DocumentQuery q = CORE.getPersistence().newDocumentQuery(UserExtension.MODULE_NAME, UserExtension.DOCUMENT_NAME);
		q.getFilter().addEquals(contactPropertyName, this.getContact());
		UserExtension ue = q.beanResult();

		if (ue == null) {
			throw new Exception("No User exists for this User Proxy");
		}

		return ue;
	}
}
