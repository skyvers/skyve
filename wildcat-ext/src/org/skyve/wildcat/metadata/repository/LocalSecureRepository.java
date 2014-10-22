package org.skyve.wildcat.metadata.repository;

import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.user.User;
import org.skyve.wildcat.metadata.user.UserImpl;
import org.skyve.wildcat.util.SQLMetaDataUtil;
import org.skyve.wildcat.util.UtilImpl;

/**
 * Adds security integration to LocalDesignRepository.
 * 
 * @author Mike
 */
public class LocalSecureRepository extends LocalDesignRepository {
	@Override
	public User retrieveUser(String userPrincipal) throws MetaDataException {
		UserImpl result = new UserImpl();

		if (userPrincipal == null) {
			throw new IllegalStateException("No-one is logged in - cannot retrieve the WILDCAT user.");
		}

		// There are 3 login situations 
		// 1. Java EE web login is used and a username which includes the customer is received eg "bizhub/mike".
		// 2. Java EE web login is used but the "CUSTOMER" parameter is set in web.xml - every login should be for this customer.
		// 3. Single Sign On (SPNEGO/KERBEROS) is used - every login is the same as the network name eg "sandsm.bizhub.com.au" and "CUSTOMER" parameter is set in web.xml
		int slashIndex = userPrincipal.indexOf('/');
		if (slashIndex >= 0) {
			String customerName = userPrincipal.substring(0, slashIndex);
			String userName = userPrincipal.substring(slashIndex + 1);
			result.setName(userName);
			result.setCustomerName(customerName);
		}
		else {
			result.setName(userPrincipal);
		}
		if (UtilImpl.CUSTOMER != null) {
			result.setCustomerName(UtilImpl.CUSTOMER);
		}

		SQLMetaDataUtil.populateUser(result);
		resetMenus(result);

		return result;
	}
}
