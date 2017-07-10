package org.skyve.impl.metadata.repository;

import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.util.SQLMetaDataUtil;

/**
 * Adds security integration to LocalDesignRepository.
 * 
 * @author Mike
 */
public class LocalSecureRepository extends LocalDesignRepository {
	@Override
	public UserImpl retrieveUser(String userPrincipal) {
		if (userPrincipal == null) {
			throw new IllegalStateException("No-one is logged in - cannot retrieve the skyve user.");
		}

		UserImpl result = AbstractRepository.setCustomerAndUserFromPrincipal(userPrincipal);

		SQLMetaDataUtil.populateUser(result);
		resetMenus(result);
		if (result.getLanguageTag() == null) {
			result.setLanguageTag(result.getCustomer().getLanguageTag());
		}
		
		return result;
	}
}
