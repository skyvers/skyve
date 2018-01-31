package org.skyve.impl.metadata.repository;

import javax.enterprise.inject.Alternative;

import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.util.SQLMetaDataUtil;
import org.skyve.metadata.user.User;

/**
 * Adds security integration to LocalDesignRepository.
 * 
 * @author Mike
 */
@Alternative
public class LocalSecureRepository extends LocalDesignRepository {
	@Override
	public UserImpl retrieveUser(String userPrincipal) {
		if (userPrincipal == null) {
			throw new IllegalStateException("No-one is logged in - cannot retrieve the skyve user.");
		}

		UserImpl result = AbstractRepository.setCustomerAndUserFromPrincipal(userPrincipal);

		resetUserPermissions(result);
		
		if (result.getLanguageTag() == null) {
			result.setLanguageTag(result.getCustomer().getLanguageTag());
		}
		
		return result;
	}
	
	
	@Override
	public void populatePermissions(User user) {
		SQLMetaDataUtil.populateUser(user);
	}
	
	
	@Override
	public void resetUserPermissions(User user) {
		UserImpl impl = (UserImpl) user;
		impl.clearAllPermissionsAndMenus();
		populatePermissions(user);
		resetMenus(user);
	}
}
