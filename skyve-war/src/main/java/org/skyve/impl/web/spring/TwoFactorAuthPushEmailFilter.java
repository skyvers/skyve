package org.skyve.impl.web.spring;

import org.skyve.CORE;
import org.skyve.impl.util.UtilImpl;
import org.skyve.persistence.DocumentQuery;
import org.springframework.security.authentication.AuthenticationManager;

import modules.admin.domain.User;

public class TwoFactorAuthPushEmailFilter extends TwoFactorAuthPushFilter {

	public TwoFactorAuthPushEmailFilter(AuthenticationManager authenticationManager) {
		super(authenticationManager);
		// TODO Auto-generated constructor stub
	}

	@Override
	protected void pushNotifcation(String code) {
		UtilImpl.LOGGER.info("ELTRACEDEV send 2fa code push notification code is : " + code ); 
		
	}

	@Override
	protected UserDB getUserDB(String customerName, String username) {
		DocumentQuery q = CORE.getPersistence().newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME);
		
		q.getFilter().addEquals(User.CUSTOMER_NAME, customerName);
		q.getFilter().addEquals(User.userNamePropertyName, username);
		
		User user = q.beanResult();
		UserDB result = null;
		
		if (user != null) {
			result = new UserDB();
			result.bizCustomer = customerName;
			result.password = user.getPassword();
			result.twoFactorCode = user.getTwoFactorCode();
			result.twoFactorCodeGeneratedDateTime = user.getTwoFactorCodeGeneratedDateTime();
			result.twoFactorToken = user.getTwoFactorToken();
		}
		
		return result;
	}

	@Override
	protected boolean customerRequiresTFAPushNotification() {
		return true;
	}

	@Override
	protected void updateUserDB(UserDB user) {
		DocumentQuery q = CORE.getPersistence().newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME);
		
		q.getFilter().addEquals(User.CUSTOMER_NAME, user.bizCustomer);
		q.getFilter().addEquals(User.userNamePropertyName, user.username);
		
		User u = q.beanResult();
		if (u != null) {
			u.setTwoFactorCode(user.twoFactorCode);
			u.setTwoFactorCodeGeneratedDateTime(user.twoFactorCodeGeneratedDateTime);
			u.setTwoFactorToken(user.twoFactorToken);
			
			u = CORE.getPersistence().save(u);
			CORE.getPersistence().commit(true);
			CORE.getPersistence().evictCached(u);
		}
	}

	
	
}
