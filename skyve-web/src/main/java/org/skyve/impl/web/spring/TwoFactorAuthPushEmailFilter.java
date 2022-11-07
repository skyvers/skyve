package org.skyve.impl.web.spring;

import org.skyve.impl.util.UtilImpl;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.provisioning.JdbcUserDetailsManager;

public class TwoFactorAuthPushEmailFilter extends TwoFactorAuthPushFilter {

	public TwoFactorAuthPushEmailFilter(AuthenticationManager authenticationManager, JdbcUserDetailsManager userDetailsService) {
		super(authenticationManager, userDetailsService);
	}

	@Override
	protected void pushNotifcation(String code) {
		UtilImpl.LOGGER.info("2fa code push notification code is : " + code ); 
		
	}


}
