package org.skyve.impl.web.spring;

import org.skyve.EXT;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Mail;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.provisioning.JdbcUserDetailsManager;

public class TwoFactorAuthPushEmailFilter extends TwoFactorAuthPushFilter {
	public static final String SYSTEM_TWO_FACTOR_CODE = "SYSTEM Two Factor Code";
	public static final String SYSTEM_TWO_FACTOR_CODE_SUBJECT = "Please verify your email security code";
	public static final String SYSTEM_TWO_FACTOR_CODE_BODY = "Hi %s,<br /><br />Your verification code is: %s<br />"; 

	public TwoFactorAuthPushEmailFilter(AuthenticationManager authenticationManager, JdbcUserDetailsManager userDetailsService) {
		super(authenticationManager, userDetailsService);
	}

	@Override
	protected void pushNotification(UserTFA user, String code) {
		UtilImpl.LOGGER.info("2fa code push notification code is : " + code); 
		
		String emailAddress = user.getEmail();
		if (emailAddress == null) {
			UtilImpl.LOGGER.warning("No email found for user : " + user.getUsername()); 
			return;
		}
		
		String emailBody = String.format(SYSTEM_TWO_FACTOR_CODE_BODY, user.getUser(), code);
		
		EXT.sendMail(new Mail().addTo(emailAddress)
								.from(UtilImpl.TWO_FACTOR_FROM_EMAIL)
								.subject(SYSTEM_TWO_FACTOR_CODE_SUBJECT)
								.body(emailBody));
	}
}
