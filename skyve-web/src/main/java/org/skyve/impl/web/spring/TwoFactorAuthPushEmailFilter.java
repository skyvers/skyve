package org.skyve.impl.web.spring;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import org.skyve.EXT;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Mail;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.core.userdetails.UserDetailsService;

public class TwoFactorAuthPushEmailFilter extends TwoFactorAuthPushFilter {
	
	private static final String TFA_CODE_KEY = "{tfaCode}"; 
	
	public static final String SYSTEM_TWO_FACTOR_CODE_SUBJECT = "Please verify your email security code";
	public static final String SYSTEM_TWO_FACTOR_CODE_BODY = "Hi,<br /><br />Your verification code is: {tfaCode}<br />"; 
	
	
	public TwoFactorAuthPushEmailFilter(AuthenticationManager authenticationManager, UserDetailsService userDetailsService) {
		super(authenticationManager, userDetailsService);
	}
	

	@Override
	protected void pushNotification(UserTFA user, String code) {
		
		String emailAddress = user.getEmail();
		if (emailAddress == null) {
			UtilImpl.LOGGER.warning("No email found for user : " + user.getUsername()); 
			return;
		}
		
		String emailSubjectDB = null;
		String emailBodyDB = null;
		
		try (Connection c = EXT.getDataStoreConnection()) {
			try (PreparedStatement s = c.prepareStatement(String.format("select %s, %s from ADM_Configuration where %s = ?",
																			"twoFactorEmailSubject",
																			"twoFactorEmailBody",
																			"bizCustomer"))) {
				s.setString(1, user.getCustomer());
				try (ResultSet rs = s.executeQuery()) {
					
					while (rs.next()) {
						emailSubjectDB = rs.getString(1);
						emailBodyDB = rs.getString(2);
					}
				}
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		
		String emailBody = (emailBodyDB == null) ? SYSTEM_TWO_FACTOR_CODE_BODY : emailBodyDB;
		String emailSubject = (emailSubjectDB == null) ? SYSTEM_TWO_FACTOR_CODE_SUBJECT : emailSubjectDB;
				
			
		
		EXT.sendMail(new Mail().addTo(emailAddress)
								.from(UtilImpl.SMTP_SENDER)
								.subject(emailSubject)
								.body(emailBody.replace(TFA_CODE_KEY, code)));
	}
	
}
