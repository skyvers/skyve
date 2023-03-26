package org.skyve.impl.web.spring;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import org.skyve.EXT;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Mail;
import org.springframework.security.provisioning.UserDetailsManager;

public class TwoFactorAuthPushEmailFilter extends TwoFactorAuthPushFilter {
	private static final String TFA_CODE_KEY = "{tfaCode}"; 
	
	public static final String SYSTEM_TWO_FACTOR_CODE_SUBJECT = "Email verification security code";
	public static final String SYSTEM_TWO_FACTOR_CODE_BODY = "Hi,<br />"
			+ "Your verification code is: {tfaCode}<br />"
			+ "Enter the code above where prompted<br />.<br />"
			+ "Having issues with your 2FA? Reach out to your system administrator.";
	
	public TwoFactorAuthPushEmailFilter(UserDetailsManager userDetailsManager) {
		super(userDetailsManager);
	}
	
	@Override
	protected void pushNotification(TwoFactorAuthUser user, String code) {
		String emailAddress = user.getEmail();
		if (emailAddress == null) {
			UtilImpl.LOGGER.warning("No email found for user : " + user.getUsername()); 
			return;
		}
		
		String emailSubjectDB = null;
		String emailBodyDB = null;
		try (Connection c = EXT.getDataStoreConnection()) {
			try (PreparedStatement s = c.prepareStatement("select twoFactorEmailSubject, twoFactorEmailBody from ADM_Configuration where bizCustomer = ?")) {
				s.setString(1, user.getCustomer());
				try (ResultSet rs = s.executeQuery()) {
					if (rs.next()) {
						emailSubjectDB = rs.getString(1);
						emailBodyDB = rs.getString(2);
					}
				}
			}
		}
		catch (SQLException e) {
			throw new DomainException("Failed to get Configuration email template", e);
		}
		
		String emailBody = (emailBodyDB == null) ? SYSTEM_TWO_FACTOR_CODE_BODY : emailBodyDB;
		String emailSubject = (emailSubjectDB == null) ? SYSTEM_TWO_FACTOR_CODE_SUBJECT : emailSubjectDB;
		EXT.sendMail(new Mail().addTo(emailAddress)
								.from(UtilImpl.SMTP_SENDER)
								.subject(emailSubject)
								.body(emailBody.replace(TFA_CODE_KEY, code)));
	}
}
