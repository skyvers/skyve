package org.skyve.impl.web.spring;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import org.skyve.EXT;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.util.TwoFactorAuthCustomerConfiguration;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Mail;
import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.springframework.security.provisioning.UserDetailsManager;

/**
 * Implements two-factor authentication push via email, using customer-configured or fallback email templates.
 * The email body template must include the "{tfaCode}" placeholder for the generated code to be included in the message.
 */
public class TwoFactorAuthPushEmailFilter extends TwoFactorAuthPushFilter {
	private static final String TFA_CODE_KEY = "{tfaCode}"; 

    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(TwoFactorAuthPushEmailFilter.class);

	/**
	 * Default email subject used when no customer-specific template subject is configured.
	 */
	public static final String SYSTEM_TWO_FACTOR_CODE_SUBJECT = "Email verification security code";

	/**
	 * Default email body used when no customer-specific template body is configured.
	 */
	public static final String SYSTEM_TWO_FACTOR_CODE_BODY = "Hi,<br />"
			+ "Your verification code is: {tfaCode}<br />"
			+ "Enter the code above where prompted<br />.<br />"
			+ "Having issues with your 2FA? Reach out to your system administrator.";
	
	/**
	 * Creates an email-based two-factor push filter.
	 *
	 * @param userDetailsManager Spring Security user manager used by the parent filter.
	 */
	public TwoFactorAuthPushEmailFilter(UserDetailsManager userDetailsManager) {
		super(userDetailsManager);
	}

	/**
	 * Indicates whether email push is enabled for the resolved customer configuration.
	 *
	 * @param config Resolved customer-level two-factor configuration.
	 * @return {@code true} when email push is enabled for the customer.
	 */
	@Override
	protected boolean supportsPushConfiguration(TwoFactorAuthCustomerConfiguration config) {
		return (config != null) && config.isTfaEmail();
	}
	
	/**
	 * Sends a two-factor code notification email using customer-configured or fallback templates.
	 *
	 * @param user Authenticated two-factor user context.
	 * @param code Generated two-factor code to include in the email body.
	 * @throws DomainException If the configured email template cannot be loaded.
	 */
	@Override
	protected void pushNotification(TwoFactorAuthUser user, String code) {
		String emailAddress = user.getEmail();
		if (emailAddress == null) {
			LOGGER.warn("No email found for user : {}", user.getUsername()); 
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
		EXT.getMailService()
				.sendMail(new Mail().addTo(emailAddress)
								.from(UtilImpl.SMTP_SENDER)
								.subject(emailSubject)
								.body(emailBody.replace(TFA_CODE_KEY, code)));
	}
}
