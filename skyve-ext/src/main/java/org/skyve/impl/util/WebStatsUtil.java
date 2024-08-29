package org.skyve.impl.util;

import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Optional;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.cdi.GeoIPService;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.SQL;
import org.skyve.util.Mail;
import org.skyve.util.SecurityUtil;
import org.skyve.web.UserAgentType;

public class WebStatsUtil {
	private static final String YEAR_FORMAT = "yyyy";
	private static final String MONTH_FORMAT = "M";
	
	private WebStatsUtil() {
		// do nothing
	}
	
	/**
	 * This method is used to populate the UserLoginRecord table with details of a new log in to the system
	 * @param user
	 * @param userIPAddress
	 * @throws Exception
	 */
	public static void recordLogin(User user, String userIPAddress)
	throws Exception {
		Customer customer = user.getCustomer();
		Module module = customer.getModule(AppConstants.ADMIN_MODULE_NAME);
		Document loginRecordDocument = module.getDocument(customer, "UserLoginRecord");
		AbstractPersistentBean loginRecord = loginRecordDocument.newInstance(user);
		BindUtil.set(loginRecord, "userName", user.getName());
		BindUtil.set(loginRecord, "loginDateTime", new DateTime(System.currentTimeMillis()));
		BindUtil.set(loginRecord, "failed", Boolean.FALSE);
		BindUtil.set(loginRecord, "ipAddress", userIPAddress);

		// Check if the IpInfo token has been set so as to get the country code
		String ipInfoToken = UtilImpl.IP_INFO_TOKEN;
		String countryCode = null;
		if (ipInfoToken != null) {
			final GeoIPService geoIPService = new GeoIPService();
			Optional<String> countryCodeOptional = geoIPService.getCountryCodeForIP(userIPAddress);
			if (countryCodeOptional.isPresent()) {
				countryCode = countryCodeOptional.get();
				BindUtil.set(loginRecord, "country", countryCode);
			}
		}
		AbstractPersistence.get()
				.save(loginRecordDocument, loginRecord);

		// Get the last login record of the current user
		DocumentQuery q = CORE.getPersistence()
				.newDocumentQuery(loginRecordDocument);
		q.getFilter()
				.addEquals("userName", user.getName());
		q.addBoundOrdering("loginDateTime", SortDirection.descending);
		AbstractPersistentBean previousLoginRecord = q.beanResult();

		// Get email and name from the metadata user
		Document userDocument = module.getDocument(customer, "User");
		AbstractPersistentBean adminUserBean = CORE.getPersistence()
				.retrieve(userDocument, user.getId());
		String contactEmailBinding = BindUtil.createCompoundBinding("contact", "email1");
		String contactNameBinding = BindUtil.createCompoundBinding("contact", "name");
		String userEmail = (String) BindUtil.get(adminUserBean, contactEmailBinding);
		String userName = (String) BindUtil.get(adminUserBean, contactNameBinding);

		// Check if the ip address changed since last login and if so log the event
		String lastIpAddress = (String) BindUtil.get(previousLoginRecord, "ipAddress");
		if (userIPAddress != lastIpAddress) {
			SecurityUtil.log("Change of IP Address from Last Login",
					"The user " + userName + " has logged in from a new IP address. "
							+ "The IP address has changed from " + lastIpAddress + " to " + userIPAddress + ". "
							+ "If this change is unexpected, it may indicate unauthorized access to the account. "
							+ "Please review the login activity and consider updating the user's security settings if necessary.");

		}

		// Check if the country has changed since the last login and if so send the user a warning message
		String previousCountryCode = (String) BindUtil.get(previousLoginRecord, "countryCode");
		
		if (countryCode != null && countryCode != previousCountryCode) {
			// Get actual country name
			Locale locale = new Locale("", countryCode);
			String country = locale.getDisplayCountry();

			Locale previousLocale = new Locale("", previousCountryCode);
			String previousCountry = previousLocale.getDisplayCountry();

			SecurityUtil.log("User Logged in from Different Country",
					"The user " + userName + " has logged in from a different country. "
							+ "Their location has changed from " + previousCountry + " to " + country + ". "
							+ "If this change is unexpected, it might indicate unauthorized access. Please review the user's recent activity for any discrepancies.");

			// Send email to user advising password reset
			try {
				EXT.sendMail(new Mail().from(UtilImpl.SMTP_SENDER)
						.addTo(userEmail)
						.subject("Security Alert: Unusual Login Activity Detected")
						.body("Dear " + userName + ",\n\n"
								+ "We have detected a login attempt to your account from a new location: " + country + ". "
								+ "If this was you, there's no need to take further action.\n\n"
								+ "However, if you do not recognize this activity, we strongly recommend that you change your password immediately to secure your account. "
								+ "You can do this by logging into your account and navigating to <strong>Admin -> Password</strong>.\n\n"
								+ "For your safety, please do not share your password with anyone.\n\n"
								+ "If you have any questions or need assistance, please contact our support team at "
								+ UtilImpl.SUPPORT_EMAIL_ADDRESS + ".\n\n"
								+ "Best regards,\n"
								+ "The Security Team"));
			} catch (Exception e) {
				UtilImpl.LOGGER.severe("Failed to send email warning user of country change");
			}
		}

// NO COMMIT
	}
	
	// this is called from the BizHubFilter - create and destroy a special persistence for this as
	// the actual persistence used for this thread is set in the servlets or the faces bean and 
	// could be different from the default created here for the thread - think webContext and conversations!!
	// If it was not closed {commit(true)} then it could create a resource leak
	public static synchronized void recordHit(User user, String userAgentHeader, UserAgentType userAgentType)
	throws Exception {
		AbstractPersistence persistence = AbstractPersistence.get();
		persistence.setUser(user); // user has not been set as the servlet for this filter have not fired yet
		persistence.begin();

		try {
			Customer customer = user.getCustomer();
			Module admin = customer.getModule(AppConstants.ADMIN_MODULE_NAME);
			@SuppressWarnings("null")
			String ADM_UserMonthlyHits = admin.getDocument(customer, "UserMonthlyHits").getPersistent().getPersistentIdentifier();

			Date now = new Date();
			Integer hitMonth = Integer.valueOf(CORE.getDateFormat(MONTH_FORMAT).format(now));
			Integer hitYear = Integer.valueOf(CORE.getDateFormat(YEAR_FORMAT).format(now));
			
			String truncatedUserAgentHeader = userAgentHeader;
			if ((truncatedUserAgentHeader != null) && (truncatedUserAgentHeader.length() > 400)) {
				truncatedUserAgentHeader = truncatedUserAgentHeader.substring(0, 400);
			}
			String deviceCode = null;
			if (userAgentType == UserAgentType.desktop) {
				deviceCode = "D";
			}
			else if (userAgentType == UserAgentType.phone) {
				deviceCode = "P";
			}
			else if (userAgentType == UserAgentType.tablet) {
				deviceCode = "T";
			}
			else {
				deviceCode = "O";
			}
			
			StringBuilder query = new StringBuilder(128);
			query.append("select bizId from ").append(ADM_UserMonthlyHits);
			query.append(" where hitMonth = :hitMonth ");
			query.append("and hitYear = :hitYear ");
			query.append("and userName = :userName ");
			query.append("and device = :device ");
			if (userAgentHeader == null) {
				query.append("and userAgentHeader is null ");
			}
			else {
				query.append("and userAgentHeader = :userAgentHeader ");
			}
			if (UtilImpl.CUSTOMER == null) { // multi-tenant
				query.append("and bizCustomer = :customer");
			}
			SQL sql = persistence.newSQL(query.toString());
			sql.putParameter("hitMonth", hitMonth);
			sql.putParameter("hitYear", hitYear);
			sql.putParameter("userName", user.getName(), false);
			sql.putParameter("device", deviceCode, false);
			if (userAgentHeader != null) {
				sql.putParameter("userAgentHeader", truncatedUserAgentHeader, false);
			}
			if (UtilImpl.CUSTOMER == null) { // multi-tenant
				sql.putParameter("customer", user.getCustomer().getName(), false);
			}
			List<String> results = sql.scalarResults(String.class);
			if (results.isEmpty()) {
				query.setLength(0);
				query.append("insert into ").append(ADM_UserMonthlyHits);
				query.append(" (bizId, bizVersion, bizCustomer, bizLock, bizUserId, userName, hitYear, hitMonth, userAgentHeader, device, numberOfHits, bizKey) values ");
				query.append("(:bizId, 0, :bizCustomer, :bizLock, :bizUserId, :userName, :hitYear, :hitMonth, :userAgentHeader, :device, 1, :bizKey)");
				sql = persistence.newSQL(query.toString());
				sql.putParameter(Bean.DOCUMENT_ID, UUIDv7.create().toString(), false);
				sql.putParameter(Bean.CUSTOMER_NAME, user.getCustomer().getName(), false);
				sql.putParameter(PersistentBean.LOCK_NAME, new OptimisticLock(user.getName(), now).toString(), false);
				sql.putParameter(Bean.USER_ID, user.getId(), false);
				sql.putParameter("userName", user.getName(), false);
				sql.putParameter("hitYear", hitYear);
				sql.putParameter("hitMonth", hitMonth);
				sql.putParameter("userAgentHeader", truncatedUserAgentHeader, false);
				sql.putParameter("device", deviceCode, false);
				sql.putParameter("bizKey", "bizKey", false);
				
				sql.execute();
			}
			else {
				query.setLength(0);
				query.append("update ").append(ADM_UserMonthlyHits);
				query.append(" set numberOfHits = numberOfHits + 1 ");
				query.append("where bizId = :bizId");
				sql = persistence.newSQL(query.toString());
				sql.putParameter(Bean.DOCUMENT_ID, results.get(0), false);
				sql.execute();
			}
		}
		catch (Exception e) {
			persistence.rollback();
			throw e;
		}
		finally {
			persistence.commit(true);
		}
	}
}
