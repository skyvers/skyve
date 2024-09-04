package modules.admin.UserLoginRecord;

import java.util.Locale;
import java.util.Objects;
import java.util.Optional;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.app.AppConstants;
import org.skyve.impl.cdi.GeoIPService;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.SecurityUtil;

import jakarta.inject.Inject;
import modules.admin.UserLoginRecord.jobs.DifferentCountryLoginNotificationJob;
import modules.admin.domain.User;
import modules.admin.domain.UserLoginRecord;

public class UserLoginRecordBizlet extends Bizlet<UserLoginRecordExtension> {

	@Inject
	private transient GeoIPService geoIPService;

	private static final String IP_CHANGE_LOG_MESSAGE = "The user %s has logged in from a new IP address. "
			+ "The IP address has changed from %s to %s. "
			+ "If this change is unexpected, it may indicate unauthorized access to the account. "
			+ "Please review the login activity and consider updating the user's security settings if necessary.";

	private static final String COUNTRY_CHANGE_LOG_MESSAGE = "The user %s has logged in from a different country. "
			+ "Their location has changed from %s (IP: %s) to %s (IP: %s). "
			+ "If this change is unexpected, it might indicate unauthorized access. Please review the user's recent activity for any discrepancies.";

	/**
	 * The preSave is overridden so as to add the country of the user based on the ip addrress and if IpInfo token is set. The
	 * method also adds security logs if the ip address and/or the country code of the user changed from the previous login. It also
	 * kicks off a job sending the user an email if the country changed from the previous login.
	 */
	@Override
	public void preSave(UserLoginRecordExtension bean) throws Exception {

		String country = null;
		// Check if the IpInfo token has been set so as to get the country code and country
		if (UtilImpl.IP_INFO_TOKEN != null) {

			Optional<String> countryCodeOptional = geoIPService.getCountryCodeForIP(bean.getIpAddress());
			if (countryCodeOptional.isPresent()) {
				String countryCode = countryCodeOptional.get();
				Locale locale = new Locale("", countryCode);
				country = locale.getDisplayCountry();
				bean.setCountry(country);
			}
		}

		// Get the previous login record of the current user
		DocumentQuery q = CORE.getPersistence()
				.newDocumentQuery(UserLoginRecord.MODULE_NAME, UserLoginRecord.DOCUMENT_NAME);
		q.getFilter()
				.addEquals(AppConstants.USER_NAME_ATTRIBUTE_NAME, bean.getUserName());
		q.addBoundOrdering(UserLoginRecord.loginDateTimePropertyName, SortDirection.descending);
		UserLoginRecordExtension previousLoginRecord = q.beanResult();

		// If there is no previous record then this is probably their first login to the system
		if (previousLoginRecord != null) {
			String userIPAddress = bean.getIpAddress();

			// Get username
			String userName = bean.getUserName();

			// Check if the ip address changed since last login and if so log the event
			String lastIpAddress = previousLoginRecord.getIpAddress();
			if (lastIpAddress != null && !Objects.equals(userIPAddress, lastIpAddress)) {

				// Check if the country has changed since the last login and if so send the user a warning message
				String previousCountry = previousLoginRecord.getCountry();

				if (country != null && previousCountry != null && !Objects.equals(country, previousCountry)) {
					SecurityUtil.log("User Logged in from Different Country",
							String.format(COUNTRY_CHANGE_LOG_MESSAGE, userName, previousCountry, lastIpAddress, country,
									userIPAddress));
					
					// Run job to email user on country change
					final Persistence persistence = CORE.getPersistence();
					final org.skyve.metadata.user.User user = persistence.getUser();
					final Customer customer = user.getCustomer();
					final Module module = customer.getModule(User.MODULE_NAME);
					final JobMetaData countryChangeNotificationJobMetaData = module
							.getJob(DifferentCountryLoginNotificationJob.JOB_NAME);
					EXT.getJobScheduler()
							.runOneShotJob(countryChangeNotificationJobMetaData, bean, user);

				} else {
					// If the country has not changed then the security log shall only have details of an IP change
					SecurityUtil.log("Change of IP Address from Last Login",
							String.format(IP_CHANGE_LOG_MESSAGE, userName, lastIpAddress, userIPAddress));
				}
			}
		}

		super.preSave(bean);
	}

}
