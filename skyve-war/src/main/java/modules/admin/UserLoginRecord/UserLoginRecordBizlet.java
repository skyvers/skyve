package modules.admin.UserLoginRecord;

import java.util.Objects;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.app.AppConstants;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;
import org.skyve.util.GeoIPService;
import org.skyve.util.SecurityUtil;

import jakarta.inject.Inject;
import modules.admin.UserLoginRecord.jobs.DifferentCountryLoginNotificationJob;
import modules.admin.domain.User;
import modules.admin.domain.UserLoginRecord;

public class UserLoginRecordBizlet extends Bizlet<UserLoginRecordExtension> {

	@Inject
	private GeoIPService geoIPService;

	private static final String IP_CHANGE_LOG_MESSAGE = "The user %s has logged in from a new IP address. "
			+ "The IP address has changed from %s to %s. "
			+ "If this change is unexpected, it may indicate unauthorized access to the account. "
			+ "Please review the login activity and consider updating the user's security settings if necessary.";

	private static final String COUNTRY_CHANGE_LOG_MESSAGE = "The user %s has logged in from a different country. "
			+ "Their location has changed from %s - %s (IP: %s) to %s - %s (IP: %s). "
			+ "If this change is unexpected, it might indicate unauthorized access. Please review the user's recent activity for any discrepancies.";

	/**
	 * The preSave is overridden so as to add the country of the user based on the ip address.
	 * The method also adds security logs if the ip address and/or the country code of the user changed from the previous login.
	 * It also kicks off a job sending the user an email if the country changed from the previous login.
	 */
	@Override
	public void preSave(UserLoginRecordExtension bean) throws Exception {
		if (bean.isNotPersisted()) {
			String userName = bean.getUserName();

			// Geolocate the IP so as to get the country code and country
			String countryCode = null;
			String ipAddress = bean.getIpAddress();
			if (ipAddress != null) {
				countryCode = geoIPService.geolocate(ipAddress).countryCode();
				if(countryCode == null) {
					UtilImpl.LOGGER.info(userName + " has logged in from IP Address " + ipAddress);
				}else {
					UtilImpl.LOGGER.info(userName + " has logged in from IP Address " + ipAddress + " in country: " + countryCode);
				}
				bean.setCountryCode(countryCode);
			}
			
			// Get the previous login record of the current user
			DocumentQuery q = CORE.getPersistence().newDocumentQuery(UserLoginRecord.MODULE_NAME, UserLoginRecord.DOCUMENT_NAME);
			q.getFilter().addEquals(AppConstants.USER_NAME_ATTRIBUTE_NAME, userName);
			q.addBoundOrdering(UserLoginRecord.loginDateTimePropertyName, SortDirection.descending);
			q.setMaxResults(1);
			UserLoginRecordExtension previousLoginRecord = q.beanResult();
	
			// If there is no previous record then this is probably their first login to the system
			if (previousLoginRecord != null) {
				String userIPAddress = bean.getIpAddress();
	
				// Check if the ip address changed since last login and if so log the event
				String lastIpAddress = previousLoginRecord.getIpAddress();
				if (lastIpAddress != null && !Objects.equals(userIPAddress, lastIpAddress)) {
					// Check if the country has changed since the last login and if so send the user a warning message
					String previousCountryCode = previousLoginRecord.getCountryCode();
	
					if (! Objects.equals(countryCode, previousCountryCode)) {
						String country = bean.getCountryName();
						String previousCountry = previousLoginRecord.getCountryName();
						SecurityUtil.log("User Logged in from Different Country",
											String.format(COUNTRY_CHANGE_LOG_MESSAGE,
															userName,
															(previousCountryCode == null) ? "??" : previousCountryCode,
															(previousCountry == null) ? "Unknown" : previousCountry,
															lastIpAddress,
															(countryCode == null) ? "??" : countryCode,
															(country == null) ? "Unknown" : country,
															userIPAddress));
						
						// Run job to email user on country change
						final Module module = CORE.getCustomer().getModule(User.MODULE_NAME);
						final JobMetaData countryChangeNotificationJobMetaData = module.getJob(DifferentCountryLoginNotificationJob.JOB_NAME);
						EXT.getJobScheduler().runOneShotJob(countryChangeNotificationJobMetaData, bean, CORE.getUser());
					} else {
						// If the country has not changed then the security log shall only have details of an IP change
						SecurityUtil.log("Change of IP Address from Last Login",
											String.format(IP_CHANGE_LOG_MESSAGE, userName, lastIpAddress, userIPAddress));
					}
				}
			}
		}
		
		super.preSave(bean);
	}
}
