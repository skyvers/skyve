package modules.admin.UserLoginRecord;

import java.util.List;
import java.util.Objects;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.app.AppConstants;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;
import org.skyve.util.GeoIPService;
import org.skyve.util.SecurityUtil;

import jakarta.inject.Inject;
import modules.admin.UserLoginRecord.jobs.DifferentCountryLoginNotificationJob;
import modules.admin.domain.Startup;
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
	 * Performs the following security checks:
	 * 1. If IP address checks are enabled, it geolocates the current IP address
	 * 2. Retrieves the last X login records for the user (where X is configured in Startup)
	 * 3. Checks if the current IP address and/or country exists in any of the previous records
	 * 4. Logs a security event only if the current IP address or country is not found in any previous records
	 * 5. Sends a notification email if a new country is detected
	 * 
	 * The method will only log one security event per login, prioritizing country changes over IP changes
	 * if both are new.
	 * 
	 * @param bean The UserLoginRecord to be saved
	 * @throws Exception if any error occurs during processing
	 */
	@Override
	@SuppressWarnings("boxing")
	public void preSave(UserLoginRecordExtension bean) throws Exception {
		if (bean.isNotPersisted()) {
			// Check if IP address checks are enabled
			Startup startup = Startup.newInstance();
			if (startup.isIpAddressChecksEnabled()) {
				String userName = bean.getUserName();

				// Geolocate the IP so as to get the country code and country
				String countryCode = null;
				String ipAddress = bean.getIpAddress();
				if (ipAddress != null) {
					countryCode = geoIPService.geolocate(ipAddress).countryCode();
					if(countryCode == null) {
						LOGGER.info(userName + " has logged in from IP Address " + ipAddress);
					} else {
						LOGGER.info(userName + " has logged in from IP Address " + ipAddress + " in country: " + countryCode);
					}
					bean.setCountryCode(countryCode);
				}
				
				// Get the x last previous login record of the current user
				Integer ipAddressHistoryCountConfig = startup.getIpAddressHistoryCheckCount();
				int ipAddressHistoryCount = ipAddressHistoryCountConfig != null ? ipAddressHistoryCountConfig : 1;

				DocumentQuery q = CORE.getPersistence().newDocumentQuery(UserLoginRecord.MODULE_NAME, UserLoginRecord.DOCUMENT_NAME);
				q.getFilter().addEquals(AppConstants.USER_NAME_ATTRIBUTE_NAME, userName);
				q.addBoundOrdering(UserLoginRecord.loginDateTimePropertyName, SortDirection.descending);
				q.setMaxResults(ipAddressHistoryCount);
				List<UserLoginRecordExtension> previousLoginRecords = q.beanResults();
		
				// If there are previous records, check if current IP/country is new
				if (previousLoginRecords != null && !previousLoginRecords.isEmpty() && previousLoginRecords.size() >= ipAddressHistoryCount) {
					String userIPAddress = bean.getIpAddress();
					boolean ipFound = false;
					boolean countryFound = false;
					String lastKnownIp = null;
					String lastKnownCountryCode = null;
					String lastKnownCountry = null;
					
					// Check each previous login record
					for (UserLoginRecordExtension previousRecord : previousLoginRecords) {
						String lastIpAddress = previousRecord.getIpAddress();
						String previousCountryCode = previousRecord.getCountryCode();
						
						// Check if IP exists in previous records
						if (lastIpAddress != null && Objects.equals(userIPAddress, lastIpAddress)) {
							ipFound = true;
						}
						
						// Check if country exists in previous records
						if (previousCountryCode != null && Objects.equals(countryCode, previousCountryCode)) {
							countryFound = true;
						}
						
						// Keep track of the most recent values for logging
						if (lastKnownIp == null && lastIpAddress != null) {
							lastKnownIp = lastIpAddress;
							lastKnownCountryCode = previousCountryCode;
							lastKnownCountry = previousRecord.getCountryName();
						}
					}
					
					// Log security event if either IP or country is new
					if (!ipFound || !countryFound) {
						if (!countryFound) {
							String country = bean.getCountryName();
							SecurityUtil.log(SecurityUtil.DIFFERENT_COUNTRY_LOGIN_EVENT_TYPE,
												String.format(COUNTRY_CHANGE_LOG_MESSAGE,
																userName,
																(lastKnownCountryCode == null) ? "??" : lastKnownCountryCode,
																(lastKnownCountry == null) ? "Unknown" : lastKnownCountry,
																(lastKnownIp == null) ? "Unknown" : lastKnownIp,
																(countryCode == null) ? "??" : countryCode,
																(country == null) ? "Unknown" : country,
																userIPAddress));
							
							// Run job to email user on country change
							final Module module = CORE.getCustomer().getModule(User.MODULE_NAME);
							final JobMetaData countryChangeNotificationJobMetaData = module.getJob(DifferentCountryLoginNotificationJob.JOB_NAME);
							EXT.getJobScheduler().runOneShotJob(countryChangeNotificationJobMetaData, bean, CORE.getUser());
						} else {
							// If only IP is new, log IP change
							SecurityUtil.log(SecurityUtil.IP_ADDRESS_CHANGE_EVENT_TYPE,
												String.format(IP_CHANGE_LOG_MESSAGE, 
																userName, 
																(lastKnownIp == null) ? "Unknown" : lastKnownIp, 
																userIPAddress));
						}
					}
				}
			}
		}
		
		super.preSave(bean);
	}
}
