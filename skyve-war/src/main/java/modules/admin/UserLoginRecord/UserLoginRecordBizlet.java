package modules.admin.UserLoginRecord;

import java.util.List;
import java.util.Objects;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.app.AppConstants;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;
import org.skyve.util.IPGeolocation;
import org.skyve.util.SecurityUtil;
import org.skyve.web.WebContext;

import modules.admin.UserLoginRecord.jobs.DifferentCountryLoginNotificationJob;
import modules.admin.domain.Startup;
import modules.admin.domain.User;
import modules.admin.domain.UserLoginRecord;

public class UserLoginRecordBizlet extends Bizlet<UserLoginRecordExtension> {
	private static final String IP_CHANGE_LOG_MESSAGE = "The user %s has logged in from a new IP address. "
			+ "The IP address has changed from %s to %s. "
			+ "If this change is unexpected, it may indicate unauthorized access to the account. "
			+ "Please review the login activity and consider updating the user's security settings if necessary.";

	private static final String COUNTRY_CHANGE_LOG_MESSAGE = "The user %s has logged in from a different country. "
			+ "Their location has changed from %s - %s (IP: %s) to %s - %s (IP: %s). "
			+ "If this change is unexpected, it might indicate unauthorized access. Please review the user's recent activity for any discrepancies.";

	@Override
	public UserLoginRecordExtension preExecute(ImplicitActionName actionName,
												UserLoginRecordExtension bean,
												Bean parentBean,
												WebContext webContext)
	throws Exception {
		if (ImplicitActionName.Edit.equals(actionName)) {
			IPGeolocation geoIP = bean.getGeoIP();
			bean.setCity(geoIP.city());
			bean.setRegion(geoIP.region());
		}
		return super.preExecute(actionName, bean, parentBean, webContext);
	}
	
	/**
	 * Performs the following security checks:
	 * 1. If IP address checks are enabled, it geolocates the current IP address
	 * 2. Retrieves the last X login records for the user (where X is configured in Startup)
	 * 3. Checks if the current IP address exists in any of the previous records
	 * 4. If the IP address is new:
	 *    - If the country has also changed (and country data is available), logs a country change event
	 *    - Otherwise, logs an IP address change event
	 * 5. Sends a notification email if a new country is detected
	 * 
	 * @param bean The UserLoginRecord to be saved
	 * @throws Exception if any error occurs during processing
	 */
	@Override
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
					countryCode = bean.getGeoIP().countryCode();
					if(countryCode == null) {
						LOGGER.info("{} has logged in from IP Address {}", userName, ipAddress);
					} else {
						LOGGER.info("{} has logged in from IP Address {} in country: {}", userName, ipAddress, countryCode);
					}
					bean.setCountryCode(countryCode);
				}
				
				// Get the x last previous login record of the current user
				Integer ipAddressHistoryCountConfig = startup.getIpAddressHistoryCheckCount();
				int ipAddressHistoryCount = ipAddressHistoryCountConfig != null ? ipAddressHistoryCountConfig.intValue() : 1;

				DocumentQuery q = CORE.getPersistence().newDocumentQuery(UserLoginRecord.MODULE_NAME, UserLoginRecord.DOCUMENT_NAME);
				q.getFilter().addEquals(AppConstants.USER_NAME_ATTRIBUTE_NAME, userName);
				q.addBoundOrdering(UserLoginRecord.loginDateTimePropertyName, SortDirection.descending);
				q.setMaxResults(ipAddressHistoryCount);
				List<UserLoginRecordExtension> previousLoginRecords = q.beanResults();
		
				// If there are previous records, check if current IP is new
				if (previousLoginRecords.size() >= ipAddressHistoryCount) {
					String userIPAddress = bean.getIpAddress();
					boolean ipFound = false;
					boolean countryFound = false;

					String lastKnownIP = null;
					String lastKnownCountryCode = null;
					String lastKnownCountry = null;
					
					// Check each previous login record
					for (UserLoginRecordExtension previousRecord : previousLoginRecords) {
						String recordIPAddress = previousRecord.getIpAddress();
						String recordCountryCode = previousRecord.getCountryCode();
						
						// Check if IP exists in previous records
						if (Objects.equals(userIPAddress, recordIPAddress)) {
							ipFound = true;
						}
						
						// Check if country exists in previous records
						if (Objects.equals(countryCode, recordCountryCode)) {
							countryFound = true;
						}
						
						// Keep track of the most recent values for logging
						if (lastKnownIP == null) {
							lastKnownIP = recordIPAddress;
							lastKnownCountryCode = recordCountryCode;
							lastKnownCountry = previousRecord.getCountryName();
						}
					}
					
					// Log security event if IP is new
					if (!ipFound) {
						String country = bean.getCountryName();
						if (!countryFound && country != null) {
							SecurityUtil.log("User Logged in from Different Country",
												String.format(COUNTRY_CHANGE_LOG_MESSAGE,
																userName,
																(lastKnownCountryCode == null) ? "??" : lastKnownCountryCode,
																(lastKnownCountry == null) ? "Unknown" : lastKnownCountry,
																(lastKnownIP == null) ? "Unknown" : lastKnownIP,
																countryCode,
																country,
																userIPAddress),
												UtilImpl.DIFFERENT_COUNTRY_LOGIN_NOTIFICATIONS);
							
							// Run job to email user on country change
							final Module module = CORE.getCustomer().getModule(User.MODULE_NAME);
							final JobMetaData countryChangeNotificationJobMetaData = module.getJob(DifferentCountryLoginNotificationJob.JOB_NAME);
							EXT.getJobScheduler().runOneShotJob(countryChangeNotificationJobMetaData, bean, CORE.getUser());
						} else {
							// If only IP is new, log IP change
							SecurityUtil.log("Change of IP Address from Last Login",
												String.format(IP_CHANGE_LOG_MESSAGE, 
																userName, 
																(lastKnownIP == null) ? "Unknown" : lastKnownIP, 
																userIPAddress),
												UtilImpl.IP_ADDRESS_CHANGE_NOTIFICATIONS);
						}
					}
				}
			}
		}
		
		super.preSave(bean);
	}
}
