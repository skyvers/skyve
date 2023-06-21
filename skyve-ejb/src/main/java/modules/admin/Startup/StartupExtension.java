package modules.admin.Startup;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

import javax.inject.Inject;

import org.apache.commons.lang3.StringUtils;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKTReader;
import org.locationtech.jts.io.WKTWriter;
import org.skyve.impl.backup.AzureBlobStorageBackup;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.util.JSON;
import org.skyve.util.Util;

import modules.admin.domain.Startup;

public class StartupExtension extends Startup {

	@Inject
	private transient Customer customer;

	private static final long serialVersionUID = -8931459527432227257L;

	static final String ACCOUNT_STANZA_KEY = "account";
	static final String ACCOUNT_ALLOW_SELF_REGISTRATION_KEY = "allowUserSelfRegistration";

	static final String API_STANZA_KEY = "api";
	static final String API_GOOGLE_MAPS_V3_KEY = "googleMapsV3Key";
	static final String API_GOOGLE_RECAPTCHA_KEY = "googleRecaptchaSiteKey";

	static final String BACKUP_STANZA_KEY = "backup";
	static final String BACKUP_EXTERNAL_BACKUP_CLASS_KEY = "externalBackupClass";
	static final String BACKUP_PROPERTIES_KEY = "properties";

	static final String ENVIRONMENT_STANZA_KEY = "environment";
	static final String ENVIRONMENT_IDENTIFIER_KEY = "identifier";
	static final String ENVIRONMENT_SHOW_SETUP_KEY = "showSetup";
	static final String ENVIRONMENT_SUPPORT_EMAIL_ADDRESS_KEY = "supportEmailAddress";

	static final String MAP_STANZA_KEY = "map";
	static final String MAP_CENTRE_KEY = "centre";
	static final String MAP_LAYERS_KEY = "layers";
	static final String MAP_TYPE_KEY = "type";
	static final String MAP_ZOOM_KEY = "zoom";

	static final String SMTP_STANZA_KEY = "smtp";
	static final String SMTP_TEST_BOGUS_SEND_KEY = "testBogusSend";
	static final String SMTP_TEST_RECIPIENT_KEY = "testRecipient";
	static final String SMTP_SENDER_KEY = "sender";
	static final String SMTP_PWD_KEY = "pwd";
	static final String SMTP_UID_KEY = "uid";
	static final String SMTP_PORT_KEY = "port";
	static final String SMTP_SERVER_KEY = "server";

	/**
	 * Populate this bean's attributes from the current configuration properties values
	 * read from the application json and override json.
	 */
	public void loadProperties() {
		setApiGoogleMapsKey(UtilImpl.GOOGLE_MAPS_V3_API_KEY);
		setApiGoogleRecaptchaKey(UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY);

		setEnvironmentIdentifier(UtilImpl.ENVIRONMENT_IDENTIFIER);
		setEnvironmentSupportEmail(UtilImpl.SUPPORT_EMAIL_ADDRESS);

		setMailBogusSend(Boolean.valueOf(UtilImpl.SMTP_TEST_BOGUS_SEND));
		setMailPassword(UtilImpl.SMTP_PWD);
		setMailPort(Integer.valueOf(UtilImpl.SMTP_PORT));
		setMailSender(UtilImpl.SMTP_SENDER);
		setMailServerUrl(UtilImpl.SMTP);
		setMailTestRecipient(UtilImpl.SMTP_TEST_RECIPIENT);
		setMailUsername(UtilImpl.SMTP_UID);

		String mapCentre = UtilImpl.MAP_CENTRE;
		if (StringUtils.isBlank(mapCentre)) {
			setMapCentre(new GeometryFactory().createPoint(new Coordinate(0, 0)));
		} else {
			try {
				setMapCentre(new WKTReader().read(mapCentre));
			} catch (@SuppressWarnings("unused") ParseException e) {
				setMapCentre(new GeometryFactory().createPoint(new Coordinate(0, 0)));
			}
		}
		setMapLayer(UtilImpl.MAP_LAYERS);
		setMapType(MapType.fromCode(UtilImpl.MAP_TYPE.name()));
		setMapZoom(Integer.valueOf(UtilImpl.MAP_ZOOM));

		setAccountAllowUserSelfRegistration(Boolean.valueOf(UtilImpl.ACCOUNT_ALLOW_SELF_REGISTRATION));

		if (UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS != null) {
			setBackupType(BackupType.fromCode(UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS));
		}
		else {
			setBackupType(BackupType.none);
		}
		if (UtilImpl.BACKUP_PROPERTIES != null) {
			Object property = UtilImpl.BACKUP_PROPERTIES.get(AzureBlobStorageBackup.AZURE_CONNECTION_STRING_KEY);
			if (property != null) {
				setBackupConnectionString(property.toString());
			}
			property = UtilImpl.BACKUP_PROPERTIES.get(AzureBlobStorageBackup.AZURE_CONTAINER_NAME_KEY);
			if (property != null) {
				setBackupDirectoryName(property.toString());
			}
		}
	}

	/**
	 * Write any modified configuration properties to the application's override json file.
	 * 
	 * @throws IOException
	 */
	public void saveConfiguration() throws IOException {
		if (Boolean.TRUE.equals(getDontShowAgain())) {
			UtilImpl.SHOW_SETUP = false;
		}

		Map<String, Object> properties = new HashMap<>(UtilImpl.OVERRIDE_CONFIGURATION);

		// update the override properties with any modified values
		putApi(properties);
		putEnvironment(properties);
		putMail(properties);
		putMap(properties);
		putAccount(properties);
		putBackup(properties);

		// write the json out to the content directory
		String json = marshall(properties);
		if (StringUtils.isNotBlank(json)) {
			writeConfiguration(json);
		}
	}

	/**
	 * Creates or updates the override configuration and sets only the SHOW_SETUP
	 * property to false so that the startup configuration page is not shown again
	 * for this Skyve application.
	 * 
	 * @throws IOException
	 */
	@SuppressWarnings({ "unchecked" })
	public void setDontShow() throws IOException {
		Map<String, Object> properties = UtilImpl.OVERRIDE_CONFIGURATION;

		Map<String, Object> environment = (Map<String, Object>) properties.get(ENVIRONMENT_STANZA_KEY);
		if (environment == null) {
			environment = new HashMap<>();
			properties.put(ENVIRONMENT_STANZA_KEY, environment);
		}

		// set the show setup key to false
		environment.put(ENVIRONMENT_SHOW_SETUP_KEY, Boolean.FALSE);
		UtilImpl.SHOW_SETUP = false;

		// write the json out to the content directory
		String json = marshall(properties);
		if (StringUtils.isNotBlank(json)) {
			writeConfiguration(json);
		}
	}

	/**
	 * Compares the current value of the account configuration against the
	 * new value from the startup page and if the value has changed, adds it to the
	 * map to be persisted and updates the running configuration with the new value.
	 * 
	 * @param properties The current override configuration property map
	 * @return The map of account properties which have been modified
	 */
	@SuppressWarnings("unchecked")
	private Map<String, Object> putAccount(Map<String, Object> properties) {

		// initialise or get the existing property map
		Map<String, Object> account = (Map<String, Object>) properties.get(ACCOUNT_STANZA_KEY);
		if (account == null) {
			account = new HashMap<>();
			properties.put(ACCOUNT_STANZA_KEY, account);
		}

		// add any values to the override configuration if they have changed
		if (UtilImpl.ACCOUNT_ALLOW_SELF_REGISTRATION != getAccountAllowUserSelfRegistration().booleanValue()) {
			account.put(ACCOUNT_ALLOW_SELF_REGISTRATION_KEY, getAccountAllowUserSelfRegistration());
			UtilImpl.ACCOUNT_ALLOW_SELF_REGISTRATION = getAccountAllowUserSelfRegistration().booleanValue();
		}

		return account;
	}

	/**
	 * Compares the current value of the api configuration against the
	 * new value from the startup page and if they value has changed, adds it to the
	 * map to be persisted and updates the running configuration with the new value.
	 * 
	 * @param properties The current override configuration property map
	 * @return The map of api properties which have been modified
	 */
	@SuppressWarnings("unchecked")
	private Map<String, Object> putApi(final Map<String, Object> properties) {

		// initialise or get the existing property map
		Map<String, Object> api = (Map<String, Object>) properties.get(API_STANZA_KEY);
		if (api == null) {
			api = new HashMap<>();
			properties.put(API_STANZA_KEY, api);
		}

		// add any values to the override configuration if they have changed
		if (getApiGoogleMapsKey() != null
				&& !StringUtils.equals(UtilImpl.GOOGLE_MAPS_V3_API_KEY, getApiGoogleMapsKey())) {
			api.put(API_GOOGLE_MAPS_V3_KEY, getApiGoogleMapsKey());
			UtilImpl.GOOGLE_MAPS_V3_API_KEY = getApiGoogleMapsKey();
		}

		if (getApiGoogleRecaptchaKey() != null
				&& !StringUtils.equals(UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY, getApiGoogleRecaptchaKey())) {
			api.put(API_GOOGLE_RECAPTCHA_KEY, getApiGoogleRecaptchaKey());
			UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY = getApiGoogleRecaptchaKey();
		}

		return api;
	}

	/**
	 * Compares the current value of the backup configuration against the
	 * new value from the startup page and if they value has changed, adds it to the
	 * map to be persisted and updates the running configuration with the new value.
	 * 
	 * @param properties The current override configuration property map
	 * @return The map of backup properties which have been modified
	 */
	@SuppressWarnings("unchecked")
	private Map<String, Object> putBackup(final Map<String, Object> properties) {
		// initialise or get the existing property map
		Map<String, Object> backup = (Map<String, Object>) properties.get(BACKUP_STANZA_KEY);
		Map<String, Object> backupProperties = null;
		if (backup == null) {
			backup = new HashMap<>();
			properties.put(BACKUP_STANZA_KEY, backup);
		}

		// add any values to the override configuration if they have changed
		BackupType backupType = getBackupType();
		if ((backupType == null) || (backupType == BackupType.none)) {
			UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = null;
		} else {
			UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = backupType.toCode();
			backupProperties = new TreeMap<>();
			backup.put(BACKUP_PROPERTIES_KEY, backupProperties);
		}
		backup.put(BACKUP_EXTERNAL_BACKUP_CLASS_KEY, UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS);

		if (backupProperties != null) {
			String property = getBackupConnectionString();
			if (property != null) {
				backupProperties.put(AzureBlobStorageBackup.AZURE_CONNECTION_STRING_KEY, property);
			}
			property = getBackupDirectoryName();
			if (property != null) {
				backupProperties.put(AzureBlobStorageBackup.AZURE_CONTAINER_NAME_KEY, property);
			}
		}
		UtilImpl.BACKUP_PROPERTIES = backupProperties;

		return backup;
	}

	/**
	 * Compares the current value of the environment configuration against the
	 * new value from the startup page and if they value has changed, adds it to the
	 * map to be persisted and updates the running configuration with the new value.
	 * 
	 * @param properties The current override configuration property map
	 * @return The map of environment properties which have been modified
	 */
	@SuppressWarnings("unchecked")
	private Map<String, Object> putEnvironment(final Map<String, Object> properties) {

		// initialise or get the existing property map
		Map<String, Object> environment = (Map<String, Object>) properties.get(ENVIRONMENT_STANZA_KEY);
		if (environment == null) {
			environment = new HashMap<>();
			properties.put(ENVIRONMENT_STANZA_KEY, environment);
		}

		// add any values to the override configuration if they have changed
		if (getEnvironmentIdentifier() == null
				|| !StringUtils.equals(UtilImpl.ENVIRONMENT_IDENTIFIER, getEnvironmentIdentifier())) {
			environment.put(ENVIRONMENT_IDENTIFIER_KEY, getEnvironmentIdentifier());
			UtilImpl.ENVIRONMENT_IDENTIFIER = getEnvironmentIdentifier();
		}

		if (getEnvironmentSupportEmail() == null
				|| !StringUtils.equals(UtilImpl.SUPPORT_EMAIL_ADDRESS, getEnvironmentSupportEmail())) {
			environment.put(ENVIRONMENT_SUPPORT_EMAIL_ADDRESS_KEY, getEnvironmentSupportEmail());
			UtilImpl.SUPPORT_EMAIL_ADDRESS = getEnvironmentSupportEmail();
		}

		if (Boolean.TRUE.equals(getDontShowAgain())) {
			environment.put(ENVIRONMENT_SHOW_SETUP_KEY, Boolean.FALSE);
			UtilImpl.SHOW_SETUP = false;
		}

		return environment;
	}

	/**
	 * Compares the current value of the mail configuration against the
	 * new value from the startup page and if they value has changed, adds it to the
	 * map to be persisted and updates the running configuration with the new value.
	 * 
	 * @param properties The current override configuration property map
	 * @return The map of mail properties which have been modified
	 */
	@SuppressWarnings("unchecked")
	private Map<String, Object> putMail(final Map<String, Object> properties) {
		// initialise or get the existing property map
		Map<String, Object> smtp = (Map<String, Object>) properties.get(SMTP_STANZA_KEY);
		if (smtp == null) {
			smtp = new HashMap<>();
			properties.put(SMTP_STANZA_KEY, smtp);
		}

		// add any values to the override configuration if they have changed
		if (!UtilImpl.SMTP.equals(getMailServerUrl())) {
			smtp.put(SMTP_SERVER_KEY, getMailServerUrl());
			UtilImpl.SMTP = getMailServerUrl();
		}

		if (UtilImpl.SMTP_PORT != getMailPort().intValue()) {
			smtp.put(SMTP_PORT_KEY, getMailPort());
			UtilImpl.SMTP_PORT = getMailPort().intValue();
		}

		if (getMailUsername() == null || !StringUtils.equals(UtilImpl.SMTP_UID, getMailUsername())) {
			smtp.put(SMTP_UID_KEY, getMailUsername());
			UtilImpl.SMTP_UID = getMailUsername();
		}

		if (getMailPassword() == null || !StringUtils.equals(UtilImpl.SMTP_PWD, getMailPassword())) {
			smtp.put(SMTP_PWD_KEY, getMailPassword());
			UtilImpl.SMTP_PWD = getMailPassword();
		}

		if (!UtilImpl.SMTP_SENDER.equals(getMailSender())) {
			smtp.put(SMTP_SENDER_KEY, getMailSender());
			UtilImpl.SMTP_SENDER = getMailSender();
		}

		if (UtilImpl.SMTP_TEST_BOGUS_SEND != getMailBogusSend().booleanValue()) {
			smtp.put(SMTP_TEST_BOGUS_SEND_KEY, getMailBogusSend());
			UtilImpl.SMTP_TEST_BOGUS_SEND = getMailBogusSend().booleanValue();
		}

		if (getMailTestRecipient() == null || !StringUtils.equals(UtilImpl.SMTP_TEST_RECIPIENT, getMailTestRecipient())) {
			smtp.put(SMTP_TEST_RECIPIENT_KEY, getMailTestRecipient());
			UtilImpl.SMTP_TEST_RECIPIENT = getMailTestRecipient();
		}

		return smtp;
	}

	/**
	 * Compares the current value of the map configuration against the
	 * new value from the startup page and if they value has changed, adds it to the
	 * map to be persisted and updates the running configuration with the new value.
	 * 
	 * @param properties The current override configuration property map
	 * @return The map of map properties which have been modified
	 */
	@SuppressWarnings("unchecked")
	private Map<String, Object> putMap(final Map<String, Object> properties) {
		// initialise or get the existing property map
		Map<String, Object> map = (Map<String, Object>) properties.get(MAP_STANZA_KEY);
		if (map == null) {
			map = new HashMap<>();
			properties.put(MAP_STANZA_KEY, map);
		}

		// add any values to the override configuration if they have changed
		if (!MapType.fromCode(UtilImpl.MAP_TYPE.name()).equals(getMapType())) {
			map.put(MAP_TYPE_KEY, getMapType().toCode());
			UtilImpl.MAP_TYPE = UtilImpl.MapType.valueOf(getMapType().toCode());
		}

		if (UtilImpl.MAP_ZOOM != getMapZoom().intValue()) {
			map.put(MAP_ZOOM_KEY, getMapZoom());
			UtilImpl.MAP_ZOOM = getMapZoom().intValue();
		}

		if (!UtilImpl.MAP_LAYERS.equals(getMapLayer())) {
			map.put(MAP_LAYERS_KEY, getMapLayer());
			UtilImpl.MAP_LAYERS = getMapLayer();
		}

		String mapCentreWkt = getMapCentre() != null ? new WKTWriter().write(getMapCentre()) : null;
		if (mapCentreWkt == null) {
			map.remove(MAP_CENTRE_KEY);
			UtilImpl.MAP_CENTRE = null;
		} else if (!StringUtils.equals(UtilImpl.MAP_CENTRE, mapCentreWkt)) {
			map.put(MAP_CENTRE_KEY, mapCentreWkt);
			UtilImpl.MAP_CENTRE = mapCentreWkt;
		}

		return map;
	}

	@SuppressWarnings({ "unchecked" })
	String marshall(final Map<String, Object> properties) {
		if (!properties.isEmpty()) {
			// remove any empty maps first
			Iterator<Map.Entry<String, Object>> itr = properties.entrySet().iterator();

			while (itr.hasNext()) {
				Map.Entry<String, Object> entry = itr.next();
				if (entry.getValue() instanceof Map) {
					if (((Map<String, Object>) entry.getValue()).isEmpty()) {
						itr.remove();
					}
				}
			}

			if (!properties.isEmpty()) {
				String json = JSON.marshall(customer, properties);
				Util.LOGGER.info(String.format("Override json: %s", json));

				return json;
			}
		}

		Util.LOGGER.info("No startup properties were modified, nothing to marshall.");
		return null;
	}

	/**
	 * Marshals the specified map of properties to JSON and creates or overwrites the
	 * override JSON folder for this Skyve application.
	 * 
	 * @param properties The map of override properties to write
	 * @throws IOException
	 */
	@SuppressWarnings("static-method")
	void writeConfiguration(final String json) throws IOException {
		if (StringUtils.isNotEmpty(json)) {
			File overridesFile = new File(UtilImpl.CONTENT_DIRECTORY, UtilImpl.ARCHIVE_NAME + ".json");

			try (BufferedWriter writer = new BufferedWriter(new FileWriter(overridesFile))) {
				writer.write(json);
			}
		}
	}
}
