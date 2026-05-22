package modules.admin.Startup;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.times;

import java.util.HashMap;
import java.util.Map;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;

import modules.admin.domain.Startup;
import modules.admin.domain.Startup.BackupType;
import modules.admin.domain.Startup.CaptchaType;
import modules.admin.domain.Startup.MapType;

public class StartupExtensionTest {
	private Map<String, Object> overrideProperties;

	@Mock
	private Customer customer;

	@InjectMocks
	@Spy
	private StartupExtension bean;

	@SuppressWarnings("resource")
	private AutoCloseable closeable;
	
	@Before
	@SuppressWarnings("resource")
	public void before() {
		closeable = MockitoAnnotations.openMocks(this);

		Map<String, Object> properties = new HashMap<>();
		UtilImpl.CONFIGURATION = properties;

		overrideProperties = new HashMap<>();
		UtilImpl.OVERRIDE_CONFIGURATION = overrideProperties;

		// set never null properties
		UtilImpl.SMTP = "localhost";
		UtilImpl.SMTP_SENDER = "test@test.com";
		UtilImpl.MAP_LAYERS = "google.maps.MapTypeId.ROADMAP";

		// mock the bean to return the default properties
		Mockito.when(bean.getMailServerUrl()).thenReturn(UtilImpl.SMTP);
		Mockito.when(bean.getMailSender()).thenReturn(UtilImpl.SMTP_SENDER);
		Mockito.when(bean.getMailPort()).thenReturn(Integer.valueOf(UtilImpl.SMTP_PORT));
		Mockito.when(bean.getMailBogusSend()).thenReturn(Boolean.valueOf(UtilImpl.SMTP_TEST_BOGUS_SEND));
		Mockito.when(bean.getMapLayer()).thenReturn(UtilImpl.MAP_LAYERS);
	}

	@After
	public void tearDown() throws Exception {
		if (closeable != null) {
			closeable.close();
		}
	}
	
	@Test
	public void testSaveConfigurationEmptyPropertiesWritesNulls() throws Exception {
		// setup mocks
		Mockito.doNothing().when(bean).writeConfiguration(anyString());

		// call the method under test
		bean.saveConfiguration();

		// verify
		Mockito.verify(bean, times(1)).writeConfiguration(anyString());
	}
	
	@Test
	public void testSaveConfigurationUpdatesApiProperties() throws Exception {
		// setup mocks
		Mockito.when(bean.getApiGoogleMapsKey()).thenReturn("12345");
		Mockito.when(bean.getApiGoogleRecaptchaSiteKey()).thenReturn("12345");
		Mockito.when(bean.getApiGoogleRecaptchaSecretKey()).thenReturn("12345");

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		Mockito.doNothing().when(bean).writeConfiguration(valueCapture.capture());

		// call the method under test
		bean.saveConfiguration();

		// verify the results
		Mockito.verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.API_STANZA_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.API_GOOGLE_MAPS_V3_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.API_GOOGLE_RECAPTCHA_SITE_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.API_GOOGLE_RECAPTCHA_SECRET_KEY));
	}

	@Test
	public void testSaveConfigurationUpdatesBackupProperties() throws Exception {
		// setup mocks
		Mockito.when(bean.getBackupType()).thenReturn(BackupType.none);

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		Mockito.doNothing().when(bean).writeConfiguration(valueCapture.capture());

		// call the method under test
		bean.saveConfiguration();

		// verify the results
		Mockito.verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.BACKUP_STANZA_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.BACKUP_EXTERNAL_BACKUP_CLASS_KEY));
	}

	@Test
	public void testSaveConfigurationUpdatesEnvironmentProperties() throws Exception {
		// setup mocks
		Mockito.when(bean.getEnvironmentIdentifier()).thenReturn("dev");
		Mockito.when(bean.getEnvironmentSupportEmail()).thenReturn("test@test.com");
		Mockito.when(bean.getDontShowAgain()).thenReturn(Boolean.TRUE);

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		Mockito.doNothing().when(bean).writeConfiguration(valueCapture.capture());

		// call the method under test
		bean.saveConfiguration();

		// verify the results
		Mockito.verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.ENVIRONMENT_STANZA_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.ENVIRONMENT_IDENTIFIER_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.ENVIRONMENT_SHOW_SETUP_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.ENVIRONMENT_SUPPORT_EMAIL_ADDRESS_KEY));
	}

	@Test
	public void testSaveConfigurationUpdatesMailProperties() throws Exception {
		UtilImpl.SMTP_TEST_BOGUS_SEND = true; // set to true to ensure it is overridden and included below
		
		// setup mocks
		Mockito.when(bean.getMailServerUrl()).thenReturn("127.0.0.1");
		Mockito.when(bean.getMailSender()).thenReturn("test2@test.com");
		Mockito.when(bean.getMailPort()).thenReturn(Integer.valueOf(465));
		Mockito.when(bean.getMailUsername()).thenReturn("username");
		Mockito.when(bean.getMailPassword()).thenReturn("password");
		Mockito.when(bean.getMailBogusSend()).thenReturn(Boolean.FALSE);
		Mockito.when(bean.getMailTestRecipient()).thenReturn("test@test.com");

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		Mockito.doNothing().when(bean).writeConfiguration(valueCapture.capture());

		// call the method under test
		bean.saveConfiguration();

		// verify the results
		Mockito.verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_STANZA_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_PORT_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_PWD_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_SENDER_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_SERVER_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_TEST_BOGUS_SEND_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_TEST_RECIPIENT_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_UID_KEY));
	}

	@Test
	public void testSaveConfigurationUpdatesMapProperties() throws Exception {
		// setup mocks
		Mockito.when(bean.getMapCentre()).thenReturn(new GeometryFactory().createPoint(new Coordinate(1, 1)));
		Mockito.when(bean.getMapLayer()).thenReturn("google.maps.MapTypeId.HYBRID");
		Mockito.when(bean.getMapType()).thenReturn(MapType.gmap);
		Mockito.when(bean.getMapZoom()).thenReturn(Integer.valueOf(19));

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		Mockito.doNothing().when(bean).writeConfiguration(valueCapture.capture());

		// call the method under test
		bean.saveConfiguration();

		// verify the results
		Mockito.verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.MAP_STANZA_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.MAP_CENTRE_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.MAP_LAYERS_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.MAP_TYPE_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.MAP_ZOOM_KEY));
	}

	@Test
	public void testSaveConfigurationUpdatesConcurrentSessionSecurityProperties() throws Exception {
		UtilImpl.CONCURRENT_SESSION_WARNINGS = true;
		UtilImpl.CONCURRENT_SESSION_NOTIFICATIONS = true;

		Mockito.when(bean.getConcurrentSessionWarnings()).thenReturn(Boolean.FALSE);
		Mockito.when(bean.getConcurrentSessionNotifications()).thenReturn(Boolean.FALSE);

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		Mockito.doNothing().when(bean).writeConfiguration(valueCapture.capture());

		bean.saveConfiguration();

		Mockito.verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SECURITY_STANZA_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SECURITY_CONCURRENT_SESSION_WARNINGS_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SECURITY_CONCURRENT_SESSION_NOTIFICATIONS_KEY));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testConcurrentSessionSettingsAccessorsAndDefaults() {
		StartupExtension startup = new StartupExtension();

		assertThat(Startup.concurrentSessionWarningsPropertyName, containsString("concurrentSessionWarnings"));
		assertThat(Startup.concurrentSessionNotificationsPropertyName, containsString("concurrentSessionNotifications"));
		assertThat(startup.getConcurrentSessionWarnings(), org.hamcrest.CoreMatchers.is(Boolean.TRUE));
		assertThat(startup.getConcurrentSessionNotifications(), org.hamcrest.CoreMatchers.is(Boolean.TRUE));

		startup.setConcurrentSessionWarnings(Boolean.FALSE);
		startup.setConcurrentSessionNotifications(Boolean.FALSE);

		assertThat(startup.getConcurrentSessionWarnings(), org.hamcrest.CoreMatchers.is(Boolean.FALSE));
		assertThat(startup.getConcurrentSessionNotifications(), org.hamcrest.CoreMatchers.is(Boolean.FALSE));
	}

	@Test
	public void testSetDontShowEmptyOverrideProperties() throws Exception {
		// setup mocks
		Mockito.when(bean.getDontShowAgain()).thenReturn(Boolean.TRUE);
		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		Mockito.doNothing().when(bean).writeConfiguration(valueCapture.capture());
		
		// call the method under test
		bean.setDontShow();
		
		// verify
		Mockito.verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.ENVIRONMENT_STANZA_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.ENVIRONMENT_SHOW_SETUP_KEY));
	}

	@Test
	public void testSetDontShowMergeExistingOverrideProperties() throws Exception {
		// setup test data
		Map<String, Object> smtp = new HashMap<>();
		smtp.put(StartupExtension.SMTP_SERVER_KEY, "localhost");
		overrideProperties.put(StartupExtension.SMTP_STANZA_KEY, smtp);

		// setup mocks
		Mockito.when(bean.getDontShowAgain()).thenReturn(Boolean.TRUE);
		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		Mockito.doNothing().when(bean).writeConfiguration(valueCapture.capture());

		// call the method under test
		bean.setDontShow();

		// verify
		Mockito.verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.ENVIRONMENT_STANZA_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.ENVIRONMENT_SHOW_SETUP_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_STANZA_KEY));
	}

	@Test
	public void testSaveConfigurationUpdatesCaptchaGoogleRecaptcha() throws Exception {
		// setup test data so recaptcha keys differ from UtilImpl values
		UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY = null;
		UtilImpl.GOOGLE_RECAPTCHA_SECRET_KEY = null;
		UtilImpl.CLOUDFLARE_TURNSTILE_SITE_KEY = null;
		UtilImpl.CLOUDFLARE_TURNSTILE_SECRET_KEY = null;

		// setup mocks
		Mockito.when(bean.getCaptchaType()).thenReturn(CaptchaType.googleRecaptcha);
		Mockito.when(bean.getApiGoogleRecaptchaSiteKey()).thenReturn("site-key");
		Mockito.when(bean.getApiGoogleRecaptchaSecretKey()).thenReturn("secret-key");

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		Mockito.doNothing().when(bean).writeConfiguration(valueCapture.capture());

		// call the method under test
		bean.saveConfiguration();

		// verify
		Mockito.verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.API_GOOGLE_RECAPTCHA_SITE_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.API_GOOGLE_RECAPTCHA_SECRET_KEY));
	}

	@Test
	public void testSaveConfigurationUpdatesCaptchaTypeCloudflareTurnstile() throws Exception {
		// setup test data so turnstile keys differ from UtilImpl values
		UtilImpl.CLOUDFLARE_TURNSTILE_SITE_KEY = null;
		UtilImpl.CLOUDFLARE_TURNSTILE_SECRET_KEY = null;
		UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY = null;
		UtilImpl.GOOGLE_RECAPTCHA_SECRET_KEY = null;

		// setup mocks
		Mockito.when(bean.getCaptchaType()).thenReturn(CaptchaType.cloudflareTurnstile);
		Mockito.when(bean.getApiCloudflareTurnstileSiteKey()).thenReturn("turnstile-site");
		Mockito.when(bean.getApiCloudflareTurnstileSecretKey()).thenReturn("turnstile-secret");

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		Mockito.doNothing().when(bean).writeConfiguration(valueCapture.capture());

		// call the method under test
		bean.saveConfiguration();

		// verify
		Mockito.verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.API_CLOUDFLARE_TURNSTILE_SITE_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.API_CLOUDFLARE_TURNSTILE_SECRET_KEY));
	}

	@Test
	public void testSaveConfigurationUpdatesCaptchaTypeNull() throws Exception {
		// setup test data so keys are present and will be cleared
		UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY = "existing-site-key";
		UtilImpl.CLOUDFLARE_TURNSTILE_SITE_KEY = "existing-turnstile";

		// setup mocks - captchaType is null, should clear all keys
		Mockito.when(bean.getCaptchaType()).thenReturn(null);

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		Mockito.doNothing().when(bean).writeConfiguration(valueCapture.capture());

		try {
			// call the method under test
			bean.saveConfiguration();
		} finally {
			// restore UtilImpl state
			UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY = null;
			UtilImpl.CLOUDFLARE_TURNSTILE_SITE_KEY = null;
		}

		// verify
		Mockito.verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.API_GOOGLE_RECAPTCHA_SITE_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.API_CLOUDFLARE_TURNSTILE_SITE_KEY));
	}

	@Test
	public void testMarshallReturnsNullForEmptyProperties() {
		Map<String, Object> properties = new HashMap<>();
		String result = bean.marshall(properties);
		org.hamcrest.MatcherAssert.assertThat(result, org.hamcrest.CoreMatchers.is(org.hamcrest.CoreMatchers.nullValue()));
	}

	@Test
	public void testMarshallReturnsNullWhenAllSubMapsEmpty() {
		Map<String, Object> properties = new HashMap<>();
		properties.put(StartupExtension.API_STANZA_KEY, new HashMap<>());
		properties.put(StartupExtension.SMTP_STANZA_KEY, new HashMap<>());
		String result = bean.marshall(properties);
		org.hamcrest.MatcherAssert.assertThat(result, org.hamcrest.CoreMatchers.is(org.hamcrest.CoreMatchers.nullValue()));
	}

	@Test
	public void testClearApiClearsApiStanzaAndWrites() throws Exception {
		// setup test data with an existing api stanza
		Map<String, Object> properties = new HashMap<>(overrideProperties);
		Map<String, Object> api = new HashMap<>();
		api.put(StartupExtension.API_GOOGLE_MAPS_V3_KEY, "test-key");
		properties.put(StartupExtension.API_STANZA_KEY, api);

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		Mockito.doNothing().when(bean).writeConfiguration(valueCapture.capture());
		Mockito.doReturn("{}").when(bean).marshall(anyMap());

		// call the method under test
		bean.clearApi(properties);

		// verify
		Mockito.verify(bean, times(1)).writeConfiguration(anyString());
	}

	@Test
	public void testSaveConfigurationUpdatesRemainingSecurityProperties() throws Exception {
		// set UtilImpl defaults to trigger all branch conditions
		UtilImpl.PASSWORD_CHANGE_NOTIFICATIONS = true;
		UtilImpl.DIFFERENT_COUNTRY_LOGIN_NOTIFICATIONS = true;
		UtilImpl.IP_ADDRESS_CHANGE_NOTIFICATIONS = true;
		UtilImpl.ACCESS_EXCEPTION_NOTIFICATIONS = true;
		UtilImpl.SECURITY_EXCEPTION_NOTIFICATIONS = true;

		// setup mocks - return false to differ from UtilImpl's true values
		Mockito.when(bean.getPasswordChangeNotifications()).thenReturn(Boolean.FALSE);
		Mockito.when(bean.getDifferentCountryLoginNotifications()).thenReturn(Boolean.FALSE);
		Mockito.when(bean.getIpAddressChangeNotifications()).thenReturn(Boolean.FALSE);
		Mockito.when(bean.getAccessExceptionNotifications()).thenReturn(Boolean.FALSE);
		Mockito.when(bean.getSecurityExceptionNotifications()).thenReturn(Boolean.FALSE);
		Mockito.when(bean.getSecurityNotificationsEmail()).thenReturn("security@example.com");

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		Mockito.doNothing().when(bean).writeConfiguration(valueCapture.capture());

		try {
			bean.saveConfiguration();
		} finally {
			// restore UtilImpl state
			UtilImpl.PASSWORD_CHANGE_NOTIFICATIONS = true;
			UtilImpl.DIFFERENT_COUNTRY_LOGIN_NOTIFICATIONS = true;
			UtilImpl.IP_ADDRESS_CHANGE_NOTIFICATIONS = true;
			UtilImpl.ACCESS_EXCEPTION_NOTIFICATIONS = true;
			UtilImpl.SECURITY_EXCEPTION_NOTIFICATIONS = true;
		}

		Mockito.verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SECURITY_PASSWORD_CHANGE_NOTIFICATIONS_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SECURITY_DIFFERENT_COUNTRY_LOGIN_NOTIFICATIONS_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SECURITY_IP_ADDRESS_CHANGE_NOTIFICATIONS_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SECURITY_ACCESS_EXCEPTION_NOTIFICATIONS_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SECURITY_SECURITY_EXCEPTION_NOTIFICATIONS_KEY));
	}

	@Test
	public void testSaveConfigurationUpdatesBackupWithAzureType() throws Exception {
		// setup mocks
		Mockito.when(bean.getBackupType()).thenReturn(BackupType.azure);
		Mockito.when(bean.getBackupConnectionString()).thenReturn("DefaultEndpointsProtocol=https");
		Mockito.when(bean.getBackupDirectoryName()).thenReturn("skyve-backup");

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		Mockito.doNothing().when(bean).writeConfiguration(valueCapture.capture());

		// call the method under test
		bean.saveConfiguration();

		// verify
		Mockito.verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.BACKUP_STANZA_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.BACKUP_EXTERNAL_BACKUP_CLASS_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.BACKUP_PROPERTIES_KEY));
	}

}
