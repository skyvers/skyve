package modules.admin.Startup;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;

import modules.admin.domain.Startup;
import modules.admin.domain.Startup.BackupType;
import modules.admin.domain.Startup.CaptchaType;
import modules.admin.domain.Startup.MapType;

class StartupExtensionTest {
	private Map<String, Object> overrideProperties;

	@Mock
	private Customer customer;

	@InjectMocks
	@Spy
	private StartupExtension bean;

	@SuppressWarnings("resource")
	private AutoCloseable closeable;
	
	@BeforeEach
	@SuppressWarnings("resource")
	void before() {
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
		when(bean.getMailServerUrl()).thenReturn(UtilImpl.SMTP);
		when(bean.getMailSender()).thenReturn(UtilImpl.SMTP_SENDER);
		when(bean.getMailPort()).thenReturn(Integer.valueOf(UtilImpl.SMTP_PORT));
		when(bean.getMailBogusSend()).thenReturn(Boolean.valueOf(UtilImpl.SMTP_TEST_BOGUS_SEND));
		when(bean.getMapLayer()).thenReturn(UtilImpl.MAP_LAYERS);
	}

	@AfterEach
	void tearDown() throws Exception {
		UtilImpl.SMTP_LOG_BODY_EXCERPT = false;
		UtilImpl.SMTP_LOG_ATTACHMENTS = false;
		UtilImpl.SMTP_LOG_ATTACHMENTS_MAX_BYTES = UtilImpl.DEFAULT_SMTP_LOG_ATTACHMENTS_MAX_BYTES;
		if (closeable != null) {
			closeable.close();
		}
	}
	
	@Test
	void testSaveConfigurationEmptyPropertiesWritesNulls() throws Exception {
		// setup mocks
		doNothing().when(bean).writeConfiguration(anyString());

		// call the method under test
		bean.saveConfiguration();

		// verify
		verify(bean, times(1)).writeConfiguration(anyString());
	}
	
	@Test
	void testSaveConfigurationUpdatesApiProperties() throws Exception {
		// setup mocks
		when(bean.getApiGoogleMapsKey()).thenReturn("12345");
		when(bean.getApiGoogleRecaptchaSiteKey()).thenReturn("12345");
		when(bean.getApiGoogleRecaptchaSecretKey()).thenReturn("12345");

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		doNothing().when(bean).writeConfiguration(valueCapture.capture());

		// call the method under test
		bean.saveConfiguration();

		// verify the results
		verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.API_STANZA_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.API_GOOGLE_MAPS_V3_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.API_GOOGLE_RECAPTCHA_SITE_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.API_GOOGLE_RECAPTCHA_SECRET_KEY));
	}

	@Test
	void testSaveConfigurationUpdatesBackupProperties() throws Exception {
		// setup mocks
		when(bean.getBackupType()).thenReturn(BackupType.none);

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		doNothing().when(bean).writeConfiguration(valueCapture.capture());

		// call the method under test
		bean.saveConfiguration();

		// verify the results
		verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.BACKUP_STANZA_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.BACKUP_EXTERNAL_BACKUP_CLASS_KEY));
	}

	@Test
	void testSaveConfigurationUpdatesEnvironmentProperties() throws Exception {
		// setup mocks
		when(bean.getEnvironmentIdentifier()).thenReturn("dev");
		when(bean.getEnvironmentSupportEmail()).thenReturn("test@test.com");
		when(bean.getDontShowAgain()).thenReturn(Boolean.TRUE);

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		doNothing().when(bean).writeConfiguration(valueCapture.capture());

		// call the method under test
		bean.saveConfiguration();

		// verify the results
		verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.ENVIRONMENT_STANZA_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.ENVIRONMENT_IDENTIFIER_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.ENVIRONMENT_SHOW_SETUP_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.ENVIRONMENT_SUPPORT_EMAIL_ADDRESS_KEY));
	}

	@Test
	void testSaveConfigurationUpdatesMailProperties() throws Exception {
		UtilImpl.SMTP_TEST_BOGUS_SEND = true; // set to true to ensure it is overridden and included below
		UtilImpl.SMTP_LOG_BODY_EXCERPT = false; // set to false to ensure it is overridden and included below
		UtilImpl.SMTP_LOG_ATTACHMENTS = false; // set to false to ensure it is overridden and included below
		
		// setup mocks
		when(bean.getMailServerUrl()).thenReturn("127.0.0.1");
		when(bean.getMailSender()).thenReturn("test2@test.com");
		when(bean.getMailPort()).thenReturn(Integer.valueOf(465));
		when(bean.getMailUsername()).thenReturn("username");
		when(bean.getMailPassword()).thenReturn("password");
		when(bean.getMailBogusSend()).thenReturn(Boolean.FALSE);
		when(bean.getMailTestRecipient()).thenReturn("test@test.com");
		when(bean.getMailLogBodyExcerpt()).thenReturn(Boolean.TRUE);
		when(bean.getMailLogAttachments()).thenReturn(Boolean.TRUE);
		when(bean.getMailLogAttachmentsMaxBytes()).thenReturn(Long.valueOf(1024));

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		doNothing().when(bean).writeConfiguration(valueCapture.capture());

		// call the method under test
		bean.saveConfiguration();

		// verify the results
		verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_STANZA_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_PORT_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_PWD_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_SENDER_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_SERVER_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_TEST_BOGUS_SEND_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_TEST_RECIPIENT_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_UID_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_LOG_BODY_EXCERPT_KEY));
		assertThat(Boolean.valueOf(UtilImpl.SMTP_LOG_BODY_EXCERPT), org.hamcrest.CoreMatchers.is(Boolean.TRUE));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_LOG_ATTACHMENTS_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_LOG_ATTACHMENTS_MAX_BYTES_KEY));
		assertThat(Boolean.valueOf(UtilImpl.SMTP_LOG_ATTACHMENTS), org.hamcrest.CoreMatchers.is(Boolean.TRUE));
		assertThat(Long.valueOf(UtilImpl.SMTP_LOG_ATTACHMENTS_MAX_BYTES), org.hamcrest.CoreMatchers.is(Long.valueOf(1024)));
	}

	@Test
	void testSaveConfigurationUpdatesMapProperties() throws Exception {
		// setup mocks
		when(bean.getMapCentre()).thenReturn(new GeometryFactory().createPoint(new Coordinate(1, 1)));
		when(bean.getMapLayer()).thenReturn("google.maps.MapTypeId.HYBRID");
		when(bean.getMapType()).thenReturn(MapType.gmap);
		when(bean.getMapZoom()).thenReturn(Integer.valueOf(19));

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		doNothing().when(bean).writeConfiguration(valueCapture.capture());

		// call the method under test
		bean.saveConfiguration();

		// verify the results
		verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.MAP_STANZA_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.MAP_CENTRE_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.MAP_LAYERS_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.MAP_TYPE_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.MAP_ZOOM_KEY));
	}

	@Test
	void testSaveConfigurationUpdatesConcurrentSessionSecurityProperties() throws Exception {
		UtilImpl.CONCURRENT_SESSION_WARNINGS = true;
		UtilImpl.CONCURRENT_SESSION_NOTIFICATIONS = true;

		when(bean.getConcurrentSessionWarnings()).thenReturn(Boolean.FALSE);
		when(bean.getConcurrentSessionNotifications()).thenReturn(Boolean.FALSE);

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		doNothing().when(bean).writeConfiguration(valueCapture.capture());

		bean.saveConfiguration();

		verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SECURITY_STANZA_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SECURITY_CONCURRENT_SESSION_WARNINGS_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SECURITY_CONCURRENT_SESSION_NOTIFICATIONS_KEY));
	}

	@Test
	@SuppressWarnings("static-method")
	void testConcurrentSessionSettingsAccessorsAndDefaults() {
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
	void testSetDontShowEmptyOverrideProperties() throws Exception {
		// setup mocks
		when(bean.getDontShowAgain()).thenReturn(Boolean.TRUE);
		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		doNothing().when(bean).writeConfiguration(valueCapture.capture());
		
		// call the method under test
		bean.setDontShow();
		
		// verify
		verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.ENVIRONMENT_STANZA_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.ENVIRONMENT_SHOW_SETUP_KEY));
	}

	@Test
	void testSetDontShowMergeExistingOverrideProperties() throws Exception {
		// setup test data
		Map<String, Object> smtp = new HashMap<>();
		smtp.put(StartupExtension.SMTP_SERVER_KEY, "localhost");
		overrideProperties.put(StartupExtension.SMTP_STANZA_KEY, smtp);

		// setup mocks
		when(bean.getDontShowAgain()).thenReturn(Boolean.TRUE);
		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		doNothing().when(bean).writeConfiguration(valueCapture.capture());

		// call the method under test
		bean.setDontShow();

		// verify
		verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.ENVIRONMENT_STANZA_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.ENVIRONMENT_SHOW_SETUP_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_STANZA_KEY));
	}

	@Test
	void testSaveConfigurationUpdatesCaptchaGoogleRecaptcha() throws Exception {
		// setup test data so recaptcha keys differ from UtilImpl values
		UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY = null;
		UtilImpl.GOOGLE_RECAPTCHA_SECRET_KEY = null;
		UtilImpl.CLOUDFLARE_TURNSTILE_SITE_KEY = null;
		UtilImpl.CLOUDFLARE_TURNSTILE_SECRET_KEY = null;

		// setup mocks
		when(bean.getCaptchaType()).thenReturn(CaptchaType.googleRecaptcha);
		when(bean.getApiGoogleRecaptchaSiteKey()).thenReturn("site-key");
		when(bean.getApiGoogleRecaptchaSecretKey()).thenReturn("secret-key");

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		doNothing().when(bean).writeConfiguration(valueCapture.capture());

		// call the method under test
		bean.saveConfiguration();

		// verify
		verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.API_GOOGLE_RECAPTCHA_SITE_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.API_GOOGLE_RECAPTCHA_SECRET_KEY));
	}

	@Test
	void testSaveConfigurationUpdatesCaptchaTypeCloudflareTurnstile() throws Exception {
		// setup test data so turnstile keys differ from UtilImpl values
		UtilImpl.CLOUDFLARE_TURNSTILE_SITE_KEY = null;
		UtilImpl.CLOUDFLARE_TURNSTILE_SECRET_KEY = null;
		UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY = null;
		UtilImpl.GOOGLE_RECAPTCHA_SECRET_KEY = null;

		// setup mocks
		when(bean.getCaptchaType()).thenReturn(CaptchaType.cloudflareTurnstile);
		when(bean.getApiCloudflareTurnstileSiteKey()).thenReturn("turnstile-site");
		when(bean.getApiCloudflareTurnstileSecretKey()).thenReturn("turnstile-secret");

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		doNothing().when(bean).writeConfiguration(valueCapture.capture());

		// call the method under test
		bean.saveConfiguration();

		// verify
		verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.API_CLOUDFLARE_TURNSTILE_SITE_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.API_CLOUDFLARE_TURNSTILE_SECRET_KEY));
	}

	@Test
	void testSaveConfigurationUpdatesCaptchaTypeNull() throws Exception {
		// setup test data so keys are present and will be cleared
		UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY = "existing-site-key";
		UtilImpl.CLOUDFLARE_TURNSTILE_SITE_KEY = "existing-turnstile";

		// setup mocks - captchaType is null, should clear all keys
		when(bean.getCaptchaType()).thenReturn(null);

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		doNothing().when(bean).writeConfiguration(valueCapture.capture());

		try {
			// call the method under test
			bean.saveConfiguration();
		} finally {
			// restore UtilImpl state
			UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY = null;
			UtilImpl.CLOUDFLARE_TURNSTILE_SITE_KEY = null;
		}

		// verify
		verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.API_GOOGLE_RECAPTCHA_SITE_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.API_CLOUDFLARE_TURNSTILE_SITE_KEY));
	}

	@Test
	void testMarshallReturnsNullForEmptyProperties() {
		Map<String, Object> properties = new HashMap<>();
		String result = bean.marshall(properties);
		org.hamcrest.MatcherAssert.assertThat(result, org.hamcrest.CoreMatchers.is(org.hamcrest.CoreMatchers.nullValue()));
	}

	@Test
	void testMarshallReturnsNullWhenAllSubMapsEmpty() {
		Map<String, Object> properties = new HashMap<>();
		properties.put(StartupExtension.API_STANZA_KEY, new HashMap<>());
		properties.put(StartupExtension.SMTP_STANZA_KEY, new HashMap<>());
		String result = bean.marshall(properties);
		org.hamcrest.MatcherAssert.assertThat(result, org.hamcrest.CoreMatchers.is(org.hamcrest.CoreMatchers.nullValue()));
	}

	@Test
	void testClearApiClearsApiStanzaAndWrites() throws Exception {
		// setup test data with an existing api stanza
		Map<String, Object> properties = new HashMap<>(overrideProperties);
		Map<String, Object> api = new HashMap<>();
		api.put(StartupExtension.API_GOOGLE_MAPS_V3_KEY, "test-key");
		properties.put(StartupExtension.API_STANZA_KEY, api);

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		doNothing().when(bean).writeConfiguration(valueCapture.capture());
		doReturn("{}").when(bean).marshall(anyMap());

		// call the method under test
		bean.clearApi(properties);

		// verify
		verify(bean, times(1)).writeConfiguration(anyString());
	}

	@Test
	void testSaveConfigurationUpdatesRemainingSecurityProperties() throws Exception {
		// set UtilImpl defaults to trigger all branch conditions
		UtilImpl.PASSWORD_CHANGE_NOTIFICATIONS = true;
		UtilImpl.DIFFERENT_COUNTRY_LOGIN_NOTIFICATIONS = true;
		UtilImpl.IP_ADDRESS_CHANGE_NOTIFICATIONS = true;
		UtilImpl.ACCESS_EXCEPTION_NOTIFICATIONS = true;
		UtilImpl.SECURITY_EXCEPTION_NOTIFICATIONS = true;

		// setup mocks - return false to differ from UtilImpl's true values
		when(bean.getPasswordChangeNotifications()).thenReturn(Boolean.FALSE);
		when(bean.getDifferentCountryLoginNotifications()).thenReturn(Boolean.FALSE);
		when(bean.getIpAddressChangeNotifications()).thenReturn(Boolean.FALSE);
		when(bean.getAccessExceptionNotifications()).thenReturn(Boolean.FALSE);
		when(bean.getSecurityExceptionNotifications()).thenReturn(Boolean.FALSE);
		when(bean.getSecurityNotificationsEmail()).thenReturn("security@example.com");

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		doNothing().when(bean).writeConfiguration(valueCapture.capture());

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

		verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SECURITY_PASSWORD_CHANGE_NOTIFICATIONS_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SECURITY_DIFFERENT_COUNTRY_LOGIN_NOTIFICATIONS_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SECURITY_IP_ADDRESS_CHANGE_NOTIFICATIONS_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SECURITY_ACCESS_EXCEPTION_NOTIFICATIONS_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SECURITY_SECURITY_EXCEPTION_NOTIFICATIONS_KEY));
	}

	@Test
	void testSaveConfigurationUpdatesBackupWithAzureType() throws Exception {
		// setup mocks
		when(bean.getBackupType()).thenReturn(BackupType.azure);
		when(bean.getBackupConnectionString()).thenReturn("DefaultEndpointsProtocol=https");
		when(bean.getBackupDirectoryName()).thenReturn("skyve-backup");

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		doNothing().when(bean).writeConfiguration(valueCapture.capture());

		// call the method under test
		bean.saveConfiguration();

		// verify
		verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.BACKUP_STANZA_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.BACKUP_EXTERNAL_BACKUP_CLASS_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.BACKUP_PROPERTIES_KEY));
	}

}
