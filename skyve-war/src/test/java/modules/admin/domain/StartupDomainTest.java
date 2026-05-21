package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.Startup.BackupType;
import modules.admin.domain.Startup.CaptchaType;
import modules.admin.domain.Startup.MapType;
import util.AbstractH2Test;

/**
 * Tests for the {@link Startup} admin domain bean (transient).
 * Exercises getter/setter coverage via {@link DataBuilder} and targeted set/get calls.
 */
@SuppressWarnings("static-method")
class StartupDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderPopulatesStartupBean() throws Exception {
		Startup bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(Startup.MODULE_NAME, Startup.DOCUMENT_NAME);
		assertNotNull(bean);
		assertNotNull(bean.getBizId());
	}

	@Test
	void bizModuleAndDocumentAreCorrect() throws Exception {
		Startup bean = Startup.newInstance();
		assertEquals(Startup.MODULE_NAME, bean.getBizModule());
		assertEquals(Startup.DOCUMENT_NAME, bean.getBizDocument());
	}

	@Test
	void environmentIdentifierSetAndGet() throws Exception {
		Startup bean = Startup.newInstance();
		bean.setEnvironmentIdentifier("DEV");
		assertEquals("DEV", bean.getEnvironmentIdentifier());
	}

	@Test
	void environmentSupportEmailSetAndGet() throws Exception {
		Startup bean = Startup.newInstance();
		bean.setEnvironmentSupportEmail("support@example.com");
		assertEquals("support@example.com", bean.getEnvironmentSupportEmail());
	}

	@Test
	void mapTypeSetAndGet() throws Exception {
		Startup bean = Startup.newInstance();
		bean.setMapType(MapType.leaflet);
		assertEquals(MapType.leaflet, bean.getMapType());
		bean.setMapType(MapType.gmap);
		assertEquals(MapType.gmap, bean.getMapType());
	}

	@Test
	void mapLayerSetAndGet() throws Exception {
		Startup bean = Startup.newInstance();
		bean.setMapLayer("custom-tiles");
		assertEquals("custom-tiles", bean.getMapLayer());
	}

	@Test
	void mailServerSettingsSetAndGet() throws Exception {
		Startup bean = Startup.newInstance();
		bean.setMailServerUrl("smtp.example.com");
		bean.setMailPort(Integer.valueOf(587));
		bean.setMailUsername("user@example.com");
		bean.setMailSender("noreply@example.com");
		assertEquals("smtp.example.com", bean.getMailServerUrl());
		assertEquals(Integer.valueOf(587), bean.getMailPort());
		assertEquals("user@example.com", bean.getMailUsername());
		assertEquals("noreply@example.com", bean.getMailSender());
	}

	@Test
	void mailTestRecipientSetAndGet() throws Exception {
		Startup bean = Startup.newInstance();
		bean.setMailTestRecipient("test@example.com");
		assertEquals("test@example.com", bean.getMailTestRecipient());
	}

	@Test
	void backupTypeSetAndGet() throws Exception {
		Startup bean = Startup.newInstance();
		bean.setBackupType(BackupType.none);
		assertEquals(BackupType.none, bean.getBackupType());
		bean.setBackupType(BackupType.azure);
		assertEquals(BackupType.azure, bean.getBackupType());
	}

	@Test
	void backupConnectionStringSetAndGet() throws Exception {
		Startup bean = Startup.newInstance();
		bean.setBackupConnectionString("connection-string");
		bean.setBackupDirectoryName("/backups");
		assertEquals("connection-string", bean.getBackupConnectionString());
		assertEquals("/backups", bean.getBackupDirectoryName());
	}

	@Test
	void captchaTypeSetAndGet() throws Exception {
		Startup bean = Startup.newInstance();
		bean.setCaptchaType(CaptchaType.googleRecaptcha);
		assertEquals(CaptchaType.googleRecaptcha, bean.getCaptchaType());
	}

	@Test
	void apiKeysSetAndGet() throws Exception {
		Startup bean = Startup.newInstance();
		bean.setApiGoogleMapsKey("maps-key");
		bean.setApiGoogleRecaptchaSiteKey("recaptcha-site");
		bean.setApiGoogleRecaptchaSecretKey("recaptcha-secret");
		assertEquals("maps-key", bean.getApiGoogleMapsKey());
		assertEquals("recaptcha-site", bean.getApiGoogleRecaptchaSiteKey());
		assertEquals("recaptcha-secret", bean.getApiGoogleRecaptchaSecretKey());
	}

	@Test
	void apiCloudflareTurnstileKeysSetAndGet() throws Exception {
		Startup bean = Startup.newInstance();
		bean.setApiCloudflareTurnstileSiteKey("cf-site");
		bean.setApiCloudflareTurnstileSecretKey("cf-secret");
		assertEquals("cf-site", bean.getApiCloudflareTurnstileSiteKey());
		assertEquals("cf-secret", bean.getApiCloudflareTurnstileSecretKey());
	}

	@Test
	void apiTwilioSettingsSetAndGet() throws Exception {
		Startup bean = Startup.newInstance();
		bean.setApiTwilioSID("ACXXXXXXXX");
		bean.setApiTwilioAuthToken("auth-token");
		bean.setApiTwilioDefaultSendNumber("+61400000000");
		assertEquals("ACXXXXXXXX", bean.getApiTwilioSID());
		assertEquals("auth-token", bean.getApiTwilioAuthToken());
		assertEquals("+61400000000", bean.getApiTwilioDefaultSendNumber());
	}

	@Test
	void securityNotificationsEmailSetAndGet() throws Exception {
		Startup bean = Startup.newInstance();
		bean.setSecurityNotificationsEmail("security@example.com");
		assertEquals("security@example.com", bean.getSecurityNotificationsEmail());
	}

	@Test
	void mailBogusSendSetAndGet() throws Exception {
		Startup bean = Startup.newInstance();
		bean.setMailBogusSend(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getMailBogusSend());
	}

	@Test
	void geoIPKeySetAndGet() throws Exception {
		Startup bean = Startup.newInstance();
		bean.setGeoIPKey("geo-key-123");
		assertEquals("geo-key-123", bean.getGeoIPKey());
	}
}
