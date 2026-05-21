package org.skyve.impl.web;

import java.io.OutputStream;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;

import org.skyve.impl.web.filter.ResponseHeaderFilter;

public class SkyveContextListenerTest {
	private final MailService originalMailService = defaultMailService();
	private final String originalMailServiceClass = UtilImpl.SKYVE_MAIL_SERVICE_CLASS;
	private final String originalSmtp = UtilImpl.SMTP;
	private final int originalSmtpPort = UtilImpl.SMTP_PORT;
	private final String originalSmtpUid = UtilImpl.SMTP_UID;
	private final String originalSmtpPwd = UtilImpl.SMTP_PWD;
	private final Map<String, String> originalSmtpProperties = UtilImpl.SMTP_PROPERTIES;
	private final Map<String, String> originalSmtpHeaders = UtilImpl.SMTP_HEADERS;
	private final String originalSmtpSender = UtilImpl.SMTP_SENDER;
	private final String originalSmtpTestRecipient = UtilImpl.SMTP_TEST_RECIPIENT;
	private final boolean originalSmtpTestBogusSend = UtilImpl.SMTP_TEST_BOGUS_SEND;
	private final UtilImpl.ArchiveConfig originalArchiveConfig = UtilImpl.ARCHIVE_CONFIG;
	private final String originalCustomer = UtilImpl.CUSTOMER;
	private final String originalSkyveContextRealPath = UtilImpl.SKYVE_CONTEXT_REAL_PATH;
	private final String originalPropertiesFilePath = UtilImpl.PROPERTIES_FILE_PATH;
	private final String originalArchiveName = UtilImpl.ARCHIVE_NAME;
	private final boolean originalDevLoginFilterUsed = UtilImpl.DEV_LOGIN_FILTER_USED;

	private static MailService defaultMailService() {
		MailServiceStaticSingleton.setDefault();
		return MailServiceStaticSingleton.get();
	}

	@After
	public void after() {
		MailServiceStaticSingleton.set(originalMailService);
		UtilImpl.SKYVE_MAIL_SERVICE_CLASS = originalMailServiceClass;
		UtilImpl.SMTP = originalSmtp;
		UtilImpl.SMTP_PORT = originalSmtpPort;
		UtilImpl.SMTP_UID = originalSmtpUid;
		UtilImpl.SMTP_PWD = originalSmtpPwd;
		UtilImpl.SMTP_PROPERTIES = originalSmtpProperties;
		UtilImpl.SMTP_HEADERS = originalSmtpHeaders;
		UtilImpl.SMTP_SENDER = originalSmtpSender;
		UtilImpl.SMTP_TEST_RECIPIENT = originalSmtpTestRecipient;
		UtilImpl.SMTP_TEST_BOGUS_SEND = originalSmtpTestBogusSend;
		UtilImpl.ARCHIVE_CONFIG = originalArchiveConfig;
		UtilImpl.CUSTOMER = originalCustomer;
		UtilImpl.SKYVE_CONTEXT_REAL_PATH = originalSkyveContextRealPath;
		UtilImpl.PROPERTIES_FILE_PATH = originalPropertiesFilePath;
		UtilImpl.ARCHIVE_NAME = originalArchiveName;
		UtilImpl.DEV_LOGIN_FILTER_USED = originalDevLoginFilterUsed;
	}

	@Test
	@SuppressWarnings("static-method")
	public void testContextInitializedRequiresSecurityHeadersFilter() {
		ServletContextEvent event = mock(ServletContextEvent.class);
		ServletContext context = mock(ServletContext.class);
		when(event.getServletContext()).thenReturn(context);
		when(context.getRealPath("/")).thenReturn("/tmp/skyve-test.war");
		when(context.getInitParameter("PROPERTIES_FILE_PATH")).thenReturn("/tmp/skyve-test.json");
		when(context.getFilterRegistrations()).thenReturn(Map.of());

		IllegalStateException e = assertThrows(IllegalStateException.class,
				() -> new SkyveContextListener().contextInitialized(event));

		assertThat(e.getMessage(),
				is("A Filter <filter-name>SecurityHeadersFilter</filter-name> of <filter-class>org.skyve.impl.web.filter.ResponseHeaderFilter</filter-class> is required in web.xml."));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testCleanupContentDirectoryHandlesEmptyStrings() throws Exception {
		// setup the test data
		final String test1 = null, test2 = "";

		// perform the method under test
		String result1 = SkyveContextListener.cleanupDirectory(test1);
		String result2 = SkyveContextListener.cleanupDirectory(test2);

		// verify the results
		assertThat(result1, is(test1));
		assertThat(result2, is(test2));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testCleanupContentDirectoryAppendsSlash() throws Exception {
		// setup the test data
		final String test1 = "C:/workspace/content",
				test2 = "C:/workspace/content/",
				test3 = "C:\\workspace\\content",
				test4 = "C:\\workspace\\content\\";
		final String expected = "C:/workspace/content/";

		// perform the method under test
		String result1 = SkyveContextListener.cleanupDirectory(test1);
		String result2 = SkyveContextListener.cleanupDirectory(test2);
		String result3 = SkyveContextListener.cleanupDirectory(test3);
		String result4 = SkyveContextListener.cleanupDirectory(test4);

		// verify the results
		assertThat(result1, is(expected));
		assertThat(result2, is(expected));
		assertThat(result3, is(expected));
		assertThat(result4, is(expected));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testCleanupModuleDirectoryHandlesEmptyStrings() throws Exception {
		// setup the test data
		final String test1 = null, test2 = "";

		// perform the method under test
		String result1 = UtilImpl.cleanupModuleDirectory(test1);
		String result2 = UtilImpl.cleanupModuleDirectory(test2);

		// verify the results
		assertThat(result1, is(test1));
		assertThat(result2, is(test2));
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testCleanupModuleDirectoryAppendsModules() throws Exception {
		// setup the test data
		final String test1 = "/src/main/java/",
				test2 = "/src/main/java",
				test3 = "\\src\\main\\java\\",
				test4 = "\\src\\main\\java";

		// perform the method under test
		String result1 = UtilImpl.cleanupModuleDirectory(test1);
		String result2 = UtilImpl.cleanupModuleDirectory(test2);
		String result3 = UtilImpl.cleanupModuleDirectory(test3);
		String result4 = UtilImpl.cleanupModuleDirectory(test4);

		// verify the results
		assertThat(result1, is(test1 + "modules/"));
		assertThat(result2, is(test2 + "/modules/"));
		assertThat(result3, is("\\src\\main\\java/modules/"));
		assertThat(result4, is(test4 + "/modules/"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testCleanupModuleDirectoryWithModulesAppendsSlash() throws Exception {
		// setup the test data
		final String test1 = "/src/main/java/modules",
				test2 = "/src/main/java/modules/",
				test3 = "\\src\\main\\java\\modules",
				test4 = "\\src\\main\\java\\modules\\";

		// perform the method under test
		String result1 = UtilImpl.cleanupModuleDirectory(test1);
		String result2 = UtilImpl.cleanupModuleDirectory(test2);
		String result3 = UtilImpl.cleanupModuleDirectory(test3);
		String result4 = UtilImpl.cleanupModuleDirectory(test4);

		// verify the results
		assertThat(result1, is(test1 + "/"));
		assertThat(result2, is(test2));
		assertThat(result3, is(test3 + "/"));
		assertThat(result4, is("\\src\\main\\java\\modules" + "/"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testConfigureMailServiceAndSmtpAllowsMissingSmtpForNonSMTPMailService() {
		Map<String, Object> properties = new HashMap<>();
		Map<String, Object> factories = new HashMap<>();
		factories.put("mailServiceClass", TestMailService.class.getName());
		Map<String, String> smtpProperties = new HashMap<>();
		smtpProperties.put("smtp.auth", "true");
		Map<String, String> smtpHeaders = new HashMap<>();
		smtpHeaders.put("X-Test", "value");

		UtilImpl.SMTP = "existing.smtp.skyve.org";
		UtilImpl.SMTP_PORT = 2525;
		UtilImpl.SMTP_UID = "existing-user";
		UtilImpl.SMTP_PWD = "existing-pwd";
		UtilImpl.SMTP_PROPERTIES = smtpProperties;
		UtilImpl.SMTP_HEADERS = smtpHeaders;
		UtilImpl.SMTP_SENDER = "existing-sender@skyve.org";
		UtilImpl.SMTP_TEST_RECIPIENT = "existing-recipient@skyve.org";
		UtilImpl.SMTP_TEST_BOGUS_SEND = true;

		SkyveContextListener.configureMailServiceAndSmtp(properties, factories);

		assertThat(MailServiceStaticSingleton.get(), instanceOf(TestMailService.class));
		assertThat(UtilImpl.SMTP, is("existing.smtp.skyve.org"));
		assertEquals(2525, UtilImpl.SMTP_PORT);
		assertThat(UtilImpl.SMTP_UID, is("existing-user"));
		assertThat(UtilImpl.SMTP_PWD, is("existing-pwd"));
		assertThat(UtilImpl.SMTP_PROPERTIES, is(smtpProperties));
		assertThat(UtilImpl.SMTP_HEADERS, is(smtpHeaders));
		assertThat(UtilImpl.SMTP_SENDER, is("existing-sender@skyve.org"));
		assertThat(UtilImpl.SMTP_TEST_RECIPIENT, is("existing-recipient@skyve.org"));
		assertTrue(UtilImpl.SMTP_TEST_BOGUS_SEND);
	}

	@Test
	public void testConfigureMailServiceAndSmtpRequiresSmtpForDefaultSMTPService() {
		Map<String, Object> properties = new HashMap<>();
		Map<String, Object> factories = new HashMap<>();

		assertThrows(IllegalStateException.class, () -> SkyveContextListener.configureMailServiceAndSmtp(properties, factories));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testConfigureMailServiceAndSmtpReadsConfiguredSmtp() {
		Map<String, Object> properties = new HashMap<>();
		Map<String, Object> smtp = new HashMap<>();
		smtp.put("server", "localhost");
		smtp.put("port", Integer.valueOf(25));
		smtp.put("sender", "mailer@skyve.org");
		smtp.put("testBogusSend", Boolean.FALSE);
		properties.put("smtp", smtp);

		Map<String, Object> factories = new HashMap<>();
		factories.put("mailServiceClass", NoOpMailService.class.getName());

		SkyveContextListener.configureMailServiceAndSmtp(properties, factories);

		assertThat(UtilImpl.SMTP, is("localhost"));
		assertEquals(25, UtilImpl.SMTP_PORT);
		assertThat(UtilImpl.SMTP_SENDER, is("mailer@skyve.org"));
	}

	@Test
	public void testConfigureMailServiceAndSmtpReadsConfiguredHeaders() {
		Map<String, Object> properties = new HashMap<>();
		Map<String, Object> smtp = new HashMap<>();
		smtp.put("server", "localhost");
		smtp.put("port", Integer.valueOf(25));
		smtp.put("sender", "mailer@skyve.org");
		smtp.put("testBogusSend", Boolean.FALSE);
		smtp.put("headers", Map.of("X-PM-Message-Stream", "outbound", "X-Test-Header", "skyve-header-test"));
		properties.put("smtp", smtp);

		Map<String, Object> factories = new HashMap<>();

		SkyveContextListener.configureMailServiceAndSmtp(properties, factories);

		assertThat(UtilImpl.SMTP_HEADERS.get("X-PM-Message-Stream"), is("outbound"));
		assertThat(UtilImpl.SMTP_HEADERS.get("X-Test-Header"), is("skyve-header-test"));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testConfigureArchivePropertiesReadsMailLogConfig() throws Exception {
		Map<String, Object> properties = new HashMap<>();
		Map<String, Object> archive = new HashMap<>();
		archive.put("exportRuntimeSec", Integer.valueOf(300));
		archive.put("exportBatchSize", Integer.valueOf(100));

		Map<String, Object> mailLogDoc = new HashMap<>();
		mailLogDoc.put("module", "admin");
		mailLogDoc.put("document", "MailLog");
		mailLogDoc.put("directory", "maillog-archive");
		mailLogDoc.put("retainDeletedDocumentsDays", Integer.valueOf(30));
		archive.put("documents", List.of(mailLogDoc));
		properties.put("archive", archive);

		UtilImpl.CUSTOMER = "demo";

		Method configureArchive = SkyveContextListener.class.getDeclaredMethod("configureArchiveProperties", Map.class);
		configureArchive.setAccessible(true);
		configureArchive.invoke(null, properties);

		Optional<UtilImpl.ArchiveConfig.ArchiveDocConfig> config = UtilImpl.ARCHIVE_CONFIG.findArchiveDocConfig("admin", "MailLog");
		assertTrue(config.isPresent());
		assertThat(config.get().directory(), is("maillog-archive"));
		assertEquals(30, config.get().retainDeletedDocumentsDays());
	}

	public static class TestMailService implements MailService {
		@Override
		public void writeMail(Mail mail, OutputStream out) {
			// no-op
		}

		@Override
		public void sendMail(Mail mail) {
			// no-op
		}

		@Override
		public void sendBulkMail(List<Mail> mails) {
			// no-op
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPopulateUtilImplReadsConcurrentSessionSettingsWhenPresent() throws Exception {
		boolean originalWarnings = UtilImpl.CONCURRENT_SESSION_WARNINGS;
		boolean originalNotifications = UtilImpl.CONCURRENT_SESSION_NOTIFICATIONS;
		String originalPropertiesFilePath = System.getProperty("PROPERTIES_FILE_PATH");
		Class<? extends AbstractPersistence> originalPersistenceImplementation = AbstractPersistence.IMPLEMENTATION_CLASS;
		Class<? extends DynamicPersistence> originalDynamicPersistenceImplementation = AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS;
		Path tempDir = Files.createTempDirectory("skyve-context-listener-present");
		try {
			UtilImpl.CONCURRENT_SESSION_WARNINGS = true;
			UtilImpl.CONCURRENT_SESSION_NOTIFICATIONS = true;

			Path configFile = writeConfiguration(tempDir, true);
			System.setProperty("PROPERTIES_FILE_PATH", configFile.toString());

			ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();
			ProvidedRepositoryFactory.set(mock(ProvidedRepository.class));
			try {
				invokePopulateUtilImpl(configFile, tempDir);
			}
			finally {
				ProvidedRepositoryFactory.set(originalRepository);
			}

			assertThat(UtilImpl.CONCURRENT_SESSION_WARNINGS, is(false));
			assertThat(UtilImpl.CONCURRENT_SESSION_NOTIFICATIONS, is(false));
		}
		finally {
			restoreProperty("PROPERTIES_FILE_PATH", originalPropertiesFilePath);
			AbstractPersistence.IMPLEMENTATION_CLASS = originalPersistenceImplementation;
			AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS = originalDynamicPersistenceImplementation;
			UtilImpl.CONCURRENT_SESSION_WARNINGS = originalWarnings;
			UtilImpl.CONCURRENT_SESSION_NOTIFICATIONS = originalNotifications;
			deleteTree(tempDir);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPopulateUtilImplLeavesConcurrentSessionSettingsWhenMissing() throws Exception {
		boolean originalWarnings = UtilImpl.CONCURRENT_SESSION_WARNINGS;
		boolean originalNotifications = UtilImpl.CONCURRENT_SESSION_NOTIFICATIONS;
		String originalPropertiesFilePath = System.getProperty("PROPERTIES_FILE_PATH");
		Class<? extends AbstractPersistence> originalPersistenceImplementation = AbstractPersistence.IMPLEMENTATION_CLASS;
		Class<? extends DynamicPersistence> originalDynamicPersistenceImplementation = AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS;
		Path tempDir = Files.createTempDirectory("skyve-context-listener-missing");
		try {
			UtilImpl.CONCURRENT_SESSION_WARNINGS = true;
			UtilImpl.CONCURRENT_SESSION_NOTIFICATIONS = false;

			Path configFile = writeConfiguration(tempDir, false);
			System.setProperty("PROPERTIES_FILE_PATH", configFile.toString());

			ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();
			ProvidedRepositoryFactory.set(mock(ProvidedRepository.class));
			try {
				invokePopulateUtilImpl(configFile, tempDir);
			}
			finally {
				ProvidedRepositoryFactory.set(originalRepository);
			}

			assertThat(UtilImpl.CONCURRENT_SESSION_WARNINGS, is(true));
			assertThat(UtilImpl.CONCURRENT_SESSION_NOTIFICATIONS, is(false));
		}
		finally {
			restoreProperty("PROPERTIES_FILE_PATH", originalPropertiesFilePath);
			AbstractPersistence.IMPLEMENTATION_CLASS = originalPersistenceImplementation;
			AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS = originalDynamicPersistenceImplementation;
			UtilImpl.CONCURRENT_SESSION_WARNINGS = originalWarnings;
			UtilImpl.CONCURRENT_SESSION_NOTIFICATIONS = originalNotifications;
			deleteTree(tempDir);
		}
	}

	private static void invokePopulateUtilImpl(Path configFile, Path tempDir) throws Exception {
		ServletContext context = mock(ServletContext.class);
		when(context.getRealPath("/")).thenReturn(tempDir.toString());
		when(context.getInitParameter("PROPERTIES_FILE_PATH")).thenReturn(configFile.toString());

		FilterRegistration securityHeadersFilter = mock(FilterRegistration.class);
		when(securityHeadersFilter.getClassName()).thenReturn(ResponseHeaderFilter.class.getName());
		when(securityHeadersFilter.getName()).thenReturn(ResponseHeaderFilter.SECURITY_HEADERS_FILTER_NAME);

		HashMap<String, FilterRegistration> filterRegistrations = new HashMap<>();
		filterRegistrations.put(ResponseHeaderFilter.SECURITY_HEADERS_FILTER_NAME, securityHeadersFilter);
		doReturn(filterRegistrations).when(context).getFilterRegistrations();

		Method populateUtilImpl = SkyveContextListener.class.getDeclaredMethod("populateUtilImpl", ServletContext.class);
		populateUtilImpl.setAccessible(true);
		populateUtilImpl.invoke(null, context);
	}

	private static Path writeConfiguration(Path tempDir, boolean includeConcurrentSessionSettings) throws Exception {
		StringBuilder security = new StringBuilder()
				.append("\"ipAddressChecks\":true,")
				.append("\"ipAddressHistoryCheckCount\":1,")
				.append("\"securityNotificationsEmail\":\"security@test.com\",")
				.append("\"geoIPBlockNotifications\":true,")
				.append("\"passwordChangeNotifications\":true,")
				.append("\"differentCountryLoginNotifications\":true,")
				.append("\"ipAddressChangeNotifications\":true,")
				.append("\"accessExceptionNotifications\":true,")
				.append("\"securityExceptionNotifications\":true");
		if (includeConcurrentSessionSettings) {
			security.append(",\"concurrentSessionWarnings\":false,\"concurrentSessionNotifications\":false");
		}

		String json = "{"
				+ "\"trace\":{\"xml\":false,\"http\":false,\"query\":false,\"command\":false,\"faces\":false,\"sql\":false,\"content\":false,\"security\":false,\"bizlet\":false,\"dirty\":false},"
				+ "\"content\":{\"directory\":\"" + escapeJson(tempDir.toString()) + "\",\"gcCron\":\"0 7 0/1 1/1 * ? *\",\"fileStorage\":true},"
				+ "\"url\":{\"server\":\"http://localhost:8080\",\"context\":\"/skyve\",\"home\":\"/\"},"
				+ "\"state\":{\"directory\":null,\"multiple\":false,"
				+ "\"conversations\":{\"heapSizeEntries\":10,\"offHeapSizeMB\":0,\"diskSizeGB\":0,\"expiryTimeMinutes\":10},"
				+ "\"csrfTokens\":{\"heapSizeEntries\":10,\"offHeapSizeMB\":0,\"diskSizeGB\":0,\"expiryTimeMinutes\":10},"
				+ "\"geoIPs\":{\"heapSizeEntries\":10,\"offHeapSizeMB\":0,\"diskSizeGB\":0,\"expiryTimeMinutes\":10},"
				+ "\"sessions\":{\"heapSizeEntries\":10,\"offHeapSizeMB\":0,\"diskSizeGB\":0,\"expiryTimeMinutes\":10},"
				+ "\"evictCron\":\"0 37 0 1/1 * ? *\"},"
				+ "\"dataStores\":{\"skyve\":{\"jndi\":\"java:/SkyveDB\",\"dialect\":\"org.skyve.impl.persistence.hibernate.dialect.H2SpatialDialect\",\"oltpConnectionTimeoutInSeconds\":30,\"asyncConnectionTimeoutInSeconds\":300}},"
				+ "\"hibernate\":{\"dataStore\":\"skyve\",\"ddlSync\":true,\"catalog\":null,\"schema\":null,\"prettySql\":false},"
				+ "\"factories\":{\"persistenceClass\":null,\"dynamicPersistenceClass\":null,\"repositoryClass\":null,\"contentManagerClass\":null,\"numberGeneratorClass\":null,\"customisationsClass\":null,\"geoIPServiceClass\":null},"
				+ "\"smtp\":{\"server\":\"localhost\",\"port\":25,\"uid\":null,\"pwd\":null,\"properties\":null,\"headers\":null,\"sender\":\"mailer@skyve.org\",\"testBogusSend\":false,\"testRecipient\":\"test@yourdomain.com\"},"
				+ "\"map\":{\"type\":\"leaflet\",\"layers\":\"[]\"},"
				+ "\"api\":{\"googleMapsV3Key\":null,\"googleRecaptchaSiteKey\":null,\"googleRecaptchaSecretKey\":null,\"cloudflareTurnstileSiteKey\":null,\"cloudflareTurnstileSecretKey\":null,\"geoIPKey\":null,\"geoIPCountryCodes\":null,\"geoIPWhitelist\":null,\"ckEditorConfigFileUrl\":null},"
				+ "\"account\":{\"passwordHashingAlgorithm\":\"argon2\",\"allowUserSelfRegistration\":false,\"accountLockoutThreshold\":3,\"accountLockoutDurationMultipleInSeconds\":10,\"loginUri\":\"/login\",\"rememberMeTokenTimeoutHours\":336},"
				+ "\"environment\":{\"identifier\":\"test\",\"devMode\":true,\"accessControl\":true,\"customer\":\"demo\",\"jobScheduler\":false,\"moduleDirectory\":null,\"supportEmailAddress\":\"support@test.com\",\"showSetup\":true},"
				+ "\"security\":{" + security + "}"
				+ "}";

		Path configFile = tempDir.resolve("skyve-test.json");
		Files.writeString(configFile, json);
		return configFile;
	}

	private static void restoreProperty(String key, String originalValue) {
		if (originalValue == null) {
			System.clearProperty(key);
		}
		else {
			System.setProperty(key, originalValue);
		}
	}

	private static void deleteTree(Path root) throws Exception {
		if (root == null) {
			return;
		}
		Files.walk(root)
				.sorted((a, b) -> b.compareTo(a))
				.forEach(path -> path.toFile().delete());
	}

	private static String escapeJson(String value) {
		return value.replace("\\", "\\\\");
	}
}
