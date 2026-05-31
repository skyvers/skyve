package org.skyve.impl.web;

import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.junit.After;
import org.junit.Test;
import org.skyve.impl.mail.MailServiceStaticSingleton;
import org.skyve.impl.mail.NoOpMailService;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.filter.ResponseHeaderFilter;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.persistence.DynamicPersistence;
import org.skyve.util.Mail;
import org.skyve.util.MailService;

import jakarta.servlet.FilterRegistration;
import jakarta.servlet.ServletContext;
import jakarta.servlet.ServletContextEvent;

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

		SkyveContextListener listener = new SkyveContextListener();
		IllegalStateException e = assertThrows(IllegalStateException.class,
				() -> listener.contextInitialized(event));

		assertThat(e.getMessage(),
				is("A Filter <filter-name>SecurityHeadersFilter</filter-name> of <filter-class>org.skyve.impl.web.filter.ResponseHeaderFilter</filter-class> is required in web.xml."));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testCleanupContentDirectoryHandlesEmptyStrings() {
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
	public void testCleanupContentDirectoryAppendsSlash() {
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
	public void testCleanupModuleDirectoryHandlesEmptyStrings() {
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
	public void testCleanupModuleDirectoryAppendsModules() {
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
	public void testCleanupModuleDirectoryWithModulesAppendsSlash() {
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
	@SuppressWarnings("static-method")
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
	@SuppressWarnings("static-method")
	public void testConfigureMailServiceAndSmtpRequiresSmtpForDefaultSMTPService() {
		Map<String, Object> properties = new HashMap<>();
		Map<String, Object> factories = new HashMap<>();

		assertThrows(IllegalStateException.class, () -> SkyveContextListener.configureMailServiceAndSmtp(properties, factories));
	}

	@Test
	@SuppressWarnings("static-method")
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
	@SuppressWarnings("static-method")
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

	@Test
	@SuppressWarnings("static-method")
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
		String savedPropertiesFilePath = System.getProperty("PROPERTIES_FILE_PATH");
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

			assertFalse(UtilImpl.CONCURRENT_SESSION_WARNINGS);
			assertFalse(UtilImpl.CONCURRENT_SESSION_NOTIFICATIONS);
		}
		finally {
			restoreProperty("PROPERTIES_FILE_PATH", savedPropertiesFilePath);
			AbstractPersistence.IMPLEMENTATION_CLASS = originalPersistenceImplementation;
			AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS = originalDynamicPersistenceImplementation;
			UtilImpl.CONCURRENT_SESSION_WARNINGS = originalWarnings;
			UtilImpl.CONCURRENT_SESSION_NOTIFICATIONS = originalNotifications;
			deleteTree(tempDir);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPopulateUtilImplDetectsDevLoginFilter() throws Exception {
		boolean savedDevLoginFilterUsed = UtilImpl.DEV_LOGIN_FILTER_USED;
		String savedPropertiesFilePath = System.getProperty("PROPERTIES_FILE_PATH");
		Class<? extends AbstractPersistence> originalPersistenceImplementation = AbstractPersistence.IMPLEMENTATION_CLASS;
		Class<? extends DynamicPersistence> originalDynamicPersistenceImplementation = AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS;
		Path tempDir = Files.createTempDirectory("skyve-context-listener-devlogin");
		try {
			Path configFile = writeConfiguration(tempDir, false);
			System.setProperty("PROPERTIES_FILE_PATH", configFile.toString());

			ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();
			ProvidedRepositoryFactory.set(mock(ProvidedRepository.class));
			try {
				invokePopulateUtilImplWithDevLoginFilter(configFile, tempDir);
			}
			finally {
				ProvidedRepositoryFactory.set(originalRepository);
			}

			assertTrue(UtilImpl.DEV_LOGIN_FILTER_USED);
		}
		finally {
			restoreProperty("PROPERTIES_FILE_PATH", savedPropertiesFilePath);
			AbstractPersistence.IMPLEMENTATION_CLASS = originalPersistenceImplementation;
			AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS = originalDynamicPersistenceImplementation;
			UtilImpl.DEV_LOGIN_FILTER_USED = savedDevLoginFilterUsed;
			deleteTree(tempDir);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPopulateUtilImplLeavesConcurrentSessionSettingsWhenMissing() throws Exception {
		boolean originalWarnings = UtilImpl.CONCURRENT_SESSION_WARNINGS;
		boolean originalNotifications = UtilImpl.CONCURRENT_SESSION_NOTIFICATIONS;
		String savedPropertiesFilePath = System.getProperty("PROPERTIES_FILE_PATH");
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

			assertTrue(UtilImpl.CONCURRENT_SESSION_WARNINGS);
			assertFalse(UtilImpl.CONCURRENT_SESSION_NOTIFICATIONS);
		}
		finally {
			restoreProperty("PROPERTIES_FILE_PATH", savedPropertiesFilePath);
			AbstractPersistence.IMPLEMENTATION_CLASS = originalPersistenceImplementation;
			AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS = originalDynamicPersistenceImplementation;
			UtilImpl.CONCURRENT_SESSION_WARNINGS = originalWarnings;
			UtilImpl.CONCURRENT_SESSION_NOTIFICATIONS = originalNotifications;
			deleteTree(tempDir);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPopulateUtilImplReadsBackupStanza() throws Exception {
		String originalBackupExternalClass = UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS;
		String savedPropertiesFilePath = System.getProperty("PROPERTIES_FILE_PATH");
		Class<? extends AbstractPersistence> originalPersistenceImplementation = AbstractPersistence.IMPLEMENTATION_CLASS;
		Class<? extends DynamicPersistence> originalDynamicPersistenceImplementation = AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS;
		Path tempDir = Files.createTempDirectory("skyve-context-listener-backup");
		try {
			String extraStanza = "\"backup\":{\"externalBackupClass\":\"com.example.TestBackup\",\"properties\":null}";
			Path configFile = writeConfigurationWithExtra(tempDir, extraStanza);
			System.setProperty("PROPERTIES_FILE_PATH", configFile.toString());
			ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();
			ProvidedRepositoryFactory.set(mock(ProvidedRepository.class));
			try {
				invokePopulateUtilImpl(configFile, tempDir);
			}
			finally {
				ProvidedRepositoryFactory.set(originalRepository);
			}
			assertEquals("com.example.TestBackup", UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS);
		}
		finally {
			restoreProperty("PROPERTIES_FILE_PATH", savedPropertiesFilePath);
			AbstractPersistence.IMPLEMENTATION_CLASS = originalPersistenceImplementation;
			AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS = originalDynamicPersistenceImplementation;
			UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = originalBackupExternalClass;
			deleteTree(tempDir);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPopulateUtilImplReadsBackupRestoreLimits() throws Exception {
		int originalMaxEntries = UtilImpl.BACKUP_RESTORE_MAX_EXTRACT_ENTRIES;
		int originalMaxSizeMB = UtilImpl.BACKUP_RESTORE_MAX_EXTRACT_SIZE_MB;
		String savedPropertiesFilePath = System.getProperty("PROPERTIES_FILE_PATH");
		Class<? extends AbstractPersistence> originalPersistenceImplementation = AbstractPersistence.IMPLEMENTATION_CLASS;
		Class<? extends DynamicPersistence> originalDynamicPersistenceImplementation = AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS;
		Path tempDir = Files.createTempDirectory("skyve-context-listener-backup-limits");
		try {
			String extraStanza = "\"backup\":{\"restoreMaxExtractEntries\":5000,\"restoreMaxExtractSizeMB\":250,\"properties\":null}";
			Path configFile = writeConfigurationWithExtra(tempDir, extraStanza);
			System.setProperty("PROPERTIES_FILE_PATH", configFile.toString());
			ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();
			ProvidedRepositoryFactory.set(mock(ProvidedRepository.class));
			try {
				invokePopulateUtilImpl(configFile, tempDir);
			}
			finally {
				ProvidedRepositoryFactory.set(originalRepository);
			}
			assertEquals(5000, UtilImpl.BACKUP_RESTORE_MAX_EXTRACT_ENTRIES);
			assertEquals(250, UtilImpl.BACKUP_RESTORE_MAX_EXTRACT_SIZE_MB);
		}
		finally {
			restoreProperty("PROPERTIES_FILE_PATH", savedPropertiesFilePath);
			AbstractPersistence.IMPLEMENTATION_CLASS = originalPersistenceImplementation;
			AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS = originalDynamicPersistenceImplementation;
			UtilImpl.BACKUP_RESTORE_MAX_EXTRACT_ENTRIES = originalMaxEntries;
			UtilImpl.BACKUP_RESTORE_MAX_EXTRACT_SIZE_MB = originalMaxSizeMB;
			deleteTree(tempDir);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPopulateUtilImplReadsUploadsStanza() throws Exception {
		int originalFileSizeMb = UtilImpl.UPLOADS_FILE_MAXIMUM_SIZE_IN_MB;
		int originalContentSizeMb = UtilImpl.UPLOADS_CONTENT_MAXIMUM_SIZE_IN_MB;
		int originalImageSizeMb = UtilImpl.UPLOADS_IMAGE_MAXIMUM_SIZE_IN_MB;
		int originalBizportSizeMb = UtilImpl.UPLOADS_BIZPORT_MAXIMUM_SIZE_IN_MB;
		String savedPropertiesFilePath = System.getProperty("PROPERTIES_FILE_PATH");
		Class<? extends AbstractPersistence> originalPersistenceImplementation = AbstractPersistence.IMPLEMENTATION_CLASS;
		Class<? extends DynamicPersistence> originalDynamicPersistenceImplementation = AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS;
		Path tempDir = Files.createTempDirectory("skyve-context-listener-uploads");
		try {
			String extraStanza = "\"uploads\":{\"file\":{\"maximumSizeMB\":75},\"content\":{\"maximumSizeMB\":150},\"image\":{\"maximumSizeMB\":20},\"bizport\":{\"maximumSizeMB\":20}}";
			Path configFile = writeConfigurationWithExtra(tempDir, extraStanza);
			System.setProperty("PROPERTIES_FILE_PATH", configFile.toString());
			ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();
			ProvidedRepositoryFactory.set(mock(ProvidedRepository.class));
			try {
				invokePopulateUtilImpl(configFile, tempDir);
			}
			finally {
				ProvidedRepositoryFactory.set(originalRepository);
			}
			assertEquals(75, UtilImpl.UPLOADS_FILE_MAXIMUM_SIZE_IN_MB);
		}
		finally {
			restoreProperty("PROPERTIES_FILE_PATH", savedPropertiesFilePath);
			AbstractPersistence.IMPLEMENTATION_CLASS = originalPersistenceImplementation;
			AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS = originalDynamicPersistenceImplementation;
			UtilImpl.UPLOADS_FILE_MAXIMUM_SIZE_IN_MB = originalFileSizeMb;
			UtilImpl.UPLOADS_CONTENT_MAXIMUM_SIZE_IN_MB = originalContentSizeMb;
			UtilImpl.UPLOADS_IMAGE_MAXIMUM_SIZE_IN_MB = originalImageSizeMb;
			UtilImpl.UPLOADS_BIZPORT_MAXIMUM_SIZE_IN_MB = originalBizportSizeMb;
			deleteTree(tempDir);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPopulateUtilImplReadsPushStanza() throws Exception {
		int originalKeepAlive = UtilImpl.PUSH_KEEP_ALIVE_TIME_IN_SECONDS;
		String savedPropertiesFilePath = System.getProperty("PROPERTIES_FILE_PATH");
		Class<? extends AbstractPersistence> originalPersistenceImplementation = AbstractPersistence.IMPLEMENTATION_CLASS;
		Class<? extends DynamicPersistence> originalDynamicPersistenceImplementation = AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS;
		Path tempDir = Files.createTempDirectory("skyve-context-listener-push");
		try {
			String extraStanza = "\"push\":{\"keepAliveTimeInSeconds\":45,\"queueSize\":20,\"maxReceiversPerUser\":3,\"maxReceiversTotal\":50,\"staleReceiverTimeoutInSeconds\":90}";
			Path configFile = writeConfigurationWithExtra(tempDir, extraStanza);
			System.setProperty("PROPERTIES_FILE_PATH", configFile.toString());
			ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();
			ProvidedRepositoryFactory.set(mock(ProvidedRepository.class));
			try {
				invokePopulateUtilImpl(configFile, tempDir);
			}
			finally {
				ProvidedRepositoryFactory.set(originalRepository);
			}
			assertEquals(45, UtilImpl.PUSH_KEEP_ALIVE_TIME_IN_SECONDS);
		}
		finally {
			restoreProperty("PROPERTIES_FILE_PATH", savedPropertiesFilePath);
			AbstractPersistence.IMPLEMENTATION_CLASS = originalPersistenceImplementation;
			AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS = originalDynamicPersistenceImplementation;
			UtilImpl.PUSH_KEEP_ALIVE_TIME_IN_SECONDS = originalKeepAlive;
			deleteTree(tempDir);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPopulateUtilImplReadsThumbnailStanza() throws Exception {
		int originalThreads = UtilImpl.THUMBNAIL_CONCURRENT_THREADS;
		String savedPropertiesFilePath = System.getProperty("PROPERTIES_FILE_PATH");
		Class<? extends AbstractPersistence> originalPersistenceImplementation = AbstractPersistence.IMPLEMENTATION_CLASS;
		Class<? extends DynamicPersistence> originalDynamicPersistenceImplementation = AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS;
		Path tempDir = Files.createTempDirectory("skyve-context-listener-thumbnail");
		try {
			String extraStanza = "\"thumbnail\":{\"concurrentThreads\":8,\"subsamplingMinimumTargetSize\":50,\"fileStorage\":false}";
			Path configFile = writeConfigurationWithExtra(tempDir, extraStanza);
			System.setProperty("PROPERTIES_FILE_PATH", configFile.toString());
			ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();
			ProvidedRepositoryFactory.set(mock(ProvidedRepository.class));
			try {
				invokePopulateUtilImpl(configFile, tempDir);
			}
			finally {
				ProvidedRepositoryFactory.set(originalRepository);
			}
			assertEquals(8, UtilImpl.THUMBNAIL_CONCURRENT_THREADS);
		}
		finally {
			restoreProperty("PROPERTIES_FILE_PATH", savedPropertiesFilePath);
			AbstractPersistence.IMPLEMENTATION_CLASS = originalPersistenceImplementation;
			AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS = originalDynamicPersistenceImplementation;
			UtilImpl.THUMBNAIL_CONCURRENT_THREADS = originalThreads;
			deleteTree(tempDir);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPopulateUtilImplReadsHealthStanza() throws Exception {
		boolean originalHealthCheck = UtilImpl.HEALTH_CHECK;
		int originalCacheTime = UtilImpl.HEALTH_CACHE_TIME_IN_SECONDS;
		String savedPropertiesFilePath = System.getProperty("PROPERTIES_FILE_PATH");
		Class<? extends AbstractPersistence> originalPersistenceImplementation = AbstractPersistence.IMPLEMENTATION_CLASS;
		Class<? extends DynamicPersistence> originalDynamicPersistenceImplementation = AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS;
		Path tempDir = Files.createTempDirectory("skyve-context-listener-health");
		try {
			String extraStanza = "\"health\":{\"check\":true,\"cacheTimeInSeconds\":60}";
			Path configFile = writeConfigurationWithExtra(tempDir, extraStanza);
			System.setProperty("PROPERTIES_FILE_PATH", configFile.toString());
			ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();
			ProvidedRepositoryFactory.set(mock(ProvidedRepository.class));
			try {
				invokePopulateUtilImpl(configFile, tempDir);
			}
			finally {
				ProvidedRepositoryFactory.set(originalRepository);
			}
			assertTrue(UtilImpl.HEALTH_CHECK);
			assertEquals(60, UtilImpl.HEALTH_CACHE_TIME_IN_SECONDS);
		}
		finally {
			restoreProperty("PROPERTIES_FILE_PATH", savedPropertiesFilePath);
			AbstractPersistence.IMPLEMENTATION_CLASS = originalPersistenceImplementation;
			AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS = originalDynamicPersistenceImplementation;
			UtilImpl.HEALTH_CHECK = originalHealthCheck;
			UtilImpl.HEALTH_CACHE_TIME_IN_SECONDS = originalCacheTime;
			deleteTree(tempDir);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPopulateUtilImplReadsBootstrapStanza() throws Exception {
		String originalBootstrapCustomer = UtilImpl.BOOTSTRAP_CUSTOMER;
		String originalUser = UtilImpl.BOOTSTRAP_USER;
		String originalEmail = UtilImpl.BOOTSTRAP_EMAIL;
		String originalPassword = UtilImpl.BOOTSTRAP_PASSWORD;
		String savedPropertiesFilePath = System.getProperty("PROPERTIES_FILE_PATH");
		Class<? extends AbstractPersistence> originalPersistenceImplementation = AbstractPersistence.IMPLEMENTATION_CLASS;
		Class<? extends DynamicPersistence> originalDynamicPersistenceImplementation = AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS;
		Path tempDir = Files.createTempDirectory("skyve-context-listener-bootstrap");
		try {
			String extraStanza = "\"bootstrap\":{\"customer\":\"testcustomer\",\"user\":\"testuser\",\"email\":\"boot@test.com\",\"password\":\"pass123\"}";
			Path configFile = writeConfigurationWithExtra(tempDir, extraStanza);
			System.setProperty("PROPERTIES_FILE_PATH", configFile.toString());
			ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();
			ProvidedRepositoryFactory.set(mock(ProvidedRepository.class));
			try {
				invokePopulateUtilImpl(configFile, tempDir);
			}
			finally {
				ProvidedRepositoryFactory.set(originalRepository);
			}
			assertEquals("testcustomer", UtilImpl.BOOTSTRAP_CUSTOMER);
			assertEquals("testuser", UtilImpl.BOOTSTRAP_USER);
			assertEquals("boot@test.com", UtilImpl.BOOTSTRAP_EMAIL);
		}
		finally {
			restoreProperty("PROPERTIES_FILE_PATH", savedPropertiesFilePath);
			AbstractPersistence.IMPLEMENTATION_CLASS = originalPersistenceImplementation;
			AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS = originalDynamicPersistenceImplementation;
			UtilImpl.BOOTSTRAP_CUSTOMER = originalBootstrapCustomer;
			UtilImpl.BOOTSTRAP_USER = originalUser;
			UtilImpl.BOOTSTRAP_EMAIL = originalEmail;
			UtilImpl.BOOTSTRAP_PASSWORD = originalPassword;
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

	private static void invokePopulateUtilImplWithDevLoginFilter(Path configFile, Path tempDir) throws Exception {
		ServletContext context = mock(ServletContext.class);
		when(context.getRealPath("/")).thenReturn(tempDir.toString());
		when(context.getInitParameter("PROPERTIES_FILE_PATH")).thenReturn(configFile.toString());

		FilterRegistration securityHeadersFilter = mock(FilterRegistration.class);
		when(securityHeadersFilter.getClassName()).thenReturn(ResponseHeaderFilter.class.getName());
		when(securityHeadersFilter.getName()).thenReturn(ResponseHeaderFilter.SECURITY_HEADERS_FILTER_NAME);

		FilterRegistration devLoginFilter = mock(FilterRegistration.class);
		when(devLoginFilter.getClassName()).thenReturn(org.skyve.impl.web.filter.DevLoginFilter.class.getName());
		when(devLoginFilter.getName()).thenReturn("DevLoginFilter");

		HashMap<String, FilterRegistration> filterRegistrations = new HashMap<>();
		filterRegistrations.put(ResponseHeaderFilter.SECURITY_HEADERS_FILTER_NAME, securityHeadersFilter);
		filterRegistrations.put("DevLoginFilter", devLoginFilter);
		doReturn(filterRegistrations).when(context).getFilterRegistrations();

		Method populateUtilImpl = SkyveContextListener.class.getDeclaredMethod("populateUtilImpl", ServletContext.class);
		populateUtilImpl.setAccessible(true);
		populateUtilImpl.invoke(null, context);
	}

	private static Path writeConfigurationWithExtra(Path tempDir, String extraStanza) throws Exception {
		// Build from writeConfiguration base JSON, appending the extra stanza
		Path baseConfig = writeConfiguration(tempDir, false);
		String baseJson = Files.readString(baseConfig);
		// Remove trailing } and append extra stanza
		String json = baseJson.substring(0, baseJson.lastIndexOf('}')) + "," + extraStanza + "}";
		Path configFile = tempDir.resolve("skyve-extra.json");
		Files.writeString(configFile, json);
		return configFile;
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
				+ "\"content\":{\"directory\":\"" + escapeJson(tempDir.toString()) + "\",\"gcCron\":\"0 7 0/1 1/1 * ? *\",\"fileStorage\":true,\"fileSuffixes\":false},"
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
		try (var walk = Files.walk(root)) {
			walk.sorted((a, b) -> b.compareTo(a))
					.forEach(path -> path.toFile().delete());
		}
	}

	private static String escapeJson(String value) {
		return value.replace("\\", "\\\\");
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPopulateUtilImplReadsApiStanzaWithValues() throws Exception {
		String originalMapsKey = UtilImpl.GOOGLE_MAPS_V3_API_KEY;
		boolean originalBreachedPwd = UtilImpl.CHECK_FOR_BREACHED_PASSWORD;
		boolean originalGeoIPWhitelist = UtilImpl.GEO_IP_WHITELIST;
		java.util.concurrent.CopyOnWriteArraySet<String> originalGeoIPCountryCodes = UtilImpl.GEO_IP_COUNTRY_CODES;
		String originalCkEditor = UtilImpl.CKEDITOR_CONFIG_FILE_URL;
		String savedPropertiesFilePath = System.getProperty("PROPERTIES_FILE_PATH");
		Class<? extends AbstractPersistence> originalPersistenceImplementation = AbstractPersistence.IMPLEMENTATION_CLASS;
		Class<? extends DynamicPersistence> originalDynamicPersistenceImplementation = AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS;
		Path tempDir = Files.createTempDirectory("skyve-context-listener-api");
		try {
			String oldApiStanza = "\"api\":{\"googleMapsV3Key\":null,\"googleRecaptchaSiteKey\":null,\"googleRecaptchaSecretKey\":null,\"cloudflareTurnstileSiteKey\":null,\"cloudflareTurnstileSecretKey\":null,\"geoIPKey\":null,\"geoIPCountryCodes\":null,\"geoIPWhitelist\":null,\"ckEditorConfigFileUrl\":null}";
			String newApiStanza = "\"api\":{\"googleMapsV3Key\":\"maps-test-key\",\"checkForBreachedPassword\":false,\"googleRecaptchaSiteKey\":null,\"googleRecaptchaSecretKey\":null,\"cloudflareTurnstileSiteKey\":null,\"cloudflareTurnstileSecretKey\":null,\"geoIPKey\":null,\"geoIPCountryCodes\":\"US|CA|GB\",\"geoIPWhitelist\":false,\"ckEditorConfigFileUrl\":\"http://example.com/ckeditor.js\"}";
			Path configFile = writeConfigurationWithReplacedApi(tempDir, oldApiStanza, newApiStanza);
			System.setProperty("PROPERTIES_FILE_PATH", configFile.toString());
			ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();
			ProvidedRepositoryFactory.set(mock(ProvidedRepository.class));
			try {
				invokePopulateUtilImpl(configFile, tempDir);
			}
			finally {
				ProvidedRepositoryFactory.set(originalRepository);
			}
			assertEquals("maps-test-key", UtilImpl.GOOGLE_MAPS_V3_API_KEY);
			assertFalse(UtilImpl.GEO_IP_WHITELIST);
			assertNotNull(UtilImpl.GEO_IP_COUNTRY_CODES);
			assertTrue(UtilImpl.GEO_IP_COUNTRY_CODES.contains("US"));
			assertTrue(UtilImpl.GEO_IP_COUNTRY_CODES.contains("CA"));
			assertTrue(UtilImpl.GEO_IP_COUNTRY_CODES.contains("GB"));
			assertEquals("http://example.com/ckeditor.js", UtilImpl.CKEDITOR_CONFIG_FILE_URL);
		}
		finally {
			restoreProperty("PROPERTIES_FILE_PATH", savedPropertiesFilePath);
			AbstractPersistence.IMPLEMENTATION_CLASS = originalPersistenceImplementation;
			AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS = originalDynamicPersistenceImplementation;
			UtilImpl.GOOGLE_MAPS_V3_API_KEY = originalMapsKey;
			UtilImpl.CHECK_FOR_BREACHED_PASSWORD = originalBreachedPwd;
			UtilImpl.GEO_IP_WHITELIST = originalGeoIPWhitelist;
			UtilImpl.GEO_IP_COUNTRY_CODES = originalGeoIPCountryCodes;
			UtilImpl.CKEDITOR_CONFIG_FILE_URL = originalCkEditor;
			deleteTree(tempDir);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPopulateUtilImplReadsAddinsStanza() throws Exception {
		String originalAddinsDirectory = UtilImpl.ADDINS_DIRECTORY;
		String savedPropertiesFilePath = System.getProperty("PROPERTIES_FILE_PATH");
		Class<? extends AbstractPersistence> originalPersistenceImplementation = AbstractPersistence.IMPLEMENTATION_CLASS;
		Class<? extends DynamicPersistence> originalDynamicPersistenceImplementation = AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS;
		Path tempDir = Files.createTempDirectory("skyve-context-listener-addins");
		try {
			String extraStanza = "\"addins\":{\"directory\":\"" + escapeJson(tempDir.toString()) + "\"}";
			Path configFile = writeConfigurationWithExtra(tempDir, extraStanza);
			System.setProperty("PROPERTIES_FILE_PATH", configFile.toString());
			ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();
			ProvidedRepositoryFactory.set(mock(ProvidedRepository.class));
			try {
				invokePopulateUtilImpl(configFile, tempDir);
			}
			finally {
				ProvidedRepositoryFactory.set(originalRepository);
			}
			assertNotNull(UtilImpl.ADDINS_DIRECTORY);
			assertTrue(UtilImpl.ADDINS_DIRECTORY.startsWith(tempDir.toString().replace("\\", "/")));
		}
		finally {
			restoreProperty("PROPERTIES_FILE_PATH", savedPropertiesFilePath);
			AbstractPersistence.IMPLEMENTATION_CLASS = originalPersistenceImplementation;
			AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS = originalDynamicPersistenceImplementation;
			UtilImpl.ADDINS_DIRECTORY = originalAddinsDirectory;
			deleteTree(tempDir);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void testMergeOverridesCompatibleNestedValues() throws Exception {
		Map<String, Object> originalNested = new HashMap<>();
		originalNested.put("enabled", Boolean.TRUE);
		originalNested.put("max", Integer.valueOf(5));

		Map<String, Object> original = new HashMap<>();
		original.put("name", "default");
		original.put("nested", originalNested);

		Map<String, Object> overrideNested = new HashMap<>();
		overrideNested.put("enabled", Boolean.FALSE);
		overrideNested.put("max", Integer.valueOf(10));

		Map<String, Object> overrides = new HashMap<>();
		overrides.put("name", "override");
		overrides.put("nested", overrideNested);

		invokePrivateStatic("merge", new Class<?>[] {Map.class, Map.class}, overrides, original);

		assertEquals("override", original.get("name"));
		@SuppressWarnings("unchecked")
		Map<String, Object> mergedNested = (Map<String, Object>) original.get("nested");
		assertEquals(Boolean.FALSE, mergedNested.get("enabled"));
		assertEquals(Integer.valueOf(10), mergedNested.get("max"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testMergeRejectsIncompatibleOverrideType() {
		Map<String, Object> original = new HashMap<>();
		original.put("nested", Map.of("value", "ok"));

		Map<String, Object> overrides = new HashMap<>();
		overrides.put("nested", "not-a-map");

		InvocationTargetException thrown = assertThrows(InvocationTargetException.class,
				() -> invokePrivateStatic("merge", new Class<?>[] {Map.class, Map.class}, overrides, original));

		assertThat(thrown.getCause(), instanceOf(IllegalStateException.class));
		assertTrue(thrown.getCause().getMessage().contains("Cannot apply override"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGetHelpersReturnTypedValues() throws Exception {
		Map<String, Object> nested = new HashMap<>();
		nested.put("sub", "value");

		Map<String, Object> properties = new HashMap<>();
		properties.put("flag", Boolean.TRUE);
		properties.put("number", Integer.valueOf(7));
		properties.put("name", "demo");
		properties.put("object", nested);
		properties.put("items", List.of("a", "b"));

		Boolean flag = (Boolean) invokePrivateStatic("getBoolean",
				new Class<?>[] {String.class, String.class, Map.class},
				"security",
				"flag",
				properties);
		Integer number = (Integer) invokePrivateStatic("getInt",
				new Class<?>[] {String.class, String.class, Map.class},
				"limits",
				"number",
				properties);
		String name = (String) invokePrivateStatic("getString",
				new Class<?>[] {String.class, String.class, Map.class, boolean.class},
				"environment",
				"name",
				properties,
				Boolean.TRUE);
		Number optionalMissingNumber = (Number) invokePrivateStatic("getNumber",
				new Class<?>[] {String.class, String.class, Map.class, boolean.class},
				null,
				"missing",
				properties,
				Boolean.FALSE);
		@SuppressWarnings("unchecked")
		Map<String, Object> object = (Map<String, Object>) invokePrivateStatic("getObject",
				new Class<?>[] {String.class, String.class, Map.class, boolean.class},
				"api",
				"object",
				properties,
				Boolean.TRUE);
		@SuppressWarnings("unchecked")
		List<String> items = (List<String>) invokePrivateStatic("getList",
				new Class<?>[] {String.class, String.class, Map.class, boolean.class},
				"api",
				"items",
				properties,
				Boolean.TRUE);

		assertTrue(flag.booleanValue());
		assertEquals(Integer.valueOf(7), number);
		assertEquals("demo", name);
		assertNull(optionalMissingNumber);
		assertEquals("value", object.get("sub"));
		assertEquals(List.of("a", "b"), items);
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGetThrowsWhenRequiredValueMissing() {
		Map<String, Object> properties = new HashMap<>();

		InvocationTargetException withPrefix = assertThrows(InvocationTargetException.class,
				() -> invokePrivateStatic("get",
						new Class<?>[] {String.class, String.class, Map.class, boolean.class},
						"security",
						"missing",
						properties,
						Boolean.TRUE));
		assertThat(withPrefix.getCause(), instanceOf(IllegalStateException.class));
		assertEquals("Property security.missing does not exist in the JSON configuration.", withPrefix.getCause().getMessage());

		InvocationTargetException withoutPrefix = assertThrows(InvocationTargetException.class,
				() -> invokePrivateStatic("get",
						new Class<?>[] {String.class, String.class, Map.class, boolean.class},
						null,
						"missing",
						properties,
						Boolean.TRUE));
		assertThat(withoutPrefix.getCause(), instanceOf(IllegalStateException.class));
		assertEquals("Property missing does not exist in the JSON configuration.", withoutPrefix.getCause().getMessage());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testIsMultiTenantReflectsConfiguredCustomer() throws Exception {
		String originalCustomerValue = UtilImpl.CUSTOMER;
		try {
			UtilImpl.CUSTOMER = null;
			Boolean multiTenant = (Boolean) invokePrivateStatic("isMultiTenant", new Class<?>[0]);
			assertTrue(multiTenant.booleanValue());

			UtilImpl.CUSTOMER = "demo";
			multiTenant = (Boolean) invokePrivateStatic("isMultiTenant", new Class<?>[0]);
			assertFalse(multiTenant.booleanValue());
		}
		finally {
			UtilImpl.CUSTOMER = originalCustomerValue;
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void testTestWritableDirectoryThrowsForMissingDirectory() {
		InvocationTargetException thrown = assertThrows(InvocationTargetException.class,
				() -> invokePrivateStatic("testWritableDirectory",
						new Class<?>[] {String.class, String.class},
						"content.directory",
						"/path/that/does/not/exist/for/skyve/tests"));

		assertThat(thrown.getCause(), instanceOf(IllegalStateException.class));
		assertTrue(thrown.getCause().getMessage().contains("does not exist"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testTestWritableDirectoryThrowsWhenPathIsAFile() throws Exception {
		Path tempFile = Files.createTempFile("skyve-context-listener", ".txt");
		try {
			InvocationTargetException thrown = assertThrows(InvocationTargetException.class,
					() -> invokePrivateStatic("testWritableDirectory",
							new Class<?>[] {String.class, String.class},
							"content.directory",
							tempFile.toString()));

			assertThat(thrown.getCause(), instanceOf(IllegalStateException.class));
			assertTrue(thrown.getCause().getMessage().contains("is not a directory"));
		}
		finally {
			Files.deleteIfExists(tempFile);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void testTestWritableDirectoryAcceptsWritableDirectory() throws Exception {
		Path tempDirectory = Files.createTempDirectory("skyve-context-listener-writable");
		try {
			invokePrivateStatic("testWritableDirectory",
					new Class<?>[] {String.class, String.class},
					"content.directory",
					tempDirectory.toString());
		}
		finally {
			deleteTree(tempDirectory);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void testClearRepositoryFactorySetsRepositoryToNull() throws Exception {
		ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();
		ProvidedRepositoryFactory.set(mock(ProvidedRepository.class));
		try {
			invokePrivateStatic("clearRepositoryFactory", new Class<?>[0]);
			assertNull(ProvidedRepositoryFactory.get());
		}
		finally {
			ProvidedRepositoryFactory.set(originalRepository);
		}
	}

	private static Object invokePrivateStatic(String methodName, Class<?>[] parameterTypes, Object... args) throws Exception {
		Method method = SkyveContextListener.class.getDeclaredMethod(methodName, parameterTypes);
		method.setAccessible(true);
		return method.invoke(null, args);
	}

	private static Path writeConfigurationWithReplacedApi(Path tempDir, String oldApiStanza, String newApiStanza) throws Exception {
		Path baseConfig = writeConfiguration(tempDir, false);
		String baseJson = Files.readString(baseConfig);
		String updatedJson = baseJson.replace(oldApiStanza, newApiStanza);
		Path configFile = tempDir.resolve("skyve-api.json");
		Files.writeString(configFile, updatedJson);
		return configFile;
	}
}
