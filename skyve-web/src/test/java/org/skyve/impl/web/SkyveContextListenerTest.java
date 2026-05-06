package org.skyve.impl.web;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import org.junit.Test;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.filter.ResponseHeaderFilter;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.persistence.DynamicPersistence;

import jakarta.servlet.FilterRegistration;
import jakarta.servlet.ServletContext;

public class SkyveContextListenerTest {
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
			UtilImpl.CONCURRENT_SESSION_WARNINGS = false;
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
