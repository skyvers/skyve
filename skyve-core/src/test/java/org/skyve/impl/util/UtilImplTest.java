package org.skyve.impl.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.InputStream;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl.ArchiveConfig;
import org.skyve.impl.util.UtilImpl.ArchiveConfig.ArchiveDocConfig;
import org.skyve.impl.util.UtilImpl.ArchiveConfig.ArchiveSchedule;

class UtilImplTest {
	
	private ClassLoader classLoader;
	private Map<String, Object> api;
	private Map<String, String> twilio;
	
	@BeforeEach
	void setup() throws Exception {
		classLoader = getClass().getClassLoader();
	}
	
	@Test
	@SuppressWarnings("unchecked")
	void testReadJSONConfigNoComments() throws Exception {
		// call the method under test
		try (InputStream in = classLoader.getResourceAsStream("json/withoutComments.json")) {
			Map<String, Object> json = UtilImpl.readJSONConfig(in);

			// verify the result
			api = (Map<String, Object>) json.get("api");
			twilio = (Map<String, String>) api.get("twilio");

			assertThat(api, is(notNullValue()));
			assertThat(api.get("twilio"), is(notNullValue()));
			assertThat(twilio.get("defaultSendNumber"), is(notNullValue()));
		}
	}
	
	@Test
	@SuppressWarnings("unchecked")
	void testReadJSONConfigWithComments() throws Exception {
		// call the method under test
		try (InputStream in = classLoader.getResourceAsStream("json/withoutComments.json")) {
			Map<String, Object> json = UtilImpl.readJSONConfig(in);

			// verify the result
			api = (Map<String, Object>) json.get("api");
			twilio = (Map<String, String>) api.get("twilio");

			assertThat(api, is(notNullValue()));
			assertThat(api.get("twilio"), is(notNullValue()));
			assertThat(twilio.get("defaultSendNumber"), is(notNullValue()));
		}
	}
	
	@Test
	@SuppressWarnings("unchecked")
	void testReadJSONConfigBlockComments() throws Exception {
		// call the method under test
		try (InputStream in = classLoader.getResourceAsStream("json/blockComments.json")) {
			Map<String, Object> json = UtilImpl.readJSONConfig(in);

			// verify the result
			api = (Map<String, Object>) json.get("api");
			twilio = (Map<String, String>) api.get("twilio");

			assertThat(api, is(notNullValue()));
			assertThat(api.get("twilio"), is(notNullValue()));
			assertThat(twilio.get("defaultSendNumber"), is(notNullValue()));
		}
	}
	
	@Test
	@SuppressWarnings("unchecked")
	void testReadJSONConfigFull() throws Exception {
		try (InputStream in = classLoader.getResourceAsStream("json/skyve.json")) {
			// call the method under test
			Map<String, Object> json = UtilImpl.readJSONConfig(in);

			// verify the result
			Map<String, Object> environment = (Map<String, Object>) json.get("environment");

			assertThat(environment, is(notNullValue()));
			assertThat(environment.get("customer"), is(notNullValue()));
		}
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testConcurrentSessionFlagsDefaultToEnabled() {
		assertThat(UtilImpl.CONCURRENT_SESSION_WARNINGS, is(true));
		assertThat(UtilImpl.CONCURRENT_SESSION_NOTIFICATIONS, is(true));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testUnidecode() {
		Assert.assertEquals("descricao", UtilImpl.unidecode("descrição"));
		Assert.assertEquals("tache", UtilImpl.unidecode("tâche"));
		Assert.assertEquals("opcoes", UtilImpl.unidecode("opções"));
		Assert.assertEquals("endereco", UtilImpl.unidecode("endereço"));
		Assert.assertEquals("LocalDeInstalacao", UtilImpl.unidecode("LocalDeInstalação"));
		Assert.assertEquals("kayttaja", UtilImpl.unidecode("käyttäjä"));
		Assert.assertEquals("flagObbligatorieta", UtilImpl.unidecode("flagObbligatorietà"));
		Assert.assertEquals("namenserganzung", UtilImpl.unidecode("namensergänzung"));
		Assert.assertEquals("prenom", UtilImpl.unidecode("prénom"));
		Assert.assertEquals("data", UtilImpl.unidecode("дата"));
		Assert.assertEquals("prioritat", UtilImpl.unidecode("priorität"));
		Assert.assertEquals("Hebergement", UtilImpl.unidecode("Hébergement"));
		Assert.assertEquals("filDactualite", UtilImpl.unidecode("filDactualité"));
		Assert.assertEquals("capacite", UtilImpl.unidecode("capacité"));
		Assert.assertEquals("endereco", UtilImpl.unidecode("endereço"));
		Assert.assertEquals("escritorio", UtilImpl.unidecode("escritório"));
		Assert.assertEquals("noRecu", UtilImpl.unidecode("noReçu"));
		Assert.assertEquals("yeuCauTuyenDung", UtilImpl.unidecode("yêuCầuTuyểnDụng"));
		Assert.assertEquals("heureDarrivee", UtilImpl.unidecode("heureDarrivée"));
	}

	// ---- processStringValue ------------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void testProcessStringValueNull() {
		assertNull(UtilImpl.processStringValue(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void testProcessStringValueEmptyReturnsNull() {
		assertNull(UtilImpl.processStringValue(""));
	}

	@Test
	@SuppressWarnings("static-method")
	void testProcessStringValueWhitespaceOnlyReturnsNull() {
		assertNull(UtilImpl.processStringValue("   "));
	}

	@Test
	@SuppressWarnings("static-method")
	void testProcessStringValueTrimsLeadingAndTrailingWhitespace() {
		assertEquals("hello", UtilImpl.processStringValue("  hello  "));
	}

	@Test
	@SuppressWarnings("static-method")
	void testProcessStringValueAlreadyTrimmedReturnsUnchanged() {
		assertEquals("hello", UtilImpl.processStringValue("hello"));
	}

	// ---- cleanupModuleDirectory --------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void testCleanupModuleDirectoryNull() {
		assertNull(UtilImpl.cleanupModuleDirectory(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCleanupModuleDirectoryEmpty() {
		assertEquals("", UtilImpl.cleanupModuleDirectory(""));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCleanupModuleDirectoryAlreadyEndsWithModulesSlash() {
		String path = "/apps/modules/";
		assertEquals(path, UtilImpl.cleanupModuleDirectory(path));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCleanupModuleDirectoryAddsModulesWhenMissing() {
		String result = UtilImpl.cleanupModuleDirectory("/apps");
		assertTrue(result.contains("modules"), "Should add /modules");
		assertTrue(result.endsWith("/"), "Should end with /");
	}

	@Test
	@SuppressWarnings("static-method")
	void testCleanupModuleDirectoryStripsTrailingSlashThenAddsModules() {
		// path with trailing slash but no modules
		String result = UtilImpl.cleanupModuleDirectory("/apps/");
		assertTrue(result.contains("modules"), "Should add /modules");
	}

	// ---- ArchiveConfig.cronScheduleEnabled ---------------------------------

	@Test
	@SuppressWarnings("static-method")
	void testArchiveConfigCronScheduleEnabledWithNonBlankCron() {
		ArchiveSchedule schedule = new ArchiveSchedule("0 * * * * ?", "customer1", "admin");
		ArchiveConfig config = new ArchiveConfig(10, 100, List.of(), null, schedule);
		assertTrue(config.cronScheduleEnabled());
	}

	@Test
	@SuppressWarnings("static-method")
	void testArchiveConfigCronScheduleEnabledWithBlankCronReturnsFalse() {
		ArchiveSchedule schedule = new ArchiveSchedule("", "customer1", "admin");
		ArchiveConfig config = new ArchiveConfig(10, 100, List.of(), null, schedule);
		assertFalse(config.cronScheduleEnabled());
	}

	@Test
	@SuppressWarnings("static-method")
	void testArchiveConfigDisabledCronReturnsFalse() {
		assertFalse(ArchiveConfig.DISABLED.cronScheduleEnabled());
	}

	// ---- ArchiveConfig.findArchiveDocConfig --------------------------------

	@Test
	@SuppressWarnings("static-method")
	void testFindArchiveDocConfigFound() {
		ArchiveDocConfig docConfig = new ArchiveDocConfig("admin", "User", "archive", 30);
		ArchiveConfig config = new ArchiveConfig(10, 100, List.of(docConfig), null, ArchiveSchedule.DEFUALT);
		Optional<ArchiveDocConfig> result = config.findArchiveDocConfig("admin", "User");
		assertTrue(result.isPresent());
	}

	@Test
	@SuppressWarnings("static-method")
	void testFindArchiveDocConfigNotFound() {
		ArchiveConfig config = new ArchiveConfig(10, 100, List.of(), null, ArchiveSchedule.DEFUALT);
		Optional<ArchiveDocConfig> result = config.findArchiveDocConfig("admin", "NonExistent");
		assertFalse(result.isPresent());
	}

	@Test
	@SuppressWarnings("static-method")
	void testFindArchiveDocConfigNullModuleReturnsEmpty() {
		ArchiveDocConfig docConfig = new ArchiveDocConfig("admin", "User", "archive", 30);
		ArchiveConfig config = new ArchiveConfig(10, 100, List.of(docConfig), null, ArchiveSchedule.DEFUALT);
		Optional<ArchiveDocConfig> result = config.findArchiveDocConfig(null, "User");
		assertFalse(result.isPresent());
	}

	@Test
	@SuppressWarnings("static-method")
	void testFindArchiveDocConfigNullDocumentReturnsEmpty() {
		ArchiveDocConfig docConfig = new ArchiveDocConfig("admin", "User", "archive", 30);
		ArchiveConfig config = new ArchiveConfig(10, 100, List.of(docConfig), null, ArchiveSchedule.DEFUALT);
		Optional<ArchiveDocConfig> result = config.findArchiveDocConfig("admin", null);
		assertFalse(result.isPresent());
	}

	// ---- ArchiveDocConfig --------------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void testArchiveDocConfigGetArchiveDirectory() {
		ArchiveDocConfig docConfig = new ArchiveDocConfig("admin", "User", "userArchive", 30);
		Path archiveDir = docConfig.getArchiveDirectory();
		assertNotNull(archiveDir);
		assertTrue(archiveDir.toString().contains("archive"), "Path should contain archive dir");
		assertTrue(archiveDir.toString().contains("userArchive"), "Path should contain the doc directory");
	}

	@Test
	@SuppressWarnings("static-method")
	void testArchiveDocConfigGetIndexDirectory() {
		ArchiveDocConfig docConfig = new ArchiveDocConfig("admin", "User", "userArchive", 30);
		Path indexDir = docConfig.getIndexDirectory();
		assertNotNull(indexDir);
		assertTrue(indexDir.toString().contains("index"), "Path should contain index dir");
	}

	// ---- ArchiveSchedule ---------------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void testArchiveScheduleGetUserReturnsNonNullUser() {
		ArchiveSchedule schedule = new ArchiveSchedule("0 * * * * ?", "testCustomer", "testUser");
		assertNotNull(schedule.getUser());
	}

	@Test
	@SuppressWarnings("static-method")
	void testArchiveScheduleGetUserHasExpectedCustomerName() {
		ArchiveSchedule schedule = new ArchiveSchedule("0 * * * * ?", "testCustomer", "testUser");
		assertEquals("testCustomer", schedule.getUser().getCustomerName());
	}

	@Test
	@SuppressWarnings("static-method")
	void testArchiveScheduleDefaultIsBlankCron() {
		assertEquals("", ArchiveSchedule.DEFUALT.cron());
	}

	// ---- ArchiveConfig.DISABLED constant -----------------------------------

	@Test
	@SuppressWarnings("static-method")
	void testArchiveConfigDisabledHasNegativeExportRuntimeSec() {
		assertEquals(-1, ArchiveConfig.DISABLED.exportRuntimeSec());
	}

	@Test
	@SuppressWarnings("static-method")
	void testArchiveConfigFindArchiveDocConfigNullDocConfigsReturnsEmpty() {
		// null docConfigs list — construct directly with null list
		ArchiveConfig config = new ArchiveConfig(10, 100, null, null, ArchiveSchedule.DEFUALT);
		Optional<ArchiveDocConfig> result = config.findArchiveDocConfig("admin", "User");
		assertFalse(result.isPresent());
	}

        // ---- deproxy --------------------------------------------------------

        @Test
        @SuppressWarnings("static-method")
        void testDeproxyWithNonProxyReturnsOriginalObject() {
                String original = "hello";
                String result = UtilImpl.deproxy(original);
                assertThat(result, is(original));
        }

        @Test
        @SuppressWarnings("static-method")
        void testDeproxyWithNullReturnsNull() {
                Object result = UtilImpl.deproxy(null);
                assertNull(result);
        }

        // ---- cloneBySerialization -------------------------------------------

        @Test
        @SuppressWarnings("static-method")
        void testCloneBySerializationReturnsEqualButDistinctObject() {
                java.util.ArrayList<String> original = new java.util.ArrayList<>();
                original.add("alpha");
                original.add("beta");

                java.util.ArrayList<String> clone = UtilImpl.cloneBySerialization(original);

                assertNotNull(clone);
                assertEquals(original, clone);
                // Verify it's a distinct object
                assertFalse(original == clone);
        }

	@Test
	@SuppressWarnings("static-method")
	void testClearResetsConfiguration() {
		UtilImpl.CONFIGURATION = new java.util.HashMap<>();
		UtilImpl.XML_TRACE = true;
		UtilImpl.SQL_TRACE = true;
		UtilImpl.ARCHIVE_NAME = "testArchive";

		UtilImpl.clear();

		assertNull(UtilImpl.CONFIGURATION);
		assertFalse(UtilImpl.XML_TRACE);
		assertFalse(UtilImpl.SQL_TRACE);
		assertNull(UtilImpl.ARCHIVE_NAME);
	}

	@Test
	@SuppressWarnings("static-method")
	void testCloneToTransientBySerializationWithSimpleString() {
		// String is Serializable, not a List or AbstractPersistentBean — should return a copy
		String original = "hello";
		String result = UtilImpl.cloneToTransientBySerialization(original);
		assertEquals(original, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void testCloneToTransientBySerializationWithEmptyList() {
		// Empty list: no populateFully calls, clone returns empty list
		java.util.ArrayList<String> original = new java.util.ArrayList<>();
		java.util.ArrayList<String> result = UtilImpl.cloneToTransientBySerialization(original);
		assertNotNull(result);
		assertTrue(result.isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetTransientWithNonBeanObject() {
		// Non-List, non-AbstractPersistentBean: should be a no-op without throwing
		UtilImpl.setTransient("not-a-bean");
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetDataGroupWithNonBeanObject() {
		// Non-List, non-AbstractPersistentBean: should be a no-op without throwing
		UtilImpl.setDataGroup("not-a-bean", "someGroup");
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetTransientWithEmptyList() {
		// Empty list: no-op loop, should complete without throwing
		UtilImpl.setTransient(new java.util.ArrayList<>());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetDataGroupWithEmptyList() {
		// Empty list: no-op loop, should complete without throwing
		UtilImpl.setDataGroup(new java.util.ArrayList<>(), "group1");
	}

	@Test
	@SuppressWarnings("static-method")
	void testCloneToTransientBySerializationWithPlainSerializable() {
		// A plain Serializable (not Bean, not List<Bean>) should be cloned without calling populateFully
		String original = "hello-world";
		String result = UtilImpl.cloneToTransientBySerialization(original);
		assertEquals(original, result);
	}
}
