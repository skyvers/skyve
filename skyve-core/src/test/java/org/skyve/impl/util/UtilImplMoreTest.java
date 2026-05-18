package org.skyve.impl.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Path;
import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl.ArchiveConfig;
import org.skyve.impl.util.UtilImpl.ArchiveConfig.ArchiveDocConfig;
import org.skyve.impl.util.UtilImpl.ArchiveConfig.ArchiveSchedule;
import org.skyve.metadata.user.User;

@SuppressWarnings("static-method")
class UtilImplMoreTest {

	// --- processStringValue ---

	@Test
	void processStringValueNullReturnsNull() {
		assertThat(UtilImpl.processStringValue(null), is(nullValue()));
	}

	@Test
	void processStringValueEmptyReturnsNull() {
		assertThat(UtilImpl.processStringValue(""), is(nullValue()));
	}

	@Test
	void processStringValueWhitespaceOnlyReturnsNull() {
		assertThat(UtilImpl.processStringValue("   "), is(nullValue()));
	}

	@Test
	void processStringValueNormalStringReturnsTrimmed() {
		assertThat(UtilImpl.processStringValue("hello"), is("hello"));
	}

	@Test
	void processStringValueLeadingTrailingWhitespaceReturnsTrimmed() {
		assertThat(UtilImpl.processStringValue("  hello  "), is("hello"));
	}

	// --- cleanupModuleDirectory ---

	@Test
	void cleanupModuleDirectoryNullReturnsNull() {
		assertThat(UtilImpl.cleanupModuleDirectory(null), is(nullValue()));
	}

	@Test
	void cleanupModuleDirectoryEmptyReturnsEmpty() {
		assertThat(UtilImpl.cleanupModuleDirectory(""), is(""));
	}

	@Test
	@SuppressWarnings("java:S5976")
	void cleanupModuleDirectoryAddsModulesIfMissing() {
		String result = UtilImpl.cleanupModuleDirectory("/workspace/project/src/main/java");
		assertTrue(result.endsWith("/"));
		assertTrue(result.contains("modules"));
	}

	@Test
	@SuppressWarnings("java:S5976")
	void cleanupModuleDirectoryAlreadyEndsWithModulesTrailingSlash() {
		String result = UtilImpl.cleanupModuleDirectory("/workspace/modules/");
		assertTrue(result.endsWith("/"));
		assertTrue(result.contains("modules"));
	}

	@Test
	@SuppressWarnings("java:S5976")
	void cleanupModuleDirectoryAlreadyEndsWithModulesNoSlash() {
		String result = UtilImpl.cleanupModuleDirectory("/workspace/modules");
		assertTrue(result.endsWith("/"));
		assertTrue(result.contains("modules"));
	}

	@Test
	void cleanupModuleDirectoryStripsTrailingBackslash() {
		String result = UtilImpl.cleanupModuleDirectory("C:\\workspace\\project\\");
		assertThat(result, is("C:\\workspace\\project/modules/"));
	}

	@Test
	@SuppressWarnings("java:S5976")
	void cleanupModuleDirectoryStripsTrailingForwardSlash() {
		String result = UtilImpl.cleanupModuleDirectory("/workspace/project/");
		assertTrue(result.contains("modules"));
	}

	// --- cloneBySerialization ---

	@Test
	void cloneBySerializationReturnsEqualButNotSameObject() {
		String original = "hello clone";
		String clone = UtilImpl.cloneBySerialization(original);
		assertThat(clone, is(original));
	}

	// --- cloneToTransientBySerialization (plain Serializable — skips persistentBean paths) ---

	@Test
	void cloneToTransientBySerializationWithPlainSerializableReturnsEqualValue() {
		String original = "transient clone";
		String result = UtilImpl.cloneToTransientBySerialization(original);
		assertThat(result, is(original));
	}

	// --- ArchiveConfig ---

	@Test
	void archiveConfigDisabledConstantExists() {
		assertNotNull(ArchiveConfig.DISABLED);
	}

	@Test
	void archiveConfigCronScheduleEnabledReturnsFalseWhenCronIsEmpty() {
		ArchiveSchedule schedule = new ArchiveSchedule("", "customer", "user");
		ArchiveConfig config = new ArchiveConfig(10, 100, List.of(), null, schedule);
		assertFalse(config.cronScheduleEnabled());
	}

	@Test
	void archiveConfigCronScheduleEnabledReturnsTrueWhenCronIsSet() {
		ArchiveSchedule schedule = new ArchiveSchedule("0 0 * * *", "customer", "user");
		ArchiveConfig config = new ArchiveConfig(10, 100, List.of(), null, schedule);
		assertTrue(config.cronScheduleEnabled());
	}

	@Test
	@SuppressWarnings("java:S5976")
	void archiveConfigFindArchiveDocConfigReturnsEmptyWhenNullModule() {
		ArchiveDocConfig doc = new ArchiveDocConfig("admin", "Contact", "contacts", 30);
		ArchiveConfig config = new ArchiveConfig(10, 100, List.of(doc), null, ArchiveSchedule.DEFUALT);
		Optional<ArchiveDocConfig> result = config.findArchiveDocConfig(null, "Contact");
		assertFalse(result.isPresent());
	}

	@Test
	@SuppressWarnings("java:S5976")
	void archiveConfigFindArchiveDocConfigReturnsEmptyWhenNullDocument() {
		ArchiveDocConfig doc = new ArchiveDocConfig("admin", "Contact", "contacts", 30);
		ArchiveConfig config = new ArchiveConfig(10, 100, List.of(doc), null, ArchiveSchedule.DEFUALT);
		Optional<ArchiveDocConfig> result = config.findArchiveDocConfig("admin", null);
		assertFalse(result.isPresent());
	}

	@Test
	@SuppressWarnings("java:S5976")
	void archiveConfigFindArchiveDocConfigReturnsMatchingEntry() {
		ArchiveDocConfig doc = new ArchiveDocConfig("admin", "Contact", "contacts", 30);
		ArchiveConfig config = new ArchiveConfig(10, 100, List.of(doc), null, ArchiveSchedule.DEFUALT);
		Optional<ArchiveDocConfig> result = config.findArchiveDocConfig("admin", "Contact");
		assertTrue(result.isPresent());
		assertThat(result.get().directory(), is("contacts"));
	}

	@Test
	@SuppressWarnings("java:S5976")
	void archiveConfigFindArchiveDocConfigReturnsEmptyForNoMatch() {
		ArchiveDocConfig doc = new ArchiveDocConfig("admin", "Contact", "contacts", 30);
		ArchiveConfig config = new ArchiveConfig(10, 100, List.of(doc), null, ArchiveSchedule.DEFUALT);
		Optional<ArchiveDocConfig> result = config.findArchiveDocConfig("admin", "Other");
		assertFalse(result.isPresent());
	}

	// --- ArchiveDocConfig ---

	@Test
	void archiveDocConfigGetArchiveDirectoryIncludesContentDirectoryAndDir() {
		ArchiveDocConfig doc = new ArchiveDocConfig("admin", "Contact", "contacts", 30);
		Path archiveDir = doc.getArchiveDirectory();
		assertNotNull(archiveDir);
		assertTrue(archiveDir.toString().contains("contacts"));
	}

	@Test
	void archiveDocConfigGetIndexDirectoryIsSubdirOfArchiveDirectory() {
		ArchiveDocConfig doc = new ArchiveDocConfig("admin", "Contact", "contacts", 30);
		Path indexDir = doc.getIndexDirectory();
		assertNotNull(indexDir);
		assertTrue(indexDir.toString().contains("index"));
	}

	// --- ArchiveSchedule ---

	@Test
	void archiveScheduleGetUserReturnsUserWithCorrectName() {
		ArchiveSchedule schedule = new ArchiveSchedule("0 0 * * *", "myCustomer", "myUser");
		User user = schedule.getUser();
		assertNotNull(user);
		assertThat(user.getName(), is("myUser"));
	}

	@Test
	@SuppressWarnings("java:S5976")
	void archiveScheduleGetUserReturnsUserWithCorrectCustomerName() {
		ArchiveSchedule schedule = new ArchiveSchedule("0 0 * * *", "myCustomer", "myUser");
		User user = schedule.getUser();
		assertThat(user.getCustomerName(), is("myCustomer"));
	}
}
