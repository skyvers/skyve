package modules.admin.ReportManager;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

/**
 * Tests for ReportManagerUtil static methods.
 */
@SuppressWarnings("static-method")
class ReportManagerUtilTest {

	@Test
	void getZipNameIsNotNull() {
		String name = ReportManagerUtil.getZipName();
		assertNotNull(name);
	}

	@Test
	void getZipNameStartsWithPrefix() {
		String name = ReportManagerUtil.getZipName();
		assertTrue(name.startsWith(ReportManagerUtil.REPORTS_BATCH_PREFIX),
				"ZIP name should start with the batch prefix");
	}

	@Test
	void getZipNameEndsWithZipExtension() {
		String name = ReportManagerUtil.getZipName();
		assertTrue(name.endsWith(".zip"), "ZIP name should end with .zip");
	}

	@Test
	void getZipNameMatchesExpectedPattern() {
		// Pattern: reportManager_yyyyMMddHHmmss.zip
		String name = ReportManagerUtil.getZipName();
		assertTrue(name.matches("reportManager_\\d{14}\\.zip"),
				"ZIP name should match pattern reportManager_yyyyMMddHHmmss.zip but was: " + name);
	}

	@Test
	void getZipNameReturnsValidValueEachCall() {
		String name1 = ReportManagerUtil.getZipName();
		String name2 = ReportManagerUtil.getZipName();
		// Both values should always be valid ZIP names regardless of call timing.
		assertNotNull(name1);
		assertNotNull(name2);
		assertTrue(name1.matches("reportManager_\\d{14}\\.zip"));
		assertTrue(name2.matches("reportManager_\\d{14}\\.zip"));
	}
}
