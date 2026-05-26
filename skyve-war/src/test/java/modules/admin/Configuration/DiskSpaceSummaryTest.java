package modules.admin.Configuration;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
public class DiskSpaceSummaryTest {

	@Test
	void constructorPopulatesTotalSpace() {
		DiskSpaceSummary summary = new DiskSpaceSummary();
		assertTrue(summary.getTotalSpace() > 0);
	}

	@Test
	void getTotalAvailableIsNonNegative() {
		DiskSpaceSummary summary = new DiskSpaceSummary();
		assertTrue(summary.getTotalAvailable() >= 0);
	}

	@Test
	void getTotalAvailableLevelIsBetween0And100() {
		DiskSpaceSummary summary = new DiskSpaceSummary();
		long level = summary.getTotalAvailableLevel();
		assertTrue(level >= 0 && level <= 100);
	}

	@Test
	void getHTMLSummaryContainsSummaryText() {
		DiskSpaceSummary summary = new DiskSpaceSummary();
		String html = summary.getHTMLSummary();
		assertNotNull(html);
		assertTrue(html.contains("Summary"));
		assertTrue(html.contains("Total="));
	}

	@Test
	void getHTMLSummaryIsCached() {
		DiskSpaceSummary summary = new DiskSpaceSummary();
		String html1 = summary.getHTMLSummary();
		String html2 = summary.getHTMLSummary();
		assertTrue(html1 == html2); // same instance, cached
	}
}
