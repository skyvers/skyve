package org.skyve.impl.backup;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.impl.backup.RestoreOptions.ContentOption;
import org.skyve.impl.backup.RestoreOptions.IndexingOption;
import org.skyve.impl.backup.RestoreOptions.PreProcess;

@SuppressWarnings("static-method")
class RestoreOptionsEnumTest {

	// ---- ContentOption ----

	@Test
	void contentOptionHasThreeValues() {
		assertEquals(3, ContentOption.values().length);
	}

	@Test
	void contentOptionValueOfClearOrphanedContentIds() {
		assertNotNull(ContentOption.valueOf("clearOrphanedContentIds"));
	}

	@Test
	void contentOptionValueOfSaveOrphanedContentIds() {
		assertEquals(ContentOption.saveOrphanedContentIds, ContentOption.valueOf("saveOrphanedContentIds"));
	}

	@Test
	void contentOptionValueOfError() {
		assertEquals(ContentOption.error, ContentOption.valueOf("error"));
	}

	// ---- PreProcess ----

	@Test
	void preProcessHasEightValues() {
		assertEquals(8, PreProcess.values().length);
	}

	@Test
	void preProcessValueOfNoProcessing() {
		assertEquals(PreProcess.noProcessing, PreProcess.valueOf("noProcessing"));
	}

	@Test
	void preProcessValueOfDeleteData() {
		assertEquals(PreProcess.deleteData, PreProcess.valueOf("deleteData"));
	}

	// ---- IndexingOption ----

	@Test
	void indexingOptionHasFourValues() {
		assertEquals(4, IndexingOption.values().length);
	}

	@Test
	void indexingOptionValueOfData() {
		assertEquals(IndexingOption.data, IndexingOption.valueOf("data"));
	}

	@Test
	void indexingOptionValueOfNone() {
		assertEquals(IndexingOption.none, IndexingOption.valueOf("none"));
	}
}
