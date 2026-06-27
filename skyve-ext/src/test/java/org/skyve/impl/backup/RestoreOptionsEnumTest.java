package org.skyve.impl.backup;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.Test;
import org.skyve.impl.backup.RestoreOptions.ContentOption;
import org.skyve.impl.backup.RestoreOptions.IndexingOption;
import org.skyve.impl.backup.RestoreOptions.PreProcess;

@SuppressWarnings("static-method")
public class RestoreOptionsEnumTest {

	// ---- ContentOption ----

	@Test
	public void contentOptionHasThreeValues() {
		assertEquals(3, ContentOption.values().length);
	}

	@Test
	public void contentOptionValueOfClearOrphanedContentIds() {
		assertNotNull(ContentOption.valueOf("clearOrphanedContentIds"));
	}

	@Test
	public void contentOptionValueOfSaveOrphanedContentIds() {
		assertEquals(ContentOption.saveOrphanedContentIds, ContentOption.valueOf("saveOrphanedContentIds"));
	}

	@Test
	public void contentOptionValueOfError() {
		assertEquals(ContentOption.error, ContentOption.valueOf("error"));
	}

	// ---- PreProcess ----

	@Test
	public void preProcessHasEightValues() {
		assertEquals(8, PreProcess.values().length);
	}

	@Test
	public void preProcessValueOfNoProcessing() {
		assertEquals(PreProcess.noProcessing, PreProcess.valueOf("noProcessing"));
	}

	@Test
	public void preProcessValueOfDeleteData() {
		assertEquals(PreProcess.deleteData, PreProcess.valueOf("deleteData"));
	}

	// ---- IndexingOption ----

	@Test
	public void indexingOptionHasFourValues() {
		assertEquals(4, IndexingOption.values().length);
	}

	@Test
	public void indexingOptionValueOfData() {
		assertEquals(IndexingOption.data, IndexingOption.valueOf("data"));
	}

	@Test
	public void indexingOptionValueOfNone() {
		assertEquals(IndexingOption.none, IndexingOption.valueOf("none"));
	}
}
