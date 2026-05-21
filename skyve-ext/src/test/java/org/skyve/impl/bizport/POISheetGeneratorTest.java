package org.skyve.impl.bizport;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class POISheetGeneratorTest {

	@Test
	void constructorSetsModuleAndDocument() {
		POISheetGenerator gen = new POISheetGenerator("testModule", "testDocument");
		assertEquals("testModule", gen.getModuleName());
		assertEquals("testDocument", gen.getDocumentName());
	}

	@Test
	void constructorInitialisesEmptyFieldsList() {
		POISheetGenerator gen = new POISheetGenerator("m", "d");
		assertNotNull(gen.getFields());
		assertTrue(gen.getFields().isEmpty());
	}

	@Test
	void setModuleNameUpdatesValue() {
		POISheetGenerator gen = new POISheetGenerator("old", "d");
		gen.setModuleName("new");
		assertEquals("new", gen.getModuleName());
	}

	@Test
	void setDocumentNameUpdatesValue() {
		POISheetGenerator gen = new POISheetGenerator("m", "old");
		gen.setDocumentName("new");
		assertEquals("new", gen.getDocumentName());
	}

	@Test
	void setDownloadNameAppendsXlsxWhenMissing() {
		POISheetGenerator gen = new POISheetGenerator("m", "d");
		gen.setDownloadName("export");
		assertEquals("export.xlsx", gen.getDownloadName());
	}

	@Test
	void setDownloadNamePreservesXlsxExtension() {
		POISheetGenerator gen = new POISheetGenerator("m", "d");
		gen.setDownloadName("report.xlsx");
		assertEquals("report.xlsx", gen.getDownloadName());
	}

	@Test
	void setColumnTitlesStoresValue() {
		POISheetGenerator gen = new POISheetGenerator("m", "d");
		gen.setColumnTitles(Boolean.TRUE);
		assertEquals(Boolean.TRUE, gen.isColumnTitles());
	}

	@Test
	void setColumnTitlesOnlyStoresValue() {
		POISheetGenerator gen = new POISheetGenerator("m", "d");
		gen.setColumnTitlesOnly(Boolean.FALSE);
		assertEquals(Boolean.FALSE, gen.getColumnTitlesOnly());
	}

	@Test
	void setFieldsReplacesFieldsList() {
		POISheetGenerator gen = new POISheetGenerator("m", "d");
		List<DataFileExportField> fields = new ArrayList<>();
		fields.add(new DataFileExportField("Title", "binding"));
		gen.setFields(fields);
		assertEquals(1, gen.getFields().size());
	}

	@Test
	void addFieldObjectAppendsToList() throws Exception {
		POISheetGenerator gen = new POISheetGenerator("m", "d");
		gen.addField(new DataFileExportField("Title", "binding"));
		assertEquals(1, gen.getFields().size());
	}

	@Test
	void addFieldStringAndBindingAppendsToList() throws Exception {
		POISheetGenerator gen = new POISheetGenerator("m", "d");
		gen.addField("Title", "binding");
		assertEquals(1, gen.getFields().size());
	}

	@Test
	void addMultipleFieldsAccumulatesInOrder() throws Exception {
		POISheetGenerator gen = new POISheetGenerator("m", "d");
		gen.addField("Col1", "attr1");
		gen.addField("Col2", "attr2");
		gen.addField("Col3", "attr3");
		List<DataFileExportField> fields = gen.getFields();
		assertEquals(3, fields.size());
		assertEquals("Col1", fields.get(0).getFieldTitle());
		assertEquals("Col2", fields.get(1).getFieldTitle());
		assertEquals("Col3", fields.get(2).getFieldTitle());
	}
}
