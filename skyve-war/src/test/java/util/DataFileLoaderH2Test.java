package util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;

import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.messages.UploadException;
import org.skyve.impl.bizport.AbstractDataFileLoader.LoaderActivityType;
import org.skyve.impl.bizport.CSVLoader;
import org.skyve.impl.bizport.DataFileField;
import org.skyve.impl.bizport.DataFileField.LoadAction;
import org.skyve.impl.bizport.POISheetLoader;

import modules.test.domain.AllAttributesPersistent;

/**
 * H2-backed tests for AbstractDataFileLoader, CSVLoader, and POISheetLoader.
 * Uses AllAttributesPersistent (module=test) as the target document.
 */
@SuppressWarnings({"static-method", "resource"})
class DataFileLoaderH2Test extends AbstractH2Test {

	private static final String MODULE = AllAttributesPersistent.MODULE_NAME;
	private static final String DOCUMENT = AllAttributesPersistent.DOCUMENT_NAME;

	// ---- Helper methods ----

	/** Build a CSV input stream with a header row and given data rows. */
	private static InputStream csvStream(String... lines) {
		StringBuilder sb = new StringBuilder();
		for (String line : lines) {
			sb.append(line).append('\n');
		}
		return new ByteArrayInputStream(sb.toString().getBytes(StandardCharsets.UTF_8));
	}

	/**
	 * Build an XLSX workbook as an InputStream with data beginning at row 0.
	 * Each element of rows is a String array of cell values; numeric strings are
	 * stored as numeric cells, others as string cells.
	 */
	private static InputStream xlsxStream(String[][] rows) throws Exception {
		try (XSSFWorkbook wb = new XSSFWorkbook()) {
			org.apache.poi.ss.usermodel.Sheet sheet = wb.createSheet("Data");
			for (int r = 0; r < rows.length; r++) {
				org.apache.poi.ss.usermodel.Row row = sheet.createRow(r);
				String[] cells = rows[r];
				for (int c = 0; c < cells.length; c++) {
					if (cells[c] == null) {
						// empty cell – leave unset
						continue;
					}
					try {
						double d = Double.parseDouble(cells[c]);
						row.createCell(c).setCellValue(d);
					} catch (@SuppressWarnings("unused") NumberFormatException e) {
						row.createCell(c).setCellValue(cells[c]);
					}
				}
			}
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			wb.write(baos);
			return new ByteArrayInputStream(baos.toByteArray());
		}
	}

	// ---- CSVLoader tests ----

	@Test
	void csvLoaderLoadsSingleTextBean() {
		// CSV: header row + 1 data row with the "text" column
		InputStream is = csvStream("text", "hello");
		UploadException exception = new UploadException();
		CSVLoader loader = new CSVLoader(LoaderActivityType.CREATE_ALL, is, exception, MODULE, DOCUMENT);
		loader.addField(new DataFileField(AllAttributesPersistent.textPropertyName, 0));

		List<AllAttributesPersistent> beans = loader.beanResults();

		assertFalse(exception.hasErrors(), "no CSV load errors expected"); // NOSONAR
		assertEquals(1, beans.size(), "one bean from one data row");
		assertEquals("hello", beans.get(0).getText());
	}

	@Test
	void csvLoaderLoadsMultipleColumnsAndRows() {
		InputStream is = csvStream(
				"text,normalInteger",
				"alpha,1",
				"beta,2");
		UploadException exception = new UploadException();
		CSVLoader loader = new CSVLoader(LoaderActivityType.CREATE_ALL, is, exception, MODULE, DOCUMENT);
		loader.addField(new DataFileField(AllAttributesPersistent.textPropertyName, 0));
		loader.addField(new DataFileField(AllAttributesPersistent.normalIntegerPropertyName, 1));

		List<AllAttributesPersistent> beans = loader.beanResults();

		assertEquals(2, beans.size(), "two data rows");
		assertEquals("alpha", beans.get(0).getText());
		assertEquals(Integer.valueOf(1), beans.get(0).getNormalInteger());
		assertEquals("beta", beans.get(1).getText());
		assertEquals(Integer.valueOf(2), beans.get(1).getNormalInteger());
	}

	@Test
	void csvLoaderEmptyDataProducesWarning() {
		// Only a header row, no data rows → beanResults adds a "no data loaded" warning
		InputStream is = csvStream("text");
		UploadException exception = new UploadException();
		CSVLoader loader = new CSVLoader(LoaderActivityType.CREATE_ALL, is, exception, MODULE, DOCUMENT);
		loader.addField(new DataFileField(AllAttributesPersistent.textPropertyName, 0));

		List<AllAttributesPersistent> beans = loader.beanResults();

		assertTrue(beans.isEmpty(), "no beans from empty CSV");
		assertTrue(exception.hasProblems(), "empty-data warning expected");
	}

	@Test
	void csvLoaderGetWhereIncludesLineAndColumn() {
		InputStream is = csvStream("text", "val");
		UploadException exception = new UploadException();
		CSVLoader loader = new CSVLoader(LoaderActivityType.CREATE_ALL, is, exception, MODULE, DOCUMENT);
		loader.addField(new DataFileField(AllAttributesPersistent.textPropertyName, 0));

		// Advance to the first data row so getWhere is meaningful
		loader.beanResults(); // populates dataIndex
		String where = loader.getWhere(0);
		assertNotNull(where);
		assertTrue(where.contains("text"), "getWhere should reference the header name");
	}

	@Test
	void csvLoaderDebugModeProducesDebugData() {
		InputStream is = csvStream("text", "debugval");
		UploadException exception = new UploadException();
		CSVLoader loader = new CSVLoader(LoaderActivityType.CREATE_ALL, is, exception, MODULE, DOCUMENT);
		loader.setDebugMode(true);
		loader.addField(new DataFileField(AllAttributesPersistent.textPropertyName, 0));

		// Advance once so valueMap is populated and debugData() is non-trivial
		loader.beanResults();
		// debugData() is called internally during beanResult() when debugMode=true
		// Just verify the loader ran without exception
		assertFalse(exception.hasErrors(), "debug mode should not cause errors");
	}

	@Test
	void csvLoaderGetValueMapIsAccessible() {
		InputStream is = csvStream("text", "mapval");
		UploadException exception = new UploadException();
		CSVLoader loader = new CSVLoader(LoaderActivityType.CREATE_ALL, is, exception, MODULE, DOCUMENT);
		loader.addField(new DataFileField(AllAttributesPersistent.textPropertyName, 0));

		loader.beanResults();

		// After exhausting the stream, valueMap is null (isNoData returns true on last read)
		// The loader has processed one data row; this exercises getValueMap()
		assertNull(loader.getValueMap(), "valueMap should be null after stream is exhausted");
	}

	@Test
	void csvLoaderActivityTypeFindQueriesExistingBean() {
		// Save a bean first so the FIND query can locate it
		AllAttributesPersistent saved = AllAttributesPersistent.newInstance();
		saved.setText("findme");
		CORE.getPersistence().save(saved);
		CORE.getPersistence().flush();

		InputStream is = csvStream("text", "findme");
		UploadException exception = new UploadException();
		CSVLoader loader = new CSVLoader(LoaderActivityType.FIND, is, exception, MODULE, DOCUMENT);
		DataFileField f = new DataFileField(AllAttributesPersistent.textPropertyName, 0);
		f.setLoadAction(LoadAction.LOOKUP_EQUALS);
		loader.addField(f);

		List<AllAttributesPersistent> beans = loader.beanResults();

		// Should find the saved bean
		assertEquals(1, beans.size(), "FIND should locate one existing bean");
		assertEquals("findme", beans.get(0).getText());
	}

	// ---- POISheetLoader tests ----

	@Test
	void poiSheetLoaderLoadsSingleTextBean() throws Exception {
		// Row 0 = first data row (no header), row 1 is absent → loop ends
		InputStream is = xlsxStream(new String[][] {
				{ "hello world" }
		});
		UploadException exception = new UploadException();
		POISheetLoader loader = new POISheetLoader(is, 0, MODULE, DOCUMENT, exception);
		loader.setActivityType(LoaderActivityType.CREATE_ALL);
		loader.addField(new DataFileField(AllAttributesPersistent.textPropertyName, 0));

		List<AllAttributesPersistent> beans = loader.beanResults();

		assertFalse(exception.hasErrors(), "no POI load errors expected");
		assertEquals(1, beans.size(), "one bean from row 0");
		assertEquals("hello world", beans.get(0).getText());
	}

	@Test
	void poiSheetLoaderLoadsMultipleColumnsAndRows() throws Exception {
		InputStream is = xlsxStream(new String[][] {
				{ "alpha", "10" },
				{ "beta", "20" }
		});
		UploadException exception = new UploadException();
		POISheetLoader loader = new POISheetLoader(is, 0, MODULE, DOCUMENT, exception);
		loader.setActivityType(LoaderActivityType.CREATE_ALL);
		loader.addField(new DataFileField(AllAttributesPersistent.textPropertyName, 0));
		loader.addField(new DataFileField(AllAttributesPersistent.normalIntegerPropertyName, 1));

		List<AllAttributesPersistent> beans = loader.beanResults();

		assertEquals(2, beans.size());
		assertEquals("alpha", beans.get(0).getText());
		assertEquals(Integer.valueOf(10), beans.get(0).getNormalInteger());
		assertEquals("beta", beans.get(1).getText());
		assertEquals(Integer.valueOf(20), beans.get(1).getNormalInteger());
	}

	@Test
	void poiSheetLoaderSkipsHeaderRowWhenDataIndexIsOne() throws Exception {
		// Row 0 = header strings, Row 1 = actual data
		InputStream is = xlsxStream(new String[][] {
				{ "text", "normalInteger" },  // header row
				{ "mydata", "99" }             // data row
		});
		UploadException exception = new UploadException();
		POISheetLoader loader = new POISheetLoader(is, 0, MODULE, DOCUMENT, exception);
		loader.setActivityType(LoaderActivityType.CREATE_ALL);
		loader.setDataIndex(1); // skip row 0 (header)
		loader.addField(new DataFileField(AllAttributesPersistent.textPropertyName, 0));
		loader.addField(new DataFileField(AllAttributesPersistent.normalIntegerPropertyName, 1));

		List<AllAttributesPersistent> beans = loader.beanResults();

		assertEquals(1, beans.size());
		assertEquals("mydata", beans.get(0).getText());
		assertEquals(Integer.valueOf(99), beans.get(0).getNormalInteger());
	}

	@Test
	void poiSheetLoaderEmptySheetProducesWarning() throws Exception {
		// No rows in the sheet → isNoData returns true immediately
		InputStream is = xlsxStream(new String[0][]);
		UploadException exception = new UploadException();
		POISheetLoader loader = new POISheetLoader(is, 0, MODULE, DOCUMENT, exception);
		loader.setActivityType(LoaderActivityType.CREATE_ALL);
		loader.addField(new DataFileField(AllAttributesPersistent.textPropertyName, 0));

		List<AllAttributesPersistent> beans = loader.beanResults();

		assertTrue(beans.isEmpty(), "no beans from empty sheet");
		assertTrue(exception.hasProblems(), "empty-data warning expected");
	}

	@Test
	void poiSheetLoaderGetWhereIncludesSheetAndRow() throws Exception {
		InputStream is = xlsxStream(new String[][] {
				{ "testval" }
		});
		UploadException exception = new UploadException();
		POISheetLoader loader = new POISheetLoader(is, 0, MODULE, DOCUMENT, exception);
		loader.setActivityType(LoaderActivityType.CREATE_ALL);
		loader.addField(new DataFileField(AllAttributesPersistent.textPropertyName, 0));

		loader.beanResults();

		String where = loader.getWhere(0);
		assertNotNull(where);
		assertTrue(where.contains("Sheet"), "getWhere should reference the sheet name");
		assertTrue(where.contains("Row"), "getWhere should reference the row");
	}

	@Test
	void poiSheetLoaderDebugMode() throws Exception {
		InputStream is = xlsxStream(new String[][] {
				{ "debugval" }
		});
		UploadException exception = new UploadException();
		POISheetLoader loader = new POISheetLoader(is, 0, MODULE, DOCUMENT, exception);
		loader.setActivityType(LoaderActivityType.CREATE_ALL);
		loader.setDebugMode(true);
		loader.addField(new DataFileField(AllAttributesPersistent.textPropertyName, 0));

		List<AllAttributesPersistent> beans = loader.beanResults();

		assertFalse(exception.hasErrors());
		assertEquals(1, beans.size());
	}

	@Test
	void poiSheetLoaderAlternateActivityTypeConstructor() throws Exception {
		InputStream is = xlsxStream(new String[][] {
				{ "altval", "7" }
		});
		UploadException exception = new UploadException();
		// Use the alternate constructor that takes LoaderActivityType
		POISheetLoader loader = new POISheetLoader(LoaderActivityType.CREATE_ALL, is, 0, exception, MODULE, DOCUMENT);
		loader.addField(new DataFileField(AllAttributesPersistent.textPropertyName, 0));
		loader.addField(new DataFileField(AllAttributesPersistent.normalIntegerPropertyName, 1));

		List<AllAttributesPersistent> beans = loader.beanResults();

		assertEquals(1, beans.size());
		assertEquals("altval", beans.get(0).getText());
	}

	// ---- AbstractDataFileLoader shared behaviour ----

	@Test
	void addFieldsAddsMultipleFieldsInOrder() {
		InputStream is = csvStream("text", "value");
		UploadException exception = new UploadException();
		CSVLoader loader = new CSVLoader(LoaderActivityType.CREATE_ALL, is, exception, MODULE, DOCUMENT);
		loader.addField(new DataFileField(AllAttributesPersistent.textPropertyName, 0));
		loader.addField(new DataFileField(AllAttributesPersistent.normalIntegerPropertyName, 1));

		List<DataFileField> fields = loader.getFields();
		assertEquals(2, fields.size());
		assertEquals(AllAttributesPersistent.textPropertyName, fields.get(0).getBinding());
		assertEquals(AllAttributesPersistent.normalIntegerPropertyName, fields.get(1).getBinding());
	}

	@Test
	void setFieldOffsetShiftsAllFieldIndices() {
		InputStream is = csvStream("_skip,text", "_,shifted");
		UploadException exception = new UploadException();
		CSVLoader loader = new CSVLoader(LoaderActivityType.CREATE_ALL, is, exception, MODULE, DOCUMENT);
		loader.addField(new DataFileField(AllAttributesPersistent.textPropertyName, 0));
		loader.setFieldOffset(1); // shift index 0 → 1

		List<DataFileField> fields = loader.getFields();
		assertEquals(Integer.valueOf(1), fields.get(0).getIndex());
	}

	@Test
	void setDocumentContextChangesTargetDocument() {
		InputStream is = csvStream("text", "contextval");
		UploadException exception = new UploadException();
		CSVLoader loader = new CSVLoader(LoaderActivityType.CREATE_ALL, is, exception, MODULE, DOCUMENT);
		// Re-set same context — should not throw
		loader.setDocumentContext(MODULE, DOCUMENT);
		loader.addField(new DataFileField(AllAttributesPersistent.textPropertyName, 0));

		List<AllAttributesPersistent> beans = loader.beanResults();
		assertEquals(1, beans.size());
	}

	@Test
	void noFieldsProducesErrorInException() {
		InputStream is = csvStream("text", "val");
		UploadException exception = new UploadException();
		CSVLoader loader = new CSVLoader(LoaderActivityType.CREATE_ALL, is, exception, MODULE, DOCUMENT);
		// Do NOT add any fields

		loader.beanResults();

		assertTrue(exception.hasErrors(), "no-fields error expected");
	}

	@Test
	void csvLoaderCreateFindActivityType() {
		// CREATE_FIND creates a new bean and sets values (no DB lookup for simple bindings)
		InputStream is = csvStream("text", "createfind");
		UploadException exception = new UploadException();
		CSVLoader loader = new CSVLoader(LoaderActivityType.CREATE_FIND, is, exception, MODULE, DOCUMENT);
		loader.addField(new DataFileField(AllAttributesPersistent.textPropertyName, 0));

		List<AllAttributesPersistent> beans = loader.beanResults();

		assertEquals(1, beans.size());
		assertEquals("createfind", beans.get(0).getText());
	}

	@Test
	void poiSheetLoaderGetStringFieldValueReturnsNullForEmptyCell() throws Exception {
		// Row 0 has only 1 populated cell; accessing column 1 should give null
		InputStream is = xlsxStream(new String[][] {
				{ "only_col0" }
		});
		UploadException exception = new UploadException();
		POISheetLoader loader = new POISheetLoader(is, 0, MODULE, DOCUMENT, exception);
		loader.setActivityType(LoaderActivityType.CREATE_ALL);
		// Only add a field for column 0
		loader.addField(new DataFileField(AllAttributesPersistent.textPropertyName, 0));
		// Manually advance to row 0
		loader.nextData();

		// Column 1 is missing → should return null
		String val = loader.getStringFieldValue(1, true);
		assertNull(val, "missing cell should yield null");
	}

	@Test
	void poiSheetLoaderGetColumnNameReturnsExpected() {
		// Static helper: column 0 = A, column 1 = B, column 26 = AA
		assertEquals("A", POISheetLoader.getPOIWorksheetColumnName(0));
		assertEquals("B", POISheetLoader.getPOIWorksheetColumnName(1));
		assertEquals("Z", POISheetLoader.getPOIWorksheetColumnName(25));
		assertEquals("AA", POISheetLoader.getPOIWorksheetColumnName(26));
	}

	@Test
	void poiSheetLoaderBooleanField() throws Exception {
		// Use "1" which maps to Boolean.TRUE in the loader
		InputStream is = xlsxStream(new String[][] {
				{ "1" }
		});
		UploadException exception = new UploadException();
		POISheetLoader loader = new POISheetLoader(is, 0, MODULE, DOCUMENT, exception);
		loader.setActivityType(LoaderActivityType.CREATE_ALL);
		loader.addField(new DataFileField(AllAttributesPersistent.booleanFlagPropertyName, 0));

		List<AllAttributesPersistent> beans = loader.beanResults();

		assertEquals(1, beans.size());
		assertEquals(Boolean.TRUE, beans.get(0).getBooleanFlag());
	}

	@Test
	void poiSheetLoaderBooleanFalseField() throws Exception {
		// Use "false" which maps to Boolean.FALSE
		InputStream is = xlsxStream(new String[][] {
				{ "false" }
		});
		UploadException exception = new UploadException();
		POISheetLoader loader = new POISheetLoader(is, 0, MODULE, DOCUMENT, exception);
		loader.setActivityType(LoaderActivityType.CREATE_ALL);
		loader.addField(new DataFileField(AllAttributesPersistent.booleanFlagPropertyName, 0));

		List<AllAttributesPersistent> beans = loader.beanResults();

		assertEquals(1, beans.size());
		assertEquals(Boolean.FALSE, beans.get(0).getBooleanFlag());
	}

	@Test
	void csvLoaderBooleanField() {
		InputStream is = csvStream("booleanFlag", "yes");
		UploadException exception = new UploadException();
		CSVLoader loader = new CSVLoader(LoaderActivityType.CREATE_ALL, is, exception, MODULE, DOCUMENT);
		loader.addField(new DataFileField(AllAttributesPersistent.booleanFlagPropertyName, 0));

		List<AllAttributesPersistent> beans = loader.beanResults();

		assertEquals(1, beans.size());
		assertEquals(Boolean.TRUE, beans.get(0).getBooleanFlag());
	}

	@Test
	void csvLoaderLongIntegerField() {
		InputStream is = csvStream("longInteger", "9876543210");
		UploadException exception = new UploadException();
		CSVLoader loader = new CSVLoader(LoaderActivityType.CREATE_ALL, is, exception, MODULE, DOCUMENT);
		loader.addField(new DataFileField(AllAttributesPersistent.longIntegerPropertyName, 0));

		List<AllAttributesPersistent> beans = loader.beanResults();

		assertEquals(1, beans.size());
		assertEquals(Long.valueOf(9876543210L), beans.get(0).getLongInteger());
	}

	@Test
	void csvLoaderDecimal2Field() {
		InputStream is = csvStream("decimal2", "3.14");
		UploadException exception = new UploadException();
		CSVLoader loader = new CSVLoader(LoaderActivityType.CREATE_ALL, is, exception, MODULE, DOCUMENT);
		loader.addField(new DataFileField(AllAttributesPersistent.decimal2PropertyName, 0));

		List<AllAttributesPersistent> beans = loader.beanResults();

		assertEquals(1, beans.size());
		assertNotNull(beans.get(0).getDecimal2());
	}

	@Test
	void csvLoaderDecimal5Field() {
		InputStream is = csvStream("decimal5", "1.23456");
		UploadException exception = new UploadException();
		CSVLoader loader = new CSVLoader(LoaderActivityType.CREATE_ALL, is, exception, MODULE, DOCUMENT);
		loader.addField(new DataFileField(AllAttributesPersistent.decimal5PropertyName, 0));

		List<AllAttributesPersistent> beans = loader.beanResults();

		assertEquals(1, beans.size());
		assertNotNull(beans.get(0).getDecimal5());
	}

	@Test
	void csvLoaderDecimal10Field() {
		InputStream is = csvStream("decimal10", "9.9999999999");
		UploadException exception = new UploadException();
		CSVLoader loader = new CSVLoader(LoaderActivityType.CREATE_ALL, is, exception, MODULE, DOCUMENT);
		loader.addField(new DataFileField(AllAttributesPersistent.decimal10PropertyName, 0));

		List<AllAttributesPersistent> beans = loader.beanResults();

		assertEquals(1, beans.size());
		assertNotNull(beans.get(0).getDecimal10());
	}

	@Test
	void poiSheetLoaderDecimalField() throws Exception {
		InputStream is = xlsxStream(new String[][] {
				{ "2.71828" }
		});
		UploadException exception = new UploadException();
		POISheetLoader loader = new POISheetLoader(is, 0, MODULE, DOCUMENT, exception);
		loader.setActivityType(LoaderActivityType.CREATE_ALL);
		loader.addField(new DataFileField(AllAttributesPersistent.decimal2PropertyName, 0));

		List<AllAttributesPersistent> beans = loader.beanResults();

		assertEquals(1, beans.size());
		assertNotNull(beans.get(0).getDecimal2());
	}

	@Test
	void poiSheetLoaderRequiredFieldMissingGeneratesWarning() throws Exception {
		// An empty cell for a required field should add a warning/error
		InputStream is = xlsxStream(new String[][] {
				{ "" } // empty string cell
		});
		UploadException exception = new UploadException();
		POISheetLoader loader = new POISheetLoader(is, 0, MODULE, DOCUMENT, exception);
		loader.setActivityType(LoaderActivityType.CREATE_ALL);
		DataFileField requiredField = new DataFileField(AllAttributesPersistent.textPropertyName, null, true, 0);
		loader.addField(requiredField);

		loader.beanResults();

		// A warning is expected because the required field had no value
		assertTrue(exception.hasProblems(),
				"required field with empty value should produce a warning or error");
	}

	@Test
	void poiSheetLoaderMemoField() throws Exception {
		InputStream is = xlsxStream(new String[][] {
				{ "This is a longer memo text" }
		});
		UploadException exception = new UploadException();
		POISheetLoader loader = new POISheetLoader(is, 0, MODULE, DOCUMENT, exception);
		loader.setActivityType(LoaderActivityType.CREATE_ALL);
		loader.addField(new DataFileField(AllAttributesPersistent.memoPropertyName, 0));

		List<AllAttributesPersistent> beans = loader.beanResults();

		assertEquals(1, beans.size());
		assertEquals("This is a longer memo text", beans.get(0).getMemo());
	}

	@Test
	void csvLoaderMemoField() {
		InputStream is = csvStream("memo", "some memo content");
		UploadException exception = new UploadException();
		CSVLoader loader = new CSVLoader(LoaderActivityType.CREATE_ALL, is, exception, MODULE, DOCUMENT);
		loader.addField(new DataFileField(AllAttributesPersistent.memoPropertyName, 0));

		List<AllAttributesPersistent> beans = loader.beanResults();

		assertEquals(1, beans.size());
		assertEquals("some memo content", beans.get(0).getMemo());
	}

	@Test
	void poiSheetLoaderMarkupField() throws Exception {
		InputStream is = xlsxStream(new String[][] {
				{ "<b>bold</b>" }
		});
		UploadException exception = new UploadException();
		POISheetLoader loader = new POISheetLoader(is, 0, MODULE, DOCUMENT, exception);
		loader.setActivityType(LoaderActivityType.CREATE_ALL);
		loader.addField(new DataFileField(AllAttributesPersistent.markupPropertyName, 0));

		List<AllAttributesPersistent> beans = loader.beanResults();

		assertEquals(1, beans.size());
		assertEquals("<b>bold</b>", beans.get(0).getMarkup());
	}

	@Test
	void dataFileFieldWithLoadActionCtor() {
		// Tests the LoadAction constructor path
		DataFileField field = new DataFileField(
				AllAttributesPersistent.textPropertyName,
				LoadAction.SET_VALUE,
				false, 3);
		assertEquals(AllAttributesPersistent.textPropertyName, field.getBinding());
		assertEquals(Integer.valueOf(3), field.getIndex());
		assertFalse(field.isRequired());
		assertEquals(LoadAction.SET_VALUE, field.getLoadAction());
	}

	@Test
	void dataFileFieldGettersAndSetters() {
		DataFileField field = new DataFileField(AllAttributesPersistent.textPropertyName, 0);
		field.setIndex(5);
		assertEquals(Integer.valueOf(5), field.getIndex());

		field.setIndex(Integer.valueOf(7));
		assertEquals(Integer.valueOf(7), field.getIndex());

		field.setRequired(true);
		assertTrue(field.isRequired());

		field.setLoadAction(LoadAction.LOOKUP_EQUALS);
		assertEquals(LoadAction.LOOKUP_EQUALS, field.getLoadAction());
	}

        // ---- AbstractDataFileLoader getter/setter coverage ----

        @Test
        void csvLoaderIsDebugModeDefaultsFalse() {
                InputStream is = csvStream("text", "hello");
                UploadException exception = new UploadException();
                CSVLoader loader = new CSVLoader(LoaderActivityType.CREATE_ALL, is, exception, MODULE, DOCUMENT);
                assertFalse(loader.isDebugMode(), "debugMode should be false by default");
                loader.setDebugMode(true);
                assertTrue(loader.isDebugMode(), "debugMode should be true after set");
        }

        @Test
        void csvLoaderGetDataIndexDefaultsZero() {
                InputStream is = csvStream("text", "hello");
                UploadException exception = new UploadException();
                CSVLoader loader = new CSVLoader(LoaderActivityType.CREATE_ALL, is, exception, MODULE, DOCUMENT);
                assertEquals(0, loader.getDataIndex(), "dataIndex should be 0 by default");
        }

        @Test
        void csvLoaderSetFieldIndexAffectsProcessing() {
                InputStream is = csvStream("skip,text", "ignore,hello");
                UploadException exception = new UploadException();
                CSVLoader loader = new CSVLoader(LoaderActivityType.CREATE_ALL, is, exception, MODULE, DOCUMENT);
                loader.setFieldIndex(1);
                loader.addField(new DataFileField(AllAttributesPersistent.textPropertyName, 1));
                List<AllAttributesPersistent> beans = loader.beanResults();
                assertEquals(1, beans.size());
        }

        @Test
        void csvLoaderSetAndGetException() {
                InputStream is = csvStream("text", "hello");
                UploadException originalException = new UploadException();
                CSVLoader loader = new CSVLoader(LoaderActivityType.CREATE_ALL, is, originalException, MODULE, DOCUMENT);
                assertEquals(originalException, loader.getException(), "getException should return the set exception");

                UploadException newException = new UploadException();
                loader.setException(newException);
                assertEquals(newException, loader.getException(), "getException should return the new exception after setException");
        }

        @Test
        void csvLoaderAddFieldWithStringBinding() {
                InputStream is = csvStream("text", "hello");
                UploadException exception = new UploadException();
                CSVLoader loader = new CSVLoader(LoaderActivityType.CREATE_ALL, is, exception, MODULE, DOCUMENT);
                // addField(String binding) - adds field with auto-index
                loader.addField(AllAttributesPersistent.textPropertyName);
                List<AllAttributesPersistent> beans = loader.beanResults();
                assertEquals(1, beans.size());
                assertEquals("hello", beans.get(0).getText());
        }

        @Test
        void csvLoaderAddFieldWithCurlyBraceBinding() {
                InputStream is = csvStream("text", "world");
                UploadException exception = new UploadException();
                CSVLoader loader = new CSVLoader(LoaderActivityType.CREATE_ALL, is, exception, MODULE, DOCUMENT);
                // addField(String) with curly-brace notation strips braces
                loader.addField("{" + AllAttributesPersistent.textPropertyName + "}");
                List<AllAttributesPersistent> beans = loader.beanResults();
                assertEquals(1, beans.size());
                assertEquals("world", beans.get(0).getText());
        }

        @Test
        void csvLoaderSetEmptyAsZeroTreatsEmptyNumericAsZero() {
                // Use two columns so the row is not treated as empty/EOF; integer column has empty value
                InputStream is = csvStream("text,normalInteger", "hello,");
                UploadException exception = new UploadException();
                CSVLoader loader = new CSVLoader(LoaderActivityType.CREATE_ALL, is, exception, MODULE, DOCUMENT);
                loader.setEmptyAsZero(true);
                loader.addField(new DataFileField(AllAttributesPersistent.textPropertyName, 0));
                loader.addField(new DataFileField(AllAttributesPersistent.normalIntegerPropertyName, 1));
                List<AllAttributesPersistent> beans = loader.beanResults();
                assertEquals(1, beans.size());
                assertEquals(Integer.valueOf(0), beans.get(0).getNormalInteger(), "empty numeric with setEmptyAsZero should become 0");
        }

        @Test
        void csvLoaderGetWhereNoArgReturnsColumnInfo() {
                InputStream is = csvStream("text", "hello");
                UploadException exception = new UploadException();
                CSVLoader loader = new CSVLoader(LoaderActivityType.CREATE_ALL, is, exception, MODULE, DOCUMENT);
                loader.addField(new DataFileField(AllAttributesPersistent.textPropertyName, 0));
                // advance to first data row
                loader.beanResult();
                String where = loader.getWhere();
                assertNotNull(where, "getWhere() should return non-null after processing");
        }
}
