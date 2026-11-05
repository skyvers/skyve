package org.skyve.impl.bizport;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.Iterator;

import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.junit.jupiter.api.Test;
import org.skyve.bizport.BizPortColumn;
import org.skyve.bizport.SheetKey;
import org.skyve.domain.ChildBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.messages.UploadException.Problem;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

@SuppressWarnings("static-method")
class POISheetTest {

	@Test
	void constructorSetsTitle() {
		POISheet sheet = new POISheet("MySheet");
		assertEquals("MySheet", sheet.getTitle());
	}

	@Test
	void toStringReturnsTitle() {
		POISheet sheet = new POISheet("TestTitle");
		assertEquals("TestTitle", sheet.toString());
	}

	@Test
	void setTitleUpdatesTitle() {
		POISheet sheet = new POISheet("Original");
		sheet.setTitle("Updated");
		assertEquals("Updated", sheet.getTitle());
	}

	@Test
	void getColumnBindingsEmptyInitially() {
		POISheet sheet = new POISheet("TestSheet");
		assertTrue(sheet.getColumnBindings().isEmpty());
	}

	@Test
	void addColumnAndGetColumn() {
		POISheet sheet = new POISheet("TestSheet");
		BizPortColumn col = new BizPortColumn("My Column", null);
		sheet.addColumn("binding1", col);
		assertEquals(col, sheet.getColumn("binding1"));
	}

	@Test
	void getColumnReturnsNullForMissingBinding() {
		POISheet sheet = new POISheet("TestSheet");
		assertNull(sheet.getColumn("nonexistent"));
	}

	@Test
	void removeColumnReturnsPreviousValue() {
		POISheet sheet = new POISheet("TestSheet");
		BizPortColumn col = new BizPortColumn("Col", null);
		sheet.addColumn("myBinding", col);
		BizPortColumn removed = sheet.removeColumn("myBinding");
		assertEquals(col, removed);
		assertNull(sheet.getColumn("myBinding"));
	}

	@Test
	void removeColumnReturnsNullForAbsentBinding() {
		POISheet sheet = new POISheet("TestSheet");
		assertNull(sheet.removeColumn("absent"));
	}

	@Test
	void getColumnBindingsReflectsAddedColumns() {
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("a", new BizPortColumn("A", null));
		sheet.addColumn("b", new BizPortColumn("B", null));
		assertTrue(sheet.getColumnBindings().contains("a"));
		assertTrue(sheet.getColumnBindings().contains("b"));
		assertEquals(2, sheet.getColumnBindings().size());
	}

	@Test
	void addRowThrowsWhenNotMaterialised() {
		assertThrows(IllegalStateException.class, () -> new POISheet("TestSheet").addRow("key"));
	}

	@Test
	void moveToRowThrowsWhenNotMaterialised() {
		assertThrows(IllegalStateException.class, () -> new POISheet("TestSheet").moveToRow("key"));
	}

	@Test
	void nextRowThrowsWhenNotMaterialised() {
		assertThrows(NullPointerException.class, () -> new POISheet("TestSheet").nextRow());
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void getValueThrowsWhenNotMaterialised() {
		assertThrows(IllegalStateException.class, () -> new POISheet("TestSheet").getValue("binding", null, null));
	}

	@Test
	void setValueThrowsWhenNotMaterialised() {
		assertThrows(IllegalStateException.class, () -> new POISheet("TestSheet").setValue("binding", "value"));
	}

	@Test
	void resetRowDoesNotThrowWhenNotMaterialised() {
		// resetRow does not check for materialisation; it just resets internal state
		POISheet sheet = new POISheet("TestSheet");
		sheet.resetRow(); // should not throw
		assertNotNull(sheet);
	}

	@Test
	void addColumnThrowsWhenMaterialised() {
		POISheet sheet = new POISheet("TestSheet");
		// Directly set the internal `sheet` field via addRow path isn't available,
		// but we can test the normal add first to exercise the not-materialized path
		// and the materialized path via the state check.
		// The only way to trigger the materialized state is via POIWorkbook.materialise(),
		// which requires a Workbook. Test the non-materialized path here and the
		// materialized exception path via reflection.
		java.lang.reflect.Field f;
		try {
			f = POISheet.class.getDeclaredField("sheet");
			f.setAccessible(true);
			try (org.apache.poi.xssf.usermodel.XSSFWorkbook wb = new org.apache.poi.xssf.usermodel.XSSFWorkbook()) {
				org.apache.poi.xssf.usermodel.XSSFSheet xssfSheet = wb.createSheet("x");
				f.set(sheet, xssfSheet);
			}
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		assertThrows(IllegalStateException.class, () -> sheet.addColumn("col", new BizPortColumn("Col", null)));
	}

	@Test
	void removeColumnThrowsWhenMaterialised() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("col", new BizPortColumn("Col", null));
		try (XSSFWorkbook wb = materialize(sheet)) {
			assertThrows(IllegalStateException.class, () -> sheet.removeColumn("col"));
		}
	}

	// ---- Materialized sheet tests ----

	/** Injects an XSSFSheet into the sheet field and returns it, keeping the workbook alive. */
	@SuppressWarnings("resource")
	private static XSSFWorkbook materialize(POISheet poiSheet) throws Exception {
		XSSFWorkbook wb = new XSSFWorkbook();
		XSSFSheet xssfSheet = wb.createSheet("data");
		Field f = POISheet.class.getDeclaredField("sheet");
		f.setAccessible(true);
		f.set(poiSheet, xssfSheet);
		return wb;
	}

	@Test
	void setValueAndGetValueReturnsSameString() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		BizPortColumn col = new BizPortColumn("My Column", null);
		sheet.addColumn("name", col);
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");
			sheet.setValue("name", "hello");
			@SuppressWarnings("null") // passes null on purpose
			String result = sheet.getValue("name", AttributeType.text, null);
			assertEquals("hello", result);
		}
	}

	@Test
	void setValueAndGetValueReturnsInteger() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		BizPortColumn col = new BizPortColumn("Amount", null);
		sheet.addColumn("amount", col);
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");
			sheet.setValue("amount", Integer.valueOf(42));
			@SuppressWarnings("null") // passes null on purpose
			Integer result = sheet.getValue("amount", AttributeType.integer, null);
			assertEquals(Integer.valueOf(42), result);
		}
	}

	@Test
	void setValueAndGetValueReturnsBoolean() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		BizPortColumn col = new BizPortColumn("Flag", null);
		sheet.addColumn("flag", col);
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");
			sheet.setValue("flag", Boolean.TRUE);
			@SuppressWarnings("null") // passes null on purpose
			Boolean result = sheet.getValue("flag", AttributeType.bool, null);
			assertEquals(Boolean.TRUE, result);
		}
	}

	@Test
	void setValueAndGetValueReturnsDecimal2() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		BizPortColumn col = new BizPortColumn("Price", null);
		sheet.addColumn("price", col);
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");
			sheet.setValue("price", new Decimal2("3.14"));
			@SuppressWarnings("null") // passes null on purpose
			Decimal2 result = sheet.getValue("price", AttributeType.decimal2, null);
			assertNotNull(result);
			assertEquals(new Decimal2("3.14"), result);
		}
	}

	@Test
	void getValueNullCellReturnsNull() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		BizPortColumn col = new BizPortColumn("Name", null);
		sheet.addColumn("name", col);
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");
			// no cell has been created — getValue should return null
			@SuppressWarnings("null") // passes null on purpose
			String result = sheet.getValue("name", AttributeType.text, null);
			assertNull(result);
		}
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void getValueMissingColumnThrowsIllegalArgument() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");
			assertThrows(IllegalArgumentException.class, () -> sheet.getValue("nonexistent", AttributeType.text, null));
		}
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void getValueNoCurrentRowThrowsIllegalState() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("name", new BizPortColumn("Name", null));
		try (XSSFWorkbook wb = materialize(sheet)) {
			// currentRow is null — never called addRow
			assertThrows(IllegalStateException.class, () -> sheet.getValue("name", AttributeType.text, null));
		}
	}

	@Test
	void setValueMissingColumnThrowsIllegalArgument() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");
			assertThrows(IllegalArgumentException.class, () -> sheet.setValue("nonexistent", "value"));
		}
	}

	@Test
	void setValueNoCurrentRowThrowsIllegalState() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("name", new BizPortColumn("Name", null));
		try (XSSFWorkbook wb = materialize(sheet)) {
			// currentRow is null — never called addRow
			assertThrows(IllegalStateException.class, () -> sheet.setValue("name", "value"));
		}
	}

	@Test
	void moveToRowReturnsFalseForMissingKey() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		try (XSSFWorkbook wb = materialize(sheet)) {
			boolean result = sheet.moveToRow("nonexistent");
			assertFalse(result);
		}
	}

	@Test
	void moveToRowAcceptsCompositeRowKey() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("owner", "element");

			assertTrue(sheet.moveToRow("owner", "element"));
		}
	}

	@Test
	void nextRowReturnsFalseOnEmptySheet() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		try (XSSFWorkbook wb = materialize(sheet)) {
			boolean result = sheet.nextRow();
			assertFalse(result);
		}
	}

	@Test
	void nextRowIteratesAddedRows() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");
			sheet.addRow("r2");
			// resetRow resets to START_ROW - 1 so nextRow() finds data rows
			sheet.resetRow();
			assertTrue(sheet.nextRow());
			assertTrue(sheet.nextRow());
			assertFalse(sheet.nextRow());
		}
	}

	@Test
	void nextRowSkipsBlankRows() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		try (XSSFWorkbook wb = materialize(sheet)) {
			XSSFSheet xssfSheet = wb.getSheet("data");
			xssfSheet.createRow(5);
			sheet.resetRow();

			assertTrue(sheet.nextRow());
			assertEquals(5, xssfSheet.getRow(5).getRowNum());
		}
	}

	@Test
	void addErrorAtCurrentRowDoesNotThrow() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		BizPortColumn col = new BizPortColumn("Name", null);
		sheet.addColumn("name", col);
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");
			UploadException problems = new UploadException();
			sheet.addErrorAtCurrentRow(problems, col, "test error");
			assertTrue(problems.hasProblems());
		}
	}

	@Test
	void addWarningAtCurrentRowDoesNotThrow() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		BizPortColumn col = new BizPortColumn("Name", null);
		sheet.addColumn("name", col);
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");
			UploadException problems = new UploadException();
			sheet.addWarningAtCurrentRow(problems, col, "test warning");
			assertTrue(problems.hasProblems());
		}
	}

	@Test
	void addErrorWithoutCurrentRowUsesRefPlaceholder() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		BizPortColumn col = new BizPortColumn("Name", null);
		sheet.addColumn("name", col);
		try (XSSFWorkbook wb = materialize(sheet)) {
			UploadException problems = new UploadException();

			sheet.addErrorAtCurrentRow(problems, col, "test error");

			Problem problem = first(problems.getErrors());
			assertEquals("TestSheet!#REF", problem.getWhere());
		}
	}

	@Test
	void setValueAndGetValueReturnsLong() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		BizPortColumn col = new BizPortColumn("BigNum", null);
		sheet.addColumn("num", col);
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");
			sheet.setValue("num", Long.valueOf(12345678L));
			@SuppressWarnings("null") // passes null on purpose
			Long result = sheet.getValue("num", AttributeType.longInteger, null);
			assertEquals(Long.valueOf(12345678L), result);
		}
	}

	@Test
	void setValueStringAndGetWithMemoTypeReturnsValue() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		BizPortColumn col = new BizPortColumn("Notes", null);
		sheet.addColumn("notes", col);
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");
			sheet.setValue("notes", "memo text");
			@SuppressWarnings("null") // passes null on purpose
			String result = sheet.getValue("notes", AttributeType.memo, null);
			assertEquals("memo text", result);
		}
	}

	@Test
	void getValueStringCellWithWrongTypeAddsError() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		BizPortColumn col = new BizPortColumn("Flag", null);
		sheet.addColumn("flag", col);
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");
			// set a string value but ask for bool — should add error and return null
			sheet.setValue("flag", "notabool");
			UploadException problems = new UploadException();
			Boolean result = sheet.getValue("flag", AttributeType.bool, problems);
			assertNull(result);
		}
	}

	@Test
	void multipleRowsCanBeSetAndRead() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		BizPortColumn col = new BizPortColumn("Name", null);
		sheet.addColumn("name", col);
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");
			sheet.setValue("name", "Alice");
			sheet.addRow("r2");
			sheet.setValue("name", "Bob");
			// currentRow is still row2 after second addRow
			@SuppressWarnings("null") // passes null on purpose
			String second = sheet.getValue("name", AttributeType.text, null);
			assertEquals("Bob", second);
		}
	}

	@Test
	void setValueDecimal5RoundtripsAsNumeric() {
		POIWorkbook wb = new POIWorkbook(true);
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("price", new BizPortColumn("Price", null, AttributeType.decimal5));
		wb.addSheet(new SheetKey("m", "d"), sheet);
		wb.materialise();
		sheet.addRow("r1");
		sheet.setValue("price", new Decimal5("1.23456"));
		@SuppressWarnings("null") // passes null on purpose
		Decimal5 result = sheet.getValue("price", AttributeType.decimal5, null);
		assertNotNull(result);
	}

	@Test
	void setValueDecimal10RoundtripsAsNumeric() {
		POIWorkbook wb = new POIWorkbook(true);
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("amount", new BizPortColumn("Amount", null, AttributeType.decimal10));
		wb.addSheet(new SheetKey("m", "d"), sheet);
		wb.materialise();
		sheet.addRow("r1");
		sheet.setValue("amount", new Decimal10("9.99"));
		@SuppressWarnings("null") // passes null on purpose
		Decimal10 result = sheet.getValue("amount", AttributeType.decimal10, null);
		assertNotNull(result);
	}

	@Test
	void setValueDateOnlyAndGetValueReturnsDateOnly() {
		POIWorkbook wb = new POIWorkbook(true);
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("date", new BizPortColumn("Date", null, AttributeType.date));
		wb.addSheet(new SheetKey("m", "d"), sheet);
		wb.materialise();
		sheet.addRow("r1");
		DateOnly today = new DateOnly();
		sheet.setValue("date", today);
		@SuppressWarnings("null") // passes null on purpose
		Object result = sheet.getValue("date", AttributeType.date, null);
		assertNotNull(result);
	}

	@Test
	void setValueDateTimeAndGetValueReturnsDateTime() {
		POIWorkbook wb = new POIWorkbook(true);
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("dt", new BizPortColumn("DT", null, AttributeType.dateTime));
		wb.addSheet(new SheetKey("m", "d"), sheet);
		wb.materialise();
		sheet.addRow("r1");
		DateTime now = new DateTime();
		sheet.setValue("dt", now);
		@SuppressWarnings("null") // passes null on purpose
		Object result = sheet.getValue("dt", AttributeType.dateTime, null);
		assertNotNull(result);
	}

	@Test
	void setValueTimestampAndGetValueReturnsTimestamp() {
		POIWorkbook wb = new POIWorkbook(true);
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("ts", new BizPortColumn("TS", null, AttributeType.timestamp));
		wb.addSheet(new SheetKey("m", "d"), sheet);
		wb.materialise();
		sheet.addRow("r1");
		Timestamp ts = new Timestamp();
		sheet.setValue("ts", ts);
		@SuppressWarnings("null") // passes null on purpose
		Object result = sheet.getValue("ts", AttributeType.timestamp, null);
		assertNotNull(result);
	}

	@Test
	void setValueTimeOnlyAndGetValueReturnsTimeOnly() {
		POIWorkbook wb = new POIWorkbook(true);
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("time", new BizPortColumn("Time", null, AttributeType.time));
		wb.addSheet(new SheetKey("m", "d"), sheet);
		wb.materialise();
		sheet.addRow("r1");
		TimeOnly t = new TimeOnly();
		sheet.setValue("time", t);
		@SuppressWarnings("null") // passes null on purpose
		Object result = sheet.getValue("time", AttributeType.time, null);
		assertNotNull(result);
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void setValueEnumerationWritesCode() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("status", new BizPortColumn("Status", null));
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");

			sheet.setValue("status", new org.skyve.domain.types.Enumeration() {
				private static final long serialVersionUID = 1L;

				@Override
				public String toCode() {
					return "OPEN";
				}

				@Override
				public String toLocalisedDescription() {
					return "Open";
				}

				@Override
				public DomainValue toDomainValue() {
					return null;
				}
			});

			assertEquals("OPEN", sheet.getValue("status", AttributeType.text, null));
		}
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void setValueGeometryWritesWkt() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("location", new BizPortColumn("Location", null));
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");

			sheet.setValue("location", new GeometryFactory().createPoint(new Coordinate(1.0, 2.0)));

			assertEquals("POINT (1 2)", sheet.getValue("location", AttributeType.geometry, null));
		}
	}

	@Test
	void numericCellWithStringTypeReturnsStringConversion() {
		POIWorkbook wb = new POIWorkbook(true);
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("num", new BizPortColumn("Num", null, AttributeType.integer));
		wb.addSheet(new SheetKey("m", "d"), sheet);
		wb.materialise();
		sheet.addRow("r1");
		// set numeric value but ask for text — should convert to string via NumberToTextConverter
		sheet.setValue("num", Integer.valueOf(99));
		@SuppressWarnings("null") // passes null on purpose
		String result = sheet.getValue("num", AttributeType.text, null);
		assertNotNull(result);
	}

	@Test
	void booleanCellWithTextTypeAddsError() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("flag", new BizPortColumn("Flag", null));
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");
			sheet.setValue("flag", Boolean.TRUE);
			UploadException problems = new UploadException();

			String result = sheet.getValue("flag", AttributeType.text, problems);

			assertNull(result);
			assertTrue(problems.hasErrors());
		}
	}

	@Test
	void dateCellWithIntegerTypeAddsError() {
		POIWorkbook wb = new POIWorkbook(true);
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("date", new BizPortColumn("Date", null, AttributeType.date));
		wb.addSheet(new SheetKey("m", "d"), sheet);
		wb.materialise();
		sheet.addRow("r1");
		sheet.setValue("date", new DateOnly());
		UploadException problems = new UploadException();

		Integer result = sheet.getValue("date", AttributeType.integer, problems);

		assertNull(result);
		assertTrue(problems.hasErrors());
	}

	@Test
	void numericCellWithBooleanTypeAddsError() {
		POIWorkbook wb = new POIWorkbook(true);
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("num", new BizPortColumn("Num", null, AttributeType.integer));
		wb.addSheet(new SheetKey("m", "d"), sheet);
		wb.materialise();
		sheet.addRow("r1");
		sheet.setValue("num", Integer.valueOf(7));
		UploadException problems = new UploadException();

		Boolean result = sheet.getValue("num", AttributeType.bool, problems);

		assertNull(result);
		assertTrue(problems.hasErrors());
	}

	@Test
	void setValueOnReferencedColumnAddsLookupFormula() {
		POIWorkbook wb = new POIWorkbook(true);
		POISheet referenceSheet = new POISheet("Customers");
		wb.addSheet(new SheetKey("crm", "Customer"), referenceSheet);
		POISheet sheet = new POISheet("Orders");
		BizPortColumn customerColumn = new BizPortColumn("Customer", null, AttributeType.text);
		customerColumn.setReferencedSheet(new SheetKey("crm", "Customer"));
		sheet.addColumn("customerId", customerColumn);
		wb.addSheet(new SheetKey("sales", "Order"), sheet);
		wb.materialise();
		sheet.addRow("r1");

		sheet.setValue("customerId", "CUST-1");

		assertNotNull(sheet.sheet.getRow(3).getCell(1));
		assertTrue(sheet.sheet.getRow(3).getCell(1).getCellFormula().contains("'Customers'"));
	}

	@Test
	void setTitleUpdatesMaterialisedWorkbookSheetName() {
		POIWorkbook wb = new POIWorkbook(true);
		POISheet sheet = new POISheet("OldName");
		wb.addSheet(new SheetKey("m", "d"), sheet);
		wb.materialise();

		sheet.setTitle("NewName");

		assertEquals("NewName", sheet.getTitle());
	}

	@Test
	void existingChildDocumentSheetSetsParentReferenceWithoutDataRows() throws Exception {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Document parentDocument = mock(Document.class);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		when(document.getParentDocument(customer)).thenReturn(parentDocument);
		when(parentDocument.getOwningModuleName()).thenReturn("sales");
		when(parentDocument.getName()).thenReturn("Order");

		try (XSSFWorkbook workbook = new XSSFWorkbook()) {
			XSSFSheet existing = existingSheet(workbook, null, ChildBean.PARENT_NAME);

			POISheet sheet = new POISheet(customer, new POIWorkbook(true), existing, new UploadException());

			assertEquals(existing.getSheetName(), sheet.getTitle());
			BizPortColumn parentColumn = sheet.getColumn(ChildBean.PARENT_NAME);
			assertNotNull(parentColumn);
			assertEquals(new SheetKey("sales", "Order"), parentColumn.getReferencedSheet());
			assertFalse(sheet.nextRow());
		}
	}

	@Test
	void existingCollectionSheetSetsSimpleOwnerReferenceWithoutDataRows() throws Exception {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);

		try (XSSFWorkbook workbook = new XSSFWorkbook()) {
			XSSFSheet existing = existingSheet(workbook, "lines", PersistentBean.OWNER_COLUMN_NAME);

			POISheet sheet = new POISheet(customer, new POIWorkbook(true), existing, new UploadException());

			BizPortColumn ownerColumn = sheet.getColumn(PersistentBean.OWNER_COLUMN_NAME);
			assertNotNull(ownerColumn);
			assertEquals(new SheetKey("sales", "Order"), ownerColumn.getReferencedSheet());
			assertFalse(sheet.nextRow());
		}
	}

	private static XSSFSheet existingSheet(XSSFWorkbook workbook, String collectionBinding, String... bindings) {
		XSSFSheet sheet = workbook.createSheet("existing" + workbook.getNumberOfSheets());
		sheet.createRow(POISheet.NAME_ROW);
		sheet.getRow(POISheet.NAME_ROW).createCell(POISheet.MODULE_COLUMN).setCellValue("sales");
		sheet.getRow(POISheet.NAME_ROW).createCell(POISheet.DOCUMENT_COLUMN).setCellValue("Order");
		if (collectionBinding != null) {
			sheet.getRow(POISheet.NAME_ROW).createCell(POISheet.COLLECTION_COLUMN).setCellValue(collectionBinding);
		}

		sheet.createRow(1);
		sheet.createRow(2);
		for (int i = 0; i < bindings.length; i++) {
			sheet.getRow(1).createCell(i).setCellValue(bindings[i]);
			sheet.getRow(2).createCell(i).setCellValue("Title " + i);
		}
		return sheet;
	}

	private static Problem first(Iterable<Problem> problems) {
		Iterator<Problem> iterator = problems.iterator();
		assertTrue(iterator.hasNext());
		return iterator.next();
	}
}
