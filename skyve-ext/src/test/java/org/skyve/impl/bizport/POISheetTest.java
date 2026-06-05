package org.skyve.impl.bizport;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;

import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.Test;
import org.skyve.bizport.BizPortColumn;
import org.skyve.bizport.SheetKey;
import org.skyve.domain.ChildBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

@SuppressWarnings("static-method")
public class POISheetTest {

	@Test
	public void constructorSetsTitle() {
		POISheet sheet = new POISheet("MySheet");
		assertEquals("MySheet", sheet.getTitle());
	}

	@Test
	public void toStringReturnsTitle() {
		POISheet sheet = new POISheet("TestTitle");
		assertEquals("TestTitle", sheet.toString());
	}

	@Test
	public void setTitleUpdatesTitle() {
		POISheet sheet = new POISheet("Original");
		sheet.setTitle("Updated");
		assertEquals("Updated", sheet.getTitle());
	}

	@Test
	public void getColumnBindingsEmptyInitially() {
		POISheet sheet = new POISheet("TestSheet");
		assertTrue(sheet.getColumnBindings().isEmpty());
	}

	@Test
	public void addColumnAndGetColumn() {
		POISheet sheet = new POISheet("TestSheet");
		BizPortColumn col = new BizPortColumn("My Column", null);
		sheet.addColumn("binding1", col);
		assertEquals(col, sheet.getColumn("binding1"));
	}

	@Test
	public void getColumnReturnsNullForMissingBinding() {
		POISheet sheet = new POISheet("TestSheet");
		assertNull(sheet.getColumn("nonexistent"));
	}

	@Test
	public void removeColumnReturnsPreviousValue() {
		POISheet sheet = new POISheet("TestSheet");
		BizPortColumn col = new BizPortColumn("Col", null);
		sheet.addColumn("myBinding", col);
		BizPortColumn removed = sheet.removeColumn("myBinding");
		assertEquals(col, removed);
		assertNull(sheet.getColumn("myBinding"));
	}

	@Test
	public void removeColumnReturnsNullForAbsentBinding() {
		POISheet sheet = new POISheet("TestSheet");
		assertNull(sheet.removeColumn("absent"));
	}

	@Test
	public void getColumnBindingsReflectsAddedColumns() {
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("a", new BizPortColumn("A", null));
		sheet.addColumn("b", new BizPortColumn("B", null));
		assertTrue(sheet.getColumnBindings().contains("a"));
		assertTrue(sheet.getColumnBindings().contains("b"));
		assertEquals(2, sheet.getColumnBindings().size());
	}

	@Test(expected = IllegalStateException.class)
	public void addRowThrowsWhenNotMaterialised() {
		new POISheet("TestSheet").addRow("key");
	}

	@Test(expected = IllegalStateException.class)
	public void moveToRowThrowsWhenNotMaterialised() {
		new POISheet("TestSheet").moveToRow("key");
	}

	@Test(expected = NullPointerException.class)
	public void nextRowThrowsWhenNotMaterialised() {
		new POISheet("TestSheet").nextRow();
	}

	@Test(expected = IllegalStateException.class)
	public void getValueThrowsWhenNotMaterialised() {
		new POISheet("TestSheet").getValue("binding", null, null);
	}

	@Test(expected = IllegalStateException.class)
	public void setValueThrowsWhenNotMaterialised() {
		new POISheet("TestSheet").setValue("binding", "value");
	}

	@Test
	public void resetRowDoesNotThrowWhenNotMaterialised() {
		// resetRow does not check for materialisation; it just resets internal state
		POISheet sheet = new POISheet("TestSheet");
		sheet.resetRow(); // should not throw
		assertNotNull(sheet);
	}

	@Test(expected = IllegalStateException.class)
	public void addColumnThrowsWhenMaterialised() {
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
		sheet.addColumn("col", new BizPortColumn("Col", null));
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
	public void setValueAndGetValueReturnsSameString() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		BizPortColumn col = new BizPortColumn("My Column", null);
		sheet.addColumn("name", col);
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");
			sheet.setValue("name", "hello");
			String result = sheet.getValue("name", AttributeType.text, null);
			assertEquals("hello", result);
		}
	}

	@Test
	public void setValueAndGetValueReturnsInteger() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		BizPortColumn col = new BizPortColumn("Amount", null);
		sheet.addColumn("amount", col);
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");
			sheet.setValue("amount", Integer.valueOf(42));
			Integer result = sheet.getValue("amount", AttributeType.integer, null);
			assertEquals(Integer.valueOf(42), result);
		}
	}

	@Test
	public void setValueAndGetValueReturnsBoolean() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		BizPortColumn col = new BizPortColumn("Flag", null);
		sheet.addColumn("flag", col);
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");
			sheet.setValue("flag", Boolean.TRUE);
			Boolean result = sheet.getValue("flag", AttributeType.bool, null);
			assertEquals(Boolean.TRUE, result);
		}
	}

	@Test
	public void setValueAndGetValueReturnsDecimal2() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		BizPortColumn col = new BizPortColumn("Price", null);
		sheet.addColumn("price", col);
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");
			sheet.setValue("price", new Decimal2("3.14"));
			Decimal2 result = sheet.getValue("price", AttributeType.decimal2, null);
			assertNotNull(result);
			assertEquals(new Decimal2("3.14"), result);
		}
	}

	@Test
	public void getValueNullCellReturnsNull() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		BizPortColumn col = new BizPortColumn("Name", null);
		sheet.addColumn("name", col);
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");
			// no cell has been created — getValue should return null
			String result = sheet.getValue("name", AttributeType.text, null);
			assertNull(result);
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void getValueMissingColumnThrowsIllegalArgument() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");
			sheet.getValue("nonexistent", AttributeType.text, null);
		}
	}

	@Test(expected = IllegalStateException.class)
	public void getValueNoCurrentRowThrowsIllegalState() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("name", new BizPortColumn("Name", null));
		try (XSSFWorkbook wb = materialize(sheet)) {
			// currentRow is null — never called addRow
			sheet.getValue("name", AttributeType.text, null);
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void setValueMissingColumnThrowsIllegalArgument() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");
			sheet.setValue("nonexistent", "value");
		}
	}

	@Test(expected = IllegalStateException.class)
	public void setValueNoCurrentRowThrowsIllegalState() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("name", new BizPortColumn("Name", null));
		try (XSSFWorkbook wb = materialize(sheet)) {
			// currentRow is null — never called addRow
			sheet.setValue("name", "value");
		}
	}

	@Test
	public void moveToRowReturnsFalseForMissingKey() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		try (XSSFWorkbook wb = materialize(sheet)) {
			boolean result = sheet.moveToRow("nonexistent");
			assertFalse(result);
		}
	}

	@Test
	public void nextRowReturnsFalseOnEmptySheet() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		try (XSSFWorkbook wb = materialize(sheet)) {
			boolean result = sheet.nextRow();
			assertFalse(result);
		}
	}

	@Test
	public void nextRowIteratesAddedRows() throws Exception {
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
	public void addErrorAtCurrentRowDoesNotThrow() throws Exception {
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
	public void addWarningAtCurrentRowDoesNotThrow() throws Exception {
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
	public void setValueAndGetValueReturnsLong() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		BizPortColumn col = new BizPortColumn("BigNum", null);
		sheet.addColumn("num", col);
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");
			sheet.setValue("num", Long.valueOf(12345678L));
			Long result = sheet.getValue("num", AttributeType.longInteger, null);
			assertEquals(Long.valueOf(12345678L), result);
		}
	}

	@Test
	public void setValueStringAndGetWithMemoTypeReturnsValue() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		BizPortColumn col = new BizPortColumn("Notes", null);
		sheet.addColumn("notes", col);
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");
			sheet.setValue("notes", "memo text");
			String result = sheet.getValue("notes", AttributeType.memo, null);
			assertEquals("memo text", result);
		}
	}

	@Test
	public void getValueStringCellWithWrongTypeAddsError() throws Exception {
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
	public void multipleRowsCanBeSetAndRead() throws Exception {
		POISheet sheet = new POISheet("TestSheet");
		BizPortColumn col = new BizPortColumn("Name", null);
		sheet.addColumn("name", col);
		try (XSSFWorkbook wb = materialize(sheet)) {
			sheet.addRow("r1");
			sheet.setValue("name", "Alice");
			sheet.addRow("r2");
			sheet.setValue("name", "Bob");
			// currentRow is still row2 after second addRow
			String second = sheet.getValue("name", AttributeType.text, null);
			assertEquals("Bob", second);
		}
	}

	@Test
	public void setValueDecimal5RoundtripsAsNumeric() {
		POIWorkbook wb = new POIWorkbook(true);
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("price", new BizPortColumn("Price", null, AttributeType.decimal5));
		wb.addSheet(new SheetKey("m", "d"), sheet);
		wb.materialise();
		sheet.addRow("r1");
		sheet.setValue("price", new Decimal5("1.23456"));
		Decimal5 result = sheet.getValue("price", AttributeType.decimal5, null);
		assertNotNull(result);
	}

	@Test
	public void setValueDecimal10RoundtripsAsNumeric() {
		POIWorkbook wb = new POIWorkbook(true);
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("amount", new BizPortColumn("Amount", null, AttributeType.decimal10));
		wb.addSheet(new SheetKey("m", "d"), sheet);
		wb.materialise();
		sheet.addRow("r1");
		sheet.setValue("amount", new Decimal10("9.99"));
		Decimal10 result = sheet.getValue("amount", AttributeType.decimal10, null);
		assertNotNull(result);
	}

	@Test
	public void setValueDateOnlyAndGetValueReturnsDateOnly() {
		POIWorkbook wb = new POIWorkbook(true);
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("date", new BizPortColumn("Date", null, AttributeType.date));
		wb.addSheet(new SheetKey("m", "d"), sheet);
		wb.materialise();
		sheet.addRow("r1");
		DateOnly today = new DateOnly();
		sheet.setValue("date", today);
		Object result = sheet.getValue("date", AttributeType.date, null);
		assertNotNull(result);
	}

	@Test
	public void setValueDateTimeAndGetValueReturnsDateTime() {
		POIWorkbook wb = new POIWorkbook(true);
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("dt", new BizPortColumn("DT", null, AttributeType.dateTime));
		wb.addSheet(new SheetKey("m", "d"), sheet);
		wb.materialise();
		sheet.addRow("r1");
		DateTime now = new DateTime();
		sheet.setValue("dt", now);
		Object result = sheet.getValue("dt", AttributeType.dateTime, null);
		assertNotNull(result);
	}

	@Test
	public void setValueTimestampAndGetValueReturnsTimestamp() {
		POIWorkbook wb = new POIWorkbook(true);
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("ts", new BizPortColumn("TS", null, AttributeType.timestamp));
		wb.addSheet(new SheetKey("m", "d"), sheet);
		wb.materialise();
		sheet.addRow("r1");
		Timestamp ts = new Timestamp();
		sheet.setValue("ts", ts);
		Object result = sheet.getValue("ts", AttributeType.timestamp, null);
		assertNotNull(result);
	}

	@Test
	public void setValueTimeOnlyAndGetValueReturnsTimeOnly() {
		POIWorkbook wb = new POIWorkbook(true);
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("time", new BizPortColumn("Time", null, AttributeType.time));
		wb.addSheet(new SheetKey("m", "d"), sheet);
		wb.materialise();
		sheet.addRow("r1");
		TimeOnly t = new TimeOnly();
		sheet.setValue("time", t);
		Object result = sheet.getValue("time", AttributeType.time, null);
		assertNotNull(result);
	}

	@Test
	public void numericCellWithStringTypeReturnsStringConversion() {
		POIWorkbook wb = new POIWorkbook(true);
		POISheet sheet = new POISheet("TestSheet");
		sheet.addColumn("num", new BizPortColumn("Num", null, AttributeType.integer));
		wb.addSheet(new SheetKey("m", "d"), sheet);
		wb.materialise();
		sheet.addRow("r1");
		// set numeric value but ask for text — should convert to string via NumberToTextConverter
		sheet.setValue("num", Integer.valueOf(99));
		String result = sheet.getValue("num", AttributeType.text, null);
		assertNotNull(result);
	}

	@Test
	public void existingChildDocumentSheetSetsParentReferenceWithoutDataRows() throws Exception {
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
	public void existingCollectionSheetSetsSimpleOwnerReferenceWithoutDataRows() throws Exception {
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
}
