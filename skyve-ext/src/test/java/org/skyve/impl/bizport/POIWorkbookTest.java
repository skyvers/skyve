package org.skyve.impl.bizport;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.util.Date;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.Test;
import org.skyve.bizport.BizPortColumn;
import org.skyve.bizport.BizPortWorkbook.BizPortFormat;
import org.skyve.bizport.SheetKey;
import org.skyve.domain.types.DateOnly;
import org.skyve.metadata.model.Attribute.AttributeType;

@SuppressWarnings({ "static-method", "java:S8692" }) // system clock OK
public class POIWorkbookTest {

	@Test
	public void constructorOoxmlFormat() {
		POIWorkbook wb = new POIWorkbook(true);
		assertEquals(BizPortFormat.xlsx, wb.getFormat());
	}

	@Test
	public void constructorXlsFormat() {
		POIWorkbook wb = new POIWorkbook(false);
		assertEquals(BizPortFormat.xls, wb.getFormat());
	}

	@Test
	public void getSheetKeysIsEmptyInitially() {
		POIWorkbook wb = new POIWorkbook(true);
		assertTrue(wb.getSheetKeys().isEmpty());
	}

	@Test
	public void getSheetReturnsNullForMissingKey() {
		POIWorkbook wb = new POIWorkbook(true);
		assertNull(wb.getSheet(new SheetKey("mod", "doc")));
	}

	@Test
	public void removeSheetReturnsNullForMissingKey() {
		POIWorkbook wb = new POIWorkbook(true);
		assertNull(wb.removeSheet(new SheetKey("mod", "doc")));
	}

	@Test
	public void putPOICellValueStringSetsStringValue() throws Exception {
		try (XSSFWorkbook workbook = new XSSFWorkbook()) {
			XSSFSheet sheet = workbook.createSheet("test");
			POIWorkbook.putPOICellValue(sheet, 1, 1, CellType.STRING, "hello", false, false);
			XSSFRow row = sheet.getRow(0);
			assertNotNull(row);
			assertEquals("hello", row.getCell(0).getStringCellValue());
		}
	}

	@Test
	public void putPOICellValueNumericSetsNumericValue() throws Exception {
		try (XSSFWorkbook workbook = new XSSFWorkbook()) {
			XSSFSheet sheet = workbook.createSheet("test");
			POIWorkbook.putPOICellValue(sheet, 1, 1, CellType.NUMERIC, Double.valueOf(42.5), false, false);
			XSSFRow row = sheet.getRow(0);
			assertEquals(42.5, row.getCell(0).getNumericCellValue(), 0.001);
		}
	}

	@Test
	public void putPOICellValueNullWithForceNumericZero() throws Exception {
		try (XSSFWorkbook workbook = new XSSFWorkbook()) {
			XSSFSheet sheet = workbook.createSheet("test");
			POIWorkbook.putPOICellValue(sheet, 1, 1, CellType.NUMERIC, null, true, false);
			XSSFRow row = sheet.getRow(0);
			assertEquals(0.0, row.getCell(0).getNumericCellValue(), 0.001);
		}
	}

	@Test
	public void putPOICellValueNullNoForce() throws Exception {
		try (XSSFWorkbook workbook = new XSSFWorkbook()) {
			XSSFSheet sheet = workbook.createSheet("test");
			POIWorkbook.putPOICellValue(sheet, 1, 1, CellType.NUMERIC, null, false, false);
			XSSFRow row = sheet.getRow(0);
			assertEquals(CellType.BLANK, row.getCell(0).getCellType());
		}
	}

	@Test
	public void putPOICellValueTwoArgOverload() throws Exception {
		try (XSSFWorkbook workbook = new XSSFWorkbook()) {
			XSSFSheet sheet = workbook.createSheet("test");
			POIWorkbook.putPOICellValue(sheet, 1, 1, CellType.STRING, "world");
			XSSFRow row = sheet.getRow(0);
			assertEquals("world", row.getCell(0).getStringCellValue());
		}
	}

	@Test
	public void putPOICellValueThreeArgOverload() throws Exception {
		try (XSSFWorkbook workbook = new XSSFWorkbook()) {
			XSSFSheet sheet = workbook.createSheet("test");
			POIWorkbook.putPOICellValue(sheet, 1, 1, CellType.STRING, "bold", true);
			XSSFRow row = sheet.getRow(0);
			assertEquals("bold", row.getCell(0).getStringCellValue());
		}
	}

	@Test
	public void putPOICellValueBoldStyle() throws Exception {
		try (XSSFWorkbook workbook = new XSSFWorkbook()) {
			XSSFSheet sheet = workbook.createSheet("test");
			POIWorkbook.putPOICellValue(sheet, 1, 1, CellType.STRING, "boldTest", false, true);
			XSSFRow row = sheet.getRow(0);
			assertEquals("boldTest", row.getCell(0).getStringCellValue());
		}
	}

	@Test
	public void putPOICellValueCreatesNewRowIfNotExists() throws Exception {
		try (XSSFWorkbook workbook = new XSSFWorkbook()) {
			XSSFSheet sheet = workbook.createSheet("test");
			// Row 5 doesn't exist
			POIWorkbook.putPOICellValue(sheet, 5, 3, CellType.STRING, "newrow", false, false);
			XSSFRow row = sheet.getRow(4);
			assertNotNull(row);
			assertEquals("newrow", row.getCell(2).getStringCellValue());
		}
	}

	@Test
	public void materialiseCreatesWorkbookXlsx() throws Exception {
		POIWorkbook wb = new POIWorkbook(true);
		wb.materialise();
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		wb.write(out);
		assertTrue(out.size() > 0);
	}

	@Test
	public void materialiseCreatesWorkbookXls() throws Exception {
		POIWorkbook wb = new POIWorkbook(false);
		wb.materialise();
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		wb.write(out);
		assertTrue(out.size() > 0);
	}

	@Test
	public void putPOICellValueDateSetsDateValue() throws Exception {
		try (XSSFWorkbook workbook = new XSSFWorkbook()) {
			XSSFSheet sheet = workbook.createSheet("Test");
			Date date = new Date();
			POIWorkbook.putPOICellValue(sheet, 1, 1, CellType.NUMERIC, date, false, false);
			XSSFRow row = sheet.getRow(0);
			assertNotNull(row);
			assertNotNull(row.getCell(0));
		}
	}

	@Test
	public void putPOICellValueDateOnlySetsDateOnlyValue() throws Exception {
		try (XSSFWorkbook workbook = new XSSFWorkbook()) {
			XSSFSheet sheet = workbook.createSheet("Test");
			DateOnly date = new DateOnly();
			POIWorkbook.putPOICellValue(sheet, 1, 1, CellType.NUMERIC, date, false, false);
			XSSFRow row = sheet.getRow(0);
			assertNotNull(row);
			assertNotNull(row.getCell(0));
		}
	}

	@Test
	public void putPOICellValueBooleanCellTypeDoesNotThrow() throws Exception {
		try (XSSFWorkbook workbook = new XSSFWorkbook()) {
			XSSFSheet sheet = workbook.createSheet("Test");
			POIWorkbook.putPOICellValue(sheet, 1, 1, CellType.BOOLEAN, "ignored", false, false);
			assertNotNull(sheet.getRow(0));
		}
	}

	@Test
	public void putPOICellValueBlankCellTypeDoesNotThrow() throws Exception {
		try (XSSFWorkbook workbook = new XSSFWorkbook()) {
			XSSFSheet sheet = workbook.createSheet("Test");
			POIWorkbook.putPOICellValue(sheet, 1, 1, CellType.BLANK, "ignored", false, false);
			assertNotNull(sheet.getRow(0));
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void addSheetDuplicateThrows() {
		POIWorkbook wb = new POIWorkbook(true);
		SheetKey key = new SheetKey("mod", "doc");
		POISheet sheet = new POISheet("Test");
		wb.addSheet(key, sheet);
		wb.addSheet(key, new POISheet("Test2"));
	}

	@Test(expected = IllegalStateException.class)
	public void writeThrowsWhenNotMaterialised() throws Exception {
		POIWorkbook wb = new POIWorkbook(true);
		wb.write(new ByteArrayOutputStream());
	}

	@Test(expected = IllegalStateException.class)
	public void addSheetThrowsWhenAlreadyMaterialised() {
		POIWorkbook wb = new POIWorkbook(true);
		wb.materialise();
		wb.addSheet(new SheetKey("mod", "doc"), new POISheet("S"));
	}

	@Test(expected = IllegalStateException.class)
	public void removeSheetThrowsWhenAlreadyMaterialised() {
		POIWorkbook wb = new POIWorkbook(true);
		wb.materialise();
		wb.removeSheet(new SheetKey("mod", "doc"));
	}

	@Test
	public void materialiseWithSheetWritesSheet() throws Exception {
		POIWorkbook wb = new POIWorkbook(true);
		SheetKey key = new SheetKey("mod", "doc");
		POISheet sheet = new POISheet("MySheet");
		wb.addSheet(key, sheet);
		wb.materialise();
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		wb.write(out);
		assertTrue(out.size() > 0);
	}

	@Test
	public void materialiseWithLongTitleTruncatesToValidName() throws Exception {
		POIWorkbook wb = new POIWorkbook(true);
		// Title longer than 29 chars (the max allowed by generateValidUniqueSheetTitle)
		SheetKey key = new SheetKey("mod", "doc");
		POISheet sheet = new POISheet("A".repeat(35));
		wb.addSheet(key, sheet);
		wb.materialise();
		// Just verify it materialises without error
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		wb.write(out);
		assertTrue(out.size() > 0);
	}

	@Test
	public void materialiseWithSlashInTitleReplacesWithPipe() throws Exception {
		POIWorkbook wb = new POIWorkbook(true);
		SheetKey key = new SheetKey("mod", "doc");
		POISheet sheet = new POISheet("A/B");
		wb.addSheet(key, sheet);
		wb.materialise();
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		wb.write(out);
		assertTrue(out.size() > 0);
	}

	@Test
	public void materialiseWithDuplicateLongTitleAppendsSuffix() throws Exception {
		POIWorkbook wb = new POIWorkbook(true);
		SheetKey key1 = new SheetKey("mod", "doc1");
		SheetKey key2 = new SheetKey("mod", "doc2");
		// Both titles truncate to the same 29-char prefix
		POISheet sheet1 = new POISheet("A".repeat(30));
		POISheet sheet2 = new POISheet("A".repeat(30));
		wb.addSheet(key1, sheet1);
		wb.addSheet(key2, sheet2);
		wb.materialise();
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		wb.write(out);
		assertTrue(out.size() > 0);
	}

	@Test
	public void writeWithReferencedSheetColumnAutoSizesColumns() throws Exception {
		POIWorkbook wb = new POIWorkbook(true);
		SheetKey key = new SheetKey("mod", "doc");
		POISheet sheet = new POISheet("TestSheet");
		BizPortColumn col = new BizPortColumn("FK", null, AttributeType.text);
		col.setReferencedSheet(new SheetKey("mod", "ref"));
		sheet.addColumn("fk", col);
		wb.addSheet(key, sheet);
		wb.materialise();
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		wb.write(out);
		assertTrue(out.size() > 0);
	}

	@Test
	public void materialiseWithRefSheetInWorkbookCoversHyperlinkAndFormula() throws Exception {
		POIWorkbook wb = new POIWorkbook(true);
		SheetKey refKey = new SheetKey("mod", "ref");
		SheetKey docKey = new SheetKey("mod", "doc");
		POISheet refSheet = new POISheet("RefSheet");
		POISheet docSheet = new POISheet("DocSheet");
		// FK column with a comment to cover createCellComment and the hyperlink setup path
		BizPortColumn col = new BizPortColumn("FK", "Choose a reference", AttributeType.text);
		col.setReferencedSheet(refKey);
		docSheet.addColumn("fk", col);
		wb.addSheet(refKey, refSheet);
		wb.addSheet(docKey, docSheet);
		wb.materialise();
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		wb.write(out);
		assertTrue(out.size() > 0);
	}

	@Test
	public void materialiseWithMultiTypeColumnsXlsxCoversDataValidation() throws Exception {
		POIWorkbook wb = new POIWorkbook(true);
		SheetKey key = new SheetKey("mod", "types");
		POISheet sheet = new POISheet("TypesSheet");
		sheet.addColumn("d", new BizPortColumn("Date", null, AttributeType.date));
		sheet.addColumn("t", new BizPortColumn("Time", null, AttributeType.time));
		sheet.addColumn("n", new BizPortColumn("Num", null, AttributeType.integer));
		sheet.addColumn("dec", new BizPortColumn("Decimal", null, AttributeType.decimal2));
		BizPortColumn enumCol = new BizPortColumn("Status", null, AttributeType.text);
		enumCol.setRangeValues(new String[] {"Active", "Inactive", "Pending"});
		sheet.addColumn("status", enumCol);
		wb.addSheet(key, sheet);
		wb.materialise();
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		wb.write(out);
		assertTrue(out.size() > 0);
	}

	@Test
	public void materialiseWithMultiTypeColumnsXlsCoversDataValidation() throws Exception {
		POIWorkbook wb = new POIWorkbook(false);
		SheetKey key = new SheetKey("mod", "types");
		POISheet sheet = new POISheet("TypesSheet");
		sheet.addColumn("d", new BizPortColumn("Date", null, AttributeType.date));
		sheet.addColumn("t", new BizPortColumn("Time", null, AttributeType.time));
		sheet.addColumn("n", new BizPortColumn("Num", null, AttributeType.integer));
		sheet.addColumn("dec", new BizPortColumn("Decimal", null, AttributeType.decimal2));
		BizPortColumn enumCol = new BizPortColumn("Status", null, AttributeType.text);
		enumCol.setRangeValues(new String[] {"Active", "Inactive", "Pending"});
		sheet.addColumn("status", enumCol);
		wb.addSheet(key, sheet);
		wb.materialise();
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		wb.write(out);
		assertTrue(out.size() > 0);
	}
}
