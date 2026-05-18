package org.skyve.impl.bizport;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.Test;
import org.skyve.bizport.BizPortWorkbook.BizPortFormat;
import org.skyve.bizport.SheetKey;

@SuppressWarnings("static-method")
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
		assertNull(wb.getSheet(new SheetKey("mod", "doc", null)));
	}

	@Test
	public void removeSheetReturnsNullForMissingKey() {
		POIWorkbook wb = new POIWorkbook(true);
		assertNull(wb.removeSheet(new SheetKey("mod", "doc", null)));
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

	@Test(expected = IllegalArgumentException.class)
	public void addSheetDuplicateThrows() {
		POIWorkbook wb = new POIWorkbook(true);
		SheetKey key = new SheetKey("mod", "doc", null);
		POISheet sheet = new POISheet("Test");
		wb.addSheet(key, sheet);
		wb.addSheet(key, new POISheet("Test2"));
	}
}
