package org.skyve.impl.bizport;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.skyve.bizport.BizPortColumn;

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
}
