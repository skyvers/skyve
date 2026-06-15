package org.skyve.impl.bizport;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Date;

import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

@SuppressWarnings({"static-method", "unchecked", "boxing"})
class POISheetLoaderTest {
	@AfterEach
	void tearDown() throws Exception {
		unbindPersistenceFromThread();
	}

	@Test
	void readsStringsNumbersDatesWhereAndNoData() throws Exception {
		bindPersistenceWithCustomer(customer(null));
		Date when = new Date(1_700_000_000_000L);
		POISheetLoader loader = loader(workbookBytes(row -> {
			row.createCell(0).setCellValue("Alice");
			row.createCell(1).setCellValue(12.5);
			row.createCell(3).setCellValue(when);
		}));
		loader.addField(fieldAt(0));

		assertThat(loader.getWhere(1), is("Sheet Data Row 1 column B."));

		loader.nextData();

		assertThat(loader.isNoData(), is(false));
		assertThat(loader.getStringFieldValue(0, true), is("Alice"));
		assertThat(loader.getNumericFieldValue(1, false), is(Double.valueOf(12.5)));
		assertNull(loader.getNumericFieldValue(2, false));
		assertThat(loader.getNumericFieldValue(2, true), is(Double.valueOf(0)));
		assertThat(loader.getDateFieldValue(3), is(when));
	}

	@Test
	void invalidNumericCellThrowsDomainExceptionWithCellType() throws Exception {
		bindPersistenceWithCustomer(customer(null));
		POISheetLoader loader = loader(workbookBytes(row -> row.createCell(0).setCellValue("not numeric")));
		loader.nextData();

		DomainException e = assertThrows(DomainException.class, () -> loader.getNumericFieldValue(0, false));
		assertThat(e.getMessage(), containsString("String value 'not numeric' is not numeric"));
	}

	@Test
	void stringDateFallsBackToCustomerConverter() throws Exception {
		Converter<DateTime> converter = mock(Converter.class);
		DateTime expected = new DateTime(1_700_000_000_000L);
		when(converter.fromDisplayValue("display date")).thenReturn(expected);
		bindPersistenceWithCustomer(customer(converter));
		POISheetLoader loader = loader(workbookBytes(row -> row.createCell(0).setCellValue("display date")));
		loader.nextData();

		assertThat(loader.getDateFieldValue(0), is(expected));
	}

	@Test
	void worksheetColumnNamesUseExcelStyleLetters() {
		assertThat(POISheetLoader.getPOIWorksheetColumnName(0), is("A"));
		assertThat(POISheetLoader.getPOIWorksheetColumnName(25), is("Z"));
		assertThat(POISheetLoader.getPOIWorksheetColumnName(26), is("AA"));
		assertThat(POISheetLoader.getPOIWorksheetColumnName(701), is("ZZ"));
	}

	@Test
	void convenienceConstructorSetsActivityTypeAndNextDataAdvancesRows() throws Exception {
		bindPersistenceWithCustomer(customer(null));
		POISheetLoader loader = new POISheetLoader(AbstractDataFileLoader.LoaderActivityType.FIND,
				new ByteArrayInputStream(workbookBytes(row -> row.createCell(0).setCellValue("first"))),
				0,
				new UploadException(),
				"sales",
				"Order");

		assertThat(activityType(loader), is(AbstractDataFileLoader.LoaderActivityType.FIND));
		assertTrue(loader.hasNextData());

		loader.nextData();
		assertThat(loader.getStringFieldValue(0, true), is("first"));

		loader.nextData();
		assertTrue(loader.isNoData());
		assertThat(loader.getStringFieldValue(0, false), is(""));
	}

	@Test
	void isNoDataDefaultsMissingFieldIndexToFirstColumn() throws Exception {
		bindPersistenceWithCustomer(customer(null));
		POISheetLoader loader = loader(workbookBytes(row -> row.createCell(0).setCellValue("value")));
		loader.addField(new DataFileField(null));
		loader.nextData();

		assertFalse(loader.isNoData());
	}

	@Test
	void poiCellTypeDescriptionsCoverCommonCellTypes() throws Exception {
		assertThat(invokeCellTypeDescription(null), is("unknown"));
		assertThat(invokeCellTypeDescription(cell(CellType.BOOLEAN, null)), is("Boolean"));
		assertThat(invokeCellTypeDescription(cell(CellType.NUMERIC, null)), is("Numeric"));
		assertThat(invokeCellTypeDescription(cell(CellType.BLANK, null)), is("Blank"));
		assertThat(invokeCellTypeDescription(cell(CellType.ERROR, null)), is("Error"));
		assertThat(invokeCellTypeDescription(cell(CellType.STRING, null)), is("String"));
		assertThat(invokeCellTypeDescription(cell(CellType.FORMULA, CellType.NUMERIC)), is("Numeric Formula"));
		assertThat(invokeCellTypeDescription(cell(CellType.FORMULA, CellType.STRING)), is("String Formula"));
		assertThat(invokeCellTypeDescription(cell(CellType.FORMULA, CellType.BOOLEAN)), is("Rich Text Formula"));
	}

	private static POISheetLoader loader(byte[] workbookBytes) throws Exception {
		return new POISheetLoader(new ByteArrayInputStream(workbookBytes), 0, "sales", "Order", new UploadException());
	}

	private static DataFileField fieldAt(int index) {
		DataFileField field = new DataFileField(null);
		field.setIndex(index);
		return field;
	}

	private static byte[] workbookBytes(RowConfigurer configurer) throws Exception {
		try (Workbook workbook = new XSSFWorkbook(); ByteArrayOutputStream out = new ByteArrayOutputStream()) {
			Sheet sheet = workbook.createSheet("Data");
			configurer.configure(sheet.createRow(0));
			workbook.write(out);
			return out.toByteArray();
		}
	}

	private static Customer customer(Converter<DateTime> converter) {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(customer.getModule("sales")).thenReturn(module);
		when(customer.getDefaultDateTimeConverter()).thenReturn(converter);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		return customer;
	}

	private static void bindPersistenceWithCustomer(Customer customer) throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		when(persistence.getUser()).thenReturn(user);
		bindPersistenceToThread(persistence);
	}

	private static void bindPersistenceToThread(AbstractPersistence persistence) throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).set(persistence);
	}

	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
	}

	private static AbstractDataFileLoader.LoaderActivityType activityType(POISheetLoader loader) throws Exception {
		Field field = AbstractDataFileLoader.class.getDeclaredField("activityType");
		field.setAccessible(true);
		return (AbstractDataFileLoader.LoaderActivityType) field.get(loader);
	}

	private static Cell cell(CellType cellType, CellType cachedFormulaResultType) {
		Cell cell = mock(Cell.class);
		when(cell.getCellType()).thenReturn(cellType);
		if (cachedFormulaResultType != null) {
			when(cell.getCachedFormulaResultType()).thenReturn(cachedFormulaResultType);
		}
		return cell;
	}

	private static String invokeCellTypeDescription(Cell cell) throws Exception {
		Method method = POISheetLoader.class.getDeclaredMethod("getPOICellTypeDescription", Cell.class);
		method.setAccessible(true);
		return (String) method.invoke(null, cell);
	}

	@FunctionalInterface
	private interface RowConfigurer {
		void configure(Row row);
	}
}
