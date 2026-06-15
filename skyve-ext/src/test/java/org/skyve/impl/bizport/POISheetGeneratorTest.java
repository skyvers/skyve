package org.skyve.impl.bizport;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;

@SuppressWarnings({"static-method", "unchecked"})
class POISheetGeneratorTest {
	@AfterEach
	void tearDown() throws Exception {
		unbindPersistenceFromThread();
	}

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
	void addFieldObjectAppendsToList() {
		POISheetGenerator gen = new POISheetGenerator("m", "d");
		gen.addField(new DataFileExportField("Title", "binding"));
		assertEquals(1, gen.getFields().size());
	}

	@Test
	void addFieldStringAndBindingAppendsToList() {
		POISheetGenerator gen = new POISheetGenerator("m", "d");
		gen.addField("Title", "binding");
		assertEquals(1, gen.getFields().size());
	}

	@Test
	void addMultipleFieldsAccumulatesInOrder() {
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

	@Test
	void getDownloadReturnsNullWhenModuleOrDocumentMissing() throws Exception {
		assertNull(new POISheetGenerator(null, "Document").getDownload());
		assertNull(new POISheetGenerator("module", null).getDownload());
	}

	@Test
	void getDownloadCanGenerateTitleOnlyWorkbook() throws Exception {
		bindPersistenceWithCustomer(customer());
		POISheetGenerator gen = new POISheetGenerator("sales", "Order");
		gen.setDownloadName("orders");
		gen.setColumnTitlesOnly(Boolean.TRUE);
		gen.addField("Name", "name");
		gen.addField("Amount", "amount");

		Download download = gen.getDownload();

		assertEquals("orders.xlsx", download.getFileName());
		assertEquals(MimeType.xlsx, download.getMimeType());
		assertNotNull(download.getBytes());
		assertTrue(download.getBytes().length > 0);
	}

	@Test
	void getDownloadGeneratesRowsUsingFallbackBindingExpression() throws Exception {
		Bean bean = new DynamicBean("sales", "Order",
				new java.util.HashMap<>(java.util.Map.of(Bean.DOCUMENT_ID, "order-1")));
		bindPersistenceWithCustomer(customer(), List.of(bean));
		POISheetGenerator gen = new POISheetGenerator("sales", "Order");
		gen.setDownloadName("orders");
		gen.addField("Identifier", "Order {bizId}");

		Download download = gen.getDownload();

		try (XSSFWorkbook workbook = new XSSFWorkbook(new ByteArrayInputStream(download.getBytes()))) {
			assertEquals("Identifier", workbook.getSheetAt(0).getRow(0).getCell(0).getStringCellValue());
			assertEquals("Order order-1", workbook.getSheetAt(0).getRow(1).getCell(0).getStringCellValue());
		}
	}

	private static Customer customer() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		return customer;
	}

	private static void bindPersistenceWithCustomer(Customer customer) throws Exception {
		bindPersistenceWithCustomer(customer, List.of());
	}

	private static void bindPersistenceWithCustomer(Customer customer, List<Bean> beans) throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		DocumentQuery query = mock(DocumentQuery.class);
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		when(persistence.getUser()).thenReturn(user);
		when(persistence.newDocumentQuery("sales", "Order")).thenReturn(query);
		when(query.beanResults()).thenReturn(beans);
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).set(persistence);
	}

	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
	}
}
