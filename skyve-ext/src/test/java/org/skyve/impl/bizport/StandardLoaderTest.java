package org.skyve.impl.bizport;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Test;
import org.skyve.bizport.BizPortColumn;
import org.skyve.bizport.BizPortSheet;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.bizport.SheetKey;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageException;
import org.skyve.domain.messages.UploadException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

@SuppressWarnings("static-method")
public class StandardLoaderTest {

	private static BizPortWorkbook mockWorkbook() {
		BizPortWorkbook wb = mock(BizPortWorkbook.class);
		when(wb.getSheetKeys()).thenReturn(new HashSet<>());
		return wb;
	}

	@Test
	public void constructorSetsWorkbookAndProblems() {
		BizPortWorkbook wb = mockWorkbook();
		UploadException problems = new UploadException();
		StandardLoader loader = new StandardLoader(wb, problems);
		assertNotNull(loader);
	}

	@Test
	public void getBeanKeysEmptyAfterConstruction() {
		StandardLoader loader = new StandardLoader(mockWorkbook(), new UploadException());
		Set<String> keys = new HashSet<>();
		loader.getBeanKeys().forEach(keys::add);
		assertTrue(keys.isEmpty());
	}

	@Test
	public void getBeanReturnsNullForUnknownKey() {
		StandardLoader loader = new StandardLoader(mockWorkbook(), new UploadException());
		assertNull(loader.getBean("unknown.key"));
	}

	@Test
	public void putBeanAndGetBeanRoundTrips() {
		StandardLoader loader = new StandardLoader(mockWorkbook(), new UploadException());
		Bean bean = mock(Bean.class);
		loader.putBean("test.module.doc.1", bean);
		assertEquals(bean, loader.getBean("test.module.doc.1"));
	}

	@Test
	public void putMultipleBeansAndGetEach() {
		StandardLoader loader = new StandardLoader(mockWorkbook(), new UploadException());
		Bean bean1 = mock(Bean.class);
		Bean bean2 = mock(Bean.class);
		loader.putBean("key1", bean1);
		loader.putBean("key2", bean2);
		assertEquals(bean1, loader.getBean("key1"));
		assertEquals(bean2, loader.getBean("key2"));
	}

	@Test
	public void getBeanKeysReflectsAddedBeans() {
		StandardLoader loader = new StandardLoader(mockWorkbook(), new UploadException());
		Bean bean = mock(Bean.class);
		loader.putBean("module.doc.id", bean);
		Set<String> keys = new HashSet<>();
		loader.getBeanKeys().forEach(keys::add);
		assertTrue(keys.contains("module.doc.id"));
	}

	@Test
	public void getSheetRowIdFromBizIdReturnsNullForUnknown() {
		StandardLoader loader = new StandardLoader(mockWorkbook(), new UploadException());
		assertNull(loader.getSheetRowIdFromBizId("nonexistent-bizid"));
	}

	@Test
	public void createSheetKeyFormatsAsModuleDocumentSheetId() {
		Document doc = mock(Document.class);
		when(doc.getOwningModuleName()).thenReturn("testModule");
		when(doc.getName()).thenReturn("TestDocument");
		String key = StandardLoader.createSheetKey(doc, "row1");
		assertEquals("testModule.TestDocument.row1", key);
	}

	@Test
	public void createSheetKeyWithIntegerSheetId() {
		Document doc = mock(Document.class);
		when(doc.getOwningModuleName()).thenReturn("admin");
		when(doc.getName()).thenReturn("User");
		String key = StandardLoader.createSheetKey(doc, Integer.valueOf(42));
		assertEquals("admin.User.42", key);
	}

	@Test
	public void createSheetKeyWithEmptyModuleAndDocument() {
		Document doc = mock(Document.class);
		when(doc.getOwningModuleName()).thenReturn("");
		when(doc.getName()).thenReturn("");
		String key = StandardLoader.createSheetKey(doc, "id");
		assertEquals("..id", key);
	}

	@Test
	public void populateCollectionFromRowAddsErrorWhenOwnerIsEmptyButElementExists() throws Exception {
		UploadException problems = new UploadException();
		StandardLoader loader = new StandardLoader(mockWorkbook(), problems);
		BizPortSheet sheet = mock(BizPortSheet.class);
		BizPortColumn ownerColumn = new BizPortColumn("Owner", null);
		BizPortColumn elementColumn = new BizPortColumn("Element", null);
		when(sheet.getValue(PersistentBean.OWNER_COLUMN_NAME, AttributeType.text, problems)).thenReturn(null);
		when(sheet.getValue(PersistentBean.ELEMENT_COLUMN_NAME, AttributeType.text, problems)).thenReturn("E1");
		when(sheet.getColumn(PersistentBean.OWNER_COLUMN_NAME)).thenReturn(ownerColumn);
		when(sheet.getColumn(PersistentBean.ELEMENT_COLUMN_NAME)).thenReturn(elementColumn);

		invokePopulateCollectionFromRow(loader, "items", document("sales", "Order"), document("sales", "Line"), sheet);

		verify(sheet).addErrorAtCurrentRow(eq(problems), eq(ownerColumn), contains("owner cell is empty"));
	}

	@Test
	public void populateCollectionFromRowAddsErrorWhenElementIsEmpty() throws Exception {
		UploadException problems = new UploadException();
		StandardLoader loader = new StandardLoader(mockWorkbook(), problems);
		BizPortSheet sheet = mock(BizPortSheet.class);
		BizPortColumn ownerColumn = new BizPortColumn("Owner", null);
		BizPortColumn elementColumn = new BizPortColumn("Element", null);
		when(sheet.getValue(PersistentBean.OWNER_COLUMN_NAME, AttributeType.text, problems)).thenReturn("O1");
		when(sheet.getValue(PersistentBean.ELEMENT_COLUMN_NAME, AttributeType.text, problems)).thenReturn(null);
		when(sheet.getColumn(PersistentBean.OWNER_COLUMN_NAME)).thenReturn(ownerColumn);
		when(sheet.getColumn(PersistentBean.ELEMENT_COLUMN_NAME)).thenReturn(elementColumn);

		invokePopulateCollectionFromRow(loader, "items", document("sales", "Order"), document("sales", "Line"), sheet);

		verify(sheet).addErrorAtCurrentRow(eq(problems), eq(elementColumn), contains("element cell is empty"));
	}

	@Test
	public void populateCollectionFromRowAddsErrorWhenOwnerReferencedSheetIsMissing() throws Exception {
		UploadException problems = new UploadException();
		BizPortWorkbook workbook = mockWorkbook();
		StandardLoader loader = new StandardLoader(workbook, problems);
		BizPortSheet sheet = collectionSheet(problems, "O1", "E1");
		BizPortColumn ownerColumn = sheet.getColumn(PersistentBean.OWNER_COLUMN_NAME);
		SheetKey ownerKey = new SheetKey("sales", "Order");
		ownerColumn.setReferencedSheet(ownerKey);
		when(workbook.getSheet(ownerKey)).thenReturn(null);

		invokePopulateCollectionFromRow(loader, "items", document("sales", "Order"), document("sales", "Line"), sheet);

		verify(sheet).addErrorAtCurrentRow(eq(problems), eq(ownerColumn), contains("referenced sheet for owner_id does not exist"));
	}

	@Test
	public void populateCollectionFromRowWarnsWhenElementReferencedSheetIsMissing() throws Exception {
		UploadException problems = new UploadException();
		BizPortWorkbook workbook = mockWorkbook();
		StandardLoader loader = new StandardLoader(workbook, problems);
		Document ownerDocument = document("sales", "Order");
		Document elementDocument = document("sales", "Line");
		BizPortSheet sheet = collectionSheet(problems, "O1", "E1");
		BizPortColumn ownerColumn = sheet.getColumn(PersistentBean.OWNER_COLUMN_NAME);
		BizPortColumn elementColumn = sheet.getColumn(PersistentBean.ELEMENT_COLUMN_NAME);
		SheetKey ownerKey = new SheetKey("sales", "Order");
		SheetKey elementKey = new SheetKey("sales", "Line");
		BizPortSheet ownerSheet = mock(BizPortSheet.class);
		ownerColumn.setReferencedSheet(ownerKey);
		elementColumn.setReferencedSheet(elementKey);
		when(workbook.getSheet(ownerKey)).thenReturn(ownerSheet);
		when(workbook.getSheet(elementKey)).thenReturn(null);
		putMappedBean(loader, ownerDocument, "O1", mock(Bean.class));

		invokePopulateCollectionFromRow(loader, "items", ownerDocument, elementDocument, sheet);

		verify(sheet).addWarningAtCurrentRow(eq(problems), eq(elementColumn), contains("referenced sheet for element_id does not exist"));
	}

	@Test
	public void linkParentFromRowIgnoresEmptyRows() throws Exception {
		UploadException problems = new UploadException();
		StandardLoader loader = new StandardLoader(mockWorkbook(), problems);
		BizPortSheet sheet = parentLinkSheet(problems, null, null);

		invokeLinkParentFromRow(loader, sheet, mock(Customer.class), document("sales", "Line"), "lines");

		verify(sheet).getValue(ChildBean.PARENT_NAME, AttributeType.text, problems);
		verify(sheet).getValue(Bean.DOCUMENT_ID, AttributeType.text, problems);
	}

	@Test
	public void linkParentFromRowAddsErrorWhenParentIdMissing() throws Exception {
		UploadException problems = new UploadException();
		StandardLoader loader = new StandardLoader(mockWorkbook(), problems);
		BizPortSheet sheet = parentLinkSheet(problems, null, "C1");
		BizPortColumn parentColumn = sheet.getColumn(ChildBean.PARENT_NAME);

		invokeLinkParentFromRow(loader, sheet, mock(Customer.class), document("sales", "Line"), "lines");

		verify(sheet).addErrorAtCurrentRow(eq(problems), eq(parentColumn), contains("No parent ID value"));
	}

	@Test
	public void linkParentFromRowAddsErrorWhenChildIdMissing() throws Exception {
		UploadException problems = new UploadException();
		StandardLoader loader = new StandardLoader(mockWorkbook(), problems);
		BizPortSheet sheet = parentLinkSheet(problems, "P1", null);
		BizPortColumn childColumn = sheet.getColumn(Bean.DOCUMENT_ID);

		invokeLinkParentFromRow(loader, sheet, mock(Customer.class), document("sales", "Line"), "lines");

		verify(sheet).addErrorAtCurrentRow(eq(problems), eq(childColumn), contains("No child ID value"));
	}

	@Test
	public void linkParentFromRowWarnsWhenReferencedParentSheetMissing() throws Exception {
		UploadException problems = new UploadException();
		BizPortWorkbook workbook = mockWorkbook();
		StandardLoader loader = new StandardLoader(workbook, problems);
		BizPortSheet sheet = parentLinkSheet(problems, "P1", "C1");
		BizPortColumn parentColumn = sheet.getColumn(ChildBean.PARENT_NAME);
		SheetKey parentKey = new SheetKey("sales", "Order");
		parentColumn.setReferencedSheet(parentKey);
		when(workbook.getSheet(parentKey)).thenReturn(null);

		invokeLinkParentFromRow(loader, sheet, mock(Customer.class), document("sales", "Line"), "lines");

		verify(sheet).addWarningAtCurrentRow(eq(problems), eq(parentColumn), contains("parent sheet does not exist"));
	}

	@Test
	public void linkParentFromRowAddsErrorWhenParentRowMissing() throws Exception {
		UploadException problems = new UploadException();
		BizPortWorkbook workbook = mockWorkbook();
		StandardLoader loader = new StandardLoader(workbook, problems);
		BizPortSheet sheet = parentLinkSheet(problems, "P1", "C1");
		BizPortColumn parentColumn = sheet.getColumn(ChildBean.PARENT_NAME);
		SheetKey parentKey = new SheetKey("sales", "Order");
		parentColumn.setReferencedSheet(parentKey);
		when(workbook.getSheet(parentKey)).thenReturn(mock(BizPortSheet.class));
		Customer customer = mock(Customer.class);
		Document childDocument = document("sales", "Line");
		Document parentDocument = document("sales", "Order");
		when(childDocument.getParentDocument(customer)).thenReturn(parentDocument);

		invokeLinkParentFromRow(loader, sheet, customer, childDocument, "lines");

		verify(sheet).addErrorAtCurrentRow(eq(problems), eq(parentColumn), contains("parent ID P1"));
	}

	@Test
	public void linkParentFromRowAddsErrorWhenChildRowMissing() throws Exception {
		UploadException problems = new UploadException();
		BizPortWorkbook workbook = mockWorkbook();
		StandardLoader loader = new StandardLoader(workbook, problems);
		BizPortSheet sheet = parentLinkSheet(problems, "P1", "C1");
		BizPortColumn parentColumn = sheet.getColumn(ChildBean.PARENT_NAME);
		BizPortColumn childColumn = sheet.getColumn(Bean.DOCUMENT_ID);
		SheetKey parentKey = new SheetKey("sales", "Order");
		parentColumn.setReferencedSheet(parentKey);
		when(workbook.getSheet(parentKey)).thenReturn(mock(BizPortSheet.class));
		Customer customer = mock(Customer.class);
		Document childDocument = document("sales", "Line");
		Document parentDocument = document("sales", "Order");
		when(childDocument.getParentDocument(customer)).thenReturn(parentDocument);
		putMappedBean(loader, parentDocument, "P1", mock(Bean.class));

		invokeLinkParentFromRow(loader, sheet, customer, childDocument, "lines");

		verify(sheet).addErrorAtCurrentRow(eq(problems), eq(childColumn), contains("child ID C1"));
	}

	@Test
	public void linkHierarchyFromRowAddsErrorWhenCurrentIdMissing() throws Exception {
		UploadException problems = new UploadException();
		StandardLoader loader = new StandardLoader(mockWorkbook(), problems);
		BizPortSheet sheet = hierarchySheet(problems, "P1", null);
		BizPortColumn idColumn = sheet.getColumn(Bean.DOCUMENT_ID);

		invokeLinkHierarchyFromRow(loader, sheet, document("sales", "Category"));

		verify(sheet).addErrorAtCurrentRow(eq(problems), eq(idColumn), contains("No ID value"));
	}

	@Test
	public void linkHierarchyFromRowAddsErrorWhenCurrentRowMissing() throws Exception {
		UploadException problems = new UploadException();
		StandardLoader loader = new StandardLoader(mockWorkbook(), problems);
		BizPortSheet sheet = hierarchySheet(problems, "P1", "C1");
		BizPortColumn idColumn = sheet.getColumn(Bean.DOCUMENT_ID);

		invokeLinkHierarchyFromRow(loader, sheet, document("sales", "Category"));

		verify(sheet).addErrorAtCurrentRow(eq(problems), eq(idColumn), contains("ID C1"));
	}

	@Test
	public void linkHierarchyFromRowSetsParentIdWhenBeanExists() throws Exception {
		UploadException problems = new UploadException();
		StandardLoader loader = new StandardLoader(mockWorkbook(), problems);
		BizPortSheet sheet = hierarchySheet(problems, "P1", "C1");
		Document document = document("sales", "Category");
		HierarchicalBean<Bean> bean = mock(HierarchicalBean.class);
		putMappedBean(loader, document, "C1", bean);

		invokeLinkHierarchyFromRow(loader, sheet, document);

		verify(bean).setBizParentId("P1");
	}

	@Test
	public void linkAssociationsFromRowWarnsWhenReferencedSheetMissing() throws Exception {
		UploadException problems = new UploadException();
		BizPortWorkbook workbook = mockWorkbook();
		StandardLoader loader = new StandardLoader(workbook, problems);
		BizPortSheet sheet = associationSheet(problems, "customer", "C1");
		BizPortColumn column = sheet.getColumn("customer");
		SheetKey customerKey = new SheetKey("sales", "Customer");
		column.setReferencedSheet(customerKey);
		when(workbook.getSheet(customerKey)).thenReturn(null);
		Association association = association("customer", "Customer");

		invokeLinkAssociationsFromRow(loader, sheet, mock(Customer.class), mock(Module.class), mock(Bean.class), List.of(association));

		verify(sheet).addWarningAtCurrentRow(eq(problems), eq(column), contains("referenced sheet for customer"));
	}

	@Test
	public void linkAssociationsFromRowAddsErrorWhenReferencedRowMissing() throws Exception {
		UploadException problems = new UploadException();
		BizPortWorkbook workbook = mockWorkbook();
		StandardLoader loader = new StandardLoader(workbook, problems);
		BizPortSheet sheet = associationSheet(problems, "customer", "C1");
		BizPortColumn column = sheet.getColumn("customer");
		SheetKey customerKey = new SheetKey("sales", "Customer");
		column.setReferencedSheet(customerKey);
		BizPortSheet customerSheet = mock(BizPortSheet.class);
		when(customerSheet.getTitle()).thenReturn("Customer");
		when(workbook.getSheet(customerKey)).thenReturn(customerSheet);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document customerDocument = document("sales", "Customer");
		when(module.getDocument(customer, "Customer")).thenReturn(customerDocument);
		Association association = association("customer", "Customer");

		invokeLinkAssociationsFromRow(loader, sheet, customer, module, mock(Bean.class), List.of(association));

		verify(sheet).addErrorAtCurrentRow(eq(problems), eq(column), contains("referencedRowDoesNotExist"));
	}

	@Test
	public void addErrorMapsPlainExceptionToOriginalSheetRow() throws Exception {
		UploadException problems = new UploadException();
		BizPortWorkbook workbook = mockWorkbook();
		StandardLoader loader = new StandardLoader(workbook, problems);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = document("sales", "Order");
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		Bean bean = mock(Bean.class);
		when(bean.getBizId()).thenReturn("B1");
		BizPortSheet sheet = mock(BizPortSheet.class);
		BizPortColumn column = new BizPortColumn("ID", null);
		SheetKey key = new SheetKey("sales", "Order");
		when(workbook.getSheet(key)).thenReturn(sheet);
		when(sheet.getColumn(Bean.DOCUMENT_ID)).thenReturn(column);
		putPrivateMapValue(loader, "bizIdToSheetKey", "B1", key);
		putPrivateMapValue(loader, "bizIdToSheetRowId", "B1", "row-1");

		loader.addError(customer, bean, new IllegalStateException("plain failure"));

		verify(sheet).moveToRow("row-1");
		verify(sheet).addErrorAtCurrentRow(problems, column, "plain failure");
	}

	@Test
	public void addErrorMapsSimpleMessageBindingToMatchingColumn() throws Exception {
		UploadException problems = new UploadException();
		BizPortWorkbook workbook = mockWorkbook();
		StandardLoader loader = new StandardLoader(workbook, problems);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = document("sales", "Order");
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		Bean bean = mock(Bean.class);
		when(bean.getBizId()).thenReturn("B2");
		when(bean.getBizModule()).thenReturn("sales");
		when(bean.getBizDocument()).thenReturn("Order");
		BizPortSheet sheet = mock(BizPortSheet.class);
		BizPortColumn idColumn = new BizPortColumn("ID", null);
		BizPortColumn nameColumn = new BizPortColumn("Name", null);
		SheetKey key = new SheetKey("sales", "Order");
		when(workbook.getSheet(key)).thenReturn(sheet);
		when(sheet.getColumn(Bean.DOCUMENT_ID)).thenReturn(idColumn);
		when(sheet.getColumn("name")).thenReturn(nameColumn);
		putPrivateMapValue(loader, "bizIdToSheetKey", "B2", key);
		putPrivateMapValue(loader, "bizIdToSheetRowId", "B2", "row-2");

		loader.addError(customer, bean, new TestMessageException(new Message("name", "Name is required")));

		verify(sheet).moveToRow("row-2");
		verify(sheet).addErrorAtCurrentRow(problems, nameColumn, "Name is required");
	}

	@Test
	public void addErrorFallsBackToIdColumnWhenMessageHasNoBindings() throws Exception {
		UploadException problems = new UploadException();
		BizPortWorkbook workbook = mockWorkbook();
		StandardLoader loader = new StandardLoader(workbook, problems);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = document("sales", "Order");
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		Bean bean = mock(Bean.class);
		when(bean.getBizId()).thenReturn("B3");
		when(bean.getBizModule()).thenReturn("sales");
		when(bean.getBizDocument()).thenReturn("Order");
		BizPortSheet sheet = mock(BizPortSheet.class);
		BizPortColumn idColumn = new BizPortColumn("ID", null);
		SheetKey key = new SheetKey("sales", "Order");
		when(workbook.getSheet(key)).thenReturn(sheet);
		when(sheet.getColumn(Bean.DOCUMENT_ID)).thenReturn(idColumn);
		putPrivateMapValue(loader, "bizIdToSheetKey", "B3", key);
		putPrivateMapValue(loader, "bizIdToSheetRowId", "B3", "row-3");

		loader.addError(customer, bean, new TestMessageException(new Message("General failure")));

		verify(sheet).moveToRow("row-3");
		verify(sheet).addErrorAtCurrentRow(problems, idColumn, "General failure");
	}

	private static Document document(String moduleName, String documentName) {
		Document document = mock(Document.class);
		when(document.getOwningModuleName()).thenReturn(moduleName);
		when(document.getName()).thenReturn(documentName);
		return document;
	}

	private static BizPortSheet collectionSheet(UploadException problems, String ownerId, String elementId) {
		BizPortSheet sheet = mock(BizPortSheet.class);
		BizPortColumn ownerColumn = new BizPortColumn("Owner", null);
		BizPortColumn elementColumn = new BizPortColumn("Element", null);
		when(sheet.getValue(PersistentBean.OWNER_COLUMN_NAME, AttributeType.text, problems)).thenReturn(ownerId);
		when(sheet.getValue(PersistentBean.ELEMENT_COLUMN_NAME, AttributeType.text, problems)).thenReturn(elementId);
		when(sheet.getColumn(PersistentBean.OWNER_COLUMN_NAME)).thenReturn(ownerColumn);
		when(sheet.getColumn(PersistentBean.ELEMENT_COLUMN_NAME)).thenReturn(elementColumn);
		return sheet;
	}

	private static BizPortSheet parentLinkSheet(UploadException problems, String parentId, String childId) {
		BizPortSheet sheet = mock(BizPortSheet.class);
		BizPortColumn parentColumn = new BizPortColumn("Parent", null);
		BizPortColumn childColumn = new BizPortColumn("ID", null);
		when(sheet.getValue(ChildBean.PARENT_NAME, AttributeType.text, problems)).thenReturn(parentId);
		when(sheet.getValue(Bean.DOCUMENT_ID, AttributeType.text, problems)).thenReturn(childId);
		when(sheet.getColumn(ChildBean.PARENT_NAME)).thenReturn(parentColumn);
		when(sheet.getColumn(Bean.DOCUMENT_ID)).thenReturn(childColumn);
		return sheet;
	}

	private static BizPortSheet hierarchySheet(UploadException problems, String parentId, String rowId) {
		BizPortSheet sheet = mock(BizPortSheet.class);
		BizPortColumn parentColumn = new BizPortColumn("Parent ID", null);
		BizPortColumn idColumn = new BizPortColumn("ID", null);
		when(sheet.getValue(HierarchicalBean.PARENT_ID, AttributeType.text, problems)).thenReturn(parentId);
		when(sheet.getValue(Bean.DOCUMENT_ID, AttributeType.text, problems)).thenReturn(rowId);
		when(sheet.getColumn(HierarchicalBean.PARENT_ID)).thenReturn(parentColumn);
		when(sheet.getColumn(Bean.DOCUMENT_ID)).thenReturn(idColumn);
		return sheet;
	}

	private static BizPortSheet associationSheet(UploadException problems, String binding, String foreignKeyId) {
		BizPortSheet sheet = mock(BizPortSheet.class);
		BizPortColumn column = new BizPortColumn(binding, null);
		when(sheet.getValue(binding, AttributeType.text, problems)).thenReturn(foreignKeyId);
		when(sheet.getColumn(binding)).thenReturn(column);
		return sheet;
	}

	private static Association association(String name, String documentName) {
		Association association = mock(Association.class);
		when(association.getName()).thenReturn(name);
		when(association.getDocumentName()).thenReturn(documentName);
		return association;
	}

	private static void invokePopulateCollectionFromRow(StandardLoader loader,
															String collectionBinding,
															Document ownerDocument,
															Document elementDocument,
															BizPortSheet sheet) throws Exception {
		Method method = StandardLoader.class.getDeclaredMethod("populateCollectionFromRow", String.class, Document.class, Document.class, BizPortSheet.class);
		method.setAccessible(true);
		method.invoke(loader, collectionBinding, ownerDocument, elementDocument, sheet);
	}

	private static void invokeLinkParentFromRow(StandardLoader loader,
													BizPortSheet sheet,
													Customer customer,
													Document childDocument,
													String collectionBinding) throws Exception {
		Method method = StandardLoader.class.getDeclaredMethod("linkParentFromRow", BizPortSheet.class, Customer.class, Document.class, String.class);
		method.setAccessible(true);
		method.invoke(loader, sheet, customer, childDocument, collectionBinding);
	}

	private static void invokeLinkHierarchyFromRow(StandardLoader loader, BizPortSheet sheet, Document childDocument) throws Exception {
		Method method = StandardLoader.class.getDeclaredMethod("linkHierarchyFromRow", BizPortSheet.class, Document.class);
		method.setAccessible(true);
		method.invoke(loader, sheet, childDocument);
	}

	private static void invokeLinkAssociationsFromRow(StandardLoader loader,
														BizPortSheet sheet,
														Customer customer,
														Module module,
														Bean masterBean,
														List<Association> associations) throws Exception {
		Method method = StandardLoader.class.getDeclaredMethod("linkAssociationsFromRow", BizPortSheet.class, Customer.class, Module.class, Bean.class, List.class);
		method.setAccessible(true);
		method.invoke(loader, sheet, customer, module, masterBean, associations);
	}

	private static void putMappedBean(StandardLoader loader, Document document, Object sheetId, Bean bean) throws Exception {
		putPrivateMapValue(loader, "beansBySheetKey", StandardLoader.createSheetKey(document, sheetId), bean);
	}

	@SuppressWarnings("unchecked")
	private static void putPrivateMapValue(StandardLoader loader, String fieldName, Object key, Object value) throws Exception {
		Field field = StandardLoader.class.getDeclaredField(fieldName);
		field.setAccessible(true);
		((Map<Object, Object>) field.get(loader)).put(key, value);
	}

	private static final class TestMessageException extends RuntimeException implements MessageException {
		private static final long serialVersionUID = 1L;
		private final List<Message> messages;

		private TestMessageException(Message... messages) {
			this.messages = List.of(messages);
		}

		@Override
		public List<Message> getMessages() {
			return messages;
		}
	}
}
