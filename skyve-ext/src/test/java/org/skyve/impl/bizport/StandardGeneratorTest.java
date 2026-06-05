package org.skyve.impl.bizport;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.arrayContaining;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.skyve.bizport.BizPortColumn;
import org.skyve.bizport.BizPortSheet;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.bizport.SheetKey;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.module.Module;

@SuppressWarnings({"static-method", "boxing"})
class StandardGeneratorTest {
	@Test
	void generateDataAddsTopLevelBeanRowAndScalarValues() {
		Customer customer = mock(Customer.class);
		Document document = document("admin", "Communication", "Communication");
		Attribute subject = attribute("subject", "Subject", AttributeType.text, null);
		BizPortWorkbook workbook = mock(BizPortWorkbook.class);
		BizPortSheet sheet = mock(BizPortSheet.class);
		Bean bean = new DynamicBean("admin", "Communication",
				new java.util.HashMap<>(Map.of(Bean.DOCUMENT_ID, "c1", "subject", "Hello")));
		doReturn(List.<Attribute> of(subject)).when(document).getAllAttributes(customer);
		when(workbook.getSheet(new SheetKey("admin", "Communication"))).thenReturn(sheet);
		when(sheet.moveToRow("c1")).thenReturn(false);
		when(sheet.getColumnBindings()).thenReturn(Set.of(Bean.DOCUMENT_ID, "subject"));

		new StandardGenerator(customer, document).generateData(workbook, List.of(bean));

		verify(sheet).addRow("c1");
		verify(sheet).setValue(Bean.DOCUMENT_ID, "c1");
		verify(sheet).setValue("subject", "Hello");
	}

	@Test
	void generateDataAddsNonChildCollectionJoinRows() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document rootDocument = document("admin", "Communication", "Communication");
		Document tagDocument = document("admin", "Tag", "Tag");
		Collection collection = mock(Collection.class);
		BizPortWorkbook workbook = mock(BizPortWorkbook.class);
		BizPortSheet rootSheet = mock(BizPortSheet.class);
		BizPortSheet tagSheet = mock(BizPortSheet.class);
		BizPortSheet collectionSheet = mock(BizPortSheet.class);
		Bean tag = new DynamicBean("admin", "Tag",
				new java.util.HashMap<>(Map.of(Bean.DOCUMENT_ID, "t1")));
		Bean root = new DynamicBean("admin", "Communication",
				new java.util.HashMap<>(Map.of(Bean.DOCUMENT_ID, "c1", "tags", List.of(tag))));
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Tag")).thenReturn(tagDocument);
		doReturn(List.<Attribute> of(collection)).when(rootDocument).getAllAttributes(customer);
		when(collection.getAttributeType()).thenReturn(AttributeType.collection);
		when(collection.getName()).thenReturn("tags");
		when(collection.getDocumentName()).thenReturn("Tag");
		when(collection.getType()).thenReturn(CollectionType.aggregation);
		when(workbook.getSheet(new SheetKey("admin", "Communication"))).thenReturn(rootSheet);
		when(workbook.getSheet(new SheetKey("admin", "Tag"))).thenReturn(tagSheet);
		when(workbook.getSheet(new SheetKey("admin", "Communication", "tags"))).thenReturn(collectionSheet);
		when(rootSheet.moveToRow("c1")).thenReturn(false);
		when(rootSheet.getColumnBindings()).thenReturn(Set.of(Bean.DOCUMENT_ID));
		when(tagSheet.moveToRow("t1")).thenReturn(false);
		when(tagSheet.getColumnBindings()).thenReturn(Set.of(Bean.DOCUMENT_ID));

		new StandardGenerator(customer, rootDocument).generateData(workbook, List.of(root));

		verify(collectionSheet).addRow("c1t1");
		verify(collectionSheet).setValue(PersistentBean.OWNER_COLUMN_NAME, "c1");
		verify(collectionSheet).setValue(PersistentBean.ELEMENT_COLUMN_NAME, "t1");
		verify(tagSheet).addRow("t1");
	}

	@Test
	void generateStructureAddsDocumentAndCollectionSheetsForNullableGraph() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document rootDocument = document("admin", "Communication", "Communication");
		Document childDocument = document("admin", "Tag", "Tag");
		Collection collection = mock(Collection.class);
		BizPortWorkbook workbook = mock(BizPortWorkbook.class);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Tag")).thenReturn(childDocument);
		doReturn(List.<Attribute> of(collection)).when(rootDocument).getAllAttributes(customer);
		when(collection.getAttributeType()).thenReturn(AttributeType.collection);
		when(collection.getName()).thenReturn("tags");
		when(collection.getDocumentName()).thenReturn("Tag");
		when(collection.getType()).thenReturn(CollectionType.aggregation);
		when(collection.getLocalisedDisplayName()).thenReturn("Tags");

		new StandardGenerator(customer, rootDocument).generateStructure(workbook);

		verify(workbook).addSheet(eq(new SheetKey("admin", "Communication")), any(BizPortSheet.class));
		verify(workbook).addSheet(eq(new SheetKey("admin", "Communication", "tags")), any(BizPortSheet.class));
		verify(workbook).addSheet(eq(new SheetKey("admin", "Tag")), any(BizPortSheet.class));
	}

	@Test
	void generateAndAddCollectionSheetAddsOwnerElementAndOrdinalColumns() throws Exception {
		Customer customer = mock(Customer.class);
		Document drivingDocument = document("admin", "Communication", "Communication");
		Document owningDocument = document("admin", "Parent", "Parent");
		Document collectionDocument = document("admin", "Child", "Child");
		Collection collection = mock(Collection.class);
		BizPortWorkbook workbook = mock(BizPortWorkbook.class);
		when(collection.getLocalisedDisplayName()).thenReturn("Children");
		when(collection.getOrdered()).thenReturn(Boolean.TRUE);

		StandardGenerator generator = new StandardGenerator(customer, drivingDocument);

		invokePrivate(generator,
				"generateAndAddCollectionSheet",
				new Class<?>[] { String.class, Document.class, Collection.class, Document.class, BizPortWorkbook.class },
				"children",
				owningDocument,
				collection,
				collectionDocument,
				workbook);

		ArgumentCaptor<SheetKey> keyCaptor = ArgumentCaptor.forClass(SheetKey.class);
		ArgumentCaptor<BizPortSheet> sheetCaptor = ArgumentCaptor.forClass(BizPortSheet.class);
		verify(workbook).addSheet(keyCaptor.capture(), sheetCaptor.capture());
		SheetKey key = keyCaptor.getValue();
		BizPortSheet sheet = sheetCaptor.getValue();
		assertThat(key.getModuleName(), is("admin"));
		assertThat(key.getDocumentName(), is("Communication"));
		assertThat(key.getCollectionBinding(), is("children"));
		assertThat(sheet.getTitle(), is("Children"));
		assertThat(sheet.getColumnBindings().contains(PersistentBean.OWNER_COLUMN_NAME), is(true));
		assertThat(sheet.getColumnBindings().contains(PersistentBean.ELEMENT_COLUMN_NAME), is(true));
		assertThat(sheet.getColumnBindings().contains(Bean.ORDINAL_NAME), is(true));
		assertThat(sheet.getColumn(PersistentBean.OWNER_COLUMN_NAME).getReferencedSheet(), is(new SheetKey("admin", "Parent")));
		assertThat(sheet.getColumn(PersistentBean.ELEMENT_COLUMN_NAME).getReferencedSheet(), is(new SheetKey("admin", "Child")));
	}

	@Test
	void generateAndAddDocumentSheetAddsScalarConstantAndAssociationColumns() throws Exception {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = document("admin", "Communication", "Communication");
		Document associationDocument = document("admin", "User", "User");
		Attribute status = attribute("status", "Status", AttributeType.text, DomainType.constant);
		Association owner = mock(Association.class);
		Bizlet<Bean> bizlet = mock(Bizlet.class);
		BizPortWorkbook workbook = mock(BizPortWorkbook.class);
		when(owner.getAttributeType()).thenReturn(AttributeType.association);
		when(owner.getName()).thenReturn("owner");
		when(owner.getLocalisedDisplayName()).thenReturn("Owner");
		when(owner.getLocalisedDescription()).thenReturn("Owner description");
		when(owner.getType()).thenReturn(AssociationType.aggregation);
		when(owner.getDocumentName()).thenReturn("User");
		doReturn(List.<Attribute> of(status, owner)).when(document).getAllAttributes(customer);
		when(document.getBizlet(customer)).thenReturn(bizlet);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "User")).thenReturn(associationDocument);
		when(customer.getConstantDomainValues(eq(bizlet), eq("admin"), eq("Communication"), eq(status)))
				.thenReturn(List.of(new DomainValue("A", "Active"), new DomainValue("I", "Inactive")));
		StandardGenerator generator = new StandardGenerator(customer, document);

		invokePrivate(generator,
				"generateAndAddDocumentSheet",
				new Class<?>[] { Document.class, org.skyve.metadata.model.document.Relation.class, BizPortWorkbook.class },
				document,
				null,
				workbook);

		BizPortSheet sheet = capturedSheet(workbook);
		assertThat(sheet.getColumnBindings().contains(Bean.DOCUMENT_ID), is(true));
		assertThat(sheet.getColumnBindings().contains(Bean.BIZ_KEY), is(true));
		assertThat(sheet.getColumnBindings().contains("status"), is(true));
		assertThat(sheet.getColumnBindings().contains("owner"), is(true));
		assertThat(sheet.getColumn("status").getRangeValues(), arrayContaining("A", "I"));
		assertThat(sheet.getColumn("owner").getTitle(), is("Owner ID"));
		assertThat(sheet.getColumn("owner").getReferencedSheet(), is(new SheetKey("admin", "User")));
	}

	@Test
	void generateAndAddDocumentSheetAddsHierarchyParentColumn() throws Exception {
		Customer customer = mock(Customer.class);
		Document document = document("admin", "Node", "Node");
		BizPortWorkbook workbook = mock(BizPortWorkbook.class);
		when(document.getParentDocumentName()).thenReturn("Node");
		when(document.getAllAttributes(customer)).thenReturn(List.of());
		StandardGenerator generator = new StandardGenerator(customer, document);

		invokePrivate(generator,
				"generateAndAddDocumentSheet",
				new Class<?>[] { Document.class, org.skyve.metadata.model.document.Relation.class, BizPortWorkbook.class },
				document,
				null,
				workbook);

		BizPortSheet sheet = capturedSheet(workbook);
		assertThat(sheet.getColumnBindings().contains(HierarchicalBean.PARENT_ID), is(true));
		BizPortColumn parentColumn = sheet.getColumn(HierarchicalBean.PARENT_ID);
		assertThat(parentColumn.getReferencedSheet(), is(new SheetKey("admin", "Node")));
	}

	@Test
	void generateAndAddDocumentSheetAddsChildParentAndOrdinalColumns() throws Exception {
		Customer customer = mock(Customer.class);
		Document document = document("admin", "Line", "Line");
		Document parentDocument = document("admin", "Invoice", "Invoice");
		Collection owningRelation = mock(Collection.class);
		BizPortWorkbook workbook = mock(BizPortWorkbook.class);
		when(document.getParentDocumentName()).thenReturn("Invoice");
		when(document.getParentDocument(customer)).thenReturn(parentDocument);
		when(document.getAllAttributes(customer)).thenReturn(List.of());
		when(owningRelation.getName()).thenReturn("lines");
		when(owningRelation.getLocalisedDisplayName()).thenReturn("Lines");
		when(owningRelation.getOrdered()).thenReturn(Boolean.TRUE);
		StandardGenerator generator = new StandardGenerator(customer, document);

		invokePrivate(generator,
				"generateAndAddDocumentSheet",
				new Class<?>[] { Document.class, org.skyve.metadata.model.document.Relation.class, BizPortWorkbook.class },
				document,
				owningRelation,
				workbook);

		ArgumentCaptor<SheetKey> keyCaptor = ArgumentCaptor.forClass(SheetKey.class);
		ArgumentCaptor<BizPortSheet> sheetCaptor = ArgumentCaptor.forClass(BizPortSheet.class);
		verify(workbook).addSheet(keyCaptor.capture(), sheetCaptor.capture());
		assertThat(keyCaptor.getValue(), is(new SheetKey("admin", "Line", "lines")));
		BizPortSheet sheet = sheetCaptor.getValue();
		assertThat(sheet.getTitle(), is("Lines (Line)"));
		assertThat(sheet.getColumnBindings().contains(Bean.ORDINAL_NAME), is(true));
		assertThat(sheet.getColumnBindings().contains(org.skyve.domain.ChildBean.PARENT_NAME), is(true));
	}

	private static Document document(String moduleName, String name, String alias) {
		Document document = mock(Document.class);
		when(document.getOwningModuleName()).thenReturn(moduleName);
		when(document.getName()).thenReturn(name);
		when(document.getLocalisedSingularAlias()).thenReturn(alias);
		when(document.getAllAttributes(any(Customer.class))).thenReturn(List.of());
		return document;
	}

	private static Attribute attribute(String name, String displayName, AttributeType type, DomainType domainType) {
		Attribute attribute = mock(Attribute.class);
		when(attribute.getName()).thenReturn(name);
		when(attribute.getLocalisedDisplayName()).thenReturn(displayName);
		when(attribute.getLocalisedDescription()).thenReturn(displayName + " description");
		when(attribute.getAttributeType()).thenReturn(type);
		when(attribute.getDomainType()).thenReturn(domainType);
		return attribute;
	}

	private static BizPortSheet capturedSheet(BizPortWorkbook workbook) {
		ArgumentCaptor<BizPortSheet> sheetCaptor = ArgumentCaptor.forClass(BizPortSheet.class);
		verify(workbook).addSheet(any(SheetKey.class), sheetCaptor.capture());
		return sheetCaptor.getValue();
	}

	private static Object invokePrivate(Object target, String methodName, Class<?>[] parameterTypes, Object... args) throws Exception {
		Method method = StandardGenerator.class.getDeclaredMethod(methodName, parameterTypes);
		method.setAccessible(true);
		return method.invoke(target, args);
	}
}
