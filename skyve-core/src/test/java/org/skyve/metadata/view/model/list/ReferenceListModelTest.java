package org.skyve.metadata.view.model.list;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;

class ReferenceListModelTest {

	/** Minimal concrete subclass. */
	private static class TestReferenceListModel extends ReferenceListModel<Bean> {
		TestReferenceListModel(Module module, Document document, String referenceBinding) {
			super(module, document, referenceBinding);
		}

		@Override
		public List<MetaDataQueryColumn> getColumns() {
			return Collections.emptyList();
		}

		@Override
		public String getDescription() {
			return "TestReferenceListModel";
		}
	}

	/** Minimal TransientBean stub. */
	private static class StubBean extends AbstractTransientBean {
		private static final long serialVersionUID = 1L;
		StubBean() { setBizId(java.util.UUID.randomUUID().toString()); }
		@Override public String getBizModule() { return "test"; }
		@Override public String getBizDocument() { return "StubBean"; }
		@Override public String getBizKey() { return "stub"; }
	}

	private Module mockModule;
	private Document mockDocument;

	@BeforeEach
	void setUp() {
		mockModule = mock(Module.class);
		mockDocument = mock(Document.class);
		when(mockModule.getName()).thenReturn("test");
		when(mockDocument.getName()).thenReturn("TestDoc");
	}

	@Test
	void getRowsReturnsEmptyWhenNoBeanSet() throws Exception {
		TestReferenceListModel model = new TestReferenceListModel(mockModule, mockDocument, "items");
		List<Bean> rows = model.getRows();
		assertNotNull(rows);
		assertTrue(rows.isEmpty());
	}

	@Test
	void getRowsReturnsBeanListWhenBeanHasCollection() throws Exception {
		// Use DynamicBean to hold a list dynamically (avoids BeanUtils reflection issues with inner classes)
		List<Bean> itemList = new ArrayList<>();
		itemList.add(new StubBean());
		itemList.add(new StubBean());

		TreeMap<String, Object> dynamicMap = new TreeMap<>();
		dynamicMap.put("items", itemList);
		DynamicBean bean = new DynamicBean("test", "TestDoc", dynamicMap);

		TestReferenceListModel model = new TestReferenceListModel(mockModule, mockDocument, "items");
		model.setBean(bean);

		List<Bean> rows = model.getRows();
		assertEquals(2, rows.size());
	}

	@Test
	void getRowsReturnsCopyOfCollectionNotSameInstance() throws Exception {
		List<Bean> itemList = new ArrayList<>();
		itemList.add(new StubBean());

		TreeMap<String, Object> dynamicMap = new TreeMap<>();
		dynamicMap.put("items", itemList);
		DynamicBean bean = new DynamicBean("test", "TestDoc", dynamicMap);

		TestReferenceListModel model = new TestReferenceListModel(mockModule, mockDocument, "items");
		model.setBean(bean);

		List<Bean> rows = model.getRows();
		// Should be a defensive copy, not the same list
		rows.add(new StubBean()); // modifying the result should not affect source
		assertEquals(1, itemList.size());
	}

	@Test
	void getRowsReturnsSingleBeanWhenBindingResolvesToBean() throws Exception {
		StubBean associatedBean = new StubBean();
		TreeMap<String, Object> dynamicMap = new TreeMap<>();
		dynamicMap.put("related", associatedBean);
		DynamicBean bean = new DynamicBean("test", "TestDoc", dynamicMap);

		TestReferenceListModel model = new TestReferenceListModel(mockModule, mockDocument, "related");
		model.setBean(bean);

		List<Bean> rows = model.getRows();
		assertEquals(1, rows.size());
		assertEquals(associatedBean, rows.get(0));
	}

	@Test
	void updateThrowsIllegalStateException() {
		TestReferenceListModel model = new TestReferenceListModel(mockModule, mockDocument, "items");
		assertThrows(IllegalStateException.class, () -> model.update("bizId", mock(SortedMap.class)));
	}

	@Test
	void removeThrowsIllegalStateException() {
		TestReferenceListModel model = new TestReferenceListModel(mockModule, mockDocument, "items");
		assertThrows(IllegalStateException.class, () -> model.remove("bizId"));
	}

	/** A convenience constructor variant that requires postConstruct to resolve module/document. */
	private static class StringConstructorModel extends ReferenceListModel<Bean> {
		StringConstructorModel(String moduleName, String documentName, String referenceBinding) {
			super(moduleName, documentName, referenceBinding);
		}
		@Override public List<MetaDataQueryColumn> getColumns() { return Collections.emptyList(); }
		@Override public String getDescription() { return "StringConstructorModel"; }
	}

	@Test
	void postConstructWithStringConstructorResolvesModuleAndDocument() {
		Customer customer = mock(Customer.class);
		Module resolvedModule = mock(Module.class);
		Document resolvedDocument = mock(Document.class);
		when(customer.getModule("myModule")).thenReturn(resolvedModule);
		when(resolvedModule.getDocument(customer, "MyDoc")).thenReturn(resolvedDocument);

		StringConstructorModel model = new StringConstructorModel("myModule", "MyDoc", "items");
		// Should not throw - postConstruct resolves module/document via customer
		model.postConstruct(customer, false);
		assertNotNull(model.getDrivingDocument(), "postConstruct should resolve the driving document");
	}

	@Test
	void postConstructWithModuleDocumentConstructorSkipsResolution() {
		// When using the Module, Document constructor, postConstruct skips resolution
		Customer customer = mock(Customer.class);

		TestReferenceListModel model = new TestReferenceListModel(mockModule, mockDocument, "items");
		// Should not throw - module/document already set
		model.postConstruct(customer, false);
		assertNotNull(model.getDrivingDocument(), "postConstruct should retain the driving document");
	}
}
