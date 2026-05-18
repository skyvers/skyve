package org.skyve.metadata.view.model.list;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;

import java.util.TreeMap;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;

class InMemoryDocumentQueryListModelTest {

	@Test
	@SuppressWarnings("static-method")
	void constructorAcceptsMockedDependencies() {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		DocumentQuery query = mock(DocumentQuery.class);
		// Should not throw
		InMemoryDocumentQueryListModel<?> model = new InMemoryDocumentQueryListModel<>(module, document, query);
		assertNull(model.getDescription());
	}

	@Test
	@SuppressWarnings("static-method")
	void postConstructDoesNotThrow() {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		DocumentQuery query = mock(DocumentQuery.class);
		Customer customer = mock(Customer.class);

		InMemoryDocumentQueryListModel<?> model = new InMemoryDocumentQueryListModel<>(module, document, query);
		model.postConstruct(customer, true);
	}

	@Test
	@SuppressWarnings("static-method")
	void getRowsReturnsNull() throws Exception {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		DocumentQuery query = mock(DocumentQuery.class);

		InMemoryDocumentQueryListModel<?> model = new InMemoryDocumentQueryListModel<>(module, document, query);
		assertNull(model.getRows());
	}

	@Test
	@SuppressWarnings("static-method")

	void getColumnsReturnsNull() {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		DocumentQuery query = mock(DocumentQuery.class);

		InMemoryDocumentQueryListModel<?> model = new InMemoryDocumentQueryListModel<>(module, document, query);
		assertNull(model.getColumns());
	}

	@Test
	@SuppressWarnings("static-method")
	void updateReturnsNull() throws Exception {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		DocumentQuery query = mock(DocumentQuery.class);

		InMemoryDocumentQueryListModel<?> model = new InMemoryDocumentQueryListModel<>(module, document, query);
		assertNull(model.update("some-id", new TreeMap<>()));
	}

	@Test
	@SuppressWarnings("static-method")
	void removeDoesNotThrow() throws Exception {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		DocumentQuery query = mock(DocumentQuery.class);

		InMemoryDocumentQueryListModel<?> model = new InMemoryDocumentQueryListModel<>(module, document, query);
		model.remove("some-id");
	}
}
