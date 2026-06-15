package org.skyve.metadata.view.model.list;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

class ListModelStubTest {
	@Test
	@SuppressWarnings("static-method")
	void sqlWithDrivingDocumentListModelCanBeInstantiated() {
		SQLWithDrivingDocumentListModel model = new SQLWithDrivingDocumentListModel();
		assertNotNull(model);
	}

	@Test
	@SuppressWarnings("static-method")
	void sqlWithoutDrivingDocumentListModel2CanBeInstantiated() {
		SQLWithoutDrivingDocumentListModel2 model = new SQLWithoutDrivingDocumentListModel2();
		assertNotNull(model);
	}

	@Test
	@SuppressWarnings("static-method")
	void inMemorySQLListModelCanBeInstantiated() {
		InMemorySQLListModel model = new InMemorySQLListModel();
		assertNotNull(model);
	}
}
