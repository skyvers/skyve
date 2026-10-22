package org.skyve.metadata.view.model.list;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.persistence.SQL;

class SQLDocumentQueryListModelTest {
	@Test
	@SuppressWarnings("static-method")
	void fetchIsNotImplemented() {
		SQLDocumentQueryListModel<Bean> model = newModel();
		assertThrows(UnsupportedOperationException.class, model::fetch);
	}

	@Test
	@SuppressWarnings("static-method")
	void iterateIsNotImplemented() {
		SQLDocumentQueryListModel<Bean> model = newModel();
		assertThrows(UnsupportedOperationException.class, model::iterate);
	}

	/**
	 * The model has no query to establish, so skip establishing them.
	 */
	private static SQLDocumentQueryListModel<Bean> newModel() {
		return new SQLDocumentQueryListModel<>(mock(SQL.class)) {
			@Override
			protected void establishQueries() {
				// no query
			}
		};
	}
}
