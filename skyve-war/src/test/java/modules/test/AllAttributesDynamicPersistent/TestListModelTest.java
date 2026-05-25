package modules.test.AllAttributesDynamicPersistent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.TreeMap;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class TestListModelTest {

	@Test
	void getRowsReturnsEmptyList() throws Exception {
		TestListModel model = new TestListModel();
		assertTrue(model.getRows().isEmpty());
	}

	@Test
	void getDescriptionReturnsTest() {
		TestListModel model = new TestListModel();
		assertEquals("Test", model.getDescription());
	}

	@Test
	void getColumnsReturnsNotEmpty() {
		TestListModel model = new TestListModel();
		assertNotNull(model.getColumns());
		assertEquals(1, model.getColumns().size());
	}

	@Test
	void updateThrowsUnsupportedOperation() {
		TestListModel model = new TestListModel();
		assertThrows(UnsupportedOperationException.class, () -> model.update("bizId", new TreeMap<>()));
	}

	@Test
	void removeThrowsUnsupportedOperation() {
		TestListModel model = new TestListModel();
		assertThrows(UnsupportedOperationException.class, () -> model.remove("bizId"));
	}
}
