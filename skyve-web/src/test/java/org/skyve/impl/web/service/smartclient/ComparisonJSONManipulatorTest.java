package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.comparison.ComparisonComposite;
import org.skyve.metadata.view.model.comparison.ComparisonComposite.Mutation;
import org.skyve.metadata.view.model.comparison.ComparisonProperty;

@SuppressWarnings("static-method")
class ComparisonJSONManipulatorTest {
	private final ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();

	@AfterEach
	void tearDown() {
		ProvidedRepositoryFactory.set(originalRepository);
	}

	@Test
	void toJSONStructureBuildsMutationIconsAndPropertyFallbackForNullDocument() throws Exception {
		ComparisonComposite root = new ComparisonComposite("1", "Root", "", "RootRel", Mutation.unchanged, null);
		root.getProperties().add(new ComparisonProperty("name", "Name", null, "new", "old"));

		ComparisonComposite deletedChild = new ComparisonComposite("2", "Deleted Child", "childRef", "ChildRel", Mutation.deleted, null);
		ComparisonComposite unchangedChild = new ComparisonComposite("3", "Unchanged Child", "childRef2", "ChildRel2", Mutation.unchanged, null);
		ComparisonComposite nullMutationChild = new ComparisonComposite("4", "No Mutation", "childRef3", "ChildRel3", null, null);
		root.getChildren().add(deletedChild);
		root.getChildren().add(unchangedChild);
		root.getChildren().add(nullMutationChild);

		ComparisonJSONManipulator manipulator = new ComparisonJSONManipulator(mock(User.class), mock(CustomerImpl.class), root);
		List<Map<String, Object>> rows = toList(manipulator.toJSONStructure());

		assertEquals(4, rows.size());

		Map<String, Object> rootRow = rows.get(0);
		assertEquals("1", rootRow.get("bizId"));
		assertEquals("", rootRow.get("_t"));
		assertEquals("icons/comparisonUpdated.png", rootRow.get("icon"));
		assertEquals("<span style='color:red'>Root *</span>", rootRow.get("bizKey"));

		@SuppressWarnings("unchecked")
		List<Map<String, Object>> rootProperties = (List<Map<String, Object>>) rootRow.get("properties");
		assertEquals(1, rootProperties.size());
		Map<String, Object> property = rootProperties.get(0);
		assertEquals("name", property.get("name"));
		assertEquals("<span style='color:red'>Name *</span>", property.get("title"));
		assertEquals("text", property.get("type"));
		assertEquals("old", property.get("oldValue"));
		assertEquals("new", property.get("newValue"));

		Map<String, Object> deletedRow = rows.get(1);
		assertEquals("icons/comparisonDeleted.png", deletedRow.get("icon"));
		assertEquals("1", deletedRow.get("parent"));
		assertEquals("A", deletedRow.get("_t"));

		Map<String, Object> unchangedRow = rows.get(2);
		assertEquals("icons/comparisonUnchanged.png", unchangedRow.get("icon"));
		assertEquals("Unchanged Child", unchangedRow.get("bizKey"));

		Map<String, Object> nullMutationRow = rows.get(3);
		assertFalse(nullMutationRow.containsKey("icon"));
		assertFalse(nullMutationRow.containsKey("bizKey"));
	}

	@Test
	void toJSONStructureMarksCollectionChildrenWithCollectionReferenceType() throws Exception {
		Document rootDocument = mock(Document.class);
		when(rootDocument.getReferenceByName("items")).thenReturn(new CollectionImpl());

		ComparisonComposite root = new ComparisonComposite("10", "Root", "", "RootRel", Mutation.unchanged, rootDocument);
		ComparisonComposite collectionChild = new ComparisonComposite("11", "Item", "items", "Items", Mutation.added, null);
		root.getChildren().add(collectionChild);

		ComparisonJSONManipulator manipulator = new ComparisonJSONManipulator(mock(User.class), mock(CustomerImpl.class), root);
		List<Map<String, Object>> rows = toList(manipulator.toJSONStructure());

		assertEquals(2, rows.size());
		Map<String, Object> childRow = rows.get(1);
		assertEquals("C", childRow.get("_t"));
		assertEquals("10", childRow.get("parent"));
		assertTrue(((List<?>) childRow.get("properties")).isEmpty());
	}

	@Test
	void toJSONStructureIncludesAddedMutationIconAndDirtyLabel() throws Exception {
		ComparisonComposite root = new ComparisonComposite("21", "Order", "", "Order", Mutation.unchanged, null);
		root.getChildren().add(new ComparisonComposite("22", "Added Child", "child", "ChildRel", Mutation.added, null));

		ComparisonJSONManipulator manipulator = new ComparisonJSONManipulator(mock(User.class), mock(CustomerImpl.class), root);
		List<Map<String, Object>> rows = toList(manipulator.toJSONStructure());

		assertEquals(2, rows.size());
		Map<String, Object> childRow = rows.get(1);
		assertEquals("icons/comparisonAdded.png", childRow.get("icon"));
		assertEquals("A", childRow.get("_t"));
		assertEquals("<span style='color:red'>Added Child *</span>", childRow.get("bizKey"));
	}

	@Test
	void toJSONStructureKeepsPlainTitleWhenPropertyIsNotDirty() throws Exception {
		ComparisonComposite root = new ComparisonComposite("41", "Root", "", "RootRel", Mutation.unchanged, null);
		root.getProperties().add(new ComparisonProperty("status", "Status", null, "ACTIVE", "ACTIVE"));

		ComparisonJSONManipulator manipulator = new ComparisonJSONManipulator(mock(User.class), mock(CustomerImpl.class), root);
		List<Map<String, Object>> rows = toList(manipulator.toJSONStructure());

		assertEquals(1, rows.size());
		@SuppressWarnings("unchecked")
		List<Map<String, Object>> properties = (List<Map<String, Object>>) rows.get(0).get("properties");
		assertEquals(1, properties.size());
		Map<String, Object> property = properties.get(0);
		assertEquals("Status", property.get("title"));
		assertEquals("ACTIVE", property.get("oldValue"));
		assertEquals("ACTIVE", property.get("newValue"));
		assertEquals("text", property.get("type"));
	}

	@Test
	void toJSONStructureUsesAssociationTypeWhenParentDocumentHasNoCollectionReference() throws Exception {
		Document rootDocument = mock(Document.class);
		when(rootDocument.getReferenceByName("childRef")).thenReturn(null);

		ComparisonComposite root = new ComparisonComposite("51", "Root", "", "RootRel", Mutation.unchanged, rootDocument);
		ComparisonComposite child = new ComparisonComposite("52", "Child", "childRef", "ChildRel", Mutation.added, null);
		root.getChildren().add(child);

		ComparisonJSONManipulator manipulator = new ComparisonJSONManipulator(mock(User.class), mock(CustomerImpl.class), root);
		List<Map<String, Object>> rows = toList(manipulator.toJSONStructure());

		assertEquals(2, rows.size());
		Map<String, Object> childRow = rows.get(1);
		assertEquals("A", childRow.get("_t"));
		assertEquals("51", childRow.get("parent"));
	}

	@Test
	void toJSONStructureFallsBackToTextWhenNonNullDocumentMetadataLookupFails() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Module module = mock(Module.class);
		ProvidedRepositoryFactory.set(repository);
		when(repository.getModule(customer, "admin")).thenReturn(module);

		Document rootDocument = mock(Document.class);
		when(rootDocument.getOwningModuleName()).thenReturn("admin");
		when(rootDocument.getName()).thenReturn("Sample");
		when(rootDocument.getAttribute("unknownBinding")).thenReturn(null);
		when(rootDocument.getExtends()).thenReturn(null);

		ComparisonComposite root = new ComparisonComposite("61", "Root", "", "RootRel", Mutation.unchanged, rootDocument);
		root.getProperties().add(new ComparisonProperty("unknownBinding", "Unknown", null, "newValue", "oldValue"));

		ComparisonJSONManipulator manipulator = new ComparisonJSONManipulator(mock(User.class), customer, root);
		List<Map<String, Object>> rows = toList(manipulator.toJSONStructure());

		assertEquals(1, rows.size());
		@SuppressWarnings("unchecked")
		List<Map<String, Object>> properties = (List<Map<String, Object>>) rows.get(0).get("properties");
		assertEquals(1, properties.size());
		Map<String, Object> property = properties.get(0);
		assertEquals("text", property.get("type"));
		assertEquals("oldValue", property.get("oldValue"));
		assertEquals("newValue", property.get("newValue"));
	}

	@Test
	void showDirtyWrapsLabelInDirtyMarkup() throws Exception {
		Method method = ComparisonJSONManipulator.class.getDeclaredMethod("showDirty", String.class);
		method.setAccessible(true);

		String result = (String) method.invoke(null, "Label");

		assertEquals("<span style='color:red'>Label *</span>", result);
	}

	@Test
	void showDirtyHandlesEmptyLabel() throws Exception {
		Method method = ComparisonJSONManipulator.class.getDeclaredMethod("showDirty", String.class);
		method.setAccessible(true);

		String result = (String) method.invoke(null, "");

		assertEquals("<span style='color:red'> *</span>", result);
	}

	private static List<Map<String, Object>> toList(Iterable<Map<String, Object>> iterable) {
		List<Map<String, Object>> result = new ArrayList<>();
		for (Map<String, Object> value : iterable) {
			result.add(value);
		}
		return result;
	}
}
