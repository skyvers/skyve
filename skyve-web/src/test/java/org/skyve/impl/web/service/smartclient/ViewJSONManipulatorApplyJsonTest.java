package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.AssociationImpl;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;

@SuppressWarnings("static-method")
class ViewJSONManipulatorApplyJsonTest {
	@Test
	void applyJSONPropertiesInvokesOnlyMutableBindings() throws Exception {
		RecordingViewJSONManipulator manipulator = newManipulator();
		ViewBindings bindings = newBindings();
		bindings.putBinding("mutableBinding", true, false, Sanitisation.text, false);
		bindings.putBinding("immutableBinding", false, false, Sanitisation.text, false);

		manipulator.applyJSONProperties(bindings,
				new DocumentImpl(),
				new HashMap<>(),
				mock(Bean.class),
				null,
				null);

		assertEquals(3, manipulator.appliedBindings.size());
		assertEquals("mutableBinding", manipulator.appliedBindings.get(2));
	}

	@Test
	void applyJSONPropertyReturnsImmediatelyWhenValueIsAbsent() {
		RecordingViewJSONManipulator manipulator = newManipulator();
		Map<String, Object> values = new HashMap<>();

		assertDoesNotThrow(() -> manipulator.applyJSONProperty(new DocumentImpl(),
				"unknownBinding",
				values,
				mock(Bean.class),
				null,
				null));
	}

	@Test
	void applyJSONPropertySwallowsMetaDataExceptionForUnknownBinding() {
		RecordingViewJSONManipulator manipulator = newManipulator();
		Map<String, Object> values = new HashMap<>();
		values.put("unknownBinding", "value");

		assertDoesNotThrow(() -> manipulator.applyJSONProperty(new DocumentImpl(),
				"unknownBinding",
				values,
				mock(Bean.class),
				null,
				null));
	}

	@Test
	void applyJSONDelegatesToApplyJSONPropertiesWhenNoChildBindingsExist() {
		ApplyJsonDelegatingManipulator manipulator = newDelegatingManipulator();
		ViewBindings bindings = newBindings();

		assertDoesNotThrow(() -> manipulator.applyJSON(bindings,
				new DocumentImpl(),
				new HashMap<>(),
				mock(Bean.class),
				null,
				null));

		assertEquals(1, manipulator.applyJSONPropertiesCalls);
	}

	@Test
	void applyJSONPropertyAndApplyJSONHandleAssociationAndCollectionMutation() throws Exception {
		try (Fixture fixture = new Fixture()) {
			TestOwnerBean owner = new TestOwnerBean();
			TestRelatedBean existingAssociation = new TestRelatedBean();
			owner.setAssociation(existingAssociation);
			TestRelatedBean existingChild = new TestRelatedBean();
			existingChild.setBizId("child-1");
			owner.getChildren().add(existingChild);

			Map<String, Object> scalarValues = new HashMap<>();
			scalarValues.put("text", "A &amp; B");

			assertDoesNotThrow(() -> fixture.manipulator.applyJSONProperty(fixture.sourceDocument,
					"text",
					scalarValues,
					owner,
					fixture.persistence,
					null));
			assertEquals("A & B", owner.getText());

			Map<String, Object> associationValues = new HashMap<>();
			associationValues.put("association", new HashMap<>());

			assertDoesNotThrow(() -> fixture.manipulator.applyJSON(fixture.rootBindings,
					fixture.sourceDocument,
					associationValues,
					owner,
					fixture.persistence,
					null));
			assertSame(existingAssociation, owner.getAssociation());

			Map<String, Object> addValues = new HashMap<>();
			Map<String, Object> requestChild = new HashMap<>();
			requestChild.put(org.skyve.domain.Bean.DOCUMENT_ID, "child-1");
			addValues.put("children", List.of(requestChild));

			assertDoesNotThrow(() -> fixture.manipulator.applyJSON(fixture.rootBindings,
					fixture.sourceDocument,
					addValues,
					owner,
					fixture.persistence,
					null));
			assertEquals(1, owner.getChildren().size());
			assertSame(existingChild, owner.getChildren().get(0));

			Map<String, Object> removeValues = new HashMap<>();
			removeValues.put("children", List.of());

			assertDoesNotThrow(() -> fixture.manipulator.applyJSON(fixture.rootBindings,
					fixture.sourceDocument,
					removeValues,
					owner,
					fixture.persistence,
					null));
			assertEquals(0, owner.getChildren().size());
		}
	}

	private static final class Fixture implements AutoCloseable {
		private static final Field THREAD_LOCAL_PERSISTENCE_FIELD;
		static {
			try {
				THREAD_LOCAL_PERSISTENCE_FIELD = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
				THREAD_LOCAL_PERSISTENCE_FIELD.setAccessible(true);
			}
			catch (Exception e) {
				throw new ExceptionInInitializerError(e);
			}
		}

		private final ProvidedRepository previousRepository;
		private final ThreadLocal<AbstractPersistence> threadLocal;
		private final AbstractPersistence previousPersistence;
		private final AbstractPersistence persistence;
		private final ViewJSONManipulator manipulator;
		private final DocumentImpl sourceDocument;
		private final ViewBindings rootBindings;

		private Fixture() throws Exception {
			previousRepository = ProvidedRepositoryFactory.get();
			threadLocal = threadLocalPersistence();
			previousPersistence = threadLocal.get();

			CustomerImpl customer = new CustomerImpl();
			customer.setName("testCustomer");

			User user = mock(User.class);
			when(user.getCustomer()).thenReturn(customer);

			persistence = mock(AbstractPersistence.class);
			when(persistence.getUser()).thenReturn(user);
			threadLocal.set(persistence);

			ModuleImpl module = new ModuleImpl();
			module.setName("testModule");

			sourceDocument = new DocumentImpl();
			sourceDocument.setName("OwnerDoc");
			sourceDocument.setOwningModuleName("testModule");
			sourceDocument.setPersistent(new Persistent());

			Text text = new Text();
			text.setName("text");
			text.setLength(50);
			sourceDocument.putAttribute(text);

			AssociationImpl association = new AssociationImpl();
			association.setName("association");
			association.setDocumentName("RelatedDoc");
			association.setType(AssociationType.aggregation);
			sourceDocument.putRelation(association);

			CollectionImpl children = new CollectionImpl();
			children.setName("children");
			children.setDocumentName("RelatedDoc");
			children.setType(CollectionType.child);
			sourceDocument.putRelation(children);

			DocumentImpl relatedDocument = new DocumentImpl();
			relatedDocument.setName("RelatedDoc");
			relatedDocument.setOwningModuleName("testModule");
			relatedDocument.setPersistent(new Persistent());

			ProvidedRepository repository = mock(ProvidedRepository.class);
			when(repository.getModule(customer, "testModule")).thenReturn(module);
			when(repository.getDocument(customer, module, "OwnerDoc")).thenReturn(sourceDocument);
			when(repository.getDocument(customer, module, "RelatedDoc")).thenReturn(relatedDocument);
			when(repository.vtable(anyString(), any())).thenReturn(null);
			when(repository.vtable(anyString(), org.mockito.ArgumentMatchers.eq("modules/testModule/RelatedDoc"))).thenReturn("modules/testModule/domain");
			ProvidedRepositoryFactory.set(repository);

			ViewImpl view = new ViewImpl();
			view.setName(ViewType.edit.toString());

			manipulator = new ViewJSONManipulator(user,
					module,
					sourceDocument,
					view,
					"external",
					null,
					0,
					0,
					false);

			rootBindings = new ViewBindings(sourceDocument);
			rootBindings.putBinding("text", true, false, Sanitisation.text, false);
			rootBindings.putOrGetChild("association", relatedDocument);
			rootBindings.putOrGetChild("children", relatedDocument);
		}

		@Override
		public void close() throws Exception {
			ProvidedRepositoryFactory.set(previousRepository);
			threadLocal.set(previousPersistence);
		}

		@SuppressWarnings("unchecked")
		private static ThreadLocal<AbstractPersistence> threadLocalPersistence() throws Exception {
			return (ThreadLocal<AbstractPersistence>) THREAD_LOCAL_PERSISTENCE_FIELD.get(null);
		}
	}

	public static final class TestOwnerBean extends org.skyve.impl.domain.AbstractPersistentBean {
		private static final long serialVersionUID = 1L;

		private String text;
		private TestRelatedBean association;
		private final List<TestRelatedBean> children = new ArrayList<>();

		@Override
		public String getBizKey() {
			return getBizId();
		}

		@Override
		public String getBizModule() {
			return "testModule";
		}

		@Override
		public String getBizDocument() {
			return "OwnerDoc";
		}

		public String getText() {
			return text;
		}

		public void setText(String text) {
			this.text = text;
		}

		public TestRelatedBean getAssociation() {
			return association;
		}

		public void setAssociation(TestRelatedBean association) {
			this.association = association;
		}

		public List<TestRelatedBean> getChildren() {
			return children;
		}

		public void setChildren(List<TestRelatedBean> children) {
			this.children.clear();
			if (children != null) {
				this.children.addAll(children);
			}
		}

		public void addChildrenElement(int index, TestRelatedBean element) {
			children.add(index, element);
		}

		public TestRelatedBean removeChildrenElement(int index) {
			return children.remove(index);
		}
	}

	public static final class TestRelatedBean extends org.skyve.impl.domain.AbstractPersistentBean {
		private static final long serialVersionUID = 1L;

		@Override
		public String getBizKey() {
			return getBizId();
		}

		@Override
		public String getBizModule() {
			return "testModule";
		}

		@Override
		public String getBizDocument() {
			return "RelatedDoc";
		}
	}

	private static final class RecordingViewJSONManipulator extends ViewJSONManipulator {
		private final List<String> appliedBindings = new ArrayList<>();

		private RecordingViewJSONManipulator(User user,
				ModuleImpl module,
				DocumentImpl document,
				ViewImpl view,
				String uxui,
				Bean bean,
				int editIdCounter,
				int createIdCounter,
				boolean forApply) {
			super(user, module, document, view, uxui, bean, editIdCounter, createIdCounter, forApply);
		}

		@Override
		protected void applyJSONProperty(org.skyve.metadata.model.document.Document startingDocument,
				String binding,
				Map<String, Object> values,
				Bean targetBean,
				AbstractPersistence persistence,
				WebContext webContext)
		throws Exception {
			appliedBindings.add(binding);
		}
	}

	private static final class ApplyJsonDelegatingManipulator extends ViewJSONManipulator {
		private int applyJSONPropertiesCalls;

		private ApplyJsonDelegatingManipulator(User user,
				ModuleImpl module,
				DocumentImpl document,
				ViewImpl view,
				String uxui,
				Bean bean,
				int editIdCounter,
				int createIdCounter,
				boolean forApply) {
			super(user, module, document, view, uxui, bean, editIdCounter, createIdCounter, forApply);
		}

		@Override
		protected void applyJSONProperties(ViewBindings bindings,
				org.skyve.metadata.model.document.Document documentToApply,
				Map<String, Object> valuesToApply,
				Bean beanToApplyTo,
				AbstractPersistence persistence,
				WebContext webContext)
		throws Exception {
			applyJSONPropertiesCalls++;
			super.applyJSONProperties(bindings, documentToApply, valuesToApply, beanToApplyTo, persistence, webContext);
		}
	}

	private static RecordingViewJSONManipulator newManipulator() {
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(new CustomerImpl());

		ModuleImpl module = new ModuleImpl();
		module.setName("testModule");

		DocumentImpl document = new DocumentImpl();
		document.setName("TestDoc");
		document.setPersistent(new Persistent());

		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());

		return new RecordingViewJSONManipulator(user,
				module,
				document,
				view,
				"external",
				null,
				0,
				0,
				false);
	}

	private static ViewBindings newBindings() {
		DocumentImpl document = new DocumentImpl();
		document.setName("TestDoc");
		document.setPersistent(new Persistent());
		return new ViewBindings(document);
	}

	private static ApplyJsonDelegatingManipulator newDelegatingManipulator() {
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(new CustomerImpl());

		ModuleImpl module = new ModuleImpl();
		module.setName("testModule");

		DocumentImpl document = new DocumentImpl();
		document.setName("TestDoc");
		document.setPersistent(new Persistent());

		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());

		return new ApplyJsonDelegatingManipulator(user,
				module,
				document,
				view,
				"external",
				null,
				0,
				0,
				false);
	}

}
