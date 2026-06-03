package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Invisible;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.user.User;
import org.skyve.util.JSON;
import org.skyve.web.BackgroundTask;

@SuppressWarnings("static-method")
class ViewJSONManipulatorCoreTest {
	@Test
	void visitViewAddsCoreConditionBindings() throws Exception {
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), null, 2, 3);

		manipulator.visitView();

		ViewBindings bindingTree = bindingTree(manipulator);
		assertTrue(bindingTree.getBindings().contains(Bean.PERSISTED_KEY));
		assertTrue(bindingTree.getBindings().contains(Bean.CREATED_KEY));
		assertTrue(bindingTree.getBindings().contains(Bean.NOT_CREATED_KEY));
	}

	@Test
	void visitTabPaneAddsConditionsAndSelectedIndexBinding() throws Exception {
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), null, 0, 0);
		TabPane tabPane = new TabPane();
		tabPane.setInvisibleConditionName("invisibleWhen");
		tabPane.setDisabledConditionName("disabledWhen");
		tabPane.setSelectedTabIndexBinding("selectedTab");

		manipulator.visitTabPane(tabPane, true, true);

		ViewBindings bindingTree = bindingTree(manipulator);
		assertTrue(bindingTree.getBindings().contains("invisibleWhen"));
		assertTrue(bindingTree.getBindings().contains("disabledWhen"));
		assertTrue(bindingTree.getBindings().contains("selectedTab"));
	}

	@Test
	void visitedFormIncrementsTheCorrectIdCounter() throws Exception {
		Form form = new Form();
		ViewJSONManipulator createManipulator = newManipulator(ViewType.create.toString(), null, 10, 20);
		createManipulator.visitedForm(form, true, true);
		assertEquals(10, intField(createManipulator, "editIdCounter"));
		assertEquals(21, intField(createManipulator, "createIdCounter"));

		ViewJSONManipulator editManipulator = newManipulator(ViewType.edit.toString(), null, 10, 20);
		editManipulator.visitedForm(form, true, true);
		assertEquals(11, intField(editManipulator, "editIdCounter"));
		assertEquals(20, intField(editManipulator, "createIdCounter"));
	}

	@Test
	void visibleAndEnabledNegateBeanConditionEvaluation() {
		Bean bean = mock(Bean.class);
		doReturn(Boolean.TRUE).when(bean).evaluateCondition("hideWhen");
		doReturn(Boolean.FALSE).when(bean).evaluateCondition("disableWhen");

		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), bean, 0, 0);

		Invisible invisible = mock(Invisible.class);
		when(invisible.getInvisibleConditionName()).thenReturn("hideWhen");
		assertFalse(manipulator.visible(invisible));

		Disableable disableable = mock(Disableable.class);
		when(disableable.getDisabledConditionName()).thenReturn("disableWhen");
		assertTrue(manipulator.enabled(disableable));
	}

	@Test
	void addConditionSkipsTrueFalseAndAddsDynamicCondition() throws Exception {
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), null, 0, 0);

		manipulator.addCondition(null);
		manipulator.addCondition("true");
		manipulator.addCondition("false");
		manipulator.addCondition("showWhen");

		ViewBindings bindingTree = bindingTree(manipulator);
		assertFalse(bindingTree.getBindings().contains("true"));
		assertFalse(bindingTree.getBindings().contains("false"));
		assertTrue(bindingTree.getBindings().contains("showWhen"));
	}

	@Test
	void addNamedAndAnonymousFormatStorePerBindingPrefixEntries() throws Exception {
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), null, 0, 0);

		manipulator.addNamedFormat("myFormat", "display:false;Hi", false, Sanitisation.none);
		manipulator.addAnonymousFormat("display:true;Anon", true, Sanitisation.text);

		Map<String, Map<String, ViewFormat>> formats = formats(manipulator);
		Map<String, ViewFormat> rootFormats = formats.get("");
		assertNotNull(rootFormats);

		ViewFormat named = rootFormats.get("myFormat");
		assertNotNull(named);
		assertEquals("display:false;Hi", named.getFormat());
		assertFalse(named.isEscape());
		assertEquals(Sanitisation.none, named.getSanitise());

		ViewFormat anonymous = rootFormats.get("_0");
		assertNotNull(anonymous);
		assertEquals("display:true;Anon", anonymous.getFormat());
		assertTrue(anonymous.isEscape());
		assertEquals(Sanitisation.text, anonymous.getSanitise());
	}

	@Test
	void toJSONIncludesContextRedirectDirtyFlagAndMessages() throws Exception {
		PersistentBean constructorBean = mock(PersistentBean.class);
		when(constructorBean.getBizId()).thenReturn("biz-1");
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), constructorBean, 0, 0);

		TestWebContext webContext = new TestWebContext("ctx-1");
		PersistentBean currentBean = mock(PersistentBean.class);
		when(currentBean.getBizId()).thenReturn("biz-1");
		doReturn(Boolean.TRUE).when(currentBean).hasChanged();
		webContext.setCurrentBean(currentBean);
		webContext.setGrowls(List.of(Map.of("severity", "info", "summary", "ok")));
		webContext.setMessages(List.of(Map.of("severity", "warn", "summary", "note")));

		String json = manipulator.toJSON(webContext, "https://skyve.org/redirect");
		@SuppressWarnings("unchecked")
		Map<String, Object> result = (Map<String, Object>) JSON.unmarshall(json);

		assertEquals("https://skyve.org/redirect", result.get("_redirectUrl"));
		assertEquals("ctx-1biz-1", result.get(AbstractWebContext.CONTEXT_NAME));
		assertEquals(Boolean.TRUE, result.get("_changed"));
		assertTrue(result.containsKey("_growls"));
		assertTrue(result.containsKey("_messages"));
	}

	@Test
	void toJSONIncludesChildBindingsValueMapsAndComparisons() throws Exception {
		TestOwnerBean owner = new TestOwnerBean();
		owner.setBizId("owner-1");
		owner.setText("A & B");

		TestRelatedBean association = new TestRelatedBean();
		association.setBizId("assoc-1");
		owner.setAssociation(association);

		TestRelatedBean child = new TestRelatedBean();
		child.setBizId("child-1");
		owner.getChildren().add(child);

		PersistentBean constructorBean = mock(PersistentBean.class);
		when(constructorBean.getBizId()).thenReturn("owner-1");
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), constructorBean, 0, 0);
		setField(manipulator, "bean", owner);

		ViewBindings rootBindings = bindingTree(manipulator);
		rootBindings.putBinding("text", true, false, Sanitisation.text, false);
		DocumentImpl relatedDocument = new DocumentImpl();
		relatedDocument.setName("RelatedDoc");
		relatedDocument.setPersistent(new Persistent());
		rootBindings.putOrGetChild("association", relatedDocument);
		rootBindings.putOrGetChild("children", relatedDocument);

		Map<String, LinkedHashMap<String, String>> valueMaps = new LinkedHashMap<>();
		LinkedHashMap<String, String> mapEntries = new LinkedHashMap<>();
		mapEntries.put("one", "1");
		valueMaps.put("text", mapEntries);
		setField(manipulator, "valueMaps", valueMaps);

		Map<String, Iterable<Map<String, Object>>> comparisons = new LinkedHashMap<>();
		comparisons.put("comparisonKey", List.of(Map.of("lhs", "rhs")));
		setField(manipulator, "comparisons", comparisons);

		TestWebContext webContext = new TestWebContext("ctx-2");
		webContext.setCurrentBean(constructorBean);

		String json = manipulator.toJSON(webContext, null);
		@SuppressWarnings("unchecked")
		Map<String, Object> result = (Map<String, Object>) JSON.unmarshall(json);

		assertEquals("ctx-2owner-1", result.get(AbstractWebContext.CONTEXT_NAME));
		assertEquals("A & B", result.get("text"));
		assertTrue(result.containsKey("association"));
		assertTrue(result.containsKey("children"));
		assertTrue(result.containsKey("_valueMaps"));
		assertTrue(result.containsKey("comparisonKey"));
	}

	private static final class TestWebContext extends AbstractWebContext {
		private static final long serialVersionUID = 1L;
		private List<Map<String, String>> growls;
		private List<Map<String, String>> messages;

		private TestWebContext(String key) {
			super(key);
		}

		private void setGrowls(List<Map<String, String>> growls) {
			this.growls = growls;
		}

		private void setMessages(List<Map<String, String>> messages) {
			this.messages = messages;
		}

		@Override
		public List<Map<String, String>> getGrowls() {
			return growls;
		}

		@Override
		public List<Map<String, String>> getMessages() {
			return messages;
		}

		@Override
		public void message(org.skyve.domain.messages.MessageSeverity severity, String message) {
			// no-op for focused unit tests
		}

		@Override
		public void growl(org.skyve.domain.messages.MessageSeverity severity, String message) {
			// no-op for focused unit tests
		}

		@Override
		public void cacheConversation() {
			// no-op for focused unit tests
		}

		@Override
		public <T extends Bean> void background(Class<? extends BackgroundTask<T>> taskClass) {
			// no-op for focused unit tests
		}

		@Override
		public <T extends Bean> void backgroundWithoutCachingConversation(Class<? extends BackgroundTask<T>> taskClass) {
			// no-op for focused unit tests
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

	private static ViewJSONManipulator newManipulator(String viewName, Bean bean, int editIdCounter, int createIdCounter) {
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(new CustomerImpl());

		ModuleImpl module = new ModuleImpl();
		module.setName("testModule");

		DocumentImpl document = new DocumentImpl();
		document.setName("TestDoc");
		document.setPersistent(new Persistent());

		ViewImpl view = new ViewImpl();
		view.setName(viewName);
		view.setTitle("Test Title");

		return new ViewJSONManipulator(user,
				module,
				document,
				view,
				"external",
				bean,
				editIdCounter,
				createIdCounter,
				false);
	}

	private static ViewBindings bindingTree(ViewJSONManipulator manipulator) throws Exception {
		Field field = ViewJSONManipulator.class.getDeclaredField("bindingTree");
		field.setAccessible(true);
		return (ViewBindings) field.get(manipulator);
	}

	@SuppressWarnings("unchecked")
	private static Map<String, Map<String, ViewFormat>> formats(ViewJSONManipulator manipulator) throws Exception {
		Field field = ViewJSONManipulator.class.getDeclaredField("formats");
		field.setAccessible(true);
		return (Map<String, Map<String, ViewFormat>>) field.get(manipulator);
	}

	private static int intField(ViewJSONManipulator manipulator, String name) throws Exception {
		Field field = ViewJSONManipulator.class.getDeclaredField(name);
		field.setAccessible(true);
		return field.getInt(manipulator);
	}

	private static void setField(ViewJSONManipulator manipulator, String name, Object value) throws Exception {
		Field field = ViewJSONManipulator.class.getDeclaredField(name);
		field.setAccessible(true);
		field.set(manipulator, value);
	}
}
