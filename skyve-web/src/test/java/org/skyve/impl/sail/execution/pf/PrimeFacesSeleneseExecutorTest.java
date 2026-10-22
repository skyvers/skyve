package org.skyve.impl.sail.execution.pf;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.sail.language.step.context.PushEditContext;
import org.skyve.metadata.sail.language.step.context.PushListContext;
import org.skyve.metadata.sail.language.step.interaction.DataEnter;
import org.skyve.metadata.sail.language.step.interaction.TabSelect;
import org.skyve.metadata.sail.language.step.interaction.TestDataEnter;
import org.skyve.metadata.sail.language.step.interaction.actions.Action;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridNew;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridSelect;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridZoom;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionPick;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateLink;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.metadata.sail.language.step.interaction.session.Login;
import org.skyve.metadata.sail.language.step.interaction.session.Logout;
import org.skyve.metadata.user.User;

import jakarta.faces.component.UIComponent;

@SuppressWarnings("static-method")
class PrimeFacesSeleneseExecutorTest {
	@AfterEach
	void clearPersistence() {
		setPersistenceThreadLocal(null);
	}

	@Test
	void loginAndLogoutEmitExpectedSeleneseRows() {
		TestPrimeFacesSeleneseExecutor executor = new TestPrimeFacesSeleneseExecutor();

		Login customerLogin = new Login();
		customerLogin.setCustomer("acme");
		customerLogin.setUser("admin");
		customerLogin.setPassword("secret");
		executor.executeLogin(customerLogin);

		Login defaultCustomerLogin = new Login();
		defaultCustomerLogin.setUser("guest");
		defaultCustomerLogin.setPassword("welcome");
		executor.executeLogin(defaultCustomerLogin);

		executor.executeLogout(new Logout());

		String script = executor.toString();
		assertTrue(script.contains("<!-- Login as acme/admin -->"), script);
		assertTrue(script.contains("<tr><td>open</td><td>.</td><td></td></tr>"), script);
		assertTrue(script.contains("<tr><td>type</td><td>name=customer</td><td>acme</td></tr>"), script);
		assertTrue(script.contains("<tr><td>type</td><td>name=user</td><td>admin</td></tr>"), script);
		assertTrue(script.contains("<tr><td>type</td><td>name=password</td><td>secret</td></tr>"), script);
		assertTrue(script.contains("<tr><td>clickAndWait</td><td>css=input[type=&quot;submit&quot;]</td><td></td></tr>"), script);
		assertTrue(script.contains("<!-- Login as guest -->"), script);
		assertTrue(script.contains("<tr><td>type</td><td>name=user</td><td>guest</td></tr>"), script);
		assertTrue(script.contains("<tr><td>type</td><td>name=password</td><td>welcome</td></tr>"), script);
		assertTrue(script.contains("<!-- Logout -->"), script);
		assertTrue(script.contains("<tr><td>open</td><td>loggedOut</td><td></td></tr>"), script);
	}

	@Test
	void navigateListEmitsQueryDefaultAndModelUrls() {
		TestPrimeFacesSeleneseExecutor executor = new TestPrimeFacesSeleneseExecutor();

		NavigateList queryList = new NavigateList();
		queryList.setModuleName("sales");
		queryList.setQueryName("activeCustomers");
		executor.executeNavigateList(queryList);

		NavigateList documentList = new NavigateList();
		documentList.setModuleName("sales");
		documentList.setDocumentName("Customer");
		executor.executeNavigateList(documentList);

		NavigateList modelList = new NavigateList();
		modelList.setModuleName("sales");
		modelList.setDocumentName("Customer");
		modelList.setModelName("recent");
		executor.executeNavigateList(modelList);

		String script = executor.toString();
		assertTrue(script.contains("<!-- List for query [sales.activeCustomers] -->"), script);
		assertTrue(script.contains("<tr><td>open</td><td>?a=l&m=sales&q=activeCustomers</td><td></td></tr>"), script);
		assertTrue(script.contains("<!-- List for default query of [sales.Customer] -->"), script);
		assertTrue(script.contains("<tr><td>open</td><td>?a=l&m=sales&q=Customer</td><td></td></tr>"), script);
		assertTrue(script.contains("<!-- List for model [sales.Customer.recent] -->"), script);
		assertTrue(script.contains("<tr><td>open</td><td>?a=l&m=sales&d=Customer&q=recent</td><td></td></tr>"), script);
	}

	@Test
	void navigateEditEmitsNewAndExistingInstanceUrls() {
		TestPrimeFacesSeleneseExecutor executor = new TestPrimeFacesSeleneseExecutor();

		NavigateEdit newEdit = new NavigateEdit();
		newEdit.setModuleName("sales");
		newEdit.setDocumentName("Customer");
		executor.executeNavigateEdit(newEdit);

		NavigateEdit existingEdit = new NavigateEdit();
		existingEdit.setModuleName("sales");
		existingEdit.setDocumentName("Customer");
		existingEdit.setBizId("abc123");
		executor.executeNavigateEdit(existingEdit);

		String script = executor.toString();
		assertTrue(script.contains("<!-- Edit new document [sales.Customer] instance -->"), script);
		assertTrue(script.contains("<tr><td>open</td><td>.?a=e&m=sales&d=Customer</td><td></td></tr>"), script);
		assertTrue(script.contains("<!-- Edit document [sales.Customer] instance with bizId abc123 -->"), script);
		assertTrue(script.contains("<tr><td>open</td><td>.?a=e&m=sales&d=Customer&i=abc123</td><td></td></tr>"), script);
	}

	@Test
	void interactionsWithoutContextThrowMetaDataException() {
		TestPrimeFacesSeleneseExecutor executor = new TestPrimeFacesSeleneseExecutor();
		TabSelect tabSelect = new TabSelect();
		tabSelect.setTabPath("Details");
		DataEnter dataEnter = new DataEnter();
		dataEnter.setBinding("name");
		dataEnter.setValue("Acme");
		Action action = new Action();
		action.setActionName("Custom");
		LookupDescriptionPick pick = new LookupDescriptionPick();
		pick.setBinding("contact");
		pick.setRow(Integer.valueOf(0));
		DataGridNew dataGridNew = new DataGridNew();
		dataGridNew.setBinding("contacts");
		ListGridSelect listGridSelect = new ListGridSelect();
		listGridSelect.setRow(Integer.valueOf(0));

		assertThrows(MetaDataException.class, () -> executor.executeTabSelect(tabSelect));
		assertThrows(MetaDataException.class, () -> executor.executeDataEnter(dataEnter));
		assertThrows(MetaDataException.class, () -> executor.executeAction(action));
		assertThrows(MetaDataException.class, () -> executor.executeLookupDescriptionPick(pick));
		assertThrows(MetaDataException.class, () -> executor.executeDataGridNew(dataGridNew));
		assertThrows(MetaDataException.class, () -> executor.executeListGridSelect(listGridSelect));
		assertThrows(MetaDataException.class, () -> executor.executeTestDataEnter(new TestDataEnter()));
	}

	@Test
	void testDataEnterAfterNavigateLinkThrowsMetaDataException() {
		TestPrimeFacesSeleneseExecutor executor = new TestPrimeFacesSeleneseExecutor();
		NavigateEdit edit = new NavigateEdit();
		edit.setModuleName("sales");
		edit.setDocumentName("Customer");
		executor.executeNavigateEdit(edit);
		executor.executeNavigateLink(new NavigateLink());

		assertThrows(MetaDataException.class, () -> executor.executeTestDataEnter(new TestDataEnter()));
	}

	@Test
	void listGridZoomWithoutQueryDocumentOrModelThrowsMetaDataException() {
		TestPrimeFacesSeleneseExecutor executor = newEditExecutorWithListGrid("sales.Customer");
		ListGridZoom zoom = new ListGridZoom();
		zoom.setRow(Integer.valueOf(0));

		assertThrows(MetaDataException.class, () -> executor.executeListGridZoom(zoom));
	}

	@Test
	void listGridZoomOnDocumentWithoutModuleRefPushesOwningModule() {
		Module module = mock(Module.class);
		DocumentRef ref = new DocumentRef();
		ref.setOwningModuleName("sales");
		when(module.getDocumentRefs()).thenReturn(Map.of("Contact", ref));
		bindModule(module);
		TestPrimeFacesSeleneseExecutor executor = newEditExecutorWithListGrid("sales.Contact");
		ListGridZoom zoom = new ListGridZoom();
		zoom.setDocumentName("Contact");
		zoom.setRow(Integer.valueOf(0));

		executor.executeListGridZoom(zoom);

		PrimeFacesAutomationContext context = executor.current();
		assertEquals("sales", context.getModuleName());
		assertEquals("Contact", context.getDocumentName());
	}

	/**
	 * Creates an executor in an edit view of sales.Customer with a list grid and its zoom button collected.
	 */
	private static TestPrimeFacesSeleneseExecutor newEditExecutorWithListGrid(String listGridIdentifier) {
		TestPrimeFacesSeleneseExecutor executor = new TestPrimeFacesSeleneseExecutor();
		NavigateEdit edit = new NavigateEdit();
		edit.setModuleName("sales");
		edit.setDocumentName("Customer");
		executor.executeNavigateEdit(edit);
		PrimeFacesAutomationContext context = executor.current();
		context.put(listGridIdentifier, mock(UIComponent.class), listGridIdentifier);
		context.put(listGridIdentifier + ".zoom", mock(UIComponent.class), listGridIdentifier);
		return executor;
	}

	/**
	 * Binds a mocked persistence to the thread whose user's customer has the given "sales" module.
	 */
	private static void bindModule(Module module) {
		Customer customer = mock(Customer.class);
		when(customer.getModule("sales")).thenReturn(module);
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(user);
		setPersistenceThreadLocal(persistence);
	}

	@SuppressWarnings("unchecked")
	private static void setPersistenceThreadLocal(AbstractPersistence persistence) {
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
			if (persistence == null) {
				threadLocal.remove();
			}
			else {
				threadLocal.set(persistence);
			}
		}
		catch (ReflectiveOperationException e) {
			throw new IllegalStateException(e);
		}
	}

	private static final class TestPrimeFacesSeleneseExecutor extends PrimeFacesSeleneseExecutor {
		private TestPrimeFacesSeleneseExecutor() {
			super(null, null);
		}

		private PrimeFacesAutomationContext current() {
			return peek();
		}

		@Override
		public void executePushListContext(PushListContext push) {
			PrimeFacesAutomationContext context = new PrimeFacesAutomationContext();
			context.setModuleName(push.getModuleName());
			context.setDocumentName(push.getDocumentName());
			push(context);
		}

		@Override
		public void executePushEditContext(PushEditContext push) {
			PrimeFacesAutomationContext context = new PrimeFacesAutomationContext();
			context.setModuleName(push.getModuleName());
			context.setDocumentName(push.getDocumentName());
			push(context);
		}
	}
}
