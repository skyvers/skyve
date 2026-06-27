package org.skyve.impl.sail.execution.pf;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.sail.language.step.context.PushEditContext;
import org.skyve.metadata.sail.language.step.context.PushListContext;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.metadata.sail.language.step.interaction.session.Login;
import org.skyve.metadata.sail.language.step.interaction.session.Logout;

@SuppressWarnings("static-method")
class PrimeFacesSeleneseExecutorTest {
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

	private static final class TestPrimeFacesSeleneseExecutor extends PrimeFacesSeleneseExecutor {
		private TestPrimeFacesSeleneseExecutor() {
			super(null, null);
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
