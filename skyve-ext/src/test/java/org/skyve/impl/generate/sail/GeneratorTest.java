package org.skyve.impl.generate.sail;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.sail.language.Procedure;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.Comment;
import org.skyve.metadata.sail.language.step.interaction.session.Login;
import org.skyve.metadata.sail.language.step.interaction.session.Logout;
import org.skyve.metadata.user.User;
import org.skyve.web.UserAgentType;

@SuppressWarnings({"static-method", "boxing"})
class GeneratorTest {
	@Test
	void addLoginAndOutCreatesBeforeAndAfterProcedures() throws Exception {
		User user = mock(User.class);
		when(user.getName()).thenReturn("admin");
		Automation automation = new Automation();

		invokeAddLoginAndOut(automation, user, "demo", "secret");

		assertThat(automation.getBefore().getSteps().size(), is(1));
		assertThat(automation.getAfter().getSteps().size(), is(1));
		Step before = automation.getBefore().getSteps().get(0);
		assertThat(before, instanceOf(Login.class));
		Login login = (Login) before;
		assertThat(login.getCustomer(), is("demo"));
		assertThat(login.getUser(), is("admin"));
		assertThat(login.getPassword(), is("secret"));
		assertThat(automation.getAfter().getSteps().get(0), instanceOf(Logout.class));
	}

	@Test
	void addLoginAndOutPrependsLoginToExistingBeforeAndAppendsLogoutToExistingAfter() throws Exception {
		User user = mock(User.class);
		when(user.getName()).thenReturn("admin");
		Automation automation = new Automation();
		Procedure before = new Procedure();
		Comment existingBefore = new Comment();
		before.getSteps().add(existingBefore);
		automation.setBefore(before);
		Procedure after = new Procedure();
		Comment existingAfter = new Comment();
		after.getSteps().add(existingAfter);
		automation.setAfter(after);

		invokeAddLoginAndOut(automation, user, null, "secret");

		assertThat(automation.getBefore().getSteps().size(), is(2));
		assertThat(automation.getBefore().getSteps().get(0), instanceOf(Login.class));
		assertThat(automation.getBefore().getSteps().get(1), is(existingBefore));
		assertThat(automation.getAfter().getSteps().size(), is(2));
		assertThat(automation.getAfter().getSteps().get(0), is(existingAfter));
		assertThat(automation.getAfter().getSteps().get(1), instanceOf(Logout.class));
	}

	@Test
	void visitModuleSkipsAbstractChildInaccessibleAndForeignDocumentRefs() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document abstractDocument = mock(Document.class);
		Document childDocument = mock(Document.class);
		Document inaccessibleDocument = mock(Document.class);
		when(user.getCustomer()).thenReturn(customer);
		when(user.canAccessDocument(abstractDocument)).thenReturn(true);
		when(user.canAccessDocument(childDocument)).thenReturn(true);
		when(user.canAccessDocument(inaccessibleDocument)).thenReturn(false);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getName()).thenReturn("sales");
		when(module.getDocumentRefs()).thenReturn(Map.of(
				"AbstractDoc", documentRef("sales"),
				"ChildDoc", documentRef("sales"),
				"InaccessibleDoc", documentRef("sales"),
				"ForeignDoc", documentRef("admin")));
		when(module.getDocument(customer, "AbstractDoc")).thenReturn(abstractDocument);
		when(module.getDocument(customer, "ChildDoc")).thenReturn(childDocument);
		when(module.getDocument(customer, "InaccessibleDoc")).thenReturn(inaccessibleDocument);
		when(abstractDocument.isAbstract()).thenReturn(true);
		when(childDocument.isAbstract()).thenReturn(false);
		when(childDocument.getParentDocumentName()).thenReturn("Parent");
		when(inaccessibleDocument.isAbstract()).thenReturn(false);
		when(inaccessibleDocument.getParentDocumentName()).thenReturn(null);

		Automation result = Generator.visitModule(user, "demo", "secret", "sales", "desktop",
				UserAgentType.desktop, TestStrategy.Assert);

		assertThat(result.getInteractions().isEmpty(), is(true));
		assertThat(result.getUxui(), is("desktop"));
		assertThat(result.getUserAgentType(), is(UserAgentType.desktop));
		assertThat(result.getTestStrategy(), is(TestStrategy.Assert));
		assertThat(result.getBefore().getSteps().get(0), instanceOf(Login.class));
		assertThat(result.getAfter().getSteps().get(0), instanceOf(Logout.class));
		verify(module, never()).getDocument(customer, "ForeignDoc");
	}

	@Test
	void visitModulesSkipsTestModuleAndOmitsEmptyAutomations() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module testModule = mock(Module.class);
		Module salesModule = mock(Module.class);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModules()).thenReturn(List.of(testModule, salesModule));
		when(testModule.getName()).thenReturn("test");
		when(salesModule.getName()).thenReturn("sales");
		when(customer.getModule("sales")).thenReturn(salesModule);
		when(salesModule.getDocumentRefs()).thenReturn(Map.of());

		List<Automation> result = Generator.visitModules(user, "desktop", UserAgentType.phone, TestStrategy.None);

		assertThat(result.isEmpty(), is(true));
		verify(customer, never()).getModule("test");
	}

	private static void invokeAddLoginAndOut(Automation automation, User user, String customer, String password) throws Exception {
		Method method = Generator.class.getDeclaredMethod("addLoginAndOut", Automation.class, User.class, String.class, String.class);
		method.setAccessible(true);
		method.invoke(null, automation, user, customer, password);
	}

	private static DocumentRef documentRef(String owningModuleName) {
		DocumentRef result = new DocumentRef();
		result.setOwningModuleName(owningModuleName);
		return result;
	}
}
