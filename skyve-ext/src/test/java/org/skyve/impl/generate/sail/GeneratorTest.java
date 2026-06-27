package org.skyve.impl.generate.sail;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MenuGroupImpl;
import org.skyve.impl.metadata.module.menu.MenuImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Interaction;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.sail.language.Procedure;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.Comment;
import org.skyve.metadata.sail.language.step.interaction.actions.Delete;
import org.skyve.metadata.sail.language.step.interaction.actions.Save;
import org.skyve.metadata.sail.language.step.interaction.TestDataEnter;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridNew;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.metadata.sail.language.step.interaction.session.Login;
import org.skyve.metadata.sail.language.step.interaction.session.Logout;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.web.UserAgentType;

@SuppressWarnings({"static-method", "boxing"})
class GeneratorTest {
	private final ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();

	@AfterEach
	void tearDown() {
		if (originalRepository == null) {
			ProvidedRepositoryFactory.clear();
		}
		else {
			ProvidedRepositoryFactory.set(originalRepository);
		}
	}

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

	@Test
	void visitMenusOverloadsOmitModulesWithEmptyMenus() throws Exception {
		UserImpl user = mock(UserImpl.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(user.getCustomer()).thenReturn(customer);
		when(user.getAccessibleModuleNames()).thenReturn(Set.of("sales"));
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getName()).thenReturn("sales");
		when(user.getModuleMenu("sales")).thenReturn(new MenuImpl());
		ProvidedRepositoryFactory.set(repository);

		List<Automation> withoutLogin = Generator.visitMenus(user, "desktop", UserAgentType.desktop, TestStrategy.Assert);
		List<Automation> withLogin = Generator.visitMenus(user, "secret", "desktop", UserAgentType.desktop, TestStrategy.Assert);

		assertThat(withoutLogin.isEmpty(), is(true));
		assertThat(withLogin.isEmpty(), is(true));
		verify(repository, times(2)).resetMenus(user);
	}

	@Test
	void visitMenusPasswordOverloadAddsLoginForNonEmptyMenuAutomation() throws Exception {
		UserImpl user = mock(UserImpl.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		MenuImpl menu = new MenuImpl();
		ListItem item = new ListItem();
		item.setName("Invoices");
		item.setDocumentName("Invoice");
		menu.getItems().add(item);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(user.getName()).thenReturn("admin");
		when(user.getCustomer()).thenReturn(customer);
		when(user.getAccessibleModuleNames()).thenReturn(Set.of("sales"));
		when(user.getModuleMenu("sales")).thenReturn(menu);
		when(user.canCreateDocument(document)).thenReturn(false);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getName()).thenReturn("sales");
		when(module.getDocument(customer, "Invoice")).thenReturn(document);
		ProvidedRepositoryFactory.set(repository);

		List<Automation> result = Generator.visitMenus(user, "secret", "desktop", UserAgentType.desktop, TestStrategy.Assert);

		assertThat(result.size(), is(1));
		Automation automation = result.get(0);
		assertThat(automation.getInteractions().size(), is(1));
		assertThat(automation.getBefore().getSteps().get(0), instanceOf(Login.class));
		assertThat(automation.getAfter().getSteps().get(0), instanceOf(Logout.class));
	}

	@Test
	void visitModuleCreatesCrudInteractionForAccessiblePersistentDocumentWithoutCreateAccess() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocumentRefs()).thenReturn(Map.of("Invoice", documentRef("sales")));
		when(module.getDocument(customer, "Invoice")).thenReturn(document);
		when(document.isAbstract()).thenReturn(false);
		when(document.getParentDocumentName()).thenReturn(null);
		when(document.getPersistent()).thenReturn(new Persistent());
		when(user.canAccessDocument(document)).thenReturn(true);
		when(user.canCreateDocument(document)).thenReturn(false);

		Automation result = Generator.visitModule(user, "sales", "desktop", UserAgentType.desktop, TestStrategy.Assert);

		assertThat(result.getInteractions().size(), is(1));
		Interaction interaction = result.getInteractions().get(0);
		assertThat(interaction.getName(), is("CRUD document Invoice"));
		assertThat(interaction.getSteps().size(), is(1));
		assertThat(interaction.getSteps().get(0), instanceOf(NavigateList.class));
		NavigateList navigate = (NavigateList) interaction.getSteps().get(0);
		assertThat(navigate.getModuleName(), is("sales"));
		assertThat(navigate.getDocumentName(), is("Invoice"));
	}

	@Test
	void visitModulesPasswordOverloadKeepsNonEmptyAutomation() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Persistent persistent = new Persistent();
		persistent.setName("INV_Invoice");
		when(user.getName()).thenReturn("admin");
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModules()).thenReturn(List.of(module));
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getName()).thenReturn("sales");
		when(module.getDocumentRefs()).thenReturn(Map.of("Invoice", documentRef("sales")));
		when(module.getDocument(customer, "Invoice")).thenReturn(document);
		when(document.isAbstract()).thenReturn(false);
		when(document.getParentDocumentName()).thenReturn(null);
		when(document.getPersistent()).thenReturn(persistent);
		when(user.canAccessDocument(document)).thenReturn(true);
		when(user.canCreateDocument(document)).thenReturn(false);

		List<Automation> result = Generator.visitModules(user, "secret", "desktop", UserAgentType.phone, TestStrategy.None);

		assertThat(result.size(), is(1));
		Automation automation = result.get(0);
		assertThat(automation.getInteractions().size(), is(1));
		assertThat(automation.getBefore().getSteps().get(0), instanceOf(Login.class));
		assertThat(automation.getAfter().getSteps().get(0), instanceOf(Logout.class));
	}

	@Test
	void visitModulePasswordOverloadAddsLoginForNonEmptyModule() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Persistent persistent = new Persistent();
		persistent.setName("INV_Invoice");
		when(user.getName()).thenReturn("admin");
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocumentRefs()).thenReturn(Map.of("Invoice", documentRef("sales")));
		when(module.getDocument(customer, "Invoice")).thenReturn(document);
		when(document.isAbstract()).thenReturn(false);
		when(document.getParentDocumentName()).thenReturn(null);
		when(document.getPersistent()).thenReturn(persistent);
		when(user.canAccessDocument(document)).thenReturn(true);
		when(user.canCreateDocument(document)).thenReturn(false);

		Automation result = Generator.visitModule(user, "secret", "sales", "desktop", UserAgentType.desktop, TestStrategy.Assert);

		assertThat(result.getInteractions().size(), is(1));
		assertThat(result.getBefore().getSteps().get(0), instanceOf(Login.class));
		assertThat(result.getAfter().getSteps().get(0), instanceOf(Logout.class));
	}

	@Test
	void visitModuleCreatesEditInteractionForAccessibleTransientDocument() throws Exception {
		User user = mock(User.class);
		CustomerImpl customer = new CustomerImpl();
		ModuleImpl module = module("sales");
		module.getDocumentRefs().put("Settings", documentRef("sales"));
		DocumentImpl document = document("Settings", null);
		ViewImpl view = view();
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(user.getCustomer()).thenReturn(customer);
		when(user.canAccessDocument(document)).thenReturn(true);
		when(repository.getModule(customer, "sales")).thenReturn(module);
		when(repository.getDocument(customer, module, "Settings")).thenReturn(document);
		when(repository.getView("desktop", customer, document, "create")).thenReturn(view);
		when(repository.getView("desktop", customer, document, "edit")).thenReturn(view);
		ProvidedRepositoryFactory.set(repository);

		Automation result = Generator.visitModule(user, "sales", "desktop", UserAgentType.desktop, TestStrategy.None);

		assertThat(result.getInteractions().size(), is(1));
		Interaction interaction = result.getInteractions().get(0);
		assertThat(interaction.getName(), is("Edit document Settings"));
		assertThat(interaction.getSteps().size(), is(2));
		assertThat(interaction.getSteps().get(0), instanceOf(NavigateEdit.class));
		NavigateEdit navigate = (NavigateEdit) interaction.getSteps().get(0);
		assertThat(navigate.getModuleName(), is("sales"));
		assertThat(navigate.getDocumentName(), is("Settings"));
		assertThat(interaction.getSteps().get(1), instanceOf(TestDataEnter.class));
	}

	@Test
	void visitModuleCreatesNewGridStepWhenPersistentDocumentCanBeCreated() throws Exception {
		User user = mock(User.class);
		CustomerImpl customer = new CustomerImpl();
		ModuleImpl module = module("sales");
		module.getDocumentRefs().put("Invoice", documentRef("sales"));
		Persistent persistent = new Persistent();
		persistent.setName("INV_Invoice");
		DocumentImpl document = document("Invoice", persistent);
		ViewImpl view = view();
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(user.getCustomer()).thenReturn(customer);
		when(user.canAccessDocument(document)).thenReturn(true);
		when(user.canCreateDocument(document)).thenReturn(true);
		when(repository.getModule(customer, "sales")).thenReturn(module);
		when(repository.getDocument(customer, module, "Invoice")).thenReturn(document);
		when(repository.getView("desktop", customer, document, "create")).thenReturn(view);
		when(repository.getView("desktop", customer, document, "edit")).thenReturn(view);
		ProvidedRepositoryFactory.set(repository);

		Automation result = Generator.visitModule(user, "sales", "desktop", UserAgentType.desktop, TestStrategy.Assert);

		assertThat(result.getInteractions().size(), is(1));
		Interaction interaction = result.getInteractions().get(0);
		assertThat(interaction.getName(), is("CRUD document Invoice"));
		assertThat(interaction.getSteps().size(), is(3));
		assertThat(interaction.getSteps().get(0), instanceOf(NavigateList.class));
		assertThat(interaction.getSteps().get(1), instanceOf(ListGridNew.class));
		ListGridNew nu = (ListGridNew) interaction.getSteps().get(1);
		assertThat(nu.getModuleName(), is("sales"));
		assertThat(nu.getDocumentName(), is("Invoice"));
		assertThat(interaction.getSteps().get(2), instanceOf(TestDataEnter.class));
	}

	@Test
	void visitModuleCreatePathAddsSaveAndDeleteStepsForPersistentDocumentActions() throws Exception {
		User user = mock(User.class);
		CustomerImpl customer = new CustomerImpl();
		ModuleImpl module = module("sales");
		module.getDocumentRefs().put("Invoice", documentRef("sales"));
		Persistent persistent = new Persistent();
		persistent.setName("INV_Invoice");
		DocumentImpl document = document("Invoice", persistent);
		ViewImpl view = viewWith(
				implicitAction(ImplicitActionName.Save),
				implicitAction(ImplicitActionName.Delete));
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(user.getCustomer()).thenReturn(customer);
		when(user.canAccessDocument(document)).thenReturn(true);
		when(user.canCreateDocument(document)).thenReturn(true);
		when(repository.getModule(customer, "sales")).thenReturn(module);
		when(repository.getDocument(customer, module, "Invoice")).thenReturn(document);
		when(repository.getView("desktop", customer, document, "create")).thenReturn(view);
		when(repository.getView("desktop", customer, document, "edit")).thenReturn(view);
		ProvidedRepositoryFactory.set(repository);

		Automation result = Generator.visitModule(user, "sales", "desktop", UserAgentType.desktop, TestStrategy.Assert);

		assertThat(result.getInteractions().size(), is(1));
		List<Step> steps = result.getInteractions().get(0).getSteps();
		assertThat(steps.size(), is(7));
		assertThat(steps.get(0), instanceOf(NavigateList.class));
		assertThat(steps.get(1), instanceOf(ListGridNew.class));
		assertThat(steps.get(2), instanceOf(TestDataEnter.class));
		assertThat(steps.get(3), instanceOf(Save.class));
		assertThat(steps.get(4), instanceOf(TestDataEnter.class));
		assertThat(steps.get(5), instanceOf(Save.class));
		assertThat(steps.get(6), instanceOf(Delete.class));
	}

	@Test
	void visitMenuCreatesGroupedListInteraction() throws Exception {
		UserImpl user = mock(UserImpl.class);
		CustomerImpl customer = new CustomerImpl();
		ModuleImpl module = module("sales");
		Document document = mock(Document.class);
		MenuImpl menu = new MenuImpl();
		MenuGroupImpl group = new MenuGroupImpl();
		group.setName("Reports");
		ListItem item = new ListItem();
		item.setName("Invoices");
		item.setDocumentName("Invoice");
		group.getItems().add(item);
		menu.getItems().add(group);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(user.getCustomer()).thenReturn(customer);
		when(user.getModuleMenu("sales")).thenReturn(menu);
		when(user.canCreateDocument(document)).thenReturn(false);
		when(repository.getModule(customer, "sales")).thenReturn(module);
		when(repository.getDocument(customer, module, "Invoice")).thenReturn(document);
		ProvidedRepositoryFactory.set(repository);

		Automation result = Generator.visitMenu(user, "sales", "desktop", UserAgentType.desktop, TestStrategy.Assert);

		assertThat(result.getInteractions().size(), is(1));
		Interaction interaction = result.getInteractions().get(0);
		assertThat(interaction.getName(), is("Menu Reports::Invoices"));
		assertThat(interaction.getSteps().size(), is(1));
		assertThat(interaction.getSteps().get(0), instanceOf(NavigateList.class));
		NavigateList navigate = (NavigateList) interaction.getSteps().get(0);
		assertThat(navigate.getModuleName(), is("sales"));
		assertThat(navigate.getDocumentName(), is("Invoice"));
	}

	@Test
	void visitMenuListItemWithNamedQuerySetsQueryName() throws Exception {
		UserImpl user = mock(UserImpl.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		MenuImpl menu = new MenuImpl();
		ListItem item = new ListItem();
		item.setName("Open invoices");
		item.setQueryName("openInvoices");
		menu.getItems().add(item);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(user.getCustomer()).thenReturn(customer);
		when(user.getModuleMenu("sales")).thenReturn(menu);
		when(user.canCreateDocument(document)).thenReturn(false);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getName()).thenReturn("sales");
		when(module.getMetaDataQuery("openInvoices")).thenReturn(query);
		when(query.getDocumentModule(customer)).thenReturn(module);
		when(query.getDocumentName()).thenReturn("Invoice");
		when(module.getDocument(customer, "Invoice")).thenReturn(document);
		ProvidedRepositoryFactory.set(repository);

		Automation result = Generator.visitMenu(user, "sales", "desktop", UserAgentType.desktop, TestStrategy.Assert);

		assertThat(result.getInteractions().size(), is(1));
		Interaction interaction = result.getInteractions().get(0);
		assertThat(interaction.getName(), is("Menu Open invoices"));
		assertThat(interaction.getSteps().size(), is(1));
		assertThat(interaction.getSteps().get(0), instanceOf(NavigateList.class));
		NavigateList navigate = (NavigateList) interaction.getSteps().get(0);
		assertThat(navigate.getModuleName(), is("sales"));
		assertThat(navigate.getQueryName(), is("openInvoices"));
		assertThat(navigate.getDocumentName(), is((String) null));
	}

	@Test
	void visitMenuListItemWithDefaultQueryUsesDocumentName() throws Exception {
		UserImpl user = mock(UserImpl.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		MenuImpl menu = new MenuImpl();
		ListItem item = new ListItem();
		item.setName("Invoices");
		item.setQueryName("Invoice");
		menu.getItems().add(item);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(user.getCustomer()).thenReturn(customer);
		when(user.getModuleMenu("sales")).thenReturn(menu);
		when(user.canCreateDocument(document)).thenReturn(false);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getName()).thenReturn("sales");
		when(module.getMetaDataQuery("Invoice")).thenReturn(null);
		when(module.getDocumentDefaultQuery(customer, "Invoice")).thenReturn(query);
		when(query.getDocumentModule(customer)).thenReturn(module);
		when(query.getDocumentName()).thenReturn("Invoice");
		when(module.getDocument(customer, "Invoice")).thenReturn(document);
		ProvidedRepositoryFactory.set(repository);

		Automation result = Generator.visitMenu(user, "sales", "desktop", UserAgentType.desktop, TestStrategy.Assert);

		assertThat(result.getInteractions().size(), is(1));
		Interaction interaction = result.getInteractions().get(0);
		assertThat(interaction.getSteps().size(), is(1));
		assertThat(interaction.getSteps().get(0), instanceOf(NavigateList.class));
		NavigateList navigate = (NavigateList) interaction.getSteps().get(0);
		assertThat(navigate.getModuleName(), is("sales"));
		assertThat(navigate.getDocumentName(), is("Invoice"));
		assertThat(navigate.getQueryName(), is((String) null));
	}

	@Test
	void visitMenuListItemWithModelNameUsesDrivingDocumentForCreateAccess() throws Exception {
		UserImpl user = mock(UserImpl.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document listDocument = mock(Document.class);
		Document drivingDocument = mock(Document.class);
		ListModel<Bean> model = mock(ListModel.class);
		MenuImpl menu = new MenuImpl();
		ListItem item = new ListItem();
		item.setName("Invoice model");
		item.setDocumentName("Invoice");
		item.setModelName("overdue");
		menu.getItems().add(item);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(user.getCustomer()).thenReturn(customer);
		when(user.getModuleMenu("sales")).thenReturn(menu);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getName()).thenReturn("sales");
		when(module.getDocument(customer, "Invoice")).thenReturn(listDocument);
		when(listDocument.getListModel(customer, "overdue", false)).thenReturn(model);
		when(model.getDrivingDocument()).thenReturn(drivingDocument);
		when(user.canCreateDocument(drivingDocument)).thenReturn(false);
		ProvidedRepositoryFactory.set(repository);

		Automation result = Generator.visitMenu(user, "sales", "desktop", UserAgentType.desktop, TestStrategy.Assert);

		assertThat(result.getInteractions().size(), is(1));
		Interaction interaction = result.getInteractions().get(0);
		assertThat(interaction.getSteps().size(), is(1));
		assertThat(interaction.getSteps().get(0), instanceOf(NavigateList.class));
		NavigateList navigate = (NavigateList) interaction.getSteps().get(0);
		assertThat(navigate.getModuleName(), is("sales"));
		assertThat(navigate.getDocumentName(), is("Invoice"));
		assertThat(navigate.getModelName(), is("overdue"));
		verify(user).canCreateDocument(drivingDocument);
	}

	@Test
	void visitMenuEditItemCreatesNavigateEditInteraction() throws Exception {
		UserImpl user = mock(UserImpl.class);
		CustomerImpl customer = new CustomerImpl();
		ModuleImpl module = module("sales");
		DocumentImpl document = document("Settings", null);
		ViewImpl view = view();
		MenuImpl menu = new MenuImpl();
		EditItem item = new EditItem();
		item.setName("Settings");
		item.setDocumentName("Settings");
		menu.getItems().add(item);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(user.getCustomer()).thenReturn(customer);
		when(user.getModuleMenu("sales")).thenReturn(menu);
		when(repository.getModule(customer, "sales")).thenReturn(module);
		when(repository.getDocument(customer, module, "Settings")).thenReturn(document);
		when(repository.getView("desktop", customer, document, "create")).thenReturn(view);
		when(repository.getView("desktop", customer, document, "edit")).thenReturn(view);
		ProvidedRepositoryFactory.set(repository);

		Automation result = Generator.visitMenu(user, "sales", "desktop", UserAgentType.desktop, TestStrategy.None);

		assertThat(result.getInteractions().size(), is(1));
		Interaction interaction = result.getInteractions().get(0);
		assertThat(interaction.getName(), is("Menu Settings"));
		assertThat(interaction.getSteps().size(), is(2));
		assertThat(interaction.getSteps().get(0), instanceOf(NavigateEdit.class));
		NavigateEdit navigate = (NavigateEdit) interaction.getSteps().get(0);
		assertThat(navigate.getModuleName(), is("sales"));
		assertThat(navigate.getDocumentName(), is("Settings"));
		assertThat(interaction.getSteps().get(1), instanceOf(TestDataEnter.class));
	}

	@Test
	void visitMenuEditItemAddsSaveStepsForPersistentDocumentActions() throws Exception {
		UserImpl user = mock(UserImpl.class);
		CustomerImpl customer = new CustomerImpl();
		ModuleImpl module = module("sales");
		Persistent persistent = new Persistent();
		persistent.setName("SET_Settings");
		DocumentImpl document = document("Settings", persistent);
		ViewImpl view = viewWith(implicitAction(ImplicitActionName.Save));
		MenuImpl menu = new MenuImpl();
		EditItem item = new EditItem();
		item.setName("Settings");
		item.setDocumentName("Settings");
		menu.getItems().add(item);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(user.getCustomer()).thenReturn(customer);
		when(user.getModuleMenu("sales")).thenReturn(menu);
		when(repository.getModule(customer, "sales")).thenReturn(module);
		when(repository.getDocument(customer, module, "Settings")).thenReturn(document);
		when(repository.getView("desktop", customer, document, "create")).thenReturn(view);
		when(repository.getView("desktop", customer, document, "edit")).thenReturn(view);
		ProvidedRepositoryFactory.set(repository);

		Automation result = Generator.visitMenu(user, "sales", "desktop", UserAgentType.desktop, TestStrategy.None);

		assertThat(result.getInteractions().size(), is(1));
		List<Step> steps = result.getInteractions().get(0).getSteps();
		assertThat(steps.size(), is(6));
		assertThat(steps.get(0), instanceOf(NavigateEdit.class));
		assertThat(steps.get(1), instanceOf(TestDataEnter.class));
		assertThat(steps.get(2), instanceOf(Save.class));
		assertThat(steps.get(3), instanceOf(TestDataEnter.class));
		assertThat(steps.get(4), instanceOf(Save.class));
		assertThat(steps.get(5), instanceOf(Save.class));
	}

	@Test
	void visitMenuPasswordOverloadAddsLoginAndLogout() throws Exception {
		UserImpl user = mock(UserImpl.class);
		CustomerImpl customer = new CustomerImpl();
		ModuleImpl module = module("sales");
		Document document = mock(Document.class);
		MenuImpl menu = new MenuImpl();
		ListItem item = new ListItem();
		item.setName("Invoices");
		item.setDocumentName("Invoice");
		menu.getItems().add(item);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(user.getName()).thenReturn("admin");
		when(user.getCustomer()).thenReturn(customer);
		when(user.getModuleMenu("sales")).thenReturn(menu);
		when(user.canCreateDocument(document)).thenReturn(false);
		when(repository.getModule(customer, "sales")).thenReturn(module);
		when(repository.getDocument(customer, module, "Invoice")).thenReturn(document);
		ProvidedRepositoryFactory.set(repository);

		Automation result = Generator.visitMenu(user, "secret", "sales", "desktop", UserAgentType.desktop, TestStrategy.Assert);

		assertThat(result.getInteractions().size(), is(1));
		assertThat(result.getBefore().getSteps().get(0), instanceOf(Login.class));
		assertThat(result.getAfter().getSteps().get(0), instanceOf(Logout.class));
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

	private static ModuleImpl module(String name) {
		ModuleImpl result = new ModuleImpl();
		result.setName(name);
		return result;
	}

	private static DocumentImpl document(String name, Persistent persistent) {
		DocumentImpl result = new DocumentImpl();
		result.setName(name);
		result.setOwningModuleName("sales");
		result.setPersistent(persistent);
		return result;
	}

	private static ViewImpl view() {
		ViewImpl result = new ViewImpl();
		result.setName("create");
		return result;
	}

	private static ViewImpl viewWith(ActionImpl... actions) {
		ViewImpl result = view();
		for (ActionImpl action : actions) {
			result.putAction(action);
		}
		return result;
	}

	private static ActionImpl implicitAction(ImplicitActionName implicitName) {
		ActionImpl result = new ActionImpl();
		result.setImplicitName(implicitName);
		return result;
	}
}
