package org.skyve.impl.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.sql.Connection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.MenuGroupImpl;
import org.skyve.impl.metadata.module.menu.MenuImpl;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.impl.metadata.module.query.BizQLReferenceImpl;
import org.skyve.impl.metadata.module.query.MetaDataQueryReferenceImpl;
import org.skyve.impl.metadata.module.query.SQLReferenceImpl;
import org.skyve.impl.metadata.repository.customer.CustomerModuleRoleMetaData;
import org.skyve.impl.metadata.repository.customer.CustomerRoleMetaData;
import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;
import org.skyve.impl.metadata.user.ActionPrivilege;
import org.skyve.impl.metadata.user.RoleImpl;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.menu.Menu;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.user.UserAccess;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.map.MapModel;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;

@SuppressWarnings("static-method")
class LocalDesignRepositoryTest {
	@Test
	void getUseScaffoldedViewsAlwaysReturnsTrue() {
		assertTrue(new LocalDesignRepository().getUseScaffoldedViews());
	}

	@Test
	void unsupportedUserAndSchedulingMethodsThrow() {
		LocalDesignRepository repository = new LocalDesignRepository();

		assertThrows(UnsupportedOperationException.class, () -> repository.retrieveUser("admin"));
		assertThrows(UnsupportedOperationException.class, () -> repository.populatePermissions(mock(User.class)));
		assertThrows(UnsupportedOperationException.class, () -> repository.resetUserPermissions(mock(User.class)));
		assertThrows(UnsupportedOperationException.class,
				() -> repository.populateUser(mock(User.class), mock(Connection.class)));
		assertThrows(UnsupportedOperationException.class, repository::retrieveAllScheduledJobsForAllCustomers);
		assertThrows(UnsupportedOperationException.class, repository::retrieveAllScheduledReportsForAllCustomers);
		assertThrows(UnsupportedOperationException.class, () -> repository.retrievePublicUserName("demo"));
	}

	@Test
	@SuppressWarnings("boxing")
	void resetMenusFiltersSecuredItemsAndRemovesEmptyGroups() {
		LocalDesignRepository repository = new LocalDesignRepository();

		MenuImpl moduleMenu = new MenuImpl();
		moduleMenu.getItems().add(menuItem("publicItem", null));
		moduleMenu.getItems().add(menuItem("adminItem", "admin"));
		moduleMenu.getItems().add(menuItem("managerItem", "manager"));

		MenuGroupImpl emptyAfterFilter = new MenuGroupImpl();
		emptyAfterFilter.setName("emptyGroup");
		emptyAfterFilter.getItems().add(menuItem("hiddenNested", "manager"));
		moduleMenu.getItems().add(emptyAfterFilter);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("admin");
		when(module.getMenu()).thenReturn(moduleMenu);

		CustomerImpl customer = mock(CustomerImpl.class);
		when(customer.getModules()).thenReturn(List.of(module));

		UserImpl user = mock(UserImpl.class);
		when(user.getCustomer()).thenReturn(customer);
		doReturn(true).when(user).isInRole("admin", "admin");
		doReturn(false).when(user).isInRole("admin", "manager");

		repository.resetMenus(user);

		ArgumentCaptor<Menu> captured = ArgumentCaptor.forClass(Menu.class);
		verify(user).putModuleMenu(eq(module), captured.capture());
		Menu filteredMenu = captured.getValue();
		List<MenuItem> filteredItems = filteredMenu.getItems();
		assertEquals(2, filteredItems.size());
		assertEquals("publicItem", filteredItems.get(0).getName());
		assertEquals("adminItem", filteredItems.get(1).getName());
	}

	@Test
	void resetMenusWithNoModulesDoesNothing() {
		LocalDesignRepository repository = new LocalDesignRepository();

		CustomerImpl customer = mock(CustomerImpl.class);
		when(customer.getModules()).thenReturn(List.of());

		UserImpl user = mock(UserImpl.class);
		when(user.getCustomer()).thenReturn(customer);

		assertDoesNotThrow(() -> repository.resetMenus(user));
		verify(user, never()).putModuleMenu(any(), any());
	}

	@Test
	void getImplementingTypeForGenerateDomainValidationReturnsTypeWhenResolvable() {
		Attribute attribute = mock(Attribute.class);
		doReturn(String.class).when(attribute).getImplementingType();

		assertEquals(String.class, LocalDesignRepository.getImplementingTypeForGenerateDomainValidation(attribute));
	}

	@Test
	void getImplementingTypeForGenerateDomainValidationReturnsNullForUnresolvedEnumerationClassLoadingFailure() {
		Enumeration enumeration = mock(Enumeration.class);
		doThrow(new MetaDataException("Enum class is not generated yet",
				new ClassNotFoundException("Enum class not found"))).when(enumeration).getImplementingType();

		assertNull(LocalDesignRepository.getImplementingTypeForGenerateDomainValidation(enumeration));
	}

	@Test
	void getImplementingTypeForGenerateDomainValidationThrowsForInvalidEnumerationMetaData() {
		Enumeration enumeration = mock(Enumeration.class);
		doThrow(new MetaDataException("Invalid enum metadata")).when(enumeration).getImplementingType();

		assertThrows(MetaDataException.class,
				() -> LocalDesignRepository.getImplementingTypeForGenerateDomainValidation(enumeration));
	}

	@Test
	void getImplementingTypeForGenerateDomainValidationThrowsForUnresolvedNonEnumeration() {
		Attribute attribute = mock(Attribute.class);
		doThrow(new MetaDataException("Any non-enum metadata error")).when(attribute).getImplementingType();

		assertThrows(MetaDataException.class,
				() -> LocalDesignRepository.getImplementingTypeForGenerateDomainValidation(attribute));
	}

	@Test
	void validateCustomerForGenerateDomainThrowsWhenHomeModuleMissing() {
		LocalDesignRepository repository = new LocalDesignRepository();
		CustomerImpl customer = mock(CustomerImpl.class);

		when(customer.getName()).thenReturn("testCustomer");
		when(customer.getHomeModule()).thenReturn(null);

		assertThrows(MetaDataException.class, () -> repository.validateCustomerForGenerateDomain(customer));
	}

	@Test
	void validateModuleForGenerateDomainThrowsWhenTransientHomeDocumentNotEditRef() {
		LocalDesignRepository repository = new LocalDesignRepository();
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);
		Document homeDocument = mock(Document.class);

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		when(module.getHomeDocumentName()).thenReturn("HomeDoc");
		when(module.getHomeRef()).thenReturn(ViewType.list);
		when(module.getDocument(customer, "HomeDoc")).thenReturn(homeDocument);
		when(homeDocument.getPersistent()).thenReturn(null);

		assertThrows(MetaDataException.class, () -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateCustomerForGenerateDomainPassesForMinimalValidSetup() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Module homeModule = mock(Module.class);

		when(customer.getName()).thenReturn("testCustomer");
		when(customer.getHomeModule()).thenReturn(homeModule);
		when(customer.getModuleEntries()).thenReturn(Map.of());
		when(customer.getRoles()).thenReturn(List.of());
		when(customer.getTextSearchRoles()).thenReturn(Set.of());
		when(customer.getFlagRoles()).thenReturn(Set.of());
		when(customer.getSwitchModeRoles()).thenReturn(Set.of());

		assertDoesNotThrow(() -> repository.validateCustomerForGenerateDomain(customer));
	}

	@Test
	void validateCustomerForGenerateDomainThrowsForTopLayoutMismatch() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Module homeModule = mock(Module.class);
		Module module = mock(Module.class);

		when(customer.getName()).thenReturn("testCustomer");
		when(customer.getHomeModule()).thenReturn(homeModule);
		when(module.getFormLabelLayout()).thenReturn(FormLabelLayout.top);

		Map<String, FormLabelLayout> entries = new LinkedHashMap<>();
		entries.put("admin", null);
		when(customer.getModuleEntries()).thenReturn(entries);
		when(customer.getRoles()).thenReturn(List.of());
		when(customer.getTextSearchRoles()).thenReturn(Set.of());
		when(customer.getFlagRoles()).thenReturn(Set.of());
		when(customer.getSwitchModeRoles()).thenReturn(Set.of());

		doReturn(module).when(repository).getModule(customer, "admin");

		assertThrows(MetaDataException.class, () -> repository.validateCustomerForGenerateDomain(customer));
	}

	@Test
	void validateCustomerForGenerateDomainThrowsForInvalidFeatureRole() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Module homeModule = mock(Module.class);
		Module module = mock(Module.class);

		when(customer.getName()).thenReturn("testCustomer");
		when(customer.getHomeModule()).thenReturn(homeModule);
		when(customer.getModuleEntries()).thenReturn(Map.of());
		when(customer.getRoles()).thenReturn(List.of());
		when(customer.getTextSearchRoles()).thenReturn(Set.of("admin.missingRole"));
		when(customer.getFlagRoles()).thenReturn(Set.of());
		when(customer.getSwitchModeRoles()).thenReturn(Set.of());
		when(module.getRole("missingRole")).thenReturn(null);

		doReturn(module).when(repository).getModule(customer, "admin");

		assertThrows(MetaDataException.class, () -> repository.validateCustomerForGenerateDomain(customer));
	}

	@Test
	void validateCustomerForGenerateDomainThrowsForUnknownModuleEntry() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Module homeModule = mock(Module.class);

		when(customer.getName()).thenReturn("testCustomer");
		when(customer.getHomeModule()).thenReturn(homeModule);

		Map<String, FormLabelLayout> entries = new LinkedHashMap<>();
		entries.put("unknown", FormLabelLayout.side);
		when(customer.getModuleEntries()).thenReturn(entries);
		when(customer.getRoles()).thenReturn(List.of());
		when(customer.getTextSearchRoles()).thenReturn(Set.of());
		when(customer.getFlagRoles()).thenReturn(Set.of());
		when(customer.getSwitchModeRoles()).thenReturn(Set.of());

		doReturn(null).when(repository).getModule(customer, "unknown");

		assertThrows(MetaDataException.class, () -> repository.validateCustomerForGenerateDomain(customer));
	}

	@Test
	void validateCustomerForGenerateDomainThrowsForInvalidCustomerRoleModuleRole() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Module homeModule = mock(Module.class);
		Module moduleFromCustomer = mock(Module.class);

		CustomerModuleRoleMetaData moduleRoleMetaData = new CustomerModuleRoleMetaData();
		moduleRoleMetaData.setModuleName("admin");
		moduleRoleMetaData.setName("missingRole");

		CustomerRoleMetaData customerRoleMetaData = new CustomerRoleMetaData();
		customerRoleMetaData.setName("testRole");
		customerRoleMetaData.getRoles().add(moduleRoleMetaData);

		when(customer.getName()).thenReturn("testCustomer");
		when(customer.getHomeModule()).thenReturn(homeModule);
		when(customer.getModuleEntries()).thenReturn(Map.of());
		when(customer.getRoles()).thenReturn(List.of(customerRoleMetaData));
		when(customer.getTextSearchRoles()).thenReturn(Set.of());
		when(customer.getFlagRoles()).thenReturn(Set.of());
		when(customer.getSwitchModeRoles()).thenReturn(Set.of());

		when(moduleFromCustomer.getRole("missingRole")).thenReturn(null);

		doReturn(moduleFromCustomer).when(repository).getModule(customer, "admin");

		assertThrows(MetaDataException.class, () -> repository.validateCustomerForGenerateDomain(customer));
	}

	@Test
	void validateModuleForGenerateDomainThrowsWhenImportedQueryModuleUnknown() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);

		SQLReferenceImpl query = new SQLReferenceImpl("importedQ", "external", "q1");
		query.setOwningModule(module);

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		when(module.getHomeDocumentName()).thenReturn(null);
		when(module.getMetadataQueries()).thenReturn(List.of(query));
		when(module.getMenu()).thenReturn(new MenuImpl());
		when(module.getRoles()).thenReturn(List.of());

		doReturn(null).when(repository).getModule(customer, "external");

		assertThrows(MetaDataException.class, () -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateModuleForGenerateDomainThrowsWhenImportedMetaDataQueryUnknown() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);
		Module importedModule = mock(Module.class);

		MetaDataQueryReferenceImpl query = new MetaDataQueryReferenceImpl("importedQ", "external", "missingQuery");
		query.setOwningModule(module);

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		when(module.getHomeDocumentName()).thenReturn(null);
		when(module.getMetadataQueries()).thenReturn(List.of(query));
		when(module.getMenu()).thenReturn(new MenuImpl());
		when(module.getRoles()).thenReturn(List.of());
		when(importedModule.getMetaDataQuery("missingQuery")).thenReturn(null);

		doReturn(importedModule).when(repository).getModule(customer, "external");

		assertThrows(MetaDataException.class, () -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateModuleForGenerateDomainThrowsWhenImportedSQLQueryUnknown() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);
		Module importedModule = mock(Module.class);

		SQLReferenceImpl query = new SQLReferenceImpl("importedSql", "external", "missingSql");
		query.setOwningModule(module);

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		when(module.getHomeDocumentName()).thenReturn(null);
		when(module.getMetadataQueries()).thenReturn(List.of(query));
		when(module.getMenu()).thenReturn(new MenuImpl());
		when(module.getRoles()).thenReturn(List.of());
		when(importedModule.getMetaDataQuery("missingSql")).thenReturn(null);

		doReturn(importedModule).when(repository).getModule(customer, "external");

		assertThrows(MetaDataException.class, () -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateModuleForGenerateDomainThrowsWhenImportedBizQLQueryUnknown() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);
		Module importedModule = mock(Module.class);

		BizQLReferenceImpl query = new BizQLReferenceImpl("importedBizQl", "external", "missingBizQl");
		query.setOwningModule(module);

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		when(module.getHomeDocumentName()).thenReturn(null);
		when(module.getMetadataQueries()).thenReturn(List.of(query));
		when(module.getMenu()).thenReturn(new MenuImpl());
		when(module.getRoles()).thenReturn(List.of());
		when(importedModule.getMetaDataQuery("missingBizQl")).thenReturn(null);

		doReturn(importedModule).when(repository).getModule(customer, "external");

		assertThrows(MetaDataException.class, () -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateModuleForGenerateDomainThrowsWhenImportedQueryDrivingDocumentUnknown() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);
		Module importedModule = mock(Module.class);
		MetaDataQueryDefinition importedDefinition = mock(MetaDataQueryDefinition.class);

		MetaDataQueryReferenceImpl query = new MetaDataQueryReferenceImpl("importedQ", "external", "knownQuery");
		query.setOwningModule(module);

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		when(module.getHomeDocumentName()).thenReturn(null);
		when(module.getMetadataQueries()).thenReturn(List.of(query));
		when(module.getMenu()).thenReturn(new MenuImpl());
		when(module.getRoles()).thenReturn(List.of());
		when(importedModule.getMetaDataQuery("knownQuery")).thenReturn(importedDefinition);
		when(importedDefinition.getDocumentName()).thenReturn("MissingDoc");

		doReturn(importedModule).when(repository).getModule(customer, "external");
		doReturn(null).when(repository).getDocument(customer, module, "MissingDoc");

		assertThrows(MetaDataException.class, () -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateModuleForGenerateDomainThrowsWhenMenuDocumentMissing() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);

		ListItem menuItem = new ListItem();
		menuItem.setName("missingDocItem");
		menuItem.setDocumentName("MissingDoc");
		MenuImpl menu = new MenuImpl();
		menu.getItems().add(menuItem);

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		when(module.getHomeDocumentName()).thenReturn(null);
		when(module.getMetadataQueries()).thenReturn(List.of());
		when(module.getMenu()).thenReturn(menu);
		when(module.getRoles()).thenReturn(List.of());
		when(module.getDocument(customer, "MissingDoc")).thenThrow(new MetaDataException("missing"));

		assertThrows(MetaDataException.class, () -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateModuleForGenerateDomainThrowsWhenTransientDocumentUsedInListItem() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		ListItem menuItem = new ListItem();
		menuItem.setName("transientList");
		menuItem.setDocumentName("TransientDoc");
		MenuImpl menu = new MenuImpl();
		menu.getItems().add(menuItem);

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		when(module.getHomeDocumentName()).thenReturn(null);
		when(module.getMetadataQueries()).thenReturn(List.of());
		when(module.getMenu()).thenReturn(menu);
		when(module.getRoles()).thenReturn(List.of());
		when(module.getDocument(customer, "TransientDoc")).thenReturn(document);
		when(document.getPersistent()).thenReturn(null);

		assertThrows(MetaDataException.class, () -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateModuleForGenerateDomainThrowsWhenMenuQueryMissing() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		ListItem menuItem = new ListItem();
		menuItem.setName("missingQueryItem");
		menuItem.setDocumentName("Doc");
		menuItem.setQueryName("MissingQuery");
		MenuImpl menu = new MenuImpl();
		menu.getItems().add(menuItem);

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		when(module.getHomeDocumentName()).thenReturn(null);
		when(module.getMetadataQueries()).thenReturn(List.of());
		when(module.getMenu()).thenReturn(menu);
		when(module.getRoles()).thenReturn(List.of());
		when(module.getDocument(customer, "Doc")).thenReturn(document);
		when(document.getPersistent()).thenReturn(mock(Persistent.class));
		when(module.getMetaDataQuery("MissingQuery")).thenReturn(null);

		assertThrows(MetaDataException.class, () -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateModuleForGenerateDomainThrowsWhenTreeDocumentNotHierarchical() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		TreeItem menuItem = new TreeItem();
		menuItem.setName("treeItem");
		menuItem.setDocumentName("Doc");
		MenuImpl menu = new MenuImpl();
		menu.getItems().add(menuItem);

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		when(module.getHomeDocumentName()).thenReturn(null);
		when(module.getMetadataQueries()).thenReturn(List.of());
		when(module.getMenu()).thenReturn(menu);
		when(module.getRoles()).thenReturn(List.of());
		when(module.getDocument(customer, "Doc")).thenReturn(document);
		when(document.getPersistent()).thenReturn(mock(Persistent.class));
		when(document.getParentDocumentName()).thenReturn("ParentDoc");
		when(document.getName()).thenReturn("Doc");

		assertThrows(MetaDataException.class, () -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateModuleForGenerateDomainThrowsWhenListModelCannotBeResolved() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		ListItem menuItem = new ListItem();
		menuItem.setName("listModelItem");
		menuItem.setDocumentName("Doc");
		menuItem.setModelName("MissingModel");
		MenuImpl menu = new MenuImpl();
		menu.getItems().add(menuItem);

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		when(module.getHomeDocumentName()).thenReturn(null);
		when(module.getMetadataQueries()).thenReturn(List.of());
		when(module.getMenu()).thenReturn(menu);
		when(module.getRoles()).thenReturn(List.of());
		when(module.getDocument(customer, "Doc")).thenReturn(document);
		when(document.getPersistent()).thenReturn(mock(Persistent.class));

		doReturn(null).when(repository).getListModel(customer, document, "MissingModel", false);

		assertThrows(MetaDataException.class, () -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	@SuppressWarnings("null")
	void validateModuleForGenerateDomainThrowsWhenMapModelCannotBeResolved() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);

		MapItem menuItem = new MapItem();
		menuItem.setName("mapModelItem");
		menuItem.setModelName("MissingMapModel");
		MenuImpl menu = new MenuImpl();
		menu.getItems().add(menuItem);

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		when(module.getHomeDocumentName()).thenReturn(null);
		when(module.getMetadataQueries()).thenReturn(List.of());
		when(module.getMenu()).thenReturn(menu);
		when(module.getRoles()).thenReturn(List.of());

		doThrow(new RuntimeException("missing map model")).when(repository).getMapModel(customer, null, "MissingMapModel", false);

		assertThrows(MetaDataException.class, () -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateModuleForGenerateDomainAllowsTransientDocumentForListModelItem() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Document drivingDocument = mock(Document.class);
		ListModel<org.skyve.domain.Bean> listModel = mock(ListModel.class);

		ListItem menuItem = new ListItem();
		menuItem.setName("transientListModelItem");
		menuItem.setDocumentName("TransientDoc");
		menuItem.setModelName("ValidModel");
		MenuImpl menu = new MenuImpl();
		menu.getItems().add(menuItem);

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		when(module.getHomeDocumentName()).thenReturn(null);
		when(module.getMetadataQueries()).thenReturn(List.of());
		when(module.getMenu()).thenReturn(menu);
		when(module.getRoles()).thenReturn(List.of());
		when(module.getDocument(customer, "TransientDoc")).thenReturn(document);
		when(document.getPersistent()).thenReturn(null);
		when(listModel.getDrivingDocument()).thenReturn(drivingDocument);

		doReturn(listModel).when(repository).getListModel(customer, document, "ValidModel", false);

		assertDoesNotThrow(() -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateModuleForGenerateDomainThrowsWhenActionPrivilegeReferencesMissingAction() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		ActionPrivilege actionPrivilege = new ActionPrivilege();
		actionPrivilege.setName("missingAction");
		actionPrivilege.setDocumentName("Doc");

		RoleImpl role = new RoleImpl();
		role.setName("admin");
		role.getPrivileges().add(actionPrivilege);

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		when(module.getHomeDocumentName()).thenReturn(null);
		when(module.getMetadataQueries()).thenReturn(List.of());
		when(module.getMenu()).thenReturn(new MenuImpl());
		when(module.getRoles()).thenReturn(List.of(role));
		when(module.getDocument(customer, "Doc")).thenReturn(document);
		when(document.getName()).thenReturn("Doc");

		doReturn(null).when(repository).getClassAction(customer, document, "missingAction", false, false);
		doReturn(null).when(repository).getMetaDataAction(customer, document, "missingAction");

		assertThrows(MetaDataException.class, () -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateModuleForGenerateDomainThrowsWhenModelAggregateAccessModelMissing() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		RoleImpl role = new RoleImpl();
		role.setName("admin");
		role.getAccesses().put(UserAccess.modelAggregate("testModule", "Doc", "missingModel"), Set.of());

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		when(module.getHomeDocumentName()).thenReturn(null);
		when(module.getMetadataQueries()).thenReturn(List.of());
		when(module.getMenu()).thenReturn(new MenuImpl());
		when(module.getRoles()).thenReturn(List.of(role));
		when(module.getDocument(customer, "Doc")).thenReturn(document);

		doThrow(new RuntimeException("missing model")).when(repository).getModel(customer, document, "missingModel", false);

		assertThrows(MetaDataException.class, () -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateModuleForGenerateDomainThrowsWhenReportAccessReportMissing() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Module reportModule = mock(Module.class);
		Document reportDocument = mock(Document.class);

		RoleImpl role = new RoleImpl();
		role.setName("admin");
		role.getAccesses().put(UserAccess.report("reports", "ReportDoc", "missingReport"), Set.of());

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		when(module.getHomeDocumentName()).thenReturn(null);
		when(module.getMetadataQueries()).thenReturn(List.of());
		when(module.getMenu()).thenReturn(new MenuImpl());
		when(module.getRoles()).thenReturn(List.of(role));
		when(customer.getModule("reports")).thenReturn(reportModule);
		when(reportModule.getDocument(customer, "ReportDoc")).thenReturn(reportDocument);

		doReturn(null).when(repository).getReportFileName(customer, reportDocument, "missingReport");

		assertThrows(MetaDataException.class, () -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateModuleForGenerateDomainPassesWhenActionPrivilegeResolvesToMetaDataAction() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		ActionPrivilege actionPrivilege = new ActionPrivilege();
		actionPrivilege.setName("metaAction");
		actionPrivilege.setDocumentName("Doc");

		RoleImpl role = new RoleImpl();
		role.setName("admin");
		role.getPrivileges().add(actionPrivilege);

		ActionMetaData actionMetaData = new ActionMetaData();
		actionMetaData.setName("metaAction");

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		when(module.getHomeDocumentName()).thenReturn(null);
		when(module.getMetadataQueries()).thenReturn(List.of());
		when(module.getMenu()).thenReturn(new MenuImpl());
		when(module.getRoles()).thenReturn(List.of(role));
		when(module.getDocument(customer, "Doc")).thenReturn(document);

		doReturn(null).when(repository).getClassAction(customer, document, "metaAction", false, false);
		doReturn(actionMetaData).when(repository).getMetaDataAction(customer, document, "metaAction");

		assertDoesNotThrow(() -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateModuleForGenerateDomainPassesWhenModelAggregateAccessResolves() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		MetaData model = mock(MetaData.class);

		RoleImpl role = new RoleImpl();
		role.setName("admin");
		role.getAccesses().put(UserAccess.modelAggregate("testModule", "Doc", "existingModel"), Set.of());

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		when(module.getHomeDocumentName()).thenReturn(null);
		when(module.getMetadataQueries()).thenReturn(List.of());
		when(module.getMenu()).thenReturn(new MenuImpl());
		when(module.getRoles()).thenReturn(List.of(role));
		when(module.getDocument(customer, "Doc")).thenReturn(document);

		doReturn(model).when(repository).getModel(customer, document, "existingModel", false);

		assertDoesNotThrow(() -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateModuleForGenerateDomainPassesWhenReportAccessResolves() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Module reportModule = mock(Module.class);
		Document reportDocument = mock(Document.class);

		RoleImpl role = new RoleImpl();
		role.setName("admin");
		role.getAccesses().put(UserAccess.report("reports", "ReportDoc", "existingReport"), Set.of());

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		when(module.getHomeDocumentName()).thenReturn(null);
		when(module.getMetadataQueries()).thenReturn(List.of());
		when(module.getMenu()).thenReturn(new MenuImpl());
		when(module.getRoles()).thenReturn(List.of(role));
		when(customer.getModule("reports")).thenReturn(reportModule);
		when(reportModule.getDocument(customer, "ReportDoc")).thenReturn(reportDocument);

		doReturn("existingReport.jasper").when(repository).getReportFileName(customer, reportDocument, "existingReport");

		assertDoesNotThrow(() -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateModuleForGenerateDomainThrowsWhenDynamicImageAccessMissing() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		RoleImpl role = new RoleImpl();
		role.setName("admin");
		role.getAccesses().put(UserAccess.dynamicImage("testModule", "Doc", "missingImage"), Set.of());

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		when(module.getHomeDocumentName()).thenReturn(null);
		when(module.getMetadataQueries()).thenReturn(List.of());
		when(module.getMenu()).thenReturn(new MenuImpl());
		when(module.getRoles()).thenReturn(List.of(role));
		when(module.getDocument(customer, "Doc")).thenReturn(document);

		doThrow(new RuntimeException("missing image")).when(repository).getDynamicImage(customer, document, "missingImage", false);

		assertThrows(MetaDataException.class, () -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateModuleForGenerateDomainPassesWhenDynamicImageAccessResolves() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		DynamicImage<org.skyve.domain.Bean> dynamicImage = mock(DynamicImage.class);

		RoleImpl role = new RoleImpl();
		role.setName("admin");
		role.getAccesses().put(UserAccess.dynamicImage("testModule", "Doc", "existingImage"), Set.of());

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		when(module.getHomeDocumentName()).thenReturn(null);
		when(module.getMetadataQueries()).thenReturn(List.of());
		when(module.getMenu()).thenReturn(new MenuImpl());
		when(module.getRoles()).thenReturn(List.of(role));
		when(module.getDocument(customer, "Doc")).thenReturn(document);

		doReturn(dynamicImage).when(repository).getDynamicImage(customer, document, "existingImage", false);

		assertDoesNotThrow(() -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateModuleForGenerateDomainPassesWhenActionPrivilegeResolvesToClassAction() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		MetaData classAction = mock(MetaData.class);

		ActionPrivilege actionPrivilege = new ActionPrivilege();
		actionPrivilege.setName("classAction");
		actionPrivilege.setDocumentName("Doc");

		RoleImpl role = new RoleImpl();
		role.setName("admin");
		role.getPrivileges().add(actionPrivilege);

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		when(module.getHomeDocumentName()).thenReturn(null);
		when(module.getMetadataQueries()).thenReturn(List.of());
		when(module.getMenu()).thenReturn(new MenuImpl());
		when(module.getRoles()).thenReturn(List.of(role));
		when(module.getDocument(customer, "Doc")).thenReturn(document);

		doReturn(classAction).when(repository).getClassAction(customer, document, "classAction", false, false);

		assertDoesNotThrow(() -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateModuleForGenerateDomainAllowsTransientDocumentForEditItem() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		EditItem menuItem = new EditItem();
		menuItem.setName("transientEdit");
		menuItem.setDocumentName("TransientDoc");
		MenuImpl menu = new MenuImpl();
		menu.getItems().add(menuItem);

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		when(module.getHomeDocumentName()).thenReturn(null);
		when(module.getMetadataQueries()).thenReturn(List.of());
		when(module.getMenu()).thenReturn(menu);
		when(module.getRoles()).thenReturn(List.of());
		when(module.getDocument(customer, "TransientDoc")).thenReturn(document);
		when(document.getPersistent()).thenReturn(null);

		assertDoesNotThrow(() -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	@SuppressWarnings("null")
	void validateModuleForGenerateDomainPassesWhenMapModelResolves() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		MapModel<org.skyve.domain.Bean> mapModel = mock(MapModel.class);

		MapItem menuItem = new MapItem();
		menuItem.setName("mapModelItem");
		menuItem.setModelName("ExistingMapModel");
		MenuImpl menu = new MenuImpl();
		menu.getItems().add(menuItem);

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		when(module.getHomeDocumentName()).thenReturn(null);
		when(module.getMetadataQueries()).thenReturn(List.of());
		when(module.getMenu()).thenReturn(menu);
		when(module.getRoles()).thenReturn(List.of());

		doReturn(mapModel).when(repository).getMapModel(customer, null, "ExistingMapModel", false);

		assertDoesNotThrow(() -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateModuleForGenerateDomainPassesWhenImportedSQLQueryExists() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Module importedModule = mock(Module.class);
		MetaDataQueryDefinition importedDefinition = mock(MetaDataQueryDefinition.class);

		SQLReferenceImpl query = new SQLReferenceImpl("importedSql", "external", "knownSql");
		query.setOwningModule(module);

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		when(module.getHomeDocumentName()).thenReturn(null);
		when(module.getMetadataQueries()).thenReturn(List.of(query));
		when(module.getMenu()).thenReturn(new MenuImpl());
		when(module.getRoles()).thenReturn(List.of());
		when(importedModule.getMetaDataQuery("knownSql")).thenReturn(importedDefinition);

		doReturn(importedModule).when(repository).getModule(customer, "external");

		assertDoesNotThrow(() -> repository.validateModuleForGenerateDomain(customer, module));
	}

	private static ListItem menuItem(String name, String roleName) {
		ListItem item = new ListItem();
		item.setName(name);
		if (roleName != null) {
			item.getRoleNames().add(roleName);
		}
		return item;
	}
}
