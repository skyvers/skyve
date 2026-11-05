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

import java.lang.reflect.Field;
import java.sql.Connection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Geometry;
import org.mockito.ArgumentCaptor;
import org.skyve.impl.metadata.OrderingImpl;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.InverseOne;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.module.ModuleImpl;
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
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.menu.Menu;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.user.UserAccess;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.map.MapModel;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
		stubPersistentHomeDocument(customer, module);
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
		stubPersistentHomeDocument(customer, module);
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
		stubPersistentHomeDocument(customer, module);
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
		stubPersistentHomeDocument(customer, module);
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
		stubPersistentHomeDocument(customer, module);
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
		stubPersistentHomeDocument(customer, module);
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
		stubPersistentHomeDocument(customer, module);
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
		stubPersistentHomeDocument(customer, module);
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
		stubPersistentHomeDocument(customer, module);
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
		stubPersistentHomeDocument(customer, module);
		when(module.getMetadataQueries()).thenReturn(List.of());
		when(module.getMenu()).thenReturn(menu);
		when(module.getRoles()).thenReturn(List.of());
		when(module.getDocument(customer, "Doc")).thenReturn(document);
		when(document.getPersistent()).thenReturn(mock(Persistent.class));

		doReturn(null).when(repository).getListModel(customer, document, "MissingModel", false);

		assertThrows(MetaDataException.class, () -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateModuleForGenerateDomainThrowsWhenMapModelCannotBeResolved() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		MapItem menuItem = new MapItem();
		menuItem.setName("mapModelItem");
		menuItem.setDocumentName("Doc");
		menuItem.setModelName("MissingMapModel");
		MenuImpl menu = new MenuImpl();
		menu.getItems().add(menuItem);

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		stubPersistentHomeDocument(customer, module);
		when(module.getMetadataQueries()).thenReturn(List.of());
		when(module.getMenu()).thenReturn(menu);
		when(module.getRoles()).thenReturn(List.of());
		when(module.getDocument(customer, "Doc")).thenReturn(document);
		when(document.getPersistent()).thenReturn(mock(Persistent.class));

		doThrow(new RuntimeException("missing map model")).when(repository).getMapModel(customer, document, "MissingMapModel", false);

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
		stubPersistentHomeDocument(customer, module);
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
		stubPersistentHomeDocument(customer, module);
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
		stubPersistentHomeDocument(customer, module);
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
		stubPersistentHomeDocument(customer, module);
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
		stubPersistentHomeDocument(customer, module);
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
		stubPersistentHomeDocument(customer, module);
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
		stubPersistentHomeDocument(customer, module);
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
		stubPersistentHomeDocument(customer, module);
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
		stubPersistentHomeDocument(customer, module);
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
		stubPersistentHomeDocument(customer, module);
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
		stubPersistentHomeDocument(customer, module);
		when(module.getMetadataQueries()).thenReturn(List.of());
		when(module.getMenu()).thenReturn(menu);
		when(module.getRoles()).thenReturn(List.of());
		when(module.getDocument(customer, "TransientDoc")).thenReturn(document);
		when(document.getPersistent()).thenReturn(null);

		assertDoesNotThrow(() -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateModuleForGenerateDomainPassesWhenMapModelResolves() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		MapModel<org.skyve.domain.Bean> mapModel = mock(MapModel.class);

		MapItem menuItem = new MapItem();
		menuItem.setName("mapModelItem");
		menuItem.setDocumentName("Doc");
		menuItem.setModelName("ExistingMapModel");
		menuItem.setGeometryBinding("location");
		MenuImpl menu = new MenuImpl();
		menu.getItems().add(menuItem);
		Attribute location = mock(Attribute.class);

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		stubPersistentHomeDocument(customer, module);
		when(module.getMetadataQueries()).thenReturn(List.of());
		when(module.getMenu()).thenReturn(menu);
		when(module.getRoles()).thenReturn(List.of());
		when(module.getDocument(customer, "Doc")).thenReturn(document);
		when(document.getPersistent()).thenReturn(mock(Persistent.class));
		when(document.getAttribute("location")).thenReturn(location);
		when(location.getAttributeType()).thenReturn(AttributeType.geometry);
		doReturn(Geometry.class).when(location).getImplementingType();

		doReturn(mapModel).when(repository).getMapModel(customer, document, "ExistingMapModel", false);

		assertDoesNotThrow(() -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateModuleForGenerateDomainThrowsWhenModelMenuItemHasNoDocument() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);

		MapItem menuItem = new MapItem();
		menuItem.setName("mapModelItem");
		menuItem.setModelName("ExistingMapModel");
		MenuImpl menu = new MenuImpl();
		menu.getItems().add(menuItem);

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		stubPersistentHomeDocument(customer, module);
		when(module.getMetadataQueries()).thenReturn(List.of());
		when(module.getMenu()).thenReturn(menu);
		when(module.getRoles()).thenReturn(List.of());

		MetaDataException e = assertThrows(MetaDataException.class, () -> repository.validateModuleForGenerateDomain(customer, module));
		assertTrue(e.getMessage().contains("has no document"));
		verify(repository, never()).getMapModel(any(), any(), any(), org.mockito.ArgumentMatchers.anyBoolean());
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
		stubPersistentHomeDocument(customer, module);
		when(module.getMetadataQueries()).thenReturn(List.of(query));
		when(module.getMenu()).thenReturn(new MenuImpl());
		when(module.getRoles()).thenReturn(List.of());
		when(importedModule.getMetaDataQuery("knownSql")).thenReturn(importedDefinition);

		doReturn(importedModule).when(repository).getModule(customer, "external");

		assertDoesNotThrow(() -> repository.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateCustomerForGenerateDomainThrowsWhenFeatureRoleModuleCannotBeResolved() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Module homeModule = mock(Module.class);

		when(customer.getName()).thenReturn("testCustomer");
		when(customer.getHomeModule()).thenReturn(homeModule);
		when(customer.getModuleEntries()).thenReturn(Map.of());
		when(customer.getRoles()).thenReturn(List.of());
		when(customer.getTextSearchRoles()).thenReturn(Set.of("admin.someRole"));
		when(customer.getFlagRoles()).thenReturn(Set.of());
		when(customer.getSwitchModeRoles()).thenReturn(Set.of());

		doReturn(null).when(repository).getModule(customer, "admin");

		MetaDataException e = assertThrows(MetaDataException.class, () -> repository.validateCustomerForGenerateDomain(customer));
		assertEquals("Repository returned null for module admin for customer testCustomer", e.getMessage());
	}

	@Test
	void validateCustomerForGenerateDomainThrowsForUnnamedCustomerRoleModuleRole() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Module homeModule = mock(Module.class);
		Module moduleFromCustomer = mock(Module.class);

		CustomerModuleRoleMetaData moduleRoleMetaData = new CustomerModuleRoleMetaData();
		moduleRoleMetaData.setModuleName("admin");

		CustomerRoleMetaData customerRoleMetaData = new CustomerRoleMetaData();
		customerRoleMetaData.setName("testRole");
		customerRoleMetaData.getRoles().add(moduleRoleMetaData);

		when(customer.getName()).thenReturn("testCustomer");
		when(customer.getHomeModule()).thenReturn(homeModule);
		when(customer.getModuleEntries()).thenReturn(Map.of());
		when(customer.getRoles()).thenReturn(List.of(customerRoleMetaData));

		doReturn(moduleFromCustomer).when(repository).getModule(customer, "admin");

		MetaDataException e = assertThrows(MetaDataException.class, () -> repository.validateCustomerForGenerateDomain(customer));
		assertTrue(e.getMessage().contains("does not reference a valid module role"));
	}

	@Test
	void validateDocumentForGenerateDomainThrowsWhenInverseTargetModuleCannotBeResolved() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);
		Document document = inverseDocument(module);

		when(customer.getName()).thenReturn("testCustomer");
		doReturn(module).when(repository).getModule(customer, "admin");
		doReturn(null).when(repository).getModule(customer, "other");

		MetaDataException e = assertThrows(MetaDataException.class, () -> repository.validateDocumentForGenerateDomain(customer, document));
		assertEquals("Repository returned null for module other for customer testCustomer", e.getMessage());
	}

	@Test
	void validateDocumentForGenerateDomainThrowsWhenInverseTargetDocumentCannotBeFound() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);
		Module otherModule = mock(Module.class);
		Document document = inverseDocument(module);

		when(customer.getName()).thenReturn("testCustomer");
		doReturn(module).when(repository).getModule(customer, "admin");
		doReturn(otherModule).when(repository).getModule(customer, "other");
		doReturn(null).when(repository).getDocument(customer, otherModule, "Target");

		MetaDataException e = assertThrows(MetaDataException.class, () -> repository.validateDocumentForGenerateDomain(customer, document));
		assertEquals("The target [documentName] of Target in Inverse inverse in document admin.Owner cannot be found.", e.getMessage());
	}

	@Test
	void resetMenusThrowsWhenAMenuItemHasNoRoleNames() {
		LocalDesignRepository repository = new LocalDesignRepository();
		MenuImpl moduleMenu = new MenuImpl();
		moduleMenu.getItems().add(new RolelessMenuItem());
		Module module = mock(Module.class);
		when(module.getName()).thenReturn("admin");
		when(module.getMenu()).thenReturn(moduleMenu);
		CustomerImpl customer = mock(CustomerImpl.class);
		when(customer.getModules()).thenReturn(List.of(module));
		UserImpl user = mock(UserImpl.class);
		when(user.getCustomer()).thenReturn(customer);

		IllegalStateException e = assertThrows(IllegalStateException.class, () -> repository.resetMenus(user));
		assertEquals("Menu item roleless in module admin has no role names", e.getMessage());
	}

	@Test
	void validateModuleForGenerateDomainThrowsWhenContentAccessBindingIsInvalid() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		Customer customer = mock(Customer.class);
		Module module = moduleWithRoleAccess(customer, UserAccess.content("testModule", "Doc", "missing"));
		when(module.getDocument(customer, "Doc")).thenReturn(mock(Document.class));

		MetaDataException e = assertThrows(MetaDataException.class, () -> repository.validateModuleForGenerateDomain(customer, module));
		assertTrue(e.getMessage().endsWith("] in module testModule with binding missing is not a valid binding."), e.getMessage());
	}

	@Test
	void validateModuleForGenerateDomainThrowsWhenADocumentAccessHasNoDocumentName() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		Customer customer = mock(Customer.class);
		@SuppressWarnings("null") // passes null on purpose
		UserAccess access = UserAccess.dynamicImage("testModule", null, "image");
		Module module = moduleWithRoleAccess(customer, access);

		IllegalStateException e = assertThrows(IllegalStateException.class, () -> repository.validateModuleForGenerateDomain(customer, module));
		assertTrue(e.getMessage().endsWith("] has no document name"), e.getMessage());
	}

	@Test
	void validateModuleForGenerateDomainThrowsWhenAContentAccessHasNoBinding() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		Customer customer = mock(Customer.class);
		@SuppressWarnings("null") // passes null on purpose
		UserAccess access = UserAccess.content("testModule", "Doc", null);
		Module module = moduleWithRoleAccess(customer, access);

		IllegalStateException e = assertThrows(IllegalStateException.class, () -> repository.validateModuleForGenerateDomain(customer, module));
		assertTrue(e.getMessage().endsWith("] has no component"), e.getMessage());
	}

	@Test
	void validateModuleForGenerateDomainThrowsWhenMapItemHasNoGeometryBinding() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		MapItem menuItem = new MapItem();
		menuItem.setName("mapItem");
		menuItem.setDocumentName("Doc");
		MenuImpl menu = new MenuImpl();
		menu.getItems().add(menuItem);

		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		stubPersistentHomeDocument(customer, module);
		when(module.getMetadataQueries()).thenReturn(List.of());
		when(module.getMenu()).thenReturn(menu);
		when(module.getDocument(customer, "Doc")).thenReturn(document);
		when(document.getPersistent()).thenReturn(mock(Persistent.class));

		MetaDataException e = assertThrows(MetaDataException.class, () -> repository.validateModuleForGenerateDomain(customer, module));
		assertEquals("Map Menu [mapItem] in module testModule has no geometryBinding.", e.getMessage());
	}

	@Test
	void validateDocumentForGenerateDomainThrowsWhenANamedConditionIsNotDefined() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Document document = mock(Document.class);
		when(document.getOwningModuleName()).thenReturn("admin");
		when(document.getName()).thenReturn("Owner");
		when(document.getConditionNames()).thenReturn(Set.of("active"));
		doReturn(mock(Module.class)).when(repository).getModule(customer, "admin");

		IllegalStateException e = assertThrows(IllegalStateException.class, () -> repository.validateDocumentForGenerateDomain(customer, document));
		assertEquals("Condition active in document admin.Owner is named but not defined", e.getMessage());
	}

	@Test
	void validateDocumentForGenerateDomainThrowsWhenACollectionOrderingBindingIsInvalid() {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);
		Module otherModule = mock(Module.class);
		Document targetDocument = mock(Document.class);
		when(targetDocument.getOwningModuleName()).thenReturn("other");
		when(targetDocument.getName()).thenReturn("Target");
		when(module.getDocumentRefs()).thenReturn(Map.of("Target", new DocumentRef()));
		when(module.getDocument(customer, "Target")).thenReturn(targetDocument);
		doReturn(module).when(repository).getModule(customer, "admin");
		doReturn(otherModule).when(repository).getModule(customer, "other");

		CollectionImpl collection = new CollectionImpl();
		collection.setName("items");
		collection.setDocumentName("Target");
		collection.getOrdering().add(new OrderingImpl("missing", SortDirection.ascending));
		Document document = mock(Document.class);
		when(document.getOwningModuleName()).thenReturn("admin");
		when(document.getName()).thenReturn("Owner");
		when(document.getConditionNames()).thenReturn(Set.of());
		doReturn(List.of(collection)).when(document).getAttributes();

		MetaDataException e = assertThrows(MetaDataException.class, () -> repository.validateDocumentForGenerateDomain(customer, document));
		assertEquals("The order by binding of missing in collection items in document admin.Owner is invalid", e.getMessage());
	}

	@Test
	void validateViewForGenerateDomainThrowsWhenAReportAccessReportDoesNotExist() throws Exception {
		LocalDesignRepository repository = org.mockito.Mockito.spy(new LocalDesignRepository());
		CustomerImpl customer = mock(CustomerImpl.class);
		ModuleImpl module = mock(ModuleImpl.class);
		when(module.getName()).thenReturn("admin");
		DocumentImpl document = new DocumentImpl();
		document.setOwningModuleName("admin");
		document.setName("Doc");
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Doc")).thenReturn(document);
		doReturn(module).when(repository).getModule(customer, "admin");
		doReturn(null).when(repository).getReportFileName(customer, document, "missingReport");

		ViewImpl view = new ViewImpl();
		view.setName("edit");
		view.setTitle("Edit");
		Set<UserAccess> accesses = new TreeSet<>();
		accesses.add(UserAccess.report("admin", "Doc", "missingReport"));
		Field accessesField = ViewImpl.class.getDeclaredField("accesses");
		accessesField.setAccessible(true);
		accessesField.set(view, accesses);

		MetaDataException e = assertThrows(MetaDataException.class, () -> repository.validateViewForGenerateDomain(customer, document, view, "desktop"));
		assertTrue(e.getMessage().endsWith(" in view edit for module/document/report admin/Doc/missingReport does not exist."), e.getMessage());
	}

	/**
	 * Stubs a module whose single role has the given access, with no menu items or queries.
	 */
	private static Module moduleWithRoleAccess(Customer customer, UserAccess access) {
		Module module = mock(Module.class);
		RoleImpl role = new RoleImpl();
		role.setName("admin");
		role.getAccesses().put(access, Set.of());
		when(customer.getName()).thenReturn("testCustomer");
		when(module.getName()).thenReturn("testModule");
		stubPersistentHomeDocument(customer, module);
		when(module.getMetadataQueries()).thenReturn(List.of());
		when(module.getMenu()).thenReturn(new MenuImpl());
		when(module.getRoles()).thenReturn(List.of(role));
		return module;
	}

	/**
	 * A menu item (not a group) that breaks the invariant that menu items have role names.
	 */
	private static final class RolelessMenuItem implements MenuItem {
		private static final long serialVersionUID = 1L;

		@Override
		public @Nonnull String getName() {
			return "roleless";
		}

		@Override
		public @Nullable Set<String> getRoleNames() {
			return null;
		}

		@Override
		public @Nonnull Set<String> getUxUis() {
			return Set.of();
		}

		@Override
		public boolean isApplicable(@Nonnull String uxui) {
			return true;
		}

		@Override
		public @Nonnull Map<String, String> getProperties() {
			return Map.of();
		}
	}

	/**
	 * Stubs a persistent home document so the module home document check passes.
	 */
	private static void stubPersistentHomeDocument(Customer customer, Module module) {
		Document homeDocument = mock(Document.class);
		when(homeDocument.getPersistent()).thenReturn(mock(Persistent.class));
		when(module.getHomeDocumentName()).thenReturn("PersistentHomeDocument");
		when(module.getDocument(customer, "PersistentHomeDocument")).thenReturn(homeDocument);
	}

	/**
	 * Builds an admin.Owner document mock with a single inverse to other.Target (imported into the admin module).
	 */
	private static Document inverseDocument(Module module) {
		InverseOne inverse = new InverseOne();
		inverse.setName("inverse");
		inverse.setDocumentName("Target");
		inverse.setReferenceName("owner");

		DocumentRef targetRef = mock(DocumentRef.class);
		when(targetRef.getReferencedModuleName()).thenReturn("other");
		when(module.getDocumentRefs()).thenReturn(Map.of("Target", targetRef));

		Document document = mock(Document.class);
		when(document.getOwningModuleName()).thenReturn("admin");
		when(document.getName()).thenReturn("Owner");
		when(document.getConditionNames()).thenReturn(Set.of());
		doReturn(List.of(inverse)).when(document).getAttributes();
		when(document.getUniqueConstraints()).thenReturn(List.of());
		return document;
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
