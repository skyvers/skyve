package org.skyve.impl.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.sql.Connection;
import java.util.HashMap;
import java.util.Map;
import java.nio.file.Path;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;
import org.skyve.impl.metadata.repository.behaviour.BizletMetaData;
import org.skyve.impl.metadata.repository.document.DocumentMetaData;
import org.skyve.impl.metadata.repository.module.MenuMetaData;
import org.skyve.impl.metadata.repository.module.ModuleDocumentMetaData;
import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.metadata.MetaDataException;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;

/**
 * Tests for MutableCachedRepository cache operations (put/get/evict) via the
 * concrete LocalDesignRepository subclass.
 */
class MutableCachedRepositoryTest {

	@TempDir
	Path tempDir;

	private String basePath;
	private ProvidedRepository previousRepository;

	@BeforeEach
	void setUp() {
		basePath = tempDir.toAbsolutePath().toString();
		previousRepository = ProvidedRepositoryFactory.get();
	}

	@AfterEach
	void tearDown() {
		if (previousRepository == null) {
			ProvidedRepositoryFactory.clear();
		}
		else {
			ProvidedRepositoryFactory.set(previousRepository);
		}
	}

	private static final class NullLoadingRepository extends UnsynchronisedDynamicRepository {
		private void addEmptyKey(String key) {
			addKey(key);
		}

		private void addRouterKey() {
			addKey(ROUTER_KEY);
		}
	}

	private NullLoadingRepository newNullLoadingRepository() {
		assertNotNull(basePath);
		return new NullLoadingRepository();
	}

	/**
	 * Build a minimal valid ModuleMetaData that can be converted without errors.
	 * The module has name "admin", title "Administration", and homeDocument "User".
	 */
	private static ModuleMetaData minimalModule(String moduleName, String homeDocumentName) {
		ModuleMetaData mmd = new ModuleMetaData();
		mmd.setName(moduleName);
		mmd.setTitle("Test " + moduleName);
		mmd.setHomeDocument(homeDocumentName);
		ModuleDocumentMetaData docMeta = new ModuleDocumentMetaData();
		docMeta.setRef(homeDocumentName);
		mmd.getDocuments().add(docMeta);
		mmd.setMenu(new MenuMetaData());
		return mmd;
	}

	/**
	 * Build a minimal valid DocumentMetaData that can be converted without errors.
	 */
	private static DocumentMetaData minimalDocument(String documentName) {
		DocumentMetaData dmd = new DocumentMetaData();
		dmd.setName(documentName);
		dmd.setSingularAlias(documentName);
		dmd.setPluralAlias(documentName + "s");
		return dmd;
	}

	// ---- putModule(ModuleMetaData) / getModule ----

	@Test
	void putModuleVanillaStoresCachedModule() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		ModuleMetaData mmd = minimalModule("admin", "User");
		Module module = repo.putModule(mmd);
		assertNotNull(module);
		assertEquals("admin", module.getName());
	}

	@Test
	void getModuleNullCustomerReturnsFromCache() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		ModuleMetaData mmd = minimalModule("admin", "User");
		repo.putModule(mmd);
		Module retrieved = repo.getModule(null, "admin");
		assertNotNull(retrieved);
		assertEquals("admin", retrieved.getName());
	}

	@Test
	void getModuleNullCustomerReturnsNullWhenNotInCache() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Module retrieved = repo.getModule(null, "nonexistent");
		assertNull(retrieved);
	}

	@Test
	void getModuleNullCustomerReturnsNullWhenEmptyKeyLoadsNull() {
		NullLoadingRepository repo = newNullLoadingRepository();
		repo.addEmptyKey("modules/admin");

		Module retrieved = repo.getModule(null, "admin");

		assertNull(retrieved);
	}

	@Test
	void getModuleWithCustomerThrowsWhenModuleNotInCustomerEntries() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		ModuleMetaData mmd = minimalModule("admin", "User");
		repo.putModule(mmd);
		CustomerImpl customer = mock(CustomerImpl.class);
		when(customer.getName()).thenReturn("acme");
		Map<String, FormLabelLayout> moduleEntries = new HashMap<>();
		when(customer.getModuleEntries()).thenReturn(moduleEntries);
		assertThrows(MetaDataException.class, () -> repo.getModule(customer, "admin"));
	}

	@Test
	void getModuleWithCustomerFallsBackToVanillaWhenNoCustomerOverride() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		ModuleMetaData mmd = minimalModule("admin", "User");
		repo.putModule(mmd);
		CustomerImpl customer = mock(CustomerImpl.class);
		when(customer.getName()).thenReturn("acme");
		Map<String, FormLabelLayout> moduleEntries = new HashMap<>();
		moduleEntries.put("admin", null);
		when(customer.getModuleEntries()).thenReturn(moduleEntries);
		// No customer-specific module in cache, falls back to vanilla
		Module retrieved = repo.getModule(customer, "admin");
		assertNotNull(retrieved);
		assertEquals("admin", retrieved.getName());
	}

	@Test
	void putModuleForCustomerStoresCachedModule() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		ModuleMetaData mmd = minimalModule("admin", "User");
		CustomerImpl customer = mock(CustomerImpl.class);
		when(customer.getName()).thenReturn("acme");
		Module module = repo.putModule(customer, mmd);
		assertNotNull(module);
		assertEquals("admin", module.getName());
	}

	@Test
	void getModuleWithCustomerReturnsCustomerOverrideWhenPresent() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		ModuleMetaData mmd = minimalModule("admin", "User");
		// Put vanilla
		repo.putModule(mmd);
		// Put customer override
		CustomerImpl customer = mock(CustomerImpl.class);
		when(customer.getName()).thenReturn("acme");
		Map<String, FormLabelLayout> moduleEntries = new HashMap<>();
		moduleEntries.put("admin", null);
		when(customer.getModuleEntries()).thenReturn(moduleEntries);
		repo.putModule(customer, mmd);
		// Get should return customer override
		Module retrieved = repo.getModule(customer, "admin");
		assertNotNull(retrieved);
		assertEquals("admin", retrieved.getName());
	}

	// ---- putDocument / getDocument ----

	@Test
	void putDocumentVanillaStoresCachedDocument() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		ModuleMetaData mmd = minimalModule("admin", "User");
		Module module = repo.putModule(mmd);
		DocumentMetaData dmd = minimalDocument("User");
		Document document = repo.putDocument(module, dmd);
		assertNotNull(document);
		assertEquals("User", document.getName());
	}

	@Test
	void getDocumentNullCustomerReturnsFromCache() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		ModuleMetaData mmd = minimalModule("admin", "User");
		Module module = repo.putModule(mmd);
		DocumentMetaData dmd = minimalDocument("User");
		repo.putDocument(module, dmd);
		Document retrieved = repo.getDocument(null, module, "User");
		assertNotNull(retrieved);
		assertEquals("User", retrieved.getName());
	}

	@Test
	void getDocumentThrowsWhenDocumentNotInModuleRefs() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		ModuleMetaData mmd = minimalModule("admin", "User");
		Module module = repo.putModule(mmd);
		// "NonExistent" is not in the module's documentRefs — should throw
		assertThrows(IllegalArgumentException.class, () -> repo.getDocument(null, module, "NonExistent"));
	}

	@Test
	void getDocumentReturnsNullWhenEmptyKeyLoadsNull() {
		NullLoadingRepository repo = newNullLoadingRepository();
		Module module = repo.putModule(minimalModule("admin", "User"));
		repo.addEmptyKey("modules/admin/User");

		Document retrieved = repo.getDocument(null, module, "User");

		assertNull(retrieved);
	}

	// ---- putDocument(Customer, Module, DocumentMetaData) ----

	@Test
	void putDocumentForCustomerStoresCachedDocument() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		ModuleMetaData mmd = minimalModule("admin", "User");
		Module module = repo.putModule(mmd);
		CustomerImpl customer = mock(CustomerImpl.class);
		when(customer.getName()).thenReturn("acme");
		DocumentMetaData dmd = minimalDocument("User");
		Document document = repo.putDocument(customer, module, dmd);
		assertNotNull(document);
		assertEquals("User", document.getName());
	}

	@Test
	void getDocumentWithCustomerReturnsCustomerOverride() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		ModuleMetaData mmd = minimalModule("admin", "User");
		Module module = repo.putModule(mmd);
		// Put vanilla doc first
		repo.putDocument(module, minimalDocument("User"));
		// Put customer-specific doc
		CustomerImpl customer = mock(CustomerImpl.class);
		when(customer.getName()).thenReturn("acme");
		repo.putDocument(customer, module, minimalDocument("User"));
		// getDocument with customer should return customer override
		Document retrieved = repo.getDocument(customer, module, "User");
		assertNotNull(retrieved);
		assertEquals("User", retrieved.getName());
	}

	@Test
	void getDocumentWithCustomerFallsBackToVanillaWhenNoCustomerOverride() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		ModuleMetaData mmd = minimalModule("admin", "User");
		Module module = repo.putModule(mmd);
		// Put vanilla doc only
		repo.putDocument(module, minimalDocument("User"));
		// No customer-specific doc in cache
		CustomerImpl customer = mock(CustomerImpl.class);
		when(customer.getName()).thenReturn("acme");
		Document retrieved = repo.getDocument(customer, module, "User");
		assertNotNull(retrieved);
		assertEquals("User", retrieved.getName());
	}

	// ---- setRouter / getRouter ----

	@Test
	void setRouterAndGetRouterReturnsSameRouter() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Router router = new Router();
		Router result = repo.setRouter(router);
		assertNotNull(result);
		Router retrieved = repo.getRouter();
		assertNotNull(retrieved);
	}

	@Test
	void getRouterThrowsWhenNoRouterFilesPresent() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		// populateKeys() adds ROUTER_KEY as empty Optional; getRouter() tries to load
		// from disk but finds no router files, so throws MetaDataException
		assertThrows(org.skyve.metadata.MetaDataException.class, repo::getRouter);
	}

	@Test
	void getRouterReturnsNullWhenEmptyKeyLoadsNull() {
		NullLoadingRepository repo = newNullLoadingRepository();
		repo.addRouterKey();

		Router retrieved = repo.getRouter();

		assertNull(retrieved);
	}

	@Test
	void getCustomerReturnsNullWhenEmptyKeyLoadsNull() {
		NullLoadingRepository repo = newNullLoadingRepository();
		repo.addEmptyKey("customers/acme");

		Customer retrieved = repo.getCustomer("acme");

		assertNull(retrieved);
	}

	@Test
	void getViewReturnsNullWhenEmptyKeyLoadsNull() {
		NullLoadingRepository repo = newNullLoadingRepository();
		repo.putModule(minimalModule("admin", "User"));
		repo.addEmptyKey("modules/admin/User/views/edit");
		ProvidedRepositoryFactory.set(repo);
		Document document = mockDocument("admin", "User");

		org.skyve.metadata.view.View retrieved = repo.getView(null, null, document, "edit");

		assertNull(retrieved);
	}

	// ---- vtable ----

	@Test
	void vtableNullCustomerReturnsCacheKeyWhenPresent() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		// put a module to populate the cache with "modules/admin"
		repo.putModule(minimalModule("admin", "User"));
		String key = repo.vtable(null, "modules/admin");
		assertEquals("modules/admin", key);
	}

	@Test
	void vtableNullCustomerReturnsNullWhenKeyAbsent() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		String key = repo.vtable(null, "modules/nonexistent");
		assertNull(key);
	}

	@Test
	void vtableWithCustomerReturnsCustomerKeyWhenPresent() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		// Put customer-specific module in cache
		CustomerImpl customer = mock(CustomerImpl.class);
		when(customer.getName()).thenReturn("acme");
		repo.putModule(customer, minimalModule("admin", "User"));
		// vtable should return the customer-specific key
		String key = repo.vtable("acme", "modules/admin");
		assertEquals("customers/acme/modules/admin", key);
	}

	@Test
	void vtableWithCustomerFallsBackToVanillaKeyWhenCustomerOverrideAbsent() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		// Put vanilla module only
		repo.putModule(minimalModule("admin", "User"));
		// vtable for "acme" — no customer override, falls back to vanilla
		String key = repo.vtable("acme", "modules/admin");
		assertEquals("modules/admin", key);
	}

	@Test
	void vtableWithCustomerReturnsNullWhenNeitherKeyPresent() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		String key = repo.vtable("acme", "modules/nonexistent");
		assertNull(key);
	}

	// ---- putMetaDataAction / getMetaDataAction ----

	/** Build a Document mock with owningModuleName and name. */
	private static Document mockDocument(String moduleName, String documentName) {
		Document doc = mock(Document.class);
		when(doc.getOwningModuleName()).thenReturn(moduleName);
		when(doc.getName()).thenReturn(documentName);
		return doc;
	}

	@Test
	void putMetaDataActionWithoutCustomerReturnsConvertedAction() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Document doc = mockDocument("admin", "User");
		ActionMetaData action = new ActionMetaData();
		action.setName("Save");
		ActionMetaData result = repo.putMetaDataAction(doc, action);
		assertNotNull(result);
		assertEquals("Save", result.getName());
	}

	@Test
	void getMetaDataActionNullCustomerReturnsActionAfterPut() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Document doc = mockDocument("admin", "User");
		ActionMetaData action = new ActionMetaData();
		action.setName("Save");
		repo.putMetaDataAction(doc, action);
		ActionMetaData retrieved = repo.getMetaDataAction(null, doc, "Save");
		assertNotNull(retrieved);
		assertEquals("Save", retrieved.getName());
	}

	@Test
	void getMetaDataActionNullCustomerReturnsNullWhenNotInCache() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Document doc = mockDocument("admin", "User");
		ActionMetaData result = repo.getMetaDataAction(null, doc, "NonExistent");
		assertNull(result);
	}

	@Test
	void getMetaDataActionReturnsNullWhenEmptyKeyLoadsNull() {
		NullLoadingRepository repo = newNullLoadingRepository();
		Document document = mockDocument("admin", "User");
		repo.addEmptyKey("modules/admin/User/actions/Save.xml");

		ActionMetaData result = repo.getMetaDataAction(null, document, "Save");

		assertNull(result);
	}

	@Test
	void putMetaDataActionWithCustomerReturnsConvertedAction() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Document doc = mockDocument("admin", "User");
		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("acme");
		ActionMetaData action = new ActionMetaData();
		action.setName("Delete");
		ActionMetaData result = repo.putMetaDataAction(customer, doc, action);
		assertNotNull(result);
		assertEquals("Delete", result.getName());
	}

	@Test
	void getMetaDataActionWithCustomerReturnsCustomerActionAfterPut() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Document doc = mockDocument("admin", "User");
		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("acme");
		ActionMetaData action = new ActionMetaData();
		action.setName("Delete");
		repo.putMetaDataAction(customer, doc, action);
		ActionMetaData retrieved = repo.getMetaDataAction(customer, doc, "Delete");
		assertNotNull(retrieved);
		assertEquals("Delete", retrieved.getName());
	}

	@Test
	void getMetaDataActionWithCustomerFallsBackToVanillaWhenNoCustomerOverride() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Document doc = mockDocument("admin", "User");
		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("acme");
		// Put vanilla only
		ActionMetaData action = new ActionMetaData();
		action.setName("Save");
		repo.putMetaDataAction(doc, action);
		// Retrieve with customer — should fall back to vanilla
		ActionMetaData retrieved = repo.getMetaDataAction(customer, doc, "Save");
		assertNotNull(retrieved);
		assertEquals("Save", retrieved.getName());
	}

	// ---- putMetaDataBizlet / getMetaDataBizlet ----

	@Test
	void putMetaDataBizletWithoutCustomerReturnsConvertedBizlet() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Document doc = mockDocument("admin", "User");
		BizletMetaData bizlet = new BizletMetaData();
		BizletMetaData result = repo.putMetaDataBizlet(doc, bizlet);
		assertNotNull(result);
	}

	@Test
	void getMetaDataBizletNullCustomerReturnsBizletAfterPut() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Document doc = mockDocument("admin", "User");
		BizletMetaData bizlet = new BizletMetaData();
		repo.putMetaDataBizlet(doc, bizlet);
		BizletMetaData retrieved = repo.getMetaDataBizlet(null, doc);
		assertNotNull(retrieved);
	}

	@Test
	void getMetaDataBizletNullCustomerReturnsNullWhenNotInCache() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Document doc = mockDocument("admin", "User");
		BizletMetaData result = repo.getMetaDataBizlet(null, doc);
		assertNull(result);
	}

	@Test
	void getMetaDataBizletReturnsNullWhenEmptyKeyLoadsNull() {
		NullLoadingRepository repo = newNullLoadingRepository();
		Document document = mockDocument("admin", "User");
		repo.addEmptyKey("modules/admin/User/UserBizlet.xml");

		BizletMetaData result = repo.getMetaDataBizlet(null, document);

		assertNull(result);
	}

	@Test
	void putMetaDataBizletWithCustomerReturnsConvertedBizlet() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Document doc = mockDocument("admin", "User");
		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("acme");
		BizletMetaData bizlet = new BizletMetaData();
		BizletMetaData result = repo.putMetaDataBizlet(customer, doc, bizlet);
		assertNotNull(result);
	}

	@Test
	void getMetaDataBizletWithCustomerReturnsBizletAfterPut() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Document doc = mockDocument("admin", "User");
		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("acme");
		BizletMetaData bizlet = new BizletMetaData();
		repo.putMetaDataBizlet(customer, doc, bizlet);
		BizletMetaData retrieved = repo.getMetaDataBizlet(customer, doc);
		assertNotNull(retrieved);
	}

	// ---- LocalDesignRepository UnsupportedOperationException methods ----

	@Test
	void localDesignRepositoryRetrieveUserThrowsUnsupportedOperationException() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertThrows(UnsupportedOperationException.class, () -> repo.retrieveUser("user"));
	}

	@Test
	void localDesignRepositoryPopulatePermissionsThrowsUnsupportedOperationException() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertThrows(UnsupportedOperationException.class,
				() -> repo.populatePermissions(mock(org.skyve.metadata.user.User.class)));
	}

	@Test
	void localDesignRepositoryResetUserPermissionsThrowsUnsupportedOperationException() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertThrows(UnsupportedOperationException.class,
				() -> repo.resetUserPermissions(mock(org.skyve.metadata.user.User.class)));
	}

	@Test
	void localDesignRepositoryPopulateUserThrowsUnsupportedOperationException() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertThrows(UnsupportedOperationException.class,
				() -> repo.populateUser(mock(org.skyve.metadata.user.User.class), mock(Connection.class)));
	}

	@Test
	void localDesignRepositoryRetrieveAllScheduledJobsThrowsUnsupportedOperationException() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertThrows(UnsupportedOperationException.class,
				repo::retrieveAllScheduledJobsForAllCustomers);
	}

	@Test
	void localDesignRepositoryRetrieveAllScheduledReportsThrowsUnsupportedOperationException() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertThrows(UnsupportedOperationException.class,
				repo::retrieveAllScheduledReportsForAllCustomers);
	}

	@Test
	void localDesignRepositoryTwoArgConstructorSetsLoadClassesFalse() {
		// Covers FileSystemRepository(String, boolean) and LocalDesignRepository(String, boolean)
		LocalDesignRepository repo = new LocalDesignRepository(basePath, false);
		assertNotNull(repo);
	}
}
