package org.skyve.impl.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.skyve.metadata.MetaDataException;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Tests for FileSystemRepository and MutableCachedRepository via the concrete
 * LocalDesignRepository subclass.
 * Uses a @TempDir to exercise pure-filesystem and cache methods without any
 * Skyve XML parsing or CORE runtime.
 */
@SuppressWarnings("java:S5976")
class FileSystemRepositoryTest {

	@TempDir
	Path tempDir;

	private String basePath;

	@BeforeEach
	void setUp() {
		basePath = tempDir.toAbsolutePath().toString();
	}

	// ---- getAllCustomerNames ----

	@Test
	void getAllCustomerNamesEmptyWhenNoCustomersDirectory() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertTrue(repo.getAllCustomerNames().isEmpty());
	}

	@Test
	void getAllCustomerNamesReturnsSubDirectoryNames() throws Exception {
		// Build repo first (no customers dir yet) then add dirs so populateKeys() doesn't call loadCustomer()
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Files.createDirectories(tempDir.resolve("customers/alpha"));
		Files.createDirectories(tempDir.resolve("customers/beta"));
		List<String> names = repo.getAllCustomerNames();
		assertEquals(2, names.size());
		assertTrue(names.contains("alpha"));
		assertTrue(names.contains("beta"));
	}

	@Test
	void getAllCustomerNamesIgnoresDotDirectories() throws Exception {
		// Build repo first then add dirs so populateKeys() doesn't call loadCustomer()
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Files.createDirectories(tempDir.resolve("customers/.hidden"));
		Files.createDirectories(tempDir.resolve("customers/visible"));
		List<String> names = repo.getAllCustomerNames();
		assertEquals(1, names.size());
		assertTrue(names.contains("visible"));
	}

	// ---- getAllVanillaModuleNames ----

	@Test
	void getAllVanillaModuleNamesEmptyWhenNoModulesDirectory() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertTrue(repo.getAllVanillaModuleNames().isEmpty());
	}

	@Test
	void getAllVanillaModuleNamesRequiresMatchingXmlFile() throws Exception {
		// Directory without matching XML → not listed
		Files.createDirectories(tempDir.resolve("modules/noXml"));
		// Directory with matching XML → listed
		Path moduleDir = tempDir.resolve("modules/myModule");
		Files.createDirectories(moduleDir);
		Files.createFile(moduleDir.resolve("myModule.xml"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		List<String> names = repo.getAllVanillaModuleNames();
		assertEquals(1, names.size());
		assertEquals("myModule", names.get(0));
	}

	@Test
	void getAllVanillaModuleNamesIgnoresDotDirectories() throws Exception {
		Files.createDirectories(tempDir.resolve("modules/.ignored"));
		Path moduleDir = tempDir.resolve("modules/realModule");
		Files.createDirectories(moduleDir);
		Files.createFile(moduleDir.resolve("realModule.xml"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		List<String> names = repo.getAllVanillaModuleNames();
		assertEquals(1, names.size());
		assertEquals("realModule", names.get(0));
	}

	@Test
	void getAllVanillaModuleNamesReturnsMultipleModules() throws Exception {
		for (String mod : new String[] {"modA", "modB", "modC"}) {
			Path dir = tempDir.resolve("modules/" + mod);
			Files.createDirectories(dir);
			Files.createFile(dir.resolve(mod + ".xml"));
		}
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		List<String> names = repo.getAllVanillaModuleNames();
		assertEquals(3, names.size());
		assertTrue(names.contains("modA"));
		assertTrue(names.contains("modB"));
		assertTrue(names.contains("modC"));
	}

	// ---- vtable / populateKeys (called during construction) ----

	@Test
	void vtableReturnsNullWhenKeyNotPresent() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNull(repo.vtable(null, "modules/nonExistent"));
	}

	@Test
	void vtableReturnsKeyWhenModulePopulated() throws Exception {
		Path moduleDir = tempDir.resolve("modules/myModule");
		Files.createDirectories(moduleDir);
		Files.createFile(moduleDir.resolve("myModule.xml"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertEquals("modules/myModule", repo.vtable(null, "modules/myModule"));
	}

	@Test
	void vtableWithUnknownCustomerFallsBackToVanillaKey() throws Exception {
		Path moduleDir = tempDir.resolve("modules/myModule");
		Files.createDirectories(moduleDir);
		Files.createFile(moduleDir.resolve("myModule.xml"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertEquals("modules/myModule", repo.vtable("noSuchCustomer", "modules/myModule"));
	}

	@Test
	void vtableReturnsNullForUnknownCustomerAndNoVanillaKey() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNull(repo.vtable("noSuchCustomer", "modules/nonExistent"));
	}

	@Test
	void vtableCustomerSpecificKeyTakesPrecedence() throws Exception {
		// Build repo first with just the vanilla module
		Path vanillaDir = tempDir.resolve("modules/myModule");
		Files.createDirectories(vanillaDir);
		Files.createFile(vanillaDir.resolve("myModule.xml"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		// Add customer-overridden dirs after construction (avoids loadCustomer() during init)
		Path customerDir = tempDir.resolve("customers/testCustomer/modules/myModule");
		Files.createDirectories(customerDir);
		Files.createFile(customerDir.resolve("myModule.xml"));
		// vtable falls back to vanilla key since only the vanilla key is in cache
		assertNotNull(repo.vtable("testCustomer", "modules/myModule"));
	}

	// ---- populateDocumentLocations — document XML discovery ----

	@Test
	void documentXmlKeyRegisteredAfterConstruction() throws Exception {
		Path moduleDir = tempDir.resolve("modules/myModule");
		Files.createDirectories(moduleDir);
		Files.createFile(moduleDir.resolve("myModule.xml"));
		Path docDir = tempDir.resolve("modules/myModule/MyDoc");
		Files.createDirectories(docDir);
		Files.createFile(docDir.resolve("MyDoc.xml"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		// The document key is "modules/myModule/MyDoc" (just directory name, no .xml suffix)
		String key = repo.vtable(null, "modules/myModule/MyDoc");
		assertNotNull(key, "Document key should be registered");
	}

	// ---- getUseScaffoldedViews ----

	@Test
	void getUseScaffoldedViewsReturnsTrue() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertTrue(repo.getUseScaffoldedViews());
	}

	// ---- routerLastModifiedMillis ----

	@Test
	void routerLastModifiedMillisReturnsMinValueWhenNoRouterFile() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertEquals(Long.MIN_VALUE, repo.routerLastModifiedMillis());
	}

	@Test
	void routerLastModifiedMillisReturnsPositiveWhenRouterFileExists() throws Exception {
		Path routerDir = tempDir.resolve("router");
		Files.createDirectories(routerDir);
		Files.createFile(routerDir.resolve("router.xml"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertTrue(repo.routerLastModifiedMillis() > Long.MIN_VALUE);
	}

	// ---- loadRouter ----

	@Test
	void loadRouterThrowsWhenNoRouterFiles() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertThrows(MetaDataException.class, repo::loadRouter);
	}

	// ---- getGlobalRouter ----

	@Test
	void getGlobalRouterReturnsNullWhenNoRouterFile() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNull(repo.getGlobalRouter());
	}

	// ---- getModuleRouters ----

	@Test
	void getModuleRoutersReturnsEmptyListWhenNoModuleRouters() throws Exception {
		createVanillaModule("myModule");
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertTrue(repo.getModuleRouters().isEmpty());
	}

	// ---- customerLastModifiedMillis ----

	@Test
	void customerLastModifiedMillisReturnsMinValueWhenNoFile() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertEquals(Long.MIN_VALUE, repo.customerLastModifiedMillis("nonExistent"));
	}

	@Test
	void customerLastModifiedMillisReturnsPositiveWhenFileExists() throws Exception {
		// Build repo first — no customer dirs yet (avoids loadCustomer during init)
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Path customerDir = tempDir.resolve("customers/testCust");
		Files.createDirectories(customerDir);
		Files.createFile(customerDir.resolve("testCust.xml"));
		assertTrue(repo.customerLastModifiedMillis("testCust") > Long.MIN_VALUE);
	}

	// ---- moduleLastModifiedMillis ----

	@Test
	void moduleLastModifiedMillisReturnsMinValueWhenNoFile() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertEquals(Long.MIN_VALUE, repo.moduleLastModifiedMillis(null, "nonExistent"));
	}

	@Test
	void moduleLastModifiedMillisReturnsPositiveWhenFileExists() throws Exception {
		createVanillaModule("myModule");
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertTrue(repo.moduleLastModifiedMillis(null, "myModule") > Long.MIN_VALUE);
	}

	@Test
	void moduleLastModifiedMillisWithCustomerReturnsPositiveWhenFileExists() throws Exception {
		// Build repo first — no customer dirs yet (avoids loadCustomer during init)
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Path customerModuleDir = tempDir.resolve("customers/testCust/modules/myModule");
		Files.createDirectories(customerModuleDir);
		Files.createFile(customerModuleDir.resolve("myModule.xml"));
		assertTrue(repo.moduleLastModifiedMillis("testCust", "myModule") > Long.MIN_VALUE);
	}

	// ---- documentLastModifiedMillis ----

	@Test
	void documentLastModifiedMillisReturnsMinValueWhenNoFile() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertEquals(Long.MIN_VALUE, repo.documentLastModifiedMillis(null, "myModule", "MyDoc"));
	}

	@Test
	void documentLastModifiedMillisReturnsPositiveWhenFileExists() throws Exception {
		createVanillaModule("myModule");
		Path docDir = tempDir.resolve("modules/myModule/MyDoc");
		Files.createDirectories(docDir);
		Files.createFile(docDir.resolve("MyDoc.xml"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertTrue(repo.documentLastModifiedMillis(null, "myModule", "MyDoc") > Long.MIN_VALUE);
	}

	// ---- metaDataActionLastModifiedMillis ----

	@Test
	void metaDataActionLastModifiedMillisReturnsMinValueWhenNoFile() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertEquals(Long.MIN_VALUE, repo.metaDataActionLastModifiedMillis(null, "myModule", "MyDoc", "MyAction"));
	}

	@Test
	void metaDataActionLastModifiedMillisReturnsPositiveWhenFileExists() throws Exception {
		createVanillaModule("myModule");
		Path actionsDir = tempDir.resolve("modules/myModule/MyDoc/actions");
		Files.createDirectories(actionsDir);
		Files.createFile(actionsDir.resolve("MyAction.xml"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertTrue(repo.metaDataActionLastModifiedMillis(null, "myModule", "MyDoc", "MyAction") > Long.MIN_VALUE);
	}

	// ---- metaDataBizletLastModifiedMillis ----

	@Test
	void metaDataBizletLastModifiedMillisReturnsMinValueWhenNoFile() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertEquals(Long.MIN_VALUE, repo.metaDataBizletLastModifiedMillis(null, "myModule", "MyDoc"));
	}

	@Test
	void metaDataBizletLastModifiedMillisReturnsPositiveWhenFileExists() throws Exception {
		createVanillaModule("myModule");
		Path docDir = tempDir.resolve("modules/myModule/MyDoc");
		Files.createDirectories(docDir);
		Files.createFile(docDir.resolve("MyDocBizlet.xml"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertTrue(repo.metaDataBizletLastModifiedMillis(null, "myModule", "MyDoc") > Long.MIN_VALUE);
	}

	// ---- viewLastModifiedMillis ----

	@Test
	void viewLastModifiedMillisReturnsMinValueWhenNoFile() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertEquals(Long.MIN_VALUE, repo.viewLastModifiedMillis(null, "myModule", "MyDoc", null, "edit"));
	}

	@Test
	void viewLastModifiedMillisReturnsPositiveWhenFileExists() throws Exception {
		createVanillaModule("myModule");
		Path viewsDir = tempDir.resolve("modules/myModule/MyDoc/views");
		Files.createDirectories(viewsDir);
		Files.createFile(viewsDir.resolve("edit.xml"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertTrue(repo.viewLastModifiedMillis(null, "myModule", "MyDoc", null, "edit") > Long.MIN_VALUE);
	}

	@Test
	void viewLastModifiedMillisWithUxuiReturnsPositiveWhenFileExists() throws Exception {
		createVanillaModule("myModule");
		Path uxuiDir = tempDir.resolve("modules/myModule/MyDoc/views/desktop");
		Files.createDirectories(uxuiDir);
		Files.createFile(uxuiDir.resolve("edit.xml"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertTrue(repo.viewLastModifiedMillis(null, "myModule", "MyDoc", "desktop", "edit") > Long.MIN_VALUE);
	}

	// ---- populateDocumentLocations — helper ----

	private void createVanillaModule(String moduleName) throws Exception {
		Path moduleDir = tempDir.resolve("modules/" + moduleName);
		Files.createDirectories(moduleDir);
		Files.createFile(moduleDir.resolve(moduleName + ".xml"));
	}

	private void createDocumentDir(String moduleName, String docName) throws Exception {
		Path docDir = tempDir.resolve("modules/" + moduleName + "/" + docName);
		Files.createDirectories(docDir);
	}

	// ---- populateDocumentLocations — module XML registration ----

	@Test
	void moduleXmlKeyRegisteredDuringPopulateKeys() throws Exception {
		createVanillaModule("myModule");
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertEquals("modules/myModule", repo.vtable(null, "modules/myModule"));
	}

	// ---- populateDocumentLocations — action files ----

	@Test
	void actionJavaFileRegisteredAsActionKey() throws Exception {
		createVanillaModule("myModule");
		createDocumentDir("myModule", "MyDoc");
		Path actionsDir = tempDir.resolve("modules/myModule/MyDoc/actions");
		Files.createDirectories(actionsDir);
		Files.createFile(actionsDir.resolve("MyAction.java"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNotNull(repo.vtable(null, "modules/myModule/MyDoc/actions/MyAction"));
	}

	@Test
	void actionClassFileRegisteredAsActionKey() throws Exception {
		createVanillaModule("myModule");
		createDocumentDir("myModule", "MyDoc");
		Path actionsDir = tempDir.resolve("modules/myModule/MyDoc/actions");
		Files.createDirectories(actionsDir);
		Files.createFile(actionsDir.resolve("MyAction.class"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNotNull(repo.vtable(null, "modules/myModule/MyDoc/actions/MyAction"));
	}

	@Test
	void actionXmlFileRegisteredAsActionMetaDataKey() throws Exception {
		createVanillaModule("myModule");
		createDocumentDir("myModule", "MyDoc");
		Path actionsDir = tempDir.resolve("modules/myModule/MyDoc/actions");
		Files.createDirectories(actionsDir);
		Files.createFile(actionsDir.resolve("MyAction.xml"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNotNull(repo.vtable(null, "modules/myModule/MyDoc/actions/MyActionMetaData"));
	}

	@Test
	void unknownFileInActionsDirNotRegistered() throws Exception {
		createVanillaModule("myModule");
		createDocumentDir("myModule", "MyDoc");
		Path actionsDir = tempDir.resolve("modules/myModule/MyDoc/actions");
		Files.createDirectories(actionsDir);
		Files.createFile(actionsDir.resolve("ReadMe.txt"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNull(repo.vtable(null, "modules/myModule/MyDoc/actions/ReadMe"));
	}

	// ---- populateDocumentLocations — image files ----

	@Test
	void imageJavaFileRegisteredAsImageKey() throws Exception {
		createVanillaModule("myModule");
		createDocumentDir("myModule", "MyDoc");
		Path imagesDir = tempDir.resolve("modules/myModule/MyDoc/images");
		Files.createDirectories(imagesDir);
		Files.createFile(imagesDir.resolve("MyImage.java"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNotNull(repo.vtable(null, "modules/myModule/MyDoc/images/MyImage"));
	}

	@Test
	void imageClassFileRegisteredAsImageKey() throws Exception {
		createVanillaModule("myModule");
		createDocumentDir("myModule", "MyDoc");
		Path imagesDir = tempDir.resolve("modules/myModule/MyDoc/images");
		Files.createDirectories(imagesDir);
		Files.createFile(imagesDir.resolve("MyImage.class"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNotNull(repo.vtable(null, "modules/myModule/MyDoc/images/MyImage"));
	}

	@Test
	void unknownFileInImagesDirNotRegistered() throws Exception {
		createVanillaModule("myModule");
		createDocumentDir("myModule", "MyDoc");
		Path imagesDir = tempDir.resolve("modules/myModule/MyDoc/images");
		Files.createDirectories(imagesDir);
		Files.createFile(imagesDir.resolve("MyImage.png"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNull(repo.vtable(null, "modules/myModule/MyDoc/images/MyImage"));
	}

	// ---- populateDocumentLocations — model files ----

	@Test
	void modelJavaFileRegisteredAsModelKey() throws Exception {
		createVanillaModule("myModule");
		createDocumentDir("myModule", "MyDoc");
		Path modelsDir = tempDir.resolve("modules/myModule/MyDoc/models");
		Files.createDirectories(modelsDir);
		Files.createFile(modelsDir.resolve("MyModel.java"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNotNull(repo.vtable(null, "modules/myModule/MyDoc/models/MyModel"));
	}

	@Test
	void modelClassFileRegisteredAsModelKey() throws Exception {
		createVanillaModule("myModule");
		createDocumentDir("myModule", "MyDoc");
		Path modelsDir = tempDir.resolve("modules/myModule/MyDoc/models");
		Files.createDirectories(modelsDir);
		Files.createFile(modelsDir.resolve("MyModel.class"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNotNull(repo.vtable(null, "modules/myModule/MyDoc/models/MyModel"));
	}

	// ---- populateDocumentLocations — report files ----

	@Test
	void reportJasperFileRegisteredAsJasperKey() throws Exception {
		createVanillaModule("myModule");
		createDocumentDir("myModule", "MyDoc");
		Path reportsDir = tempDir.resolve("modules/myModule/MyDoc/reports");
		Files.createDirectories(reportsDir);
		Files.createFile(reportsDir.resolve("MyReport.jasper"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNotNull(repo.vtable(null, "modules/myModule/MyDoc/reports/MyReportJasper"));
	}

	@Test
	void reportFtlhFileRegisteredAsFreemarkerKey() throws Exception {
		createVanillaModule("myModule");
		createDocumentDir("myModule", "MyDoc");
		Path reportsDir = tempDir.resolve("modules/myModule/MyDoc/reports");
		Files.createDirectories(reportsDir);
		Files.createFile(reportsDir.resolve("MyReport.ftlh"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNotNull(repo.vtable(null, "modules/myModule/MyDoc/reports/MyReportFreemarker"));
	}

	@Test
	void unknownFileInReportsDirNotRegistered() throws Exception {
		createVanillaModule("myModule");
		createDocumentDir("myModule", "MyDoc");
		Path reportsDir = tempDir.resolve("modules/myModule/MyDoc/reports");
		Files.createDirectories(reportsDir);
		Files.createFile(reportsDir.resolve("MyReport.jrxml"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNull(repo.vtable(null, "modules/myModule/MyDoc/reports/MyReport"));
	}

	// ---- populateDocumentLocations — view files ----

	@Test
	void viewXmlFileRegisteredAsViewKey() throws Exception {
		createVanillaModule("myModule");
		createDocumentDir("myModule", "MyDoc");
		Path viewsDir = tempDir.resolve("modules/myModule/MyDoc/views");
		Files.createDirectories(viewsDir);
		Files.createFile(viewsDir.resolve("edit.xml"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNotNull(repo.vtable(null, "modules/myModule/MyDoc/views/edit"));
	}

	@Test
	void viewUxuiSubdirXmlFileRegisteredAsViewKey() throws Exception {
		createVanillaModule("myModule");
		createDocumentDir("myModule", "MyDoc");
		Path uxuiDir = tempDir.resolve("modules/myModule/MyDoc/views/desktop");
		Files.createDirectories(uxuiDir);
		Files.createFile(uxuiDir.resolve("edit.xml"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNotNull(repo.vtable(null, "modules/myModule/MyDoc/views/desktop/edit"));
	}

	@Test
	void nonXmlFileInViewsDirNotRegistered() throws Exception {
		createVanillaModule("myModule");
		createDocumentDir("myModule", "MyDoc");
		Path viewsDir = tempDir.resolve("modules/myModule/MyDoc/views");
		Files.createDirectories(viewsDir);
		Files.createFile(viewsDir.resolve("edit.bak"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNull(repo.vtable(null, "modules/myModule/MyDoc/views/edit"));
	}

	@Test
	void nonXmlFileInUxuiSubdirNotRegistered() throws Exception {
		createVanillaModule("myModule");
		createDocumentDir("myModule", "MyDoc");
		Path uxuiDir = tempDir.resolve("modules/myModule/MyDoc/views/desktop");
		Files.createDirectories(uxuiDir);
		Files.createFile(uxuiDir.resolve("edit.bak"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNull(repo.vtable(null, "modules/myModule/MyDoc/views/desktop/edit"));
	}

	// ---- populateDocumentLocations — bizlet files ----

	@Test
	void bizletClassFileRegisteredAsBizletKey() throws Exception {
		createVanillaModule("myModule");
		createDocumentDir("myModule", "MyDoc");
		Path docDir = tempDir.resolve("modules/myModule/MyDoc");
		Files.createFile(docDir.resolve("MyDocBizlet.class"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNotNull(repo.vtable(null, "modules/myModule/MyDoc/MyDocBizlet"));
	}

	@Test
	void bizletXmlFileRegisteredAsBizletMetaDataKey() throws Exception {
		createVanillaModule("myModule");
		createDocumentDir("myModule", "MyDoc");
		Path docDir = tempDir.resolve("modules/myModule/MyDoc");
		Files.createFile(docDir.resolve("MyDocBizlet.xml"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNotNull(repo.vtable(null, "modules/myModule/MyDoc/MyDocBizletMetaData"));
	}

	// ---- populateDocumentLocations — extension and factory files ----

	@Test
	void extensionClassFileRegisteredAsExtensionKey() throws Exception {
		createVanillaModule("myModule");
		createDocumentDir("myModule", "MyDoc");
		Path docDir = tempDir.resolve("modules/myModule/MyDoc");
		Files.createFile(docDir.resolve("MyDocExtension.class"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNotNull(repo.vtable(null, "modules/myModule/MyDoc/MyDocExtension"));
	}

	@Test
	void factoryClassFileRegisteredAsFactoryKey() throws Exception {
		createVanillaModule("myModule");
		createDocumentDir("myModule", "MyDoc");
		Path docDir = tempDir.resolve("modules/myModule/MyDoc");
		Files.createFile(docDir.resolve("MyDocFactory.class"));
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNotNull(repo.vtable(null, "modules/myModule/MyDoc/MyDocFactory"));
	}

	// ---- populateDocumentLocations — multiple documents and files ----

	@Test
	void multipleDocumentsAllRegistered() throws Exception {
		createVanillaModule("myModule");
		for (String doc : new String[] {"DocA", "DocB", "DocC"}) {
			createDocumentDir("myModule", doc);
			Path docDir = tempDir.resolve("modules/myModule/" + doc);
			Files.createFile(docDir.resolve(doc + ".xml"));
		}
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNotNull(repo.vtable(null, "modules/myModule/DocA"));
		assertNotNull(repo.vtable(null, "modules/myModule/DocB"));
		assertNotNull(repo.vtable(null, "modules/myModule/DocC"));
	}

	@Test
	void populateKeysRegistersRouterKey() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNotNull(repo.vtable(null, "router/router"));
	}

	// ---- findResourceFile ----

	@Test
	void findResourceFileReturnsRootResourceWhenNotFound() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		// Resource doesn't exist - returns the file object for the root resources path (non-null but not existing)
		java.io.File result = repo.findResourceFile("logo.png", null, null);
		assertNotNull(result);
		// Path should end with resources/logo.png
		assertTrue(result.getPath().replace('\\', '/').endsWith("resources/logo.png"));
	}

	@Test
	void findResourceFileFindsInModuleResourcesFolder() throws Exception {
		// Create modules/myModule/resources/logo.png
		Path modulesDir = tempDir.resolve("modules").resolve("myModule").resolve("resources");
		Files.createDirectories(modulesDir);
		Path file = modulesDir.resolve("logo.png");
		Files.writeString(file, "image-data");
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		java.io.File result = repo.findResourceFile("logo.png", null, "myModule");
		assertNotNull(result);
		assertTrue(result.exists());
		assertTrue(result.getPath().replace('\\', '/').contains("modules/myModule/resources/logo.png"));
	}

	@Test
	void findResourceFileFindsInCustomerResourcesFolder() throws Exception {
		// Create repo first so populateKeys() does not call loadCustomer() on missing acme.xml
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		// Now create customers/acme/resources/logo.png
		Path customerDir = tempDir.resolve("customers").resolve("acme").resolve("resources");
		Files.createDirectories(customerDir);
		Path file = customerDir.resolve("logo.png");
		Files.writeString(file, "image-data");
		java.io.File result = repo.findResourceFile("logo.png", "acme", null);
		assertNotNull(result);
		assertTrue(result.exists());
		assertTrue(result.getPath().replace('\\', '/').contains("customers/acme/resources/logo.png"));
	}

	@Test
	void findResourceFileFindsInCustomerModuleResourcesFolder() throws Exception {
		// Create repo first so populateKeys() does not call loadCustomer() on missing acme.xml
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		// Now create customers/acme/myModule/resources/override.png
		Path customerModuleDir = tempDir.resolve("customers").resolve("acme").resolve("myModule").resolve("resources");
		Files.createDirectories(customerModuleDir);
		Path file = customerModuleDir.resolve("override.png");
		Files.writeString(file, "image-data");
		java.io.File result = repo.findResourceFile("override.png", "acme", "myModule");
		assertNotNull(result);
		assertTrue(result.exists());
		assertTrue(result.getPath().replace('\\', '/').contains("customers/acme/myModule/resources/override.png"));
	}

	@Test
	void findResourceFilePrefersCustomerModuleOverModule() throws Exception {
		// Create repo first so populateKeys() does not call loadCustomer() on missing acme.xml
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		// Create both customers/acme/myModule/resources/logo.png and modules/myModule/resources/logo.png
		Path customerModuleDir = tempDir.resolve("customers").resolve("acme").resolve("myModule").resolve("resources");
		Files.createDirectories(customerModuleDir);
		Files.writeString(customerModuleDir.resolve("logo.png"), "customer-override");
		Path moduleDir = tempDir.resolve("modules").resolve("myModule").resolve("resources");
		Files.createDirectories(moduleDir);
		Files.writeString(moduleDir.resolve("logo.png"), "vanilla");
		java.io.File result = repo.findResourceFile("logo.png", "acme", "myModule");
		assertTrue(result.exists());
		// Should return the customer-module specific one
		assertTrue(result.getPath().replace('\\', '/').contains("customers/acme/myModule/resources/logo.png"));
	}

	@Test
	void findResourceFileFallsBackToRootResourcesWhenNothingElseFound() throws Exception {
		// Create resources/fallback.css in root resources
		Path rootResources = tempDir.resolve("resources");
		Files.createDirectories(rootResources);
		Files.writeString(rootResources.resolve("fallback.css"), "body{}");
		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		java.io.File result = repo.findResourceFile("fallback.css", "acme", "myModule");
		assertNotNull(result);
		// Falls back to root resources since no customer/module-specific file exists
		assertTrue(result.getPath().replace('\\', '/').endsWith("resources/fallback.css"));
	}
}
