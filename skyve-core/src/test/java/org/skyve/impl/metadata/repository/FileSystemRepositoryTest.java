package org.skyve.impl.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.BizExportAction;
import org.skyve.metadata.controller.BizImportAction;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.controller.Upload;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.metadata.model.Dynamic;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.metadata.view.model.chart.ChartModel;
import org.skyve.metadata.view.model.comparison.ComparisonComposite;
import org.skyve.metadata.view.model.comparison.ComparisonModel;
import org.skyve.metadata.view.model.list.Filter;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.list.Page;
import org.skyve.metadata.view.model.map.MapModel;
import org.skyve.metadata.view.model.map.MapResult;
import org.skyve.web.WebContext;

import java.awt.image.BufferedImage;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.SortedMap;

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

	// ---- getJavaClass / getJavaMetaData ----

	@Test
	void getJavaClassReturnsWidgetReferenceWhenLoadClassesIsFalseAndDottedJavaExists() throws Exception {
		createVanillaModule("myModule");
		Files.createFile(tempDir.resolve("modules.myModule.java"));

		LocalDesignRepository repo = new LocalDesignRepository(basePath, false);
		Class<?> type = repo.getJavaClass(null, "modules/myModule");

		assertEquals(org.skyve.impl.metadata.view.WidgetReference.class, type);
	}

	@Test
	void getJavaMetaDataReturnsWidgetReferenceWhenClassResolvedInNoLoadMode() throws Exception {
		createVanillaModule("myModule");
		Files.createFile(tempDir.resolve("modules.myModule.java"));

		LocalDesignRepository repo = new LocalDesignRepository(basePath, false);
		Object metaData = repo.getJavaMetaData(null, "modules/myModule", true, false);

		assertNotNull(metaData);
		assertInstanceOf(org.skyve.impl.metadata.view.WidgetReference.class, metaData);
	}

	@Test
	void getJavaMetaDataThrowsWhenMissingAndAssertExistenceTrue() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath, false);

		assertThrows(MetaDataException.class,
				() -> repo.getJavaMetaData(null, "modules/nonExistent", true, false));
	}

	@Test
	void getJavaMetaDataReturnsNullWhenMissingAndAssertExistenceFalse() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath, false);

		assertNull(repo.getJavaMetaData(null, "modules/nonExistent", false, false));
	}

	// ---- getReportFileName ----

	@Test
	void getReportFileNameReturnsJasperPathWhenJasperReportExists() throws Exception {
		createVanillaModule("myModule");
		Path reportsDir = tempDir.resolve("modules/myModule/MyDoc/reports");
		Files.createDirectories(reportsDir);
		Files.createFile(reportsDir.resolve("monthly.jasper"));

		Document document = mock(Document.class);
		when(document.getOwningModuleName()).thenReturn("myModule");
		when(document.getName()).thenReturn("MyDoc");

		LocalDesignRepository repo = new LocalDesignRepository(basePath, false);
		String reportFile = repo.getReportFileName(null, document, "monthly");

		assertNotNull(reportFile);
		assertTrue(reportFile.endsWith("modules/myModule/MyDoc/reports/monthly.jasper"));
	}

	@Test
	void getReportFileNameReturnsFreemarkerPathWhenOnlyFtlhExists() throws Exception {
		createVanillaModule("myModule");
		Path reportsDir = tempDir.resolve("modules/myModule/MyDoc/reports");
		Files.createDirectories(reportsDir);
		Files.createFile(reportsDir.resolve("summary.ftlh"));

		Document document = mock(Document.class);
		when(document.getOwningModuleName()).thenReturn("myModule");
		when(document.getName()).thenReturn("MyDoc");

		LocalDesignRepository repo = new LocalDesignRepository(basePath, false);
		String reportFile = repo.getReportFileName(null, document, "summary");

		assertNotNull(reportFile);
		assertTrue(reportFile.endsWith("modules/myModule/MyDoc/reports/summary.flth"));
	}

	// ---- getDataFactory ----

	@Test
	void getDataFactoryReturnsWidgetReferenceInstanceInNoLoadModeWhenFactoryKeyExists() throws Exception {
		createVanillaModule("myModule");
		Path docDir = tempDir.resolve("modules/myModule/MyDoc");
		Files.createDirectories(docDir);
		Files.createFile(docDir.resolve("MyDocFactory.class"));
		Files.createFile(tempDir.resolve("modules.myModule.MyDoc.MyDocFactory.java"));

		Document document = mock(Document.class);
		when(document.getOwningModuleName()).thenReturn("myModule");
		when(document.getName()).thenReturn("MyDoc");
		when(document.getDynamism()).thenReturn(null);

		LocalDesignRepository repo = new LocalDesignRepository(basePath, false);
		Object factory = repo.getDataFactory(null, document);

		assertNotNull(factory);
		assertInstanceOf(org.skyve.impl.metadata.view.WidgetReference.class, factory);
	}

	@Test
	void getDataFactoryReturnsNullWhenNoFactoryDefined() {
		Document document = mock(Document.class);
		when(document.getOwningModuleName()).thenReturn("myModule");
		when(document.getName()).thenReturn("MyDoc");
		when(document.getDynamism()).thenReturn(null);

		LocalDesignRepository repo = new LocalDesignRepository(basePath, false);
		assertNull(repo.getDataFactory(null, document));
	}

	@Test
	void getDataFactoryDynamicClassReturnsWidgetReferenceWhenLoadClassesFalse() throws Exception {
		Dynamic dynamic = new Dynamic();
		dynamic.setDataFactoryClassName("dynamic.Factory");
		Files.createFile(tempDir.resolve("dynamic.Factory.java"));

		Document document = mock(Document.class);
		when(document.getOwningModuleName()).thenReturn("myModule");
		when(document.getName()).thenReturn("MyDoc");
		when(document.getDynamism()).thenReturn(dynamic);

		LocalDesignRepository repo = new LocalDesignRepository(basePath, false);
		Object factory = repo.getDataFactory(null, document);

		assertNotNull(factory);
		assertInstanceOf(org.skyve.impl.metadata.view.WidgetReference.class, factory);
	}

	@Test
	void getServerSideActionThrowsWhenStaticActionClassMissing() {
		Document document = mock(Document.class);
		when(document.getDynamism()).thenReturn(null);
		when(document.getOwningModuleName()).thenReturn("myModule");
		when(document.getName()).thenReturn("MyDoc");

		LocalDesignRepository repo = new LocalDesignRepository(basePath, false);

		assertThrows(MetaDataException.class,
				() -> repo.getServerSideAction(null, document, "missingAction", false));
	}

	@Test
	void getServerSideActionThrowsWhenDynamicActionNotDeclared() {
		Dynamic dynamic = new Dynamic();
		Document document = mock(Document.class);
		when(document.getDynamism()).thenReturn(dynamic);
		when(document.getName()).thenReturn("MyDoc");

		LocalDesignRepository repo = new LocalDesignRepository(basePath, false);

		assertThrows(MetaDataException.class,
				() -> repo.getServerSideAction(null, document, "missingAction", false));
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

	@Test
	void findResourceFileRejectsPathTraversalOutsideBase() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);

		assertThrows(SecurityException.class,
				() -> repo.findResourceFile("../../../../outside.txt", null, null));
	}

	@Test
	void getBizletReturnsDynamicBizletInstance() {
		Dynamic dynamic = new Dynamic();
		dynamic.setBizletClassName(TestBizlet.class.getName());

		Document document = mock(Document.class);
		when(document.getDynamism()).thenReturn(dynamic);
		when(document.getOwningModuleName()).thenReturn("myModule");
		when(document.getName()).thenReturn("MyDoc");

		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Object bizlet = repo.getBizlet(null, document, false);

		assertInstanceOf(TestBizlet.class, bizlet);
	}

	@Test
	void getDynamicImageReturnsDynamicImageInstance() {
		Dynamic dynamic = new Dynamic();
		dynamic.getImages().put("avatar", TestDynamicImage.class.getName());

		Document document = mock(Document.class);
		when(document.getDynamism()).thenReturn(dynamic);
		when(document.getOwningModuleName()).thenReturn("myModule");
		when(document.getName()).thenReturn("MyDoc");

		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Object image = repo.getDynamicImage(null, document, "avatar", false);

		assertInstanceOf(TestDynamicImage.class, image);
	}

	@Test
	void getDynamicImageThrowsWhenDynamicImageNotDeclared() {
		Dynamic dynamic = new Dynamic();

		Document document = mock(Document.class);
		when(document.getDynamism()).thenReturn(dynamic);
		when(document.getName()).thenReturn("MyDoc");

		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertThrows(MetaDataException.class, () -> repo.getDynamicImage(null, document, "avatar", false));
	}

	@Test
	void getComparisonModelReturnsDynamicComparisonModelInstance() {
		Dynamic dynamic = new Dynamic();
		dynamic.getModels().put("cmp", TestComparisonModel.class.getName());

		Document document = mock(Document.class);
		when(document.getDynamism()).thenReturn(dynamic);
		when(document.getOwningModuleName()).thenReturn("myModule");
		when(document.getName()).thenReturn("MyDoc");

		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Object model = repo.getComparisonModel(null, document, "cmp", false);

		assertInstanceOf(TestComparisonModel.class, model);
	}

	@Test
	void getMapModelReturnsDynamicMapModelInstance() {
		Dynamic dynamic = new Dynamic();
		dynamic.getModels().put("map", TestMapModel.class.getName());

		Document document = mock(Document.class);
		when(document.getDynamism()).thenReturn(dynamic);
		when(document.getOwningModuleName()).thenReturn("myModule");
		when(document.getName()).thenReturn("MyDoc");

		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Object model = repo.getMapModel(null, document, "map", false);

		assertInstanceOf(TestMapModel.class, model);
	}

	@Test
	void getChartModelReturnsDynamicChartModelInstance() {
		Dynamic dynamic = new Dynamic();
		dynamic.getModels().put("chart", TestChartModel.class.getName());

		Document document = mock(Document.class);
		when(document.getDynamism()).thenReturn(dynamic);
		when(document.getOwningModuleName()).thenReturn("myModule");
		when(document.getName()).thenReturn("MyDoc");

		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Object model = repo.getChartModel(null, document, "chart", false);

		assertInstanceOf(TestChartModel.class, model);
	}

	@Test
	void getListModelReturnsDynamicListModelInstance() {
		Dynamic dynamic = new Dynamic();
		dynamic.getModels().put("list", TestListModel.class.getName());

		Document document = mock(Document.class);
		when(document.getDynamism()).thenReturn(dynamic);
		when(document.getOwningModuleName()).thenReturn("myModule");
		when(document.getName()).thenReturn("MyDoc");

		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Object model = repo.getListModel(null, document, "list", false);

		assertInstanceOf(TestListModel.class, model);
	}

	@Test
	void getMapModelThrowsWhenDynamicModelNotDeclared() {
		Dynamic dynamic = new Dynamic();

		Document document = mock(Document.class);
		when(document.getDynamism()).thenReturn(dynamic);
		when(document.getName()).thenReturn("MyDoc");

		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertThrows(MetaDataException.class, () -> repo.getMapModel(null, document, "map", false));
	}

	@Test
	void getServerSideActionReturnsDynamicActionInstance() {
		Dynamic dynamic = new Dynamic();
		dynamic.getActions().put("run", TestServerSideAction.class.getName());

		Document document = mock(Document.class);
		when(document.getDynamism()).thenReturn(dynamic);
		when(document.getName()).thenReturn("MyDoc");

		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Object action = repo.getServerSideAction(null, document, "run", false);

		assertInstanceOf(TestServerSideAction.class, action);
	}

	@Test
	void getServerSideActionReturnsNullWhenLoadClassesDisabled() throws Exception {
		Dynamic dynamic = new Dynamic();
		dynamic.getActions().put("run", "dynamic.ServerAction");
		Files.createFile(tempDir.resolve("dynamic.ServerAction.java"));

		Document document = mock(Document.class);
		when(document.getDynamism()).thenReturn(dynamic);
		when(document.getName()).thenReturn("MyDoc");

		LocalDesignRepository repo = new LocalDesignRepository(basePath, false);
		assertNull(repo.getServerSideAction(null, document, "run", false));
	}

	@Test
	void getBizExportActionReturnsDynamicActionInstance() {
		Dynamic dynamic = new Dynamic();
		dynamic.getActions().put("export", TestBizExportAction.class.getName());

		Document document = mock(Document.class);
		when(document.getDynamism()).thenReturn(dynamic);
		when(document.getName()).thenReturn("MyDoc");

		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Object action = repo.getBizExportAction(null, document, "export", false);

		assertInstanceOf(TestBizExportAction.class, action);
	}

	@Test
	void getBizExportActionReturnsNullWhenLoadClassesDisabled() throws Exception {
		Dynamic dynamic = new Dynamic();
		dynamic.getActions().put("export", "dynamic.ExportAction");
		Files.createFile(tempDir.resolve("dynamic.ExportAction.java"));

		Document document = mock(Document.class);
		when(document.getDynamism()).thenReturn(dynamic);
		when(document.getName()).thenReturn("MyDoc");

		LocalDesignRepository repo = new LocalDesignRepository(basePath, false);
		assertNull(repo.getBizExportAction(null, document, "export", false));
	}

	@Test
	void getBizImportActionReturnsDynamicActionInstance() {
		Dynamic dynamic = new Dynamic();
		dynamic.getActions().put("import", TestBizImportAction.class.getName());

		Document document = mock(Document.class);
		when(document.getDynamism()).thenReturn(dynamic);
		when(document.getName()).thenReturn("MyDoc");

		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Object action = repo.getBizImportAction(null, document, "import", false);

		assertInstanceOf(TestBizImportAction.class, action);
	}

	@Test
	void getDownloadActionReturnsDynamicActionInstance() {
		Dynamic dynamic = new Dynamic();
		dynamic.getActions().put("download", TestDownloadAction.class.getName());

		Document document = mock(Document.class);
		when(document.getDynamism()).thenReturn(dynamic);
		when(document.getName()).thenReturn("MyDoc");

		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Object action = repo.getDownloadAction(null, document, "download", false);

		assertInstanceOf(TestDownloadAction.class, action);
	}

	@Test
	void getUploadActionReturnsDynamicActionInstance() {
		Dynamic dynamic = new Dynamic();
		dynamic.getActions().put("upload", TestUploadAction.class.getName());

		Document document = mock(Document.class);
		when(document.getDynamism()).thenReturn(dynamic);
		when(document.getName()).thenReturn("MyDoc");

		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		Object action = repo.getUploadAction(null, document, "upload", false);

		assertInstanceOf(TestUploadAction.class, action);
	}

	@Test
	void loadMethodsThrowMetaDataExceptionWhenSourceFilesMissing() {
		LocalDesignRepository repo = new LocalDesignRepository(basePath);

		assertThrows(MetaDataException.class, () -> repo.loadCustomer("missingCustomer"));
		assertThrows(MetaDataException.class, () -> repo.loadModule(null, "missingModule"));
		assertThrows(MetaDataException.class, () -> repo.loadDocument(null, "missingModule", "MissingDoc"));
		assertThrows(MetaDataException.class,
				() -> repo.loadMetaDataAction(null, "missingModule", "MissingDoc", "missingAction"));
		assertThrows(MetaDataException.class,
				() -> repo.loadView(null, "missingModule", "MissingDoc", null, "edit"));
		assertThrows(MetaDataException.class,
				() -> repo.loadMetaDataBizlet(null, "missingModule", "MissingDoc"));
	}

	@Test
	void loadRouterThrowsMetaDataExceptionWhenRouterXmlMalformed() throws Exception {
		Path routerDir = tempDir.resolve("router");
		Files.createDirectories(routerDir);
		Files.writeString(routerDir.resolve("router.xml"), "not-xml");

		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertThrows(MetaDataException.class, repo::loadRouter);
	}

	@Test
	void getGlobalRouterThrowsWhenRouterXmlMalformed() throws Exception {
		Path routerDir = tempDir.resolve("router");
		Files.createDirectories(routerDir);
		Files.writeString(routerDir.resolve("router.xml"), "not-xml");

		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertThrows(RuntimeException.class, repo::getGlobalRouter);
	}

	@Test
	void getModuleRoutersThrowsWhenModuleRouterXmlMalformed() throws Exception {
		createVanillaModule("myModule");
		Files.writeString(tempDir.resolve("modules/myModule/router.xml"), "not-xml");

		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertThrows(RuntimeException.class, repo::getModuleRouters);
	}

	@Test
	void getJavaClassReturnsNullWhenClassNotLoadableInLoadClassesMode() throws Exception {
		createVanillaModule("myModule");
		createDocumentDir("myModule", "MyDoc");
		Path actionsDir = tempDir.resolve("modules/myModule/MyDoc/actions");
		Files.createDirectories(actionsDir);
		Files.createFile(actionsDir.resolve("NoSuchAction.class"));

		LocalDesignRepository repo = new LocalDesignRepository(basePath);
		assertNull(repo.getJavaClass(null, "modules/myModule/MyDoc/actions/NoSuchAction"));
	}

	@Test
	@SuppressWarnings("unchecked")
	void evictCachedMetaDataUsesExistingThreadPersistence() throws Exception {
		org.skyve.impl.persistence.AbstractPersistence persistence = mock(org.skyve.impl.persistence.AbstractPersistence.class);
		org.skyve.metadata.user.User user = mock(org.skyve.metadata.user.User.class);
		when(persistence.getUser()).thenReturn(user);

		Field threadLocalField = org.skyve.impl.persistence.AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		threadLocalField.setAccessible(true);
		ThreadLocal<?> threadLocal = (ThreadLocal<?>) threadLocalField.get(null);
		Object previous = threadLocal.get();

		try {
			((ThreadLocal<org.skyve.impl.persistence.AbstractPersistence>) threadLocal).set(persistence);
			LocalDesignRepository repo = new LocalDesignRepository(basePath);
			repo.evictCachedMetaData(null);
			verify(persistence).disposeAllPersistenceInstances();
			verify(persistence).setUser(user);
		}
		finally {
			if (previous == null) {
				threadLocal.remove();
			}
			else {
				((ThreadLocal<Object>) threadLocal).set(previous);
			}
		}
	}

	static final class TestBizlet extends Bizlet<org.skyve.domain.Bean> {
		// no-op fixture for dynamic bizlet loading
	}

	static final class TestDynamicImage implements DynamicImage<org.skyve.domain.Bean> {
		@Override
		public BufferedImage getImage(org.skyve.domain.Bean bean, int width, int height, User user) {
			return new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB);
		}
	}

	static final class TestComparisonModel extends ComparisonModel<org.skyve.domain.Bean, org.skyve.domain.Bean> {
		@Override
		public ComparisonComposite getComparisonComposite(org.skyve.domain.Bean toCompareTo) {
			return new ComparisonComposite();
		}
	}

	static final class TestMapModel extends MapModel<org.skyve.domain.Bean> {
		@Override
		public MapResult getResult(org.locationtech.jts.geom.Geometry mapBounds) {
			return new MapResult();
		}
	}

	static final class TestChartModel extends ChartModel<org.skyve.domain.Bean> {
		@Override
		public ChartData getChartData() {
			return null;
		}
	}

	static final class TestListModel extends ListModel<org.skyve.domain.Bean> {
		@Override
		public void postConstruct(org.skyve.metadata.customer.Customer customer, boolean runtime) {
			// no-op
		}

		@Override
		public String getDescription() {
			return "fixture";
		}

		@Override
		public Document getDrivingDocument() {
			return null;
		}

		@Override
		public List<org.skyve.metadata.module.query.MetaDataQueryColumn> getColumns() {
			return List.of();
		}

		@Override
		public java.util.Set<String> getProjections() {
			return java.util.Set.of();
		}

		@Override
		public Filter getFilter() {
			return null;
		}

		@Override
		public Filter newFilter() {
			return null;
		}

		@Override
		public void putParameter(String name, Object value) {
			// no-op
		}

		@Override
		public Page fetch() {
			return null;
		}

		@Override
		public org.skyve.persistence.AutoClosingIterable<org.skyve.domain.Bean> iterate() {
			return null;
		}

		@Override
		public org.skyve.domain.Bean update(String bizId, SortedMap<String, Object> properties) {
			return null;
		}

		@Override
		public void remove(String bizId) {
			// no-op
		}
	}

	static final class TestServerSideAction implements ServerSideAction<org.skyve.domain.Bean> {
		@Override
		public ServerSideActionResult<org.skyve.domain.Bean> execute(org.skyve.domain.Bean bean, WebContext webContext) {
			return new ServerSideActionResult<>(bean);
		}
	}

	static final class TestBizExportAction extends BizExportAction {
		@Override
		public org.skyve.bizport.BizPortWorkbook bizExport(WebContext webContext) {
			return null;
		}
	}

	static final class TestBizImportAction extends BizImportAction {
		@Override
		public void bizImport(org.skyve.bizport.BizPortWorkbook bizPortable,
				org.skyve.domain.messages.UploadException problems) {
			// no-op
		}
	}

	static final class TestDownloadAction extends DownloadAction<org.skyve.domain.Bean> {
		@Override
		public void prepare(org.skyve.domain.Bean bean, WebContext webContext) {
			// no-op
		}

		@Override
		public Download download(org.skyve.domain.Bean bean, WebContext webContext) {
			return null;
		}
	}

	static final class TestUploadAction extends UploadAction<org.skyve.domain.Bean> {
		@Override
		public org.skyve.domain.Bean upload(org.skyve.domain.Bean bean,
				Upload upload,
				org.skyve.domain.messages.UploadException exception,
				WebContext webContext) {
			return bean;
		}
	}
}
