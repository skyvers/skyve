package org.skyve.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

class NoOpProvidedRepositoryTest {

	private final NoOpProvidedRepository repo = new NoOpProvidedRepository();

	@Test
	void findResourceFileReturnsNull() {
		assertNull(repo.findResourceFile("path", "customer", "module"));
	}

	@Test
	void getRouterReturnsNull() {
		assertNull(repo.getRouter());
	}

	@Test
	void getCustomerReturnsNull() {
		assertNull(repo.getCustomer("test"));
	}

	@Test
	void getDynamicImageReturnsNull() {
		assertNull(repo.getDynamicImage(null, null, "image", false));
	}

	@Test
	void getViewReturnsNull() {
		assertNull(repo.getView("desktop", null, null, "edit"));
	}

	@Test
	void getMetaDataActionReturnsNull() {
		assertNull(repo.getMetaDataAction(null, null, "Save"));
	}

	@Test
	void getComparisonModelReturnsNull() {
		assertNull(repo.getComparisonModel(null, null, "model", false));
	}

	@Test
	void getMapModelReturnsNull() {
		assertNull(repo.getMapModel(null, null, "model", false));
	}

	@Test
	void getChartModelReturnsNull() {
		assertNull(repo.getChartModel(null, null, "model", false));
	}

	@Test
	void getListModelReturnsNull() {
		assertNull(repo.getListModel(null, null, "model", false));
	}

	@Test
	void getServerSideActionReturnsNull() {
		assertNull(repo.getServerSideAction(null, null, "Action", false));
	}

	@Test
	void getBizExportActionReturnsNull() {
		assertNull(repo.getBizExportAction(null, null, "Export", false));
	}

	@Test
	void getBizImportActionReturnsNull() {
		assertNull(repo.getBizImportAction(null, null, "Import", false));
	}

	@Test
	void getDownloadActionReturnsNull() {
		assertNull(repo.getDownloadAction(null, null, "Download", false));
	}

	@Test
	void getUploadActionReturnsNull() {
		assertNull(repo.getUploadAction(null, null, "Upload", false));
	}

	@Test
	void getDataFactoryReturnsNull() {
		assertNull(repo.getDataFactory(null, null));
	}

	@Test
	void retrieveUserReturnsNull() {
		assertNull(repo.retrieveUser("user@test.com"));
	}

	@Test
	void populatePermissionsReturnsFalse() {
		assertFalse(repo.populatePermissions(null));
	}

	@Test
	void populateUserReturnsFalse() {
		assertFalse(repo.populateUser(null, null));
	}

	@Test
	void retrieveAllScheduledJobsReturnsNull() {
		assertNull(repo.retrieveAllScheduledJobsForAllCustomers());
	}

	@Test
	void retrieveAllScheduledReportsReturnsNull() {
		assertNull(repo.retrieveAllScheduledReportsForAllCustomers());
	}

	@Test
	void retrievePublicUserNameReturnsNull() {
		assertNull(repo.retrievePublicUserName("test"));
	}

	@Test
	void getAllCustomerNamesReturnsNull() {
		assertNull(repo.getAllCustomerNames());
	}

	@Test
	void getAllVanillaModuleNamesReturnsNull() {
		assertNull(repo.getAllVanillaModuleNames());
	}

	@Test
	void getModuleReturnsNull() {
		assertNull(repo.getModule(null, "admin"));
	}

	@Test
	void getDocumentReturnsNull() {
		assertNull(repo.getDocument(null, null, "User"));
	}

	@Test
	void getBizletReturnsNull() {
		assertNull(repo.getBizlet(null, null, false));
	}

	@Test
	void getMetaDataBizletReturnsNull() {
		assertNull(repo.getMetaDataBizlet(null, null));
	}

	@Test
	void getGlobalRouterReturnsNull() {
		assertNull(repo.getGlobalRouter());
	}

	@Test
	void getModuleRoutersReturnsNull() {
		assertNull(repo.getModuleRouters());
	}

	@Test
	void getReportFileNameReturnsNull() {
		assertNull(repo.getReportFileName(null, null, "report"));
	}

	@Test
	void getJavaClassReturnsNull() {
		assertNull(repo.getJavaClass(null, "com.example.Foo"));
	}

	@Test
	void vtableReturnsNull() {
		assertNull(repo.vtable("test", "key"));
	}

	@Test
	void evictCachedMetaDataDoesNotThrow() {
		assertDoesNotThrow(() -> repo.evictCachedMetaData(null)); // should silently do nothing
	}

	@Test
	void resetMenusDoesNotThrow() {
		assertDoesNotThrow(() -> repo.resetMenus(null));
	}

	@Test
	void resetUserPermissionsDoesNotThrow() {
		assertDoesNotThrow(() -> repo.resetUserPermissions(null));
	}

	@Test
	void validateCustomerForGenerateDomainDoesNotThrow() {
		assertDoesNotThrow(() -> repo.validateCustomerForGenerateDomain(null));
	}

	@Test
	void validateModuleForGenerateDomainDoesNotThrow() {
		assertDoesNotThrow(() -> repo.validateModuleForGenerateDomain(null, null));
	}

	@Test
	void validateDocumentForGenerateDomainDoesNotThrow() {
		repo.validateDocumentForGenerateDomain(null, null);
	}

	@Test
	void validateViewForGenerateDomainDoesNotThrow() {
		repo.validateViewForGenerateDomain(null, null, null, "desktop");
	}

	@Test
	void getUseScaffoldedViewsReturnsFalse() {
		assertFalse(repo.getUseScaffoldedViews());
	}
}
