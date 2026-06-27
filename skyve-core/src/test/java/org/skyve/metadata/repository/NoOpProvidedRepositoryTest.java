package org.skyve.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;

import java.sql.Connection;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;

@SuppressWarnings("all")
class NoOpProvidedRepositoryTest {

	private final NoOpProvidedRepository repo = new NoOpProvidedRepository();
	private final Customer customer = mock(Customer.class);
	private final Module module = mock(Module.class);
	private final Document document = mock(Document.class);
	private final User user = mock(User.class);
	private final Connection connection = mock(Connection.class);
	private final View view = mock(View.class);

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
		assertNull(repo.getDynamicImage(customer, document, "image", false));
	}

	@Test
	void getViewReturnsNull() {
		assertNull(repo.getView("desktop", customer, document, "edit"));
	}

	@Test
	void getMetaDataActionReturnsNull() {
		assertNull(repo.getMetaDataAction(customer, document, "Save"));
	}

	@Test
	void getComparisonModelReturnsNull() {
		assertNull(repo.getComparisonModel(customer, document, "model", false));
	}

	@Test
	void getMapModelReturnsNull() {
		assertNull(repo.getMapModel(customer, document, "model", false));
	}

	@Test
	void getChartModelReturnsNull() {
		assertNull(repo.getChartModel(customer, document, "model", false));
	}

	@Test
	void getListModelReturnsNull() {
		assertNull(repo.getListModel(customer, document, "model", false));
	}

	@Test
	void getServerSideActionReturnsNull() {
		assertNull(repo.getServerSideAction(customer, document, "Action", false));
	}

	@Test
	void getBizExportActionReturnsNull() {
		assertNull(repo.getBizExportAction(customer, document, "Export", false));
	}

	@Test
	void getBizImportActionReturnsNull() {
		assertNull(repo.getBizImportAction(customer, document, "Import", false));
	}

	@Test
	void getDownloadActionReturnsNull() {
		assertNull(repo.getDownloadAction(customer, document, "Download", false));
	}

	@Test
	void getUploadActionReturnsNull() {
		assertNull(repo.getUploadAction(customer, document, "Upload", false));
	}

	@Test
	void getDataFactoryReturnsNull() {
		assertNull(repo.getDataFactory(customer, document));
	}

	@Test
	void retrieveUserReturnsNull() {
		assertNull(repo.retrieveUser("user@test.com"));
	}

	@Test
	void populatePermissionsReturnsFalse() {
		assertFalse(repo.populatePermissions(user));
	}

	@Test
	void populateUserReturnsFalse() {
		assertFalse(repo.populateUser(user, connection));
	}

	@Test
	void retrieveAllScheduledJobsReturnsEmptyList() {
		assertEquals(List.of(), repo.retrieveAllScheduledJobsForAllCustomers());
	}

	@Test
	void retrieveAllScheduledReportsReturnsEmptyList() {
		assertEquals(List.of(), repo.retrieveAllScheduledReportsForAllCustomers());
	}

	@Test
	void retrievePublicUserNameReturnsNull() {
		assertNull(repo.retrievePublicUserName("test"));
	}

	@Test
	void getAllCustomerNamesReturnsEmptyList() {
		assertEquals(List.of(), repo.getAllCustomerNames());
	}

	@Test
	void getAllVanillaModuleNamesReturnsEmptyList() {
		assertEquals(List.of(), repo.getAllVanillaModuleNames());
	}

	@Test
	void getModuleReturnsNull() {
		assertNull(repo.getModule(null, "admin"));
	}

	@Test
	void getDocumentReturnsNull() {
		assertNull(repo.getDocument(customer, module, "User"));
	}

	@Test
	void getBizletReturnsNull() {
		assertNull(repo.getBizlet(customer, document, false));
	}

	@Test
	void getMetaDataBizletReturnsNull() {
		assertNull(repo.getMetaDataBizlet(customer, document));
	}

	@Test
	void getGlobalRouterReturnsNull() {
		assertNull(repo.getGlobalRouter());
	}

	@Test
	void getModuleRoutersReturnsEmptyList() {
		assertEquals(List.of(), repo.getModuleRouters());
	}

	@Test
	void getReportFileNameReturnsNull() {
		assertNull(repo.getReportFileName(customer, document, "report"));
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
		assertDoesNotThrow(() -> repo.resetMenus(user));
	}

	@Test
	void resetUserPermissionsDoesNotThrow() {
		assertDoesNotThrow(() -> repo.resetUserPermissions(user));
	}

	@Test
	void validateCustomerForGenerateDomainDoesNotThrow() {
		assertDoesNotThrow(() -> repo.validateCustomerForGenerateDomain(customer));
	}

	@Test
	void validateModuleForGenerateDomainDoesNotThrow() {
		assertDoesNotThrow(() -> repo.validateModuleForGenerateDomain(customer, module));
	}

	@Test
	void validateDocumentForGenerateDomainDoesNotThrow() {
		assertDoesNotThrow(() -> repo.validateDocumentForGenerateDomain(customer, document));
	}

	@Test
	void validateViewForGenerateDomainDoesNotThrow() {
		assertDoesNotThrow(() -> repo.validateViewForGenerateDomain(customer, document, view, "desktop"));
	}

	@Test
	void getUseScaffoldedViewsReturnsFalse() {
		assertFalse(repo.getUseScaffoldedViews());
	}
}
