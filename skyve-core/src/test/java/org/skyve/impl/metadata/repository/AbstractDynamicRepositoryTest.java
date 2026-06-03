package org.skyve.impl.metadata.repository;

import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;

import java.sql.Connection;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;

/**
 * Covers the stub implementations in {@link AbstractDynamicRepository} by
 * exercising them via {@link UnsynchronisedDynamicRepository}.
 */
class AbstractDynamicRepositoryTest {

	private UnsynchronisedDynamicRepository repo;
	private Customer customer;
	private Document document;
	private User user;
	private Module module;
	private View view;

	@BeforeEach
	void setUp() {
		repo = new UnsynchronisedDynamicRepository();
		customer = mock(Customer.class);
		document = mock(Document.class);
		user = mock(User.class);
		module = mock(Module.class);
		view = mock(View.class);
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
	void getDynamicImageReturnsNull() {
		assertNull(repo.getDynamicImage(customer, document, "image", false));
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
		assertNull(repo.getServerSideAction(customer, document, "ActionClass", false));
	}

	@Test
	void getMetaDataActionReturnsNull() {
		assertNull(repo.getMetaDataAction(customer, document, "ActionName"));
	}

	@Test
	void getBizExportActionReturnsNull() {
		assertNull(repo.getBizExportAction(customer, document, "ActionClass", false));
	}

	@Test
	void getBizImportActionReturnsNull() {
		assertNull(repo.getBizImportAction(customer, document, "ActionClass", false));
	}

	@Test
	void getDownloadActionReturnsNull() {
		assertNull(repo.getDownloadAction(customer, document, "ActionClass", false));
	}

	@Test
	void getUploadActionReturnsNull() {
		assertNull(repo.getUploadAction(customer, document, "ActionClass", false));
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
	void getGlobalRouterReturnsNull() {
		assertNull(repo.getGlobalRouter());
	}

	@Test
	void getModuleRoutersReturnsNull() {
		assertTrue(repo.getModuleRouters().isEmpty());
	}

	@Test
	void getReportFileNameReturnsNull() {
		assertNull(repo.getReportFileName(customer, document, "report"));
	}

	@Test
	void getJavaClassReturnsNull() {
		assertNull(repo.getJavaClass(customer, "com.example.Foo"));
	}

	@Test
	void retrieveUserReturnsNull() {
		assertNull(repo.retrieveUser("admin/admin"));
	}

	@Test
	void resetMenusDoesNotThrow() {
		assertDoesNotThrow(() -> repo.resetMenus(user));
	}

	@Test
	void populatePermissionsReturnsFalse() {
		assertFalse(repo.populatePermissions(user));
	}

	@Test
	void resetUserPermissionsDoesNotThrow() {
		assertDoesNotThrow(() -> repo.resetUserPermissions(user));
	}

	@Test
	@SuppressWarnings("static-method")
	void populateUserReturnsFalse() {
		try (Connection connection = mock(Connection.class)) {
			assertFalse(new UnsynchronisedDynamicRepository().populateUser(mock(User.class), connection));
		}
		catch (@SuppressWarnings("unused") Exception ignored) {
			// mock close is a no-op; ignore
		}
	}

	@Test
	void retrieveAllScheduledJobsForAllCustomersReturnsNull() {
		assertTrue(repo.retrieveAllScheduledJobsForAllCustomers().isEmpty());
	}

	@Test
	void retrieveAllScheduledReportsForAllCustomersReturnsNull() {
		assertTrue(repo.retrieveAllScheduledReportsForAllCustomers().isEmpty());
	}

	@Test
	void retrievePublicUserNameReturnsNull() {
		assertNull(repo.retrievePublicUserName("defaultCustomer"));
	}

	@Test
	void findResourceFileReturnsNull() {
		assertNull(repo.findResourceFile("path/to/resource", "defaultCustomer", "admin"));
	}

	@Test
	void getDataFactoryReturnsNull() {
		assertNull(repo.getDataFactory(customer, document));
	}
}


