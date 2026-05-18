package org.skyve.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.io.File;
import java.lang.reflect.Field;
import java.sql.Connection;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;
import org.skyve.impl.metadata.repository.behaviour.BizletMetaData;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.job.UserJobSchedule;
import org.skyve.metadata.controller.BizExportAction;
import org.skyve.metadata.controller.BizImportAction;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.model.chart.ChartModel;
import org.skyve.metadata.view.model.comparison.ComparisonModel;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.map.MapModel;

@SuppressWarnings({ "boxing", "resource", "static-method" })
class SessionScopedDelegatingProvidedRepositoryTest {
	private static void clearPersistenceThreadLocal() {
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			@SuppressWarnings("unchecked")
			ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
			threadLocal.remove();
		}
		catch (ReflectiveOperationException e) {
			throw new AssertionError(e);
		}
	}

	@AfterEach
	void cleanup() {
		clearPersistenceThreadLocal();
	}

	@Test
	void defaultsAreReturnedWhenNoSessionDelegateAvailable() {
		SessionScopedDelegatingProvidedRepository repo = new SessionScopedDelegatingProvidedRepository();
		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);
		Module module = mock(Module.class);
		User user = mock(User.class);
		Connection connection = mock(Connection.class);

		assertTrue(repo.getSessionDelegate() == null);
		assertTrue(repo.findResourceFile("r", "c", "m") == null);
		repo.evictCachedMetaData(customer);
		repo.resetMenus(user);
		repo.resetUserPermissions(user);
		repo.validateCustomerForGenerateDomain(customer);
		repo.validateModuleForGenerateDomain(customer, module);
		repo.validateDocumentForGenerateDomain(customer, document);
		repo.validateViewForGenerateDomain(customer, document, mock(View.class), "ux");
		assertTrue(repo.getRouter() == null);
		assertTrue(repo.getCustomer("c") == null);
		assertTrue(repo.getDynamicImage(customer, document, "img", false) == null);
		assertTrue(repo.getView("ux", customer, document, "v") == null);
		assertTrue(repo.getMetaDataAction(customer, document, "a") == null);
		assertTrue(repo.getComparisonModel(customer, document, "cmp", false) == null);
		assertTrue(repo.getMapModel(customer, document, "map", false) == null);
		assertTrue(repo.getChartModel(customer, document, "chart", false) == null);
		assertTrue(repo.getListModel(customer, document, "list", false) == null);
		assertTrue(repo.getServerSideAction(customer, document, "S", false) == null);
		assertTrue(repo.getBizExportAction(customer, document, "E", false) == null);
		assertTrue(repo.getBizImportAction(customer, document, "I", false) == null);
		assertTrue(repo.getDownloadAction(customer, document, "D", false) == null);
		assertTrue(repo.getUploadAction(customer, document, "U", false) == null);
		assertTrue(repo.getDataFactory(customer, document) == null);
		assertTrue(repo.retrieveUser("u") == null);
		assertTrue(repo.retrieveAllScheduledJobsForAllCustomers() == null);
		assertTrue(repo.retrieveAllScheduledReportsForAllCustomers() == null);
		assertTrue(repo.retrievePublicUserName("c") == null);
		assertFalse(repo.populatePermissions(user));
		assertFalse(repo.populateUser(user, connection));
		assertEquals(Collections.emptyList(), repo.getAllCustomerNames());
		assertEquals(Collections.emptyList(), repo.getAllVanillaModuleNames());
		assertTrue(repo.getModule(customer, "mod") == null);
		assertTrue(repo.getDocument(customer, module, "doc") == null);
		assertTrue(repo.getBizlet(customer, document, false) == null);
		assertTrue(repo.getMetaDataBizlet(customer, document) == null);
		assertTrue(repo.getGlobalRouter() == null);
		assertEquals(Collections.emptyList(), repo.getModuleRouters());
		assertTrue(repo.getReportFileName(customer, document, "rpt") == null);
		assertTrue(repo.getJavaClass(customer, "key") == null);
		assertTrue(repo.vtable("cust", "key") == null);
		assertFalse(repo.getUseScaffoldedViews());
	}

	@Test
	void methodsDelegateToSessionScopedRepositoryWhenSessionIsBound() {
		SessionScopedDelegatingProvidedRepository repo = new SessionScopedDelegatingProvidedRepository();
		ProvidedRepository delegate = mock(ProvidedRepository.class);

		User user = mock(User.class);
		when(user.getSessionId()).thenReturn("S1");
		when(user.getName()).thenReturn("test.user");

		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();
		repo.setSessionDelegate(user, delegate);

		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);
		Module module = mock(Module.class);
		Connection connection = mock(Connection.class);
		Router router = mock(Router.class);
		File file = new File("x");
		DynamicImage<Bean> dynamicImage = mock(DynamicImage.class);
		View view = mock(View.class);
		ActionMetaData actionMetaData = mock(ActionMetaData.class);
		ComparisonModel<Bean, Bean> comparisonModel = mock(ComparisonModel.class);
		MapModel<Bean> mapModel = mock(MapModel.class);
		ChartModel<Bean> chartModel = mock(ChartModel.class);
		ListModel<Bean> listModel = mock(ListModel.class);
		ServerSideAction<Bean> serverAction = mock(ServerSideAction.class);
		BizExportAction bizExportAction = mock(BizExportAction.class);
		BizImportAction bizImportAction = mock(BizImportAction.class);
		DownloadAction<Bean> downloadAction = mock(DownloadAction.class);
		UploadAction<Bean> uploadAction = mock(UploadAction.class);
		UserImpl userImpl = mock(UserImpl.class);
		Object dataFactory = new Object();
		Bizlet<Bean> bizlet = mock(Bizlet.class);
		BizletMetaData bizletMetaData = mock(BizletMetaData.class);

		when(delegate.findResourceFile("r", "c", "m")).thenReturn(file);
		when(delegate.getRouter()).thenReturn(router);
		when(delegate.getCustomer("c")).thenReturn(customer);
		when(delegate.getDynamicImage(customer, document, "img", true)).thenReturn(dynamicImage);
		when(delegate.getView("ux", customer, document, "v")).thenReturn(view);
		when(delegate.getMetaDataAction(customer, document, "a")).thenReturn(actionMetaData);
		when(delegate.getComparisonModel(customer, document, "cmp", true)).thenReturn(comparisonModel);
		when(delegate.getMapModel(customer, document, "map", true)).thenReturn(mapModel);
		when(delegate.getChartModel(customer, document, "chart", true)).thenReturn(chartModel);
		when(delegate.getListModel(customer, document, "list", true)).thenReturn(listModel);
		when(delegate.getServerSideAction(customer, document, "S", true)).thenReturn(serverAction);
		when(delegate.getBizExportAction(customer, document, "E", true)).thenReturn(bizExportAction);
		when(delegate.getBizImportAction(customer, document, "I", true)).thenReturn(bizImportAction);
		when(delegate.getDownloadAction(customer, document, "D", true)).thenReturn(downloadAction);
		when(delegate.getUploadAction(customer, document, "U", true)).thenReturn(uploadAction);
		when(delegate.getDataFactory(customer, document)).thenReturn(dataFactory);
		when(delegate.retrieveUser("u")).thenReturn(userImpl);
		when(delegate.retrieveAllScheduledJobsForAllCustomers()).thenReturn(List.of(mock(UserJobSchedule.class)));
		when(delegate.retrieveAllScheduledReportsForAllCustomers()).thenReturn(List.of(mock(UserJobSchedule.class)));
		when(delegate.retrievePublicUserName("c")).thenReturn("public");
		doReturn(true).when(delegate).populatePermissions(user);
		doReturn(true).when(delegate).populateUser(user, connection);
		when(delegate.getAllCustomerNames()).thenReturn(List.of("a"));
		when(delegate.getAllVanillaModuleNames()).thenReturn(List.of("m"));
		when(delegate.getModule(customer, "mod")).thenReturn(module);
		when(delegate.getDocument(customer, module, "doc")).thenReturn(document);
		when(delegate.getBizlet(customer, document, true)).thenReturn(bizlet);
		when(delegate.getMetaDataBizlet(customer, document)).thenReturn(bizletMetaData);
		when(delegate.getGlobalRouter()).thenReturn(router);
		when(delegate.getModuleRouters()).thenReturn(List.of(router));
		when(delegate.getReportFileName(customer, document, "rpt")).thenReturn("report");
		doReturn(String.class).when(delegate).getJavaClass(customer, "key");
		when(delegate.vtable("cust", "key")).thenReturn("vt");
		doReturn(true).when(delegate).getUseScaffoldedViews();

		assertSame(delegate, repo.getSessionDelegate());
		assertSame(delegate, repo.getSessionDelegate(user));
		assertSame(file, repo.findResourceFile("r", "c", "m"));
		assertSame(router, repo.getRouter());
		assertSame(customer, repo.getCustomer("c"));
		assertSame(dynamicImage, repo.getDynamicImage(customer, document, "img", true));
		assertSame(view, repo.getView("ux", customer, document, "v"));
		assertSame(actionMetaData, repo.getMetaDataAction(customer, document, "a"));
		assertSame(comparisonModel, repo.getComparisonModel(customer, document, "cmp", true));
		assertSame(mapModel, repo.getMapModel(customer, document, "map", true));
		assertSame(chartModel, repo.getChartModel(customer, document, "chart", true));
		assertSame(listModel, repo.getListModel(customer, document, "list", true));
		assertSame(serverAction, repo.getServerSideAction(customer, document, "S", true));
		assertSame(bizExportAction, repo.getBizExportAction(customer, document, "E", true));
		assertSame(bizImportAction, repo.getBizImportAction(customer, document, "I", true));
		assertSame(downloadAction, repo.getDownloadAction(customer, document, "D", true));
		assertSame(uploadAction, repo.getUploadAction(customer, document, "U", true));
		assertSame(dataFactory, repo.getDataFactory(customer, document));
		assertSame(userImpl, repo.retrieveUser("u"));
		assertEquals(1, repo.retrieveAllScheduledJobsForAllCustomers().size());
		assertEquals(1, repo.retrieveAllScheduledReportsForAllCustomers().size());
		assertEquals("public", repo.retrievePublicUserName("c"));
		assertTrue(repo.populatePermissions(user));
		assertTrue(repo.populateUser(user, connection));
		assertEquals(List.of("a"), repo.getAllCustomerNames());
		assertEquals(List.of("m"), repo.getAllVanillaModuleNames());
		assertSame(module, repo.getModule(customer, "mod"));
		assertSame(document, repo.getDocument(customer, module, "doc"));
		assertSame(bizlet, repo.getBizlet(customer, document, true));
		assertSame(bizletMetaData, repo.getMetaDataBizlet(customer, document));
		assertSame(router, repo.getGlobalRouter());
		assertEquals(1, repo.getModuleRouters().size());
		assertEquals("report", repo.getReportFileName(customer, document, "rpt"));
		assertSame(String.class, repo.getJavaClass(customer, "key"));
		assertEquals("vt", repo.vtable("cust", "key"));
		assertTrue(repo.getUseScaffoldedViews());

		repo.resetMenus(user);
		repo.resetUserPermissions(user);
		repo.validateCustomerForGenerateDomain(customer);
		repo.validateModuleForGenerateDomain(customer, module);
		repo.validateDocumentForGenerateDomain(customer, document);
		repo.validateViewForGenerateDomain(customer, document, view, "ux");
		repo.evictCachedMetaData(customer);

		verify(delegate).resetMenus(user);
		verify(delegate).resetUserPermissions(user);
		verify(delegate).validateCustomerForGenerateDomain(customer);
		verify(delegate).validateModuleForGenerateDomain(customer, module);
		verify(delegate).validateDocumentForGenerateDomain(customer, document);
		verify(delegate).validateViewForGenerateDomain(customer, document, view, "ux");
		verify(delegate).evictCachedMetaData(customer);

		repo.removeSessionDelegate(user);
		assertNull(repo.getSessionDelegate(user));
	}

	@Test
	void setAndRemoveSessionDelegateEnforceThreadAndSessionPreconditions() {
		SessionScopedDelegatingProvidedRepository repo = new SessionScopedDelegatingProvidedRepository();
		ProvidedRepository delegate = mock(ProvidedRepository.class);
		User user = mock(User.class);
		when(user.getName()).thenReturn("u");

		IllegalStateException noPersistenceOnSet = assertThrows(IllegalStateException.class, () -> repo.setSessionDelegate(delegate));
		assertEquals("No persistence on this thread", noPersistenceOnSet.getMessage());
		IllegalStateException noPersistenceOnRemove = assertThrows(IllegalStateException.class, repo::removeSessionDelegate);
		assertEquals("No persistence on this thread", noPersistenceOnRemove.getMessage());

		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setForThread();
		IllegalStateException noUserOnSet = assertThrows(IllegalStateException.class, () -> repo.setSessionDelegate(delegate));
		assertEquals("No user on the persistence on this thread", noUserOnSet.getMessage());
		IllegalStateException noUserOnRemove = assertThrows(IllegalStateException.class, repo::removeSessionDelegate);
		assertEquals("No user on the persistence on this thread", noUserOnRemove.getMessage());

		when(user.getSessionId()).thenReturn(null);
		IllegalStateException noSessionOnSet = assertThrows(IllegalStateException.class, () -> repo.setSessionDelegate(user, delegate));
		assertEquals("User u does not belong to a session", noSessionOnSet.getMessage());
		IllegalStateException noSessionOnRemove = assertThrows(IllegalStateException.class, () -> repo.removeSessionDelegate(user));
		assertEquals("User u does not belong to a session", noSessionOnRemove.getMessage());
		assertNull(repo.getSessionDelegate(user));
	}

	@Test
	void noArgSetAndRemoveSessionDelegateUseThreadUserSession() {
		SessionScopedDelegatingProvidedRepository repo = new SessionScopedDelegatingProvidedRepository();
		ProvidedRepository delegate = mock(ProvidedRepository.class);
		User user = mock(User.class);
		when(user.getSessionId()).thenReturn("S2");

		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();

		repo.setSessionDelegate(delegate);
		assertSame(delegate, repo.getSessionDelegate());

		repo.removeSessionDelegate();
		assertNull(repo.getSessionDelegate());
	}
}
