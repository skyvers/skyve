package org.skyve.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.io.File;
import java.sql.Connection;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;
import org.skyve.impl.metadata.repository.behaviour.BizletMetaData;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.job.UserJobSchedule;
import org.skyve.metadata.MetaDataException;
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

@SuppressWarnings({ "boxing", "static-method" })
class DelegatingProvidedRepositoryChainTest {
	@Test
	void voidDelegatingMethodsInvokeAllDelegates() {
		ProvidedRepository d1 = mock(ProvidedRepository.class);
		ProvidedRepository d2 = mock(ProvidedRepository.class);
		DelegatingProvidedRepositoryChain chain = new DelegatingProvidedRepositoryChain(d1, d2);

		Customer customer = mock(Customer.class);
		User user = mock(User.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		View view = mock(View.class);

		chain.evictCachedMetaData(customer);
		chain.resetMenus(user);
		chain.resetUserPermissions(user);
		chain.validateCustomerForGenerateDomain(customer);
		chain.validateModuleForGenerateDomain(customer, module);
		chain.validateDocumentForGenerateDomain(customer, document);
		chain.validateViewForGenerateDomain(customer, document, view, "uxui");

		verify(d1).evictCachedMetaData(customer);
		verify(d2).evictCachedMetaData(customer);
		verify(d1).resetMenus(user);
		verify(d2).resetMenus(user);
		verify(d1).resetUserPermissions(user);
		verify(d2).resetUserPermissions(user);
		verify(d1).validateCustomerForGenerateDomain(customer);
		verify(d2).validateCustomerForGenerateDomain(customer);
		verify(d1).validateModuleForGenerateDomain(customer, module);
		verify(d2).validateModuleForGenerateDomain(customer, module);
		verify(d1).validateDocumentForGenerateDomain(customer, document);
		verify(d2).validateDocumentForGenerateDomain(customer, document);
		verify(d1).validateViewForGenerateDomain(customer, document, view, "uxui");
		verify(d2).validateViewForGenerateDomain(customer, document, view, "uxui");
	}

	@Test
	void firstNonNullLookupMethodsReturnFirstAvailableValue() {
		ProvidedRepository d1 = mock(ProvidedRepository.class);
		ProvidedRepository d2 = mock(ProvidedRepository.class);
		DelegatingProvidedRepositoryChain chain = new DelegatingProvidedRepositoryChain(d1, d2);

		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);
		Module module = mock(Module.class);
		File file = new File("x");
		Router router = mock(Router.class);
		DynamicImage<Bean> dynamicImage = mock(DynamicImage.class);
		View view = mock(View.class);
		ActionMetaData actionMetaData = mock(ActionMetaData.class);
		ComparisonModel<Bean, Bean> comparisonModel = mock(ComparisonModel.class);
		MapModel<Bean> mapModel = mock(MapModel.class);
		ChartModel<Bean> chartModel = mock(ChartModel.class);
		ListModel<Bean> listModel = mock(ListModel.class);
		ServerSideAction<Bean> serverSideAction = mock(ServerSideAction.class);
		BizExportAction bizExportAction = mock(BizExportAction.class);
		BizImportAction bizImportAction = mock(BizImportAction.class);
		DownloadAction<Bean> downloadAction = mock(DownloadAction.class);
		UploadAction<Bean> uploadAction = mock(UploadAction.class);
		Object dataFactory = new Object();
		UserImpl userImpl = mock(UserImpl.class);
		List<UserJobSchedule> scheduledJobs = List.of(mock(UserJobSchedule.class));
		List<UserJobSchedule> scheduledReports = List.of(mock(UserJobSchedule.class));
		Bizlet<Bean> bizlet = mock(Bizlet.class);
		BizletMetaData bizletMetaData = mock(BizletMetaData.class);

		when(d1.findResourceFile("r", "c", "m")).thenReturn(null);
		when(d2.findResourceFile("r", "c", "m")).thenReturn(file);
		assertSame(file, chain.findResourceFile("r", "c", "m"));

		when(d1.getRouter()).thenReturn(null);
		when(d2.getRouter()).thenReturn(router);
		assertSame(router, chain.getRouter());

		when(d1.getCustomer("c")).thenReturn(null);
		when(d2.getCustomer("c")).thenReturn(customer);
		assertSame(customer, chain.getCustomer("c"));

		when(d1.getDynamicImage(customer, document, "img", true)).thenReturn(null);
		when(d2.getDynamicImage(customer, document, "img", true)).thenReturn(dynamicImage);
		assertSame(dynamicImage, chain.getDynamicImage(customer, document, "img", true));

		when(d1.getView("ux", customer, document, "v")).thenReturn(null);
		when(d2.getView("ux", customer, document, "v")).thenReturn(view);
		assertSame(view, chain.getView("ux", customer, document, "v"));

		when(d1.getMetaDataAction(customer, document, "a")).thenReturn(null);
		when(d2.getMetaDataAction(customer, document, "a")).thenReturn(actionMetaData);
		assertSame(actionMetaData, chain.getMetaDataAction(customer, document, "a"));

		when(d1.getComparisonModel(customer, document, "cmp", false)).thenReturn(null);
		when(d2.getComparisonModel(customer, document, "cmp", false)).thenReturn(comparisonModel);
		assertSame(comparisonModel, chain.getComparisonModel(customer, document, "cmp", false));

		when(d1.getMapModel(customer, document, "map", false)).thenReturn(null);
		when(d2.getMapModel(customer, document, "map", false)).thenReturn(mapModel);
		assertSame(mapModel, chain.getMapModel(customer, document, "map", false));

		when(d1.getChartModel(customer, document, "chart", false)).thenReturn(null);
		when(d2.getChartModel(customer, document, "chart", false)).thenReturn(chartModel);
		assertSame(chartModel, chain.getChartModel(customer, document, "chart", false));

		when(d1.getListModel(customer, document, "list", false)).thenReturn(null);
		when(d2.getListModel(customer, document, "list", false)).thenReturn(listModel);
		assertSame(listModel, chain.getListModel(customer, document, "list", false));

		when(d1.getServerSideAction(customer, document, "S", false)).thenReturn(null);
		when(d2.getServerSideAction(customer, document, "S", false)).thenReturn(serverSideAction);
		assertSame(serverSideAction, chain.getServerSideAction(customer, document, "S", false));

		when(d1.getBizExportAction(customer, document, "E", false)).thenReturn(null);
		when(d2.getBizExportAction(customer, document, "E", false)).thenReturn(bizExportAction);
		assertSame(bizExportAction, chain.getBizExportAction(customer, document, "E", false));

		when(d1.getBizImportAction(customer, document, "I", false)).thenReturn(null);
		when(d2.getBizImportAction(customer, document, "I", false)).thenReturn(bizImportAction);
		assertSame(bizImportAction, chain.getBizImportAction(customer, document, "I", false));

		when(d1.getDownloadAction(customer, document, "D", false)).thenReturn(null);
		when(d2.getDownloadAction(customer, document, "D", false)).thenReturn(downloadAction);
		assertSame(downloadAction, chain.getDownloadAction(customer, document, "D", false));

		when(d1.getUploadAction(customer, document, "U", false)).thenReturn(null);
		when(d2.getUploadAction(customer, document, "U", false)).thenReturn(uploadAction);
		assertSame(uploadAction, chain.getUploadAction(customer, document, "U", false));

		when(d1.getDataFactory(customer, document)).thenReturn(null);
		when(d2.getDataFactory(customer, document)).thenReturn(dataFactory);
		assertSame(dataFactory, chain.getDataFactory(customer, document));

		when(d1.retrieveUser("u")).thenReturn(null);
		when(d2.retrieveUser("u")).thenReturn(userImpl);
		assertSame(userImpl, chain.retrieveUser("u"));

		when(d1.retrieveAllScheduledJobsForAllCustomers()).thenReturn(Collections.emptyList());
		when(d2.retrieveAllScheduledJobsForAllCustomers()).thenReturn(scheduledJobs);
		assertEquals(1, chain.retrieveAllScheduledJobsForAllCustomers().size());

		when(d1.retrieveAllScheduledReportsForAllCustomers()).thenReturn(Collections.emptyList());
		when(d2.retrieveAllScheduledReportsForAllCustomers()).thenReturn(scheduledReports);
		assertEquals(1, chain.retrieveAllScheduledReportsForAllCustomers().size());

		when(d1.retrievePublicUserName("c")).thenReturn(null);
		when(d2.retrievePublicUserName("c")).thenReturn("publicUser");
		assertEquals("publicUser", chain.retrievePublicUserName("c"));

		when(d1.getBizlet(customer, document, true)).thenReturn(null);
		when(d2.getBizlet(customer, document, true)).thenReturn(bizlet);
		assertSame(bizlet, chain.getBizlet(customer, document, true));

		when(d1.getMetaDataBizlet(customer, document)).thenReturn(null);
		when(d2.getMetaDataBizlet(customer, document)).thenReturn(bizletMetaData);
		assertSame(bizletMetaData, chain.getMetaDataBizlet(customer, document));

		when(d1.getGlobalRouter()).thenReturn(null);
		when(d2.getGlobalRouter()).thenReturn(router);
		assertSame(router, chain.getGlobalRouter());

		when(d1.getReportFileName(customer, document, "rpt")).thenReturn(null);
		when(d2.getReportFileName(customer, document, "rpt")).thenReturn("report.jasper");
		assertEquals("report.jasper", chain.getReportFileName(customer, document, "rpt"));

		doReturn(null).when(d1).getJavaClass(customer, "key");
		doReturn(String.class).when(d2).getJavaClass(customer, "key");
		assertSame(String.class, chain.getJavaClass(customer, "key"));

		when(d1.vtable("cust", "key")).thenReturn(null);
		when(d2.vtable("cust", "key")).thenReturn("vt");
		assertEquals("vt", chain.vtable("cust", "key"));

		when(d1.getModule(customer, "mod")).thenReturn(null);
		when(d2.getModule(customer, "mod")).thenReturn(module);
		assertSame(module, chain.getModule(customer, "mod"));

		when(d1.getDocument(customer, module, "doc")).thenReturn(null);
		when(d2.getDocument(customer, module, "doc")).thenReturn(document);
		assertSame(document, chain.getDocument(customer, module, "doc"));

		assertNull(chain.getDataFactory(mock(Customer.class), mock(Document.class)));
	}

	@Test
	@SuppressWarnings("resource")
	void booleanAndAggregatingMethodsBehaveAsExpected() {
		ProvidedRepository d1 = mock(ProvidedRepository.class);
		ProvidedRepository d2 = mock(ProvidedRepository.class);
		DelegatingProvidedRepositoryChain chain = new DelegatingProvidedRepositoryChain(d1, d2);

		User user = mock(User.class);
		Connection connection = mock(Connection.class);
		Router router = mock(Router.class);

		doReturn(false).when(d1).populatePermissions(user);
		doReturn(true).when(d2).populatePermissions(user);
		assertTrue(chain.populatePermissions(user));

		doReturn(false).when(d1).populateUser(user, connection);
		doReturn(true).when(d2).populateUser(user, connection);
		assertTrue(chain.populateUser(user, connection));

		when(d1.getAllCustomerNames()).thenReturn(List.of("a"));
		when(d2.getAllCustomerNames()).thenReturn(List.of("b", "c"));
		assertEquals(List.of("a", "b", "c"), chain.getAllCustomerNames());

		when(d1.getAllVanillaModuleNames()).thenReturn(List.of("m1"));
		when(d2.getAllVanillaModuleNames()).thenReturn(List.of("m2"));
		assertEquals(List.of("m1", "m2"), chain.getAllVanillaModuleNames());

		when(d1.getModuleRouters()).thenReturn(List.of(router));
		when(d2.getModuleRouters()).thenReturn(List.of());
		assertEquals(1, chain.getModuleRouters().size());

		doReturn(false).when(d1).getUseScaffoldedViews();
		doReturn(true).when(d2).getUseScaffoldedViews();
		assertTrue(chain.getUseScaffoldedViews());
	}

	@Test
	@SuppressWarnings("resource")
	void booleanMethodsReturnFalseWhenNoDelegateReturnsTrue() {
		ProvidedRepository d1 = mock(ProvidedRepository.class);
		ProvidedRepository d2 = mock(ProvidedRepository.class);
		DelegatingProvidedRepositoryChain chain = new DelegatingProvidedRepositoryChain(d1, d2);

		User user = mock(User.class);
		Connection connection = mock(Connection.class);

		doReturn(false).when(d1).populatePermissions(user);
		doReturn(false).when(d2).populatePermissions(user);
		assertFalse(chain.populatePermissions(user));

		doReturn(false).when(d1).populateUser(user, connection);
		doReturn(false).when(d2).populateUser(user, connection);
		assertFalse(chain.populateUser(user, connection));

		doReturn(false).when(d1).getUseScaffoldedViews();
		doReturn(false).when(d2).getUseScaffoldedViews();
		assertFalse(chain.getUseScaffoldedViews());
	}

	@Test
	void moduleAndDocumentLookupsThrowWhenMissing() {
		ProvidedRepository d1 = mock(ProvidedRepository.class);
		DelegatingProvidedRepositoryChain chain = new DelegatingProvidedRepositoryChain(d1);

		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("ACME");
		Module module = mock(Module.class);
		when(module.getName()).thenReturn("sales");

		MetaDataException moduleException = assertThrows(MetaDataException.class, () -> chain.getModule(customer, "sales"));
		assertEquals("sales does not exist for customer ACME", moduleException.getMessage());

		MetaDataException documentException = assertThrows(MetaDataException.class, () -> chain.getDocument(customer, module, "Invoice"));
		assertEquals("Invoice does not exist for module sales for customer ACME", documentException.getMessage());
	}

	@Test
	void delegateListMutationMethodsWork() {
		ProvidedRepository d1 = mock(ProvidedRepository.class);
		ProvidedRepository d2 = mock(ProvidedRepository.class);
		ProvidedRepository d3 = mock(ProvidedRepository.class);
		DelegatingProvidedRepositoryChain chain = new DelegatingProvidedRepositoryChain();
		Customer customer = mock(Customer.class);

		chain.addDelegate(d1);
		chain.addDelegate(0, d2);
		chain.addDelegate(d3);
		chain.removeDelegate(d1);
		chain.removeDelegate(1);

		chain.evictCachedMetaData(customer);

		verify(d2).evictCachedMetaData(customer);
	}

	@Test
	void lookupMethodsReturnNullWhenAllDelegatesReturnNull() {
		ProvidedRepository d1 = mock(ProvidedRepository.class);
		ProvidedRepository d2 = mock(ProvidedRepository.class);
		DelegatingProvidedRepositoryChain chain = new DelegatingProvidedRepositoryChain(d1, d2);

		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);
		Module module = mock(Module.class);
		when(d1.retrieveAllScheduledJobsForAllCustomers()).thenReturn(Collections.emptyList());
		when(d2.retrieveAllScheduledJobsForAllCustomers()).thenReturn(Collections.emptyList());
		when(d1.retrieveAllScheduledReportsForAllCustomers()).thenReturn(Collections.emptyList());
		when(d2.retrieveAllScheduledReportsForAllCustomers()).thenReturn(Collections.emptyList());

			assertNull(chain.findResourceFile("r", "c", "m"));
			assertNull(chain.getRouter());
			assertNull(chain.getCustomer("c"));
			assertNull(chain.getDynamicImage(customer, document, "img", false));
			assertNull(chain.getView("ux", customer, document, "v"));
			assertNull(chain.getMetaDataAction(customer, document, "a"));
			assertNull(chain.getComparisonModel(customer, document, "cmp", false));
			assertNull(chain.getMapModel(customer, document, "map", false));
			assertNull(chain.getChartModel(customer, document, "chart", false));
			assertNull(chain.getListModel(customer, document, "list", false));
			assertNull(chain.getServerSideAction(customer, document, "S", false));
			assertNull(chain.getBizExportAction(customer, document, "E", false));
			assertNull(chain.getBizImportAction(customer, document, "I", false));
			assertNull(chain.getDownloadAction(customer, document, "D", false));
			assertNull(chain.getUploadAction(customer, document, "U", false));
			assertNull(chain.getDataFactory(customer, document));
			assertNull(chain.retrieveUser("u"));
		assertTrue(chain.retrieveAllScheduledJobsForAllCustomers().isEmpty());
		assertTrue(chain.retrieveAllScheduledReportsForAllCustomers().isEmpty());
			assertNull(chain.retrievePublicUserName("c"));
			assertNull(chain.getBizlet(customer, document, false));
			assertNull(chain.getMetaDataBizlet(customer, document));
			assertNull(chain.getGlobalRouter());
			assertNull(chain.getReportFileName(customer, document, "rpt"));
			assertNull(chain.getJavaClass(customer, "key"));
			assertNull(chain.vtable("cust", "key"));
		assertThrows(MetaDataException.class, () -> chain.getDocument(customer, module, "doc"));
	}

	@Test
	@SuppressWarnings("resource")
	void firstDelegateShortCircuitsBooleanMethods() {
		ProvidedRepository d1 = mock(ProvidedRepository.class);
		ProvidedRepository d2 = mock(ProvidedRepository.class);
		DelegatingProvidedRepositoryChain chain = new DelegatingProvidedRepositoryChain(d1, d2);

		User user = mock(User.class);
		Connection connection = mock(Connection.class);

		doReturn(true).when(d1).populatePermissions(user);
		doReturn(true).when(d1).populateUser(user, connection);
		doReturn(true).when(d1).getUseScaffoldedViews();

		assertTrue(chain.populatePermissions(user));
		assertTrue(chain.populateUser(user, connection));
		assertTrue(chain.getUseScaffoldedViews());

		verifyNoInteractions(d2);
	}

	@Test
	void moduleAndDocumentThrowMessagesHandleNullCustomer() {
		ProvidedRepository d1 = mock(ProvidedRepository.class);
		DelegatingProvidedRepositoryChain chain = new DelegatingProvidedRepositoryChain(d1);
		Module module = mock(Module.class);
		when(module.getName()).thenReturn("sales");

		MetaDataException moduleException = assertThrows(MetaDataException.class, () -> chain.getModule(null, "sales"));
		assertEquals("sales does not exist", moduleException.getMessage());

		MetaDataException documentException = assertThrows(MetaDataException.class, () -> chain.getDocument(null, module, "Invoice"));
		assertEquals("Invoice does not exist for module sales", documentException.getMessage());
	}
}

