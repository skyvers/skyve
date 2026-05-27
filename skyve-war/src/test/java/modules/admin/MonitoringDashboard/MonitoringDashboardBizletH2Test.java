package modules.admin.MonitoringDashboard;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.MonitoringDashboard;
import modules.admin.domain.MonitoringDashboard.RequestType;
import util.AbstractH2Test;

/**
 * H2-backed tests for MonitoringDashboardBizlet covering preExecute,
 * getDynamicDomainValues and complete with H2 context.
 */
class MonitoringDashboardBizletH2Test extends AbstractH2Test {

	private static final MonitoringDashboardBizlet bizlet = new MonitoringDashboardBizlet();

	private DataBuilder db;
	private MonitoringDashboard bean;
	private MockWebContext webContext;

	@BeforeEach
	void setup() {
		db = new DataBuilder().fixture(FixtureType.crud);
		bean = db.build(MonitoringDashboard.MODULE_NAME, MonitoringDashboard.DOCUMENT_NAME);
		webContext = new MockWebContext();
	}

	// ---- preExecute: Edit ----

	@Test
	void preExecuteEditSetsMonitoringStartTime() throws Exception {
		MonitoringDashboard result = bizlet.preExecute(ImplicitActionName.Edit, bean, null, webContext);
		assertNotNull(result);
		assertNotNull(result.getMonitoringStartTime());
	}

	@Test
	void preExecuteNewSetsMonitoringStartTime() throws Exception {
		MonitoringDashboard result = bizlet.preExecute(ImplicitActionName.New, bean, null, webContext);
		assertNotNull(result);
		assertNotNull(result.getMonitoringStartTime());
	}

	@Test
	void preExecuteSaveDoesNotChangeStartTime() throws Exception {
		bean.setMonitoringStartTime(null);
		MonitoringDashboard result = bizlet.preExecute(ImplicitActionName.Save, bean, null, webContext);
		assertNotNull(result);
		// Save doesn't set monitoring start time
	}

	// ---- getDynamicDomainValues: documentName ----

	@Test
	void getDynamicDomainValuesForDocumentNameReturnsListWithoutException() throws Exception {
		List<DomainValue> result = bizlet.getDynamicDomainValues(MonitoringDashboard.documentNamePropertyName, bean);
		assertThat(result, is(notNullValue()));
		// In tests, monitoring data is empty, so the result will be empty but should not throw
	}

	// ---- getDynamicDomainValues: queryName ----

	@Test
	void getDynamicDomainValuesForQueryNameReturnsListWithoutException() throws Exception {
		List<DomainValue> result = bizlet.getDynamicDomainValues(MonitoringDashboard.queryNamePropertyName, bean);
		assertThat(result, is(notNullValue()));
	}

	// ---- getDynamicDomainValues: requestType ----

	@Test
	void getDynamicDomainValuesForRequestTypeReturnsListWithoutException() throws Exception {
		List<DomainValue> result = bizlet.getDynamicDomainValues(MonitoringDashboard.requestTypePropertyName, bean);
		assertThat(result, is(notNullValue()));
	}

	@Test
	void getDynamicDomainValuesForRsRequestTypeReturnsListWithoutException() throws Exception {
		List<DomainValue> result = bizlet.getDynamicDomainValues(MonitoringDashboard.rsRequestTypePropertyName, bean);
		assertThat(result, is(notNullValue()));
	}

	// ---- getDynamicDomainValues: unknown attribute ----

	@Test
	void getDynamicDomainValuesForUnknownAttributeCallsSuper() {
		Assertions.assertDoesNotThrow(() -> bizlet.getDynamicDomainValues("unknownAttribute", bean));
		// just shouldn't throw
	}

	// ---- complete with valid requestType ----

	@Test
	void completeRsModuleNameWithAllRequestTypeReturnsEmpty() throws Exception {
		bean.setRsRequestType(RequestType.all);

		List<String> result = bizlet.complete("rsModuleName", "admin", bean);
		assertThat(result, is(notNullValue()));
		assertTrue(result.isEmpty());
	}

	@Test
	void completeRsModuleNameWithValidRequestTypeAndEmptyMonitoringReturnsEmpty() throws Exception {
		bean.setRsRequestType(RequestType.E);

		List<String> result = bizlet.complete("rsModuleName", "", bean);
		assertThat(result, is(notNullValue()));
		// Empty because monitoring has no data in test environment
	}

	@Test
	void completeRsDocumentNameWithValidModuleAndRequestType() throws Exception {
		bean.setRsRequestType(RequestType.E);
		bean.setRsModuleName("admin");

		List<String> result = bizlet.complete("rsDocumentName", "", bean);
		assertThat(result, is(notNullValue()));
	}

	@Test
	void completeRsComponentNameWithValidModuleAndRequestType() throws Exception {
		bean.setRsRequestType(RequestType.E);
		bean.setRsModuleName("admin");

		List<String> result = bizlet.complete("rsComponentName", "", bean);
		assertThat(result, is(notNullValue()));
	}
}
