package modules.admin.MonitoringDashboard;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.junit.jupiter.api.Test;

import modules.admin.domain.MonitoringDashboard;

/**
 * Tests for MonitoringDashboardBizlet.
 *
 * Tests focus on paths that do not require H2 persistence:
 * - preRerender() — modifies bean fields only
 * - complete() — uses Monitoring static in-memory maps
 */
@SuppressWarnings("static-method")
class MonitoringDashboardBizletTest {

	// ===== preRerender =====

	@Test
	void preRerenderRsModuleNameClearsDocumentAndComponent() throws Exception {
		MonitoringDashboardBizlet bizlet = new MonitoringDashboardBizlet();
		MonitoringDashboard bean = new MonitoringDashboard();
		bean.setRsDocumentName("User");
		bean.setRsComponentName("grid");

		bizlet.preRerender(MonitoringDashboard.rsModuleNamePropertyName, bean, null);

		assertNull(bean.getRsDocumentName());
		assertNull(bean.getRsComponentName());
	}

	@Test
	void preRerenderRsModuleNameClearsComponentWhenNoDocument() throws Exception {
		MonitoringDashboardBizlet bizlet = new MonitoringDashboardBizlet();
		MonitoringDashboard bean = new MonitoringDashboard();
		bean.setRsDocumentName(null);
		bean.setRsComponentName("grid");

		bizlet.preRerender(MonitoringDashboard.rsModuleNamePropertyName, bean, null);

		assertNull(bean.getRsDocumentName());
		assertNull(bean.getRsComponentName());
	}

	@Test
	void preRerenderRsModuleNameNoOpWhenBothNull() throws Exception {
		MonitoringDashboardBizlet bizlet = new MonitoringDashboardBizlet();
		MonitoringDashboard bean = new MonitoringDashboard();
		bean.setRsDocumentName(null);
		bean.setRsComponentName(null);

		bizlet.preRerender(MonitoringDashboard.rsModuleNamePropertyName, bean, null);

		assertNull(bean.getRsDocumentName());
		assertNull(bean.getRsComponentName());
	}

	@Test
	void preRerenderRsDocumentNameClearsComponent() throws Exception {
		MonitoringDashboardBizlet bizlet = new MonitoringDashboardBizlet();
		MonitoringDashboard bean = new MonitoringDashboard();
		bean.setRsComponentName("submitForm");

		bizlet.preRerender(MonitoringDashboard.rsDocumentNamePropertyName, bean, null);

		assertNull(bean.getRsComponentName());
	}

	@Test
	void preRerenderRsDocumentNameNoOpWhenComponentNull() throws Exception {
		MonitoringDashboardBizlet bizlet = new MonitoringDashboardBizlet();
		MonitoringDashboard bean = new MonitoringDashboard();
		bean.setRsComponentName(null);

		bizlet.preRerender(MonitoringDashboard.rsDocumentNamePropertyName, bean, null);

		assertNull(bean.getRsComponentName());
	}

	@Test
	void preRerenderUnknownSourceDoesNotModifyBean() throws Exception {
		MonitoringDashboardBizlet bizlet = new MonitoringDashboardBizlet();
		MonitoringDashboard bean = new MonitoringDashboard();
		bean.setRsDocumentName("User");
		bean.setRsComponentName("grid");

		bizlet.preRerender("unknownProperty", bean, null);

		assertEquals("User", bean.getRsDocumentName());
		assertEquals("grid", bean.getRsComponentName());
	}

	// ===== complete() — no H2 needed (uses Monitoring static maps) =====

	@Test
	void completeRsModuleNameWithNullRequestTypeReturnsEmpty() throws Exception {
		MonitoringDashboardBizlet bizlet = new MonitoringDashboardBizlet();
		MonitoringDashboard bean = new MonitoringDashboard();
		bean.setRsRequestType(null);

		List<String> result = bizlet.complete("rsModuleName", "admin", bean);

		assertNotNull(result);
		assertTrue(result.isEmpty());
	}

	@Test
	void completeRsDocumentNameWithNullModuleReturnsEmpty() throws Exception {
		MonitoringDashboardBizlet bizlet = new MonitoringDashboardBizlet();
		MonitoringDashboard bean = new MonitoringDashboard();
		bean.setRsRequestType(MonitoringDashboard.RequestType.E);
		bean.setRsModuleName(null);

		List<String> result = bizlet.complete("rsDocumentName", "User", bean);

		assertNotNull(result);
		assertTrue(result.isEmpty());
	}

	@Test
	void completeRsDocumentNameWithEmptyModuleReturnsEmpty() throws Exception {
		MonitoringDashboardBizlet bizlet = new MonitoringDashboardBizlet();
		MonitoringDashboard bean = new MonitoringDashboard();
		bean.setRsRequestType(MonitoringDashboard.RequestType.E);
		bean.setRsModuleName("  ");

		List<String> result = bizlet.complete("rsDocumentName", "User", bean);

		assertNotNull(result);
		assertTrue(result.isEmpty());
	}

	@Test
	void completeRsComponentNameWithNullModuleReturnsEmpty() throws Exception {
		MonitoringDashboardBizlet bizlet = new MonitoringDashboardBizlet();
		MonitoringDashboard bean = new MonitoringDashboard();
		bean.setRsRequestType(MonitoringDashboard.RequestType.E);
		bean.setRsModuleName(null);

		List<String> result = bizlet.complete("rsComponentName", "form", bean);

		assertNotNull(result);
		assertTrue(result.isEmpty());
	}

	@Test
	void completeUnknownAttributeReturnsNull() throws Exception {
		MonitoringDashboardBizlet bizlet = new MonitoringDashboardBizlet();
		MonitoringDashboard bean = new MonitoringDashboard();

		List<String> result = bizlet.complete("unknownAttr", "value", bean);

		// super.complete() returns null when metaDataBizlet is null
		assertNull(result);
	}
}
