package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.MonitoringDashboard.Metric;
import modules.admin.domain.MonitoringDashboard.Period;
import modules.admin.domain.MonitoringDashboard.RequestType;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class MonitoringDashboardDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() {
		MonitoringDashboard bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(MonitoringDashboard.MODULE_NAME, MonitoringDashboard.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() {
		MonitoringDashboard bean = MonitoringDashboard.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("MonitoringDashboard", bean.getBizDocument());
	}

	@Test
	void topNSetAndGet() {
		MonitoringDashboard bean = MonitoringDashboard.newInstance();
		bean.setTopN(Integer.valueOf(10));
		assertEquals(Integer.valueOf(10), bean.getTopN());
	}

	@Test
	void requestTypeSetAndGet() {
		MonitoringDashboard bean = MonitoringDashboard.newInstance();
		bean.setRequestType(RequestType.all);
		assertEquals(RequestType.all, bean.getRequestType());
	}

	@Test
	void metricSetAndGet() {
		MonitoringDashboard bean = MonitoringDashboard.newInstance();
		bean.setMetric(Metric.elapsedRequestTime);
		assertEquals(Metric.elapsedRequestTime, bean.getMetric());
	}

	@Test
	void periodSetAndGet() {
		MonitoringDashboard bean = MonitoringDashboard.newInstance();
		bean.setPeriod(Period.currentHour);
		assertEquals(Period.currentHour, bean.getPeriod());
	}

	@Test
	void requestTypeEnumFromCode() {
		assertEquals(RequestType.all, RequestType.fromCode("all"));
		assertNull(RequestType.fromCode("nonexistent"));
	}

	@Test
	void requestTypeEnumToCode() {
		assertNotNull(RequestType.all.toCode());
	}

	@Test
	void requestTypeEnumToLocalisedDescription() {
		assertNotNull(RequestType.all.toLocalisedDescription());
	}

	@Test
	void requestTypeEnumToDomainValue() {
		assertNotNull(RequestType.all.toDomainValue());
	}

	@Test
	void requestTypeEnumToDomainValues() {
		assertNotNull(RequestType.toDomainValues());
		assertFalse(RequestType.toDomainValues().isEmpty());
	}

	@Test
	void requestTypeEnumFromLocalisedDescription() {
		String desc = RequestType.all.toLocalisedDescription();
		assertEquals(RequestType.all, RequestType.fromLocalisedDescription(desc));
	}

	@Test
	void requestTypeEnumFromLocalisedDescriptionUnknownReturnsNull() {
		assertNull(RequestType.fromLocalisedDescription("nonexistent xyz description"));
	}

	@Test
	void metricEnumFromCode() {
		assertEquals(Metric.elapsedRequestTime, Metric.fromCode("t"));
		assertNull(Metric.fromCode("nonexistent"));
	}

	@Test
	void metricEnumToLocalisedDescription() {
		assertNotNull(Metric.elapsedRequestTime.toLocalisedDescription());
	}

	@Test
	void metricEnumToDomainValue() {
		assertNotNull(Metric.elapsedRequestTime.toDomainValue());
	}

	@Test
	void metricEnumToDomainValues() {
		assertNotNull(Metric.toDomainValues());
		assertFalse(Metric.toDomainValues().isEmpty());
	}

	@Test
	void metricEnumFromLocalisedDescription() {
		String desc = Metric.elapsedRequestTime.toLocalisedDescription();
		assertEquals(Metric.elapsedRequestTime, Metric.fromLocalisedDescription(desc));
	}

	@Test
	void metricEnumFromLocalisedDescriptionUnknownReturnsNull() {
		assertNull(Metric.fromLocalisedDescription("nonexistent xyz description"));
	}

	@Test
	void periodEnumFromCode() {
		assertEquals(Period.currentHour, Period.fromCode("h"));
		assertNull(Period.fromCode("nonexistent"));
	}

	@Test
	void periodEnumToLocalisedDescription() {
		assertNotNull(Period.currentHour.toLocalisedDescription());
	}

	@Test
	void periodEnumToDomainValue() {
		assertNotNull(Period.currentHour.toDomainValue());
	}

	@Test
	void periodEnumToDomainValues() {
		assertNotNull(Period.toDomainValues());
		assertFalse(Period.toDomainValues().isEmpty());
	}

	@Test
	void periodEnumFromLocalisedDescription() {
		String desc = Period.currentHour.toLocalisedDescription();
		assertEquals(Period.currentHour, Period.fromLocalisedDescription(desc));
	}

	@Test
	void periodEnumFromLocalisedDescriptionUnknownReturnsNull() {
		assertNull(Period.fromLocalisedDescription("nonexistent xyz description"));
	}
}
