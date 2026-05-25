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
public class MonitoringDashboardDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() throws Exception {
		MonitoringDashboard bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(MonitoringDashboard.MODULE_NAME, MonitoringDashboard.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() throws Exception {
		MonitoringDashboard bean = MonitoringDashboard.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("MonitoringDashboard", bean.getBizDocument());
	}

	@Test
	void topNSetAndGet() throws Exception {
		MonitoringDashboard bean = MonitoringDashboard.newInstance();
		bean.setTopN(Integer.valueOf(10));
		assertEquals(Integer.valueOf(10), bean.getTopN());
	}

	@Test
	void requestTypeSetAndGet() throws Exception {
		MonitoringDashboard bean = MonitoringDashboard.newInstance();
		bean.setRequestType(RequestType.all);
		assertEquals(RequestType.all, bean.getRequestType());
	}

	@Test
	void metricSetAndGet() throws Exception {
		MonitoringDashboard bean = MonitoringDashboard.newInstance();
		bean.setMetric(Metric.elapsedRequestTime);
		assertEquals(Metric.elapsedRequestTime, bean.getMetric());
	}

	@Test
	void periodSetAndGet() throws Exception {
		MonitoringDashboard bean = MonitoringDashboard.newInstance();
		bean.setPeriod(Period.currentHour);
		assertEquals(Period.currentHour, bean.getPeriod());
	}

	@Test
	void requestTypeEnumFromCode() throws Exception {
		assertEquals(RequestType.all, RequestType.fromCode("all"));
		assertNull(RequestType.fromCode("nonexistent"));
	}

	@Test
	void requestTypeEnumToCode() throws Exception {
		assertNotNull(RequestType.all.toCode());
	}

	@Test
	void requestTypeEnumToLocalisedDescription() throws Exception {
		assertNotNull(RequestType.all.toLocalisedDescription());
	}

	@Test
	void requestTypeEnumToDomainValue() throws Exception {
		assertNotNull(RequestType.all.toDomainValue());
	}

	@Test
	void requestTypeEnumToDomainValues() throws Exception {
		assertNotNull(RequestType.toDomainValues());
		assertFalse(RequestType.toDomainValues().isEmpty());
	}

	@Test
	void requestTypeEnumFromLocalisedDescription() throws Exception {
		String desc = RequestType.all.toLocalisedDescription();
		assertEquals(RequestType.all, RequestType.fromLocalisedDescription(desc));
	}

	@Test
	void requestTypeEnumFromLocalisedDescriptionUnknownReturnsNull() throws Exception {
		assertNull(RequestType.fromLocalisedDescription("nonexistent xyz description"));
	}

	@Test
	void metricEnumFromCode() throws Exception {
		assertEquals(Metric.elapsedRequestTime, Metric.fromCode("t"));
		assertNull(Metric.fromCode("nonexistent"));
	}

	@Test
	void metricEnumToLocalisedDescription() throws Exception {
		assertNotNull(Metric.elapsedRequestTime.toLocalisedDescription());
	}

	@Test
	void metricEnumToDomainValue() throws Exception {
		assertNotNull(Metric.elapsedRequestTime.toDomainValue());
	}

	@Test
	void metricEnumToDomainValues() throws Exception {
		assertNotNull(Metric.toDomainValues());
		assertFalse(Metric.toDomainValues().isEmpty());
	}

	@Test
	void metricEnumFromLocalisedDescription() throws Exception {
		String desc = Metric.elapsedRequestTime.toLocalisedDescription();
		assertEquals(Metric.elapsedRequestTime, Metric.fromLocalisedDescription(desc));
	}

	@Test
	void metricEnumFromLocalisedDescriptionUnknownReturnsNull() throws Exception {
		assertNull(Metric.fromLocalisedDescription("nonexistent xyz description"));
	}

	@Test
	void periodEnumFromCode() throws Exception {
		assertEquals(Period.currentHour, Period.fromCode("h"));
		assertNull(Period.fromCode("nonexistent"));
	}

	@Test
	void periodEnumToLocalisedDescription() throws Exception {
		assertNotNull(Period.currentHour.toLocalisedDescription());
	}

	@Test
	void periodEnumToDomainValue() throws Exception {
		assertNotNull(Period.currentHour.toDomainValue());
	}

	@Test
	void periodEnumToDomainValues() throws Exception {
		assertNotNull(Period.toDomainValues());
		assertFalse(Period.toDomainValues().isEmpty());
	}

	@Test
	void periodEnumFromLocalisedDescription() throws Exception {
		String desc = Period.currentHour.toLocalisedDescription();
		assertEquals(Period.currentHour, Period.fromLocalisedDescription(desc));
	}

	@Test
	void periodEnumFromLocalisedDescriptionUnknownReturnsNull() throws Exception {
		assertNull(Period.fromLocalisedDescription("nonexistent xyz description"));
	}
}
