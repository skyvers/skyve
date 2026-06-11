package modules.admin.MonitoringDashboard.models;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.util.monitoring.Monitoring;
import org.skyve.util.monitoring.RequestKey;
import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.Generic;
import modules.admin.domain.MonitoringDashboard.Period;
import util.AbstractH2Test;

/**
 * Unit tests for the static helper methods in RequestListModel.
 * These methods are package-private and testable without H2.
 */
@SuppressWarnings("static-method")
class RequestListModelTest extends AbstractH2Test {

	private static final long TEST_TIME_MILLIS = Instant.parse("2024-06-15T10:30:45Z")
			.toEpochMilli();

	@BeforeEach
	void purgeMonitoringBefore() {
		Monitoring.purge();
	}

	@AfterEach
	void purgeMonitoringAfter() {
		Monitoring.purge();
	}

	// ---- calculateTimestampForIndex ----

	@Test
	void calculateTimestampForIndexCurrentMinute() {
		long ts = RequestListModel.calculateTimestampForIndex(TEST_TIME_MILLIS, 30, Period.currentMinute);
		LocalDateTime result = LocalDateTime.ofInstant(Instant.ofEpochMilli(ts), ZoneId.systemDefault());
		assertEquals(30, result.getSecond(), "Seconds should be 30");
	}

	@Test
	void calculateTimestampForIndexCurrentHour() {
		long ts = RequestListModel.calculateTimestampForIndex(TEST_TIME_MILLIS, 15, Period.currentHour);
		LocalDateTime result = LocalDateTime.ofInstant(Instant.ofEpochMilli(ts), ZoneId.systemDefault());
		assertEquals(15, result.getMinute(), "Minutes should be 15");
		assertEquals(0, result.getSecond(), "Seconds should be 0");
	}

	@Test
	void calculateTimestampForIndexCurrentDay() {
		long ts = RequestListModel.calculateTimestampForIndex(TEST_TIME_MILLIS, 8, Period.currentDay);
		LocalDateTime result = LocalDateTime.ofInstant(Instant.ofEpochMilli(ts), ZoneId.systemDefault());
		assertEquals(8, result.getHour(), "Hours should be 8");
		assertEquals(0, result.getMinute(), "Minutes should be 0");
	}

	@Test
	void calculateTimestampForIndexCurrentWeek() {
		long ts = RequestListModel.calculateTimestampForIndex(TEST_TIME_MILLIS, 2, Period.currentWeek);
		assertTrue(ts > 0, "Timestamp should be positive");
		// Just verifying it executes without exception
		LocalDateTime result = LocalDateTime.ofInstant(Instant.ofEpochMilli(ts), ZoneId.systemDefault());
		assertNotNull(result);
	}

	@Test
	void calculateTimestampForIndexCurrentYear() {
		long ts = RequestListModel.calculateTimestampForIndex(TEST_TIME_MILLIS, 10, Period.currentYear);
		assertTrue(ts > 0, "Timestamp should be positive");
		LocalDateTime result = LocalDateTime.ofInstant(Instant.ofEpochMilli(ts), ZoneId.systemDefault());
		assertNotNull(result);
	}

	// ---- formatTimestampLabel ----

	@Test
	void formatTimestampLabelCurrentMinute() {
		long ts = RequestListModel.calculateTimestampForIndex(TEST_TIME_MILLIS, 30, Period.currentMinute);
		String label = RequestListModel.formatTimestampLabel(ts, Period.currentMinute);
		assertNotNull(label);
		// HH:mm:ss format
		assertTrue(label.matches("\\d{2}:\\d{2}:\\d{2}"), "Should match HH:mm:ss format, got: " + label);
	}

	@Test
	void formatTimestampLabelCurrentHour() {
		String label = RequestListModel.formatTimestampLabel(TEST_TIME_MILLIS, Period.currentHour);
		assertNotNull(label);
		// HH:mm format
		assertTrue(label.matches("\\d{2}:\\d{2}"), "Should match HH:mm format, got: " + label);
	}

	@Test
	void formatTimestampLabelCurrentDay() {
		String label = RequestListModel.formatTimestampLabel(TEST_TIME_MILLIS, Period.currentDay);
		assertNotNull(label);
		// MM/dd HH:00 format
		assertTrue(label.matches("\\d{2}/\\d{2} \\d{2}:00"), "Should match MM/dd HH:00 format, got: " + label);
	}

	@Test
	void formatTimestampLabelCurrentWeek() {
		String label = RequestListModel.formatTimestampLabel(TEST_TIME_MILLIS, Period.currentWeek);
		assertNotNull(label);
		// MM/dd format
		assertTrue(label.matches("\\d{2}/\\d{2}"), "Should match MM/dd format, got: " + label);
	}

	@Test
	void formatTimestampLabelCurrentYear() {
		String label = RequestListModel.formatTimestampLabel(TEST_TIME_MILLIS, Period.currentYear);
		assertNotNull(label);
		assertTrue(label.matches("\\d{2}/\\d{2}"), "Should match MM/dd format, got: " + label);
	}

	// ---- extract* methods ----

	@Test
	void extractMillisForCurrentMinute() {
		RequestMeasurements m = new RequestMeasurements();
		Map<Integer, Integer> result = RequestListModel.extractMillisForTimePeriod(m, Period.currentMinute);
		assertNotNull(result);
	}

	@Test
	void extractMillisForCurrentHour() {
		RequestMeasurements m = new RequestMeasurements();
		Map<Integer, Integer> result = RequestListModel.extractMillisForTimePeriod(m, Period.currentHour);
		assertNotNull(result);
	}

	@Test
	void extractMillisForCurrentDay() {
		RequestMeasurements m = new RequestMeasurements();
		Map<Integer, Integer> result = RequestListModel.extractMillisForTimePeriod(m, Period.currentDay);
		assertNotNull(result);
	}

	@Test
	void extractMillisForCurrentWeek() {
		RequestMeasurements m = new RequestMeasurements();
		Map<Integer, Integer> result = RequestListModel.extractMillisForTimePeriod(m, Period.currentWeek);
		assertNotNull(result);
	}

	@Test
	void extractMillisForCurrentYear() {
		RequestMeasurements m = new RequestMeasurements();
		Map<Integer, Integer> result = RequestListModel.extractMillisForTimePeriod(m, Period.currentYear);
		assertNotNull(result);
	}

	@Test
	void extractCpuUtilisationForCurrentMinute() {
		RequestMeasurements m = new RequestMeasurements();
		Map<Integer, Float> result = RequestListModel.extractCpuUtilisationForTimePeriod(m, Period.currentMinute);
		assertNotNull(result);
	}

	@Test
	void extractCpuUtilisationForCurrentHour() {
		RequestMeasurements m = new RequestMeasurements();
		Map<Integer, Float> result = RequestListModel.extractCpuUtilisationForTimePeriod(m, Period.currentHour);
		assertNotNull(result);
	}

	@Test
	void extractCpuUtilisationForCurrentDay() {
		RequestMeasurements m = new RequestMeasurements();
		Map<Integer, Float> result = RequestListModel.extractCpuUtilisationForTimePeriod(m, Period.currentDay);
		assertNotNull(result);
	}

	@Test
	void extractCpuUtilisationForCurrentWeek() {
		RequestMeasurements m = new RequestMeasurements();
		Map<Integer, Float> result = RequestListModel.extractCpuUtilisationForTimePeriod(m, Period.currentWeek);
		assertNotNull(result);
	}

	@Test
	void extractCpuUtilisationForCurrentYear() {
		RequestMeasurements m = new RequestMeasurements();
		Map<Integer, Float> result = RequestListModel.extractCpuUtilisationForTimePeriod(m, Period.currentYear);
		assertNotNull(result);
	}

	@Test
	void extractSystemCpuUsageForCurrentMinute() {
		RequestMeasurements m = new RequestMeasurements();
		Map<Integer, Float> result = RequestListModel.extractSystemCpuUsageForTimePeriod(m, Period.currentMinute);
		assertNotNull(result);
	}

	@Test
	void extractSystemCpuUsageForCurrentHour() {
		RequestMeasurements m = new RequestMeasurements();
		Map<Integer, Float> result = RequestListModel.extractSystemCpuUsageForTimePeriod(m, Period.currentHour);
		assertNotNull(result);
	}

	@Test
	void extractSystemCpuUsageForCurrentDay() {
		RequestMeasurements m = new RequestMeasurements();
		Map<Integer, Float> result = RequestListModel.extractSystemCpuUsageForTimePeriod(m, Period.currentDay);
		assertNotNull(result);
	}

	@Test
	void extractSystemCpuUsageForCurrentWeek() {
		RequestMeasurements m = new RequestMeasurements();
		Map<Integer, Float> result = RequestListModel.extractSystemCpuUsageForTimePeriod(m, Period.currentWeek);
		assertNotNull(result);
	}

	@Test
	void extractSystemCpuUsageForCurrentYear() {
		RequestMeasurements m = new RequestMeasurements();
		Map<Integer, Float> result = RequestListModel.extractSystemCpuUsageForTimePeriod(m, Period.currentYear);
		assertNotNull(result);
	}

	@Test
	void extractHeapRamUsageForCurrentMinute() {
		RequestMeasurements m = new RequestMeasurements();
		Map<Integer, Float> result = RequestListModel.extractHeapRamUsageForTimePeriod(m, Period.currentMinute);
		assertNotNull(result);
	}

	@Test
	void extractHeapRamUsageForCurrentHour() {
		RequestMeasurements m = new RequestMeasurements();
		Map<Integer, Float> result = RequestListModel.extractHeapRamUsageForTimePeriod(m, Period.currentHour);
		assertNotNull(result);
	}

	@Test
	void extractHeapRamUsageForCurrentDay() {
		RequestMeasurements m = new RequestMeasurements();
		Map<Integer, Float> result = RequestListModel.extractHeapRamUsageForTimePeriod(m, Period.currentDay);
		assertNotNull(result);
	}

	@Test
	void extractHeapRamUsageForCurrentWeek() {
		RequestMeasurements m = new RequestMeasurements();
		Map<Integer, Float> result = RequestListModel.extractHeapRamUsageForTimePeriod(m, Period.currentWeek);
		assertNotNull(result);
	}

	@Test
	void extractHeapRamUsageForCurrentYear() {
		RequestMeasurements m = new RequestMeasurements();
		Map<Integer, Float> result = RequestListModel.extractHeapRamUsageForTimePeriod(m, Period.currentYear);
		assertNotNull(result);
	}

	@Test
	void postConstructDefinesGenericDrivingDocumentAndReadOnlyColumns() {
		RequestListModel model = new RequestListModel();

		model.postConstruct(CORE.getCustomer(), false);

		assertEquals("Requests", model.getDescription());
		assertEquals(Generic.DOCUMENT_NAME, model.getDrivingDocument().getName());
		List<MetaDataQueryColumn> columns = model.getColumns();
		assertEquals(6, columns.size());
		assertEquals(Generic.timestamp1PropertyName, columns.get(0).getBinding());
		assertEquals("Time", columns.get(0).getDisplayName());
		assertEquals(Generic.memo1PropertyName, columns.get(1).getBinding());
		assertEquals(Integer.valueOf(75), columns.get(2).getPixelWidth());
		assertEquals(Integer.valueOf(125), columns.get(3).getPixelWidth());
	}

	@Test
	void getRowsReturnsEmptyListWhenMonitoringHasNoRequestMeasurements() throws Exception {
		RequestListModel model = new RequestListModel();

		assertTrue(model.getRows().isEmpty());
	}

	@Test
	void getRowsMaterialisesSeededMonitoringMeasurements() throws Exception {
		Monitoring.start();
		Monitoring.measure(RequestKey.queryListModel("admin", "Users"));

		RequestListModel model = new RequestListModel();
		List<Bean> rows = model.getRows();

		assertEquals(1, rows.size());
		Generic row = (Generic) rows.get(0);
		assertNotNull(row.getTimestamp1());
		assertEquals("Model admin Users", row.getMemo1());
		assertNotNull(row.getInteger1());
		assertNotNull(row.getDecimal21());
		assertNotNull(row.getDecimal22());
		assertNotNull(row.getDecimal23());
	}

	@Test
	void unsupportedListModelOperationsThrow() {
		RequestListModel model = new RequestListModel();

		assertThrows(IllegalStateException.class, () -> model.update("id", new TreeMap<>()));
		assertThrows(IllegalStateException.class, () -> model.remove("id"));
	}
}
