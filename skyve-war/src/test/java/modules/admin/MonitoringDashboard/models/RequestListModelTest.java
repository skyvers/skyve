package modules.admin.MonitoringDashboard.models;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Unit tests for the static helper methods in RequestListModel.
 * These methods are package-private and testable without H2.
 */
public class RequestListModelTest {

	private static final long TEST_TIME_MILLIS = Instant.parse("2024-06-15T10:30:45Z")
			.toEpochMilli();

	// ---- calculateTimestampForIndex ----

	@Test
	void calculateTimestampForIndexCurrentMinute() {
		long ts = RequestListModel.calculateTimestampForIndex(TEST_TIME_MILLIS, 30, Period.currentMinute);
		LocalDateTime result = LocalDateTime.ofInstant(Instant.ofEpochMilli(ts), ZoneId.systemDefault());
		// Seconds should be replaced with 30
		assertTrue(result.getSecond() == 30, "Seconds should be 30");
	}

	@Test
	void calculateTimestampForIndexCurrentHour() {
		long ts = RequestListModel.calculateTimestampForIndex(TEST_TIME_MILLIS, 15, Period.currentHour);
		LocalDateTime result = LocalDateTime.ofInstant(Instant.ofEpochMilli(ts), ZoneId.systemDefault());
		assertTrue(result.getMinute() == 15, "Minutes should be 15");
		assertTrue(result.getSecond() == 0, "Seconds should be 0");
	}

	@Test
	void calculateTimestampForIndexCurrentDay() {
		long ts = RequestListModel.calculateTimestampForIndex(TEST_TIME_MILLIS, 8, Period.currentDay);
		LocalDateTime result = LocalDateTime.ofInstant(Instant.ofEpochMilli(ts), ZoneId.systemDefault());
		assertTrue(result.getHour() == 8, "Hours should be 8");
		assertTrue(result.getMinute() == 0, "Minutes should be 0");
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
}
