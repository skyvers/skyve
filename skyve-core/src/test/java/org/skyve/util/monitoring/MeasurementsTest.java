package org.skyve.util.monitoring;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

/**
 * Tests for RequestMeasurements and ResourceMeasurements.
 */
class MeasurementsTest {

	// --- RequestMeasurements ---

	@Test
	@SuppressWarnings("static-method")
	void requestMeasurementsDefaultConstructorCreatesInstance() {
		RequestMeasurements rm = new RequestMeasurements();
		assertNotNull(rm.getSecondsMillis());
		assertNotNull(rm.getMinutesMillis());
		assertNotNull(rm.getHoursMillis());
		assertNotNull(rm.getDaysMillis());
		assertNotNull(rm.getWeeksMillis());
	}

	@Test
	@SuppressWarnings("static-method")
	void requestMeasurementsGettersReturnNonNullMaps() {
		RequestMeasurements rm = new RequestMeasurements();
		assertNotNull(rm.getSecondsCpuUtilisation());
		assertNotNull(rm.getMinutesCpuUtilisation());
		assertNotNull(rm.getHoursCpuUtilisation());
		assertNotNull(rm.getDaysCpuUtilisation());
		assertNotNull(rm.getWeeksCpuUtilisation());
		assertNotNull(rm.getSecondsHeapRamUsage());
		assertNotNull(rm.getMinutesHeapRamUsage());
		assertNotNull(rm.getHoursHeapRamUsage());
		assertNotNull(rm.getDaysHeapRamUsage());
		assertNotNull(rm.getWeeksHeapRamUsage());
		assertNotNull(rm.getSecondsSystemCpuUsage());
		assertNotNull(rm.getMinutesSystemCpuUsage());
		assertNotNull(rm.getHoursSystemCpuUsage());
		assertNotNull(rm.getDaysSystemCpuUsage());
		assertNotNull(rm.getWeeksSystemCpuUsage());
	}

	@Test
	@SuppressWarnings("static-method")
	void requestMeasurementsUpdateMeasurementsDoesNotThrow() {
		RequestMeasurements rm = new RequestMeasurements();
		rm.updateMeasurements(100, (short) 50, (short) 30, (short) 60);
	}

	@Test
	@SuppressWarnings("static-method")
	void requestMeasurementsRollupDoesNotThrow() {
		RequestMeasurements rm = new RequestMeasurements();
		rm.rollup();
	}

	@Test
	@SuppressWarnings("static-method")
	void requestMeasurementsToStringReturnsNonNull() {
		RequestMeasurements rm = new RequestMeasurements();
		assertNotNull(rm.toString());
	}

	@Test
	@SuppressWarnings("static-method")
	void requestMeasurementsGetTimeLastUpdate() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getTimeLastUpdate() >= 0);
	}

	// --- ResourceMeasurements ---

	@Test
	@SuppressWarnings("static-method")
	void resourceMeasurementsDefaultConstructorCreatesInstance() {
		ResourceMeasurements rm = new ResourceMeasurements();
		assertNotNull(rm.getSecondsSystemCpuUsageUsage());
		assertNotNull(rm.getMinutesSystemCpuUsageUsage());
		assertNotNull(rm.getHoursSystemCpuUsage());
		assertNotNull(rm.getDaysSystemCpuUsage());
		assertNotNull(rm.getWeeksSystemCpuUsage());
		assertNotNull(rm.getSecondsHeapRamUsage());
		assertNotNull(rm.getMinutesHeapRamUsage());
		assertNotNull(rm.getHoursHeapRamUsage());
		assertNotNull(rm.getDaysHeapRamUsage());
		assertNotNull(rm.getWeeksHeapRamUsage());
	}

	@Test
	@SuppressWarnings("static-method")
	void resourceMeasurementsUpdateMeasurementsDoesNotThrow() {
		ResourceMeasurements rm = new ResourceMeasurements();
		rm.updateMeasurements(50.0f, 30.0f);
	}

	@Test
	@SuppressWarnings("static-method")
	void resourceMeasurementsRollupDoesNotThrow() {
		ResourceMeasurements rm = new ResourceMeasurements();
		rm.rollup();
	}

	@Test
	@SuppressWarnings("static-method")
	void resourceMeasurementsToStringReturnsNonNull() {
		ResourceMeasurements rm = new ResourceMeasurements();
		assertNotNull(rm.toString());
	}
}
