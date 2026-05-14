package org.skyve.util.monitoring;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

/**
 * Tests for {@link Monitoring} static utility methods.
 */
public class MonitoringTest {

	@AfterEach
	@SuppressWarnings("static-method")
	void cleanup() {
		// Ensure the ThreadLocal is cleaned up after each test
		Monitoring.end();
	}

	@Test
	@SuppressWarnings("static-method")
	void startAndEndReturnsMeasure() {
		Monitoring.start();
		Measure result = Monitoring.end();
		assertNotNull(result);
	}

	@Test
	@SuppressWarnings("static-method")
	void endWithoutStartReturnsNonNullMeasure() {
		// end() handles null ThreadLocal gracefully by calling start() internally
		Measure result = Monitoring.end();
		assertNotNull(result);
	}

	@Test
	@SuppressWarnings("static-method")
	void currentThreadCpuTimeReturnsNonNegative() {
		long cpuTime = Monitoring.currentThreadCpuTime();
		assertTrue(cpuTime >= 0);
	}

	@Test
	@SuppressWarnings("static-method")
	void percentageSystemLoadReturnsNonNegative() {
		float load = Monitoring.percentageSystemLoad();
		assertTrue(load >= 0F);
	}

	@Test
	@SuppressWarnings("static-method")
	void totalMemoryInMiBReturnsPositive() {
		float total = Monitoring.totalMemoryInMiB();
		assertTrue(total > 0F);
	}

	@Test
	@SuppressWarnings("static-method")
	void freeMemoryInMiBReturnsNonNegative() {
		float free = Monitoring.freeMemoryInMiB();
		assertTrue(free >= 0F);
	}

	@Test
	@SuppressWarnings("static-method")
	void maxMemoryInMiBReturnsPositive() {
		float max = Monitoring.maxMemoryInMiB();
		assertTrue(max > 0F);
	}

	@Test
	@SuppressWarnings("static-method")
	void percentageUsedMemoryReturnsBetweenZeroAndOneHundred() {
		float used = Monitoring.percentageUsedMemory();
		assertTrue(used >= 0F && used <= 100F);
	}

	@Test
	@SuppressWarnings("static-method")
	void getResourceMeasurementsReturnsNonNull() {
		ResourceMeasurements rm = Monitoring.getResourceMeasurements();
		assertNotNull(rm);
	}

	@Test
	@SuppressWarnings("static-method")
	void getMonitoringStartTimeReturnsPositive() {
		long startTime = Monitoring.getMonitoringStartTime();
		assertTrue(startTime > 0L);
	}

	@Test
	@SuppressWarnings("static-method")
	void getRequestKeyCodesReturnsNonNull() {
		assertNotNull(Monitoring.getRequestKeyCodes());
	}

	@Test
	@SuppressWarnings("static-method")
	void toDomainValuesReturnsNonNull() {
		assertNotNull(Monitoring.toDomainValues());
	}

	@Test
	@SuppressWarnings("static-method")
	void measureWithRequestKeyRecordsEntry() {
		Monitoring.start();
		RequestKey key = RequestKey.queryListModel("testModule", "testQuery");
		Monitoring.measure(key);
		Monitoring.end();
		// If we get here without exception the measure was recorded
		assertNotNull(Monitoring.getRequestMeasurements(key.toString()));
	}

	@Test
	@SuppressWarnings("static-method")
	void purgeDoesNotThrow() {
		Monitoring.purge();
	}

	@Test
	@SuppressWarnings("static-method")
	void consecutiveStartCallsAreIdempotent() {
		Monitoring.start();
		Monitoring.start();
		Measure result = Monitoring.end();
		assertNotNull(result);
	}

	@Test
	@SuppressWarnings("static-method")
	void twoIndependentMeasuresAreNotSame() {
		Monitoring.start();
		Measure first = Monitoring.end();
		Monitoring.start();
		Measure second = Monitoring.end();
		assertNotSame(first, second);
	}
}
