package org.skyve.util.monitoring;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Map;

import org.junit.jupiter.api.Test;

public class ResourceMeasurementsTest {

	@Test
	@SuppressWarnings("static-method")
	public void constructorInitialisesSuccessfully() {
		ResourceMeasurements rm = new ResourceMeasurements();
		assertNotNull(rm);
	}

	@Test
	@SuppressWarnings("static-method")
	public void getSecondsSystemCpuUsageInitiallyEmpty() {
		ResourceMeasurements rm = new ResourceMeasurements();
		assertTrue(rm.getSecondsSystemCpuUsageUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getMinutesSystemCpuUsageInitiallyEmpty() {
		ResourceMeasurements rm = new ResourceMeasurements();
		assertTrue(rm.getMinutesSystemCpuUsageUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getHoursSystemCpuUsageInitiallyEmpty() {
		ResourceMeasurements rm = new ResourceMeasurements();
		assertTrue(rm.getHoursSystemCpuUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getDaysSystemCpuUsageInitiallyEmpty() {
		ResourceMeasurements rm = new ResourceMeasurements();
		assertTrue(rm.getDaysSystemCpuUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getWeeksSystemCpuUsageInitiallyEmpty() {
		ResourceMeasurements rm = new ResourceMeasurements();
		assertTrue(rm.getWeeksSystemCpuUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getSecondsHeapRamUsageInitiallyEmpty() {
		ResourceMeasurements rm = new ResourceMeasurements();
		assertTrue(rm.getSecondsHeapRamUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getMinutesHeapRamUsageInitiallyEmpty() {
		ResourceMeasurements rm = new ResourceMeasurements();
		assertTrue(rm.getMinutesHeapRamUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getHoursHeapRamUsageInitiallyEmpty() {
		ResourceMeasurements rm = new ResourceMeasurements();
		assertTrue(rm.getHoursHeapRamUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getDaysHeapRamUsageInitiallyEmpty() {
		ResourceMeasurements rm = new ResourceMeasurements();
		assertTrue(rm.getDaysHeapRamUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getWeeksHeapRamUsageInitiallyEmpty() {
		ResourceMeasurements rm = new ResourceMeasurements();
		assertTrue(rm.getWeeksHeapRamUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void toStringContainsClassName() {
		ResourceMeasurements rm = new ResourceMeasurements();
		String s = rm.toString();
		assertTrue(s.contains("ResourceMeasurements"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toStringInitiallyShowsEmpty() {
		ResourceMeasurements rm = new ResourceMeasurements();
		String s = rm.toString();
		assertTrue(s.contains("(empty)"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void updateMeasurementsRecordsSystemCpuInSecondsMap() {
		ResourceMeasurements rm = new ResourceMeasurements();
		rm.updateMeasurements(0.5f, 0.3f);
		Map<Integer, Float> seconds = rm.getSecondsSystemCpuUsageUsage();
		assertEquals(1, seconds.size());
		// 0.5 * 100 = 50 short, then 50/100F = 0.5
		assertEquals(0.5f, seconds.values().iterator().next().floatValue(), 0.01f);
	}

	@Test
	@SuppressWarnings("static-method")
	public void updateMeasurementsRecordsHeapRamInSecondsMap() {
		ResourceMeasurements rm = new ResourceMeasurements();
		rm.updateMeasurements(0.1f, 0.75f);
		Map<Integer, Float> seconds = rm.getSecondsHeapRamUsage();
		assertEquals(1, seconds.size());
		// 0.75 * 100 = 75 short, then 75/100F = 0.75
		assertEquals(0.75f, seconds.values().iterator().next().floatValue(), 0.01f);
	}

	@Test
	@SuppressWarnings("static-method")
	public void rollupDoesNotThrow() {
		ResourceMeasurements rm = new ResourceMeasurements();
		rm.updateMeasurements(0.5f, 0.3f);
		rm.rollup();
	}
}
