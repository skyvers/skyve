package org.skyve.util.monitoring;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Map;

import org.junit.jupiter.api.Test;

public class RequestMeasurementsTest {

	@Test
	@SuppressWarnings("static-method")
	public void constructorInitialisesSuccessfully() {
		RequestMeasurements rm = new RequestMeasurements();
		assertNotNull(rm);
	}

	@Test
	@SuppressWarnings("static-method")
	public void getSecondsMillisInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getSecondsMillis().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getMinutesMillisInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getMinutesMillis().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getHoursMillisInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getHoursMillis().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getDaysMillisInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getDaysMillis().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getWeeksMillisInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getWeeksMillis().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getSecondsCpuUtilisationInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getSecondsCpuUtilisation().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getMinutesCpuUtilisationInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getMinutesCpuUtilisation().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getHoursCpuUtilisationInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getHoursCpuUtilisation().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getDaysCpuUtilisationInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getDaysCpuUtilisation().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getWeeksCpuUtilisationInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getWeeksCpuUtilisation().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getSecondsHeapRamUsageInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getSecondsHeapRamUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getMinutesHeapRamUsageInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getMinutesHeapRamUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getHoursHeapRamUsageInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getHoursHeapRamUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getDaysHeapRamUsageInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getDaysHeapRamUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getWeeksHeapRamUsageInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getWeeksHeapRamUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getSecondsSystemCpuUsageInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getSecondsSystemCpuUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getMinutesSystemCpuUsageInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getMinutesSystemCpuUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getHoursSystemCpuUsageInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getHoursSystemCpuUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getDaysSystemCpuUsageInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getDaysSystemCpuUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getWeeksSystemCpuUsageInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getWeeksSystemCpuUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getTimeLastUpdateIsRecentAfterConstruction() {
		long before = System.currentTimeMillis();
		RequestMeasurements rm = new RequestMeasurements();
		long after = System.currentTimeMillis();
		assertTrue(rm.getTimeLastUpdate() >= before);
		assertTrue(rm.getTimeLastUpdate() <= after);
	}

	@Test
	@SuppressWarnings("static-method")
	public void toStringContainsClassName() {
		RequestMeasurements rm = new RequestMeasurements();
		String s = rm.toString();
		assertTrue(s.contains("RequestMeasurements"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toStringInitiallyShowsEmptyForAllArrays() {
		RequestMeasurements rm = new RequestMeasurements();
		String s = rm.toString();
		// All arrays are initially cleared so each label shows "(empty)"
		assertTrue(s.contains("(empty)"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void updateMeasurementsRecordsMillisInSecondsMap() {
		RequestMeasurements rm = new RequestMeasurements();
		rm.updateMeasurements(250, (short) 10, (short) 5, (short) 20);
		Map<Integer, Integer> secondsMillis = rm.getSecondsMillis();
		assertEquals(1, secondsMillis.size());
		assertEquals(Integer.valueOf(250), secondsMillis.values().iterator().next());
	}

	@Test
	@SuppressWarnings("static-method")
	public void updateMeasurementsRecordsCpuInSecondsCpuMap() {
		RequestMeasurements rm = new RequestMeasurements();
		rm.updateMeasurements(100, (short) 50, (short) 0, (short) 0);
		Map<Integer, Float> secondsCpu = rm.getSecondsCpuUtilisation();
		assertEquals(1, secondsCpu.size());
		// short 50 → 50/100F = 0.5
		assertEquals(0.5f, secondsCpu.values().iterator().next().floatValue(), 0.001f);
	}

	@Test
	@SuppressWarnings("static-method")
	public void updateMeasurementsRecordsHeapRamInSecondsMap() {
		RequestMeasurements rm = new RequestMeasurements();
		rm.updateMeasurements(100, (short) 0, (short) 0, (short) 75);
		Map<Integer, Float> secondsHeap = rm.getSecondsHeapRamUsage();
		assertEquals(1, secondsHeap.size());
		// short 75 → 75/100F = 0.75
		assertEquals(0.75f, secondsHeap.values().iterator().next().floatValue(), 0.001f);
	}

	@Test
	@SuppressWarnings("static-method")
	public void updateMeasurementsRecordsSystemCpuInSecondsMap() {
		RequestMeasurements rm = new RequestMeasurements();
		rm.updateMeasurements(100, (short) 0, (short) 30, (short) 0);
		Map<Integer, Float> secondsSystemCpu = rm.getSecondsSystemCpuUsage();
		assertEquals(1, secondsSystemCpu.size());
		// short 30 → 30/100F = 0.3
		assertEquals(0.3f, secondsSystemCpu.values().iterator().next().floatValue(), 0.001f);
	}

	@Test
	@SuppressWarnings("static-method")
	public void getTimeLastUpdateUpdatedAfterUpdateMeasurements() {
		RequestMeasurements rm = new RequestMeasurements();
		long before = System.currentTimeMillis();
		rm.updateMeasurements(100, (short) 10, (short) 5, (short) 20);
		long after = System.currentTimeMillis();
		assertTrue(rm.getTimeLastUpdate() >= before);
		assertTrue(rm.getTimeLastUpdate() <= after);
	}

	@Test
	@SuppressWarnings("static-method")
	public void rollupDoesNotThrow() {
		RequestMeasurements rm = new RequestMeasurements();
		rm.updateMeasurements(100, (short) 10, (short) 5, (short) 20);
		// rollup should complete without exception
		rm.rollup();
	}
}
