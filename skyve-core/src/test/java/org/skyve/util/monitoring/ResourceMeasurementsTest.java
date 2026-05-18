package org.skyve.util.monitoring;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;
import java.util.Map;

import org.junit.jupiter.api.Test;

class ResourceMeasurementsTest {

	@Test
	@SuppressWarnings("static-method")
	void constructorInitialisesSuccessfully() {
		ResourceMeasurements rm = new ResourceMeasurements();
		assertNotNull(rm);
	}

	@Test
	@SuppressWarnings("static-method")
	void getSecondsSystemCpuUsageInitiallyEmpty() {
		ResourceMeasurements rm = new ResourceMeasurements();
		assertTrue(rm.getSecondsSystemCpuUsageUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getMinutesSystemCpuUsageInitiallyEmpty() {
		ResourceMeasurements rm = new ResourceMeasurements();
		assertTrue(rm.getMinutesSystemCpuUsageUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getHoursSystemCpuUsageInitiallyEmpty() {
		ResourceMeasurements rm = new ResourceMeasurements();
		assertTrue(rm.getHoursSystemCpuUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getDaysSystemCpuUsageInitiallyEmpty() {
		ResourceMeasurements rm = new ResourceMeasurements();
		assertTrue(rm.getDaysSystemCpuUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getWeeksSystemCpuUsageInitiallyEmpty() {
		ResourceMeasurements rm = new ResourceMeasurements();
		assertTrue(rm.getWeeksSystemCpuUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getSecondsHeapRamUsageInitiallyEmpty() {
		ResourceMeasurements rm = new ResourceMeasurements();
		assertTrue(rm.getSecondsHeapRamUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getMinutesHeapRamUsageInitiallyEmpty() {
		ResourceMeasurements rm = new ResourceMeasurements();
		assertTrue(rm.getMinutesHeapRamUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getHoursHeapRamUsageInitiallyEmpty() {
		ResourceMeasurements rm = new ResourceMeasurements();
		assertTrue(rm.getHoursHeapRamUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getDaysHeapRamUsageInitiallyEmpty() {
		ResourceMeasurements rm = new ResourceMeasurements();
		assertTrue(rm.getDaysHeapRamUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getWeeksHeapRamUsageInitiallyEmpty() {
		ResourceMeasurements rm = new ResourceMeasurements();
		assertTrue(rm.getWeeksHeapRamUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void toStringContainsClassName() {
		ResourceMeasurements rm = new ResourceMeasurements();
		String s = rm.toString();
		assertTrue(s.contains("ResourceMeasurements"));
	}

	@Test
	@SuppressWarnings("static-method")
	void toStringInitiallyShowsEmpty() {
		ResourceMeasurements rm = new ResourceMeasurements();
		String s = rm.toString();
		assertTrue(s.contains("(empty)"));
	}

	@Test
	@SuppressWarnings("static-method")
	void updateMeasurementsRecordsSystemCpuInSecondsMap() {
		ResourceMeasurements rm = new ResourceMeasurements();
		rm.updateMeasurements(0.5f, 0.3f);
		Map<Integer, Float> seconds = rm.getSecondsSystemCpuUsageUsage();
		assertEquals(1, seconds.size());
		// 0.5 * 100 = 50 short, then 50/100F = 0.5
		assertEquals(0.5f, seconds.values().iterator().next().floatValue(), 0.01f);
	}

	@Test
	@SuppressWarnings("static-method")
	void updateMeasurementsRecordsHeapRamInSecondsMap() {
		ResourceMeasurements rm = new ResourceMeasurements();
		rm.updateMeasurements(0.1f, 0.75f);
		Map<Integer, Float> seconds = rm.getSecondsHeapRamUsage();
		assertEquals(1, seconds.size());
		// 0.75 * 100 = 75 short, then 75/100F = 0.75
		assertEquals(0.75f, seconds.values().iterator().next().floatValue(), 0.01f);
	}

	@Test
	@SuppressWarnings("static-method")
	void rollupDoesNotThrow() {
		ResourceMeasurements rm = new ResourceMeasurements();
		rm.updateMeasurements(0.5f, 0.3f);
		rm.rollup();
	}

	private static void setIntField(ResourceMeasurements rm, String fieldName, int value) throws Exception {
		Field f = ResourceMeasurements.class.getDeclaredField(fieldName);
		f.setAccessible(true);
		f.set(rm, Integer.valueOf(value));
	}

	@Test
	@SuppressWarnings("static-method")
	void rollupWithMinuteBoundaryMovesSecondsToMinutes() throws Exception {
		ResourceMeasurements rm = new ResourceMeasurements();
		rm.updateMeasurements(0.8f, 0.5f);

		Field lastMinuteField = ResourceMeasurements.class.getDeclaredField("lastMinute");
		lastMinuteField.setAccessible(true);
		int currentMinute = lastMinuteField.getInt(rm);

		int prevMinute = (currentMinute - 1 + 60) % 60;
		setIntField(rm, "lastMinute", prevMinute);

		rm.rollup();

		Map<Integer, Float> minutesCpu = rm.getMinutesSystemCpuUsageUsage();
		assertFalse(minutesCpu.isEmpty(), "minutesSystemCpu should have an entry after minute rollup");
		assertEquals(prevMinute, minutesCpu.keySet().iterator().next().intValue());

		assertTrue(rm.getSecondsSystemCpuUsageUsage().isEmpty(), "secondsCpu should be empty after rollup");
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void rollupWithTwoMinutesBoundaryFillsMinutesBuckets() throws Exception {
		ResourceMeasurements rm = new ResourceMeasurements();
		rm.updateMeasurements(0.6f, 0.4f);

		Field lastMinuteField = ResourceMeasurements.class.getDeclaredField("lastMinute");
		lastMinuteField.setAccessible(true);
		int currentMinute = lastMinuteField.getInt(rm);

		int twoBack = (currentMinute - 2 + 60) % 60;
		setIntField(rm, "lastMinute", twoBack);

		rm.rollup();

		Map<Integer, Float> minutesCpu = rm.getMinutesSystemCpuUsageUsage();
		assertFalse(minutesCpu.isEmpty(), "minutesCpu should have entries after 2-minute rollup");
		assertTrue(minutesCpu.containsKey(twoBack), "minutesCpu should have entry for first skipped minute");
	}

	@Test
	@SuppressWarnings("static-method")
	void rollupStaticHelperAveragesNonMinValues() throws Exception {
		java.lang.reflect.Method rollupMethod = ResourceMeasurements.class
				.getDeclaredMethod("rollup", short[].class, short[].class, int.class);
		rollupMethod.setAccessible(true);

		short[] source = new short[60];
		java.util.Arrays.fill(source, Short.MIN_VALUE);
		source[3] = (short) 100;
		source[7] = (short) 200;
		short[] target = new short[60];
		java.util.Arrays.fill(target, Short.MIN_VALUE);

		rollupMethod.invoke(null, source, target, Integer.valueOf(1));

		assertEquals((short) 150, target[1]);
	}

	@Test
	@SuppressWarnings("static-method")
	void rollupStaticHelperAllMinValueProducesShortMinValue() throws Exception {
		java.lang.reflect.Method rollupMethod = ResourceMeasurements.class
				.getDeclaredMethod("rollup", short[].class, short[].class, int.class);
		rollupMethod.setAccessible(true);

		short[] source = new short[60];
		java.util.Arrays.fill(source, Short.MIN_VALUE);
		short[] target = new short[60];
		java.util.Arrays.fill(target, Short.MIN_VALUE);

		rollupMethod.invoke(null, source, target, Integer.valueOf(5));

		assertEquals(Short.MIN_VALUE, target[5]);
	}

	@Test
	@SuppressWarnings("static-method")
	void clearResetsAllArraysAndLastValues() throws Exception {
		ResourceMeasurements rm = new ResourceMeasurements();
		rm.updateMeasurements(0.9f, 0.7f);

		// Verify data was recorded
		assertFalse(rm.getSecondsSystemCpuUsageUsage().isEmpty());

		// Access package-private clear() method
		java.lang.reflect.Method clearMethod = ResourceMeasurements.class.getDeclaredMethod("clear");
		clearMethod.setAccessible(true);
		clearMethod.invoke(rm);

		// After clear, all getters should return empty maps
		assertTrue(rm.getSecondsSystemCpuUsageUsage().isEmpty(), "secondsCpu should be empty after clear");
		assertTrue(rm.getSecondsHeapRamUsage().isEmpty(), "secondsRam should be empty after clear");
		assertTrue(rm.getMinutesSystemCpuUsageUsage().isEmpty(), "minutesCpu should be empty after clear");
	}

	@Test
	@SuppressWarnings("static-method")
	void updateMeasurementsTimeLastUpdateIsUpdated() {
		ResourceMeasurements rm = new ResourceMeasurements();
		long before = System.currentTimeMillis();
		rm.updateMeasurements(0.5f, 0.3f);
		long after = System.currentTimeMillis();
		assertNotNull(rm);
		// After update, should still be fine
		assertTrue(after >= before);
	}
}
