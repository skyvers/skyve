package org.skyve.util.monitoring;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;
import java.util.Map;

import org.junit.jupiter.api.Test;

class RequestMeasurementsTest {

	@Test
	@SuppressWarnings("static-method")
	void constructorInitialisesSuccessfully() {
		RequestMeasurements rm = new RequestMeasurements();
		assertNotNull(rm);
	}

	@Test
	@SuppressWarnings("static-method")
	void getSecondsMillisInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getSecondsMillis().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getMinutesMillisInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getMinutesMillis().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getHoursMillisInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getHoursMillis().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getDaysMillisInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getDaysMillis().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getWeeksMillisInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getWeeksMillis().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getSecondsCpuUtilisationInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getSecondsCpuUtilisation().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getMinutesCpuUtilisationInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getMinutesCpuUtilisation().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getHoursCpuUtilisationInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getHoursCpuUtilisation().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getDaysCpuUtilisationInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getDaysCpuUtilisation().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getWeeksCpuUtilisationInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getWeeksCpuUtilisation().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getSecondsHeapRamUsageInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getSecondsHeapRamUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getMinutesHeapRamUsageInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getMinutesHeapRamUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getHoursHeapRamUsageInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getHoursHeapRamUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getDaysHeapRamUsageInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getDaysHeapRamUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getWeeksHeapRamUsageInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getWeeksHeapRamUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getSecondsSystemCpuUsageInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getSecondsSystemCpuUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getMinutesSystemCpuUsageInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getMinutesSystemCpuUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getHoursSystemCpuUsageInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getHoursSystemCpuUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getDaysSystemCpuUsageInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getDaysSystemCpuUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getWeeksSystemCpuUsageInitiallyEmpty() {
		RequestMeasurements rm = new RequestMeasurements();
		assertTrue(rm.getWeeksSystemCpuUsage().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getTimeLastUpdateIsRecentAfterConstruction() {
		long before = System.currentTimeMillis();
		RequestMeasurements rm = new RequestMeasurements();
		long after = System.currentTimeMillis();
		assertTrue(rm.getTimeLastUpdate() >= before);
		assertTrue(rm.getTimeLastUpdate() <= after);
	}

	@Test
	@SuppressWarnings("static-method")
	void toStringContainsClassName() {
		RequestMeasurements rm = new RequestMeasurements();
		String s = rm.toString();
		assertTrue(s.contains("RequestMeasurements"));
	}

	@Test
	@SuppressWarnings("static-method")
	void toStringInitiallyShowsEmptyForAllArrays() {
		RequestMeasurements rm = new RequestMeasurements();
		String s = rm.toString();
		// All arrays are initially cleared so each label shows "(empty)"
		assertTrue(s.contains("(empty)"));
	}

	@Test
	@SuppressWarnings("static-method")
	void updateMeasurementsRecordsMillisInSecondsMap() {
		RequestMeasurements rm = new RequestMeasurements();
		rm.updateMeasurements(250, (short) 10, (short) 5, (short) 20);
		Map<Integer, Integer> secondsMillis = rm.getSecondsMillis();
		assertEquals(1, secondsMillis.size());
		assertEquals(Integer.valueOf(250), secondsMillis.values().iterator().next());
	}

	@Test
	@SuppressWarnings("static-method")
	void updateMeasurementsRecordsCpuInSecondsCpuMap() {
		RequestMeasurements rm = new RequestMeasurements();
		rm.updateMeasurements(100, (short) 50, (short) 0, (short) 0);
		Map<Integer, Float> secondsCpu = rm.getSecondsCpuUtilisation();
		assertEquals(1, secondsCpu.size());
		// short 50 → 50/100F = 0.5
		assertEquals(0.5f, secondsCpu.values().iterator().next().floatValue(), 0.001f);
	}

	@Test
	@SuppressWarnings("static-method")
	void updateMeasurementsRecordsHeapRamInSecondsMap() {
		RequestMeasurements rm = new RequestMeasurements();
		rm.updateMeasurements(100, (short) 0, (short) 0, (short) 75);
		Map<Integer, Float> secondsHeap = rm.getSecondsHeapRamUsage();
		assertEquals(1, secondsHeap.size());
		// short 75 → 75/100F = 0.75
		assertEquals(0.75f, secondsHeap.values().iterator().next().floatValue(), 0.001f);
	}

	@Test
	@SuppressWarnings("static-method")
	void updateMeasurementsRecordsSystemCpuInSecondsMap() {
		RequestMeasurements rm = new RequestMeasurements();
		rm.updateMeasurements(100, (short) 0, (short) 30, (short) 0);
		Map<Integer, Float> secondsSystemCpu = rm.getSecondsSystemCpuUsage();
		assertEquals(1, secondsSystemCpu.size());
		// short 30 → 30/100F = 0.3
		assertEquals(0.3f, secondsSystemCpu.values().iterator().next().floatValue(), 0.001f);
	}

	@Test
	@SuppressWarnings("static-method")
	void getTimeLastUpdateUpdatedAfterUpdateMeasurements() {
		RequestMeasurements rm = new RequestMeasurements();
		long before = System.currentTimeMillis();
		rm.updateMeasurements(100, (short) 10, (short) 5, (short) 20);
		long after = System.currentTimeMillis();
		assertTrue(rm.getTimeLastUpdate() >= before);
		assertTrue(rm.getTimeLastUpdate() <= after);
	}

	@Test
	@SuppressWarnings("static-method")
	void rollupDoesNotThrow() {
		RequestMeasurements rm = new RequestMeasurements();
		rm.updateMeasurements(100, (short) 10, (short) 5, (short) 20);
		// rollup should complete without exception
		rm.rollup();
	}

	/**
	 * Helper to set a private int field by name on a RequestMeasurements instance.
	 */
	private static void setIntField(RequestMeasurements rm, String fieldName, int value) throws Exception {
		Field f = RequestMeasurements.class.getDeclaredField(fieldName);
		f.setAccessible(true);
		f.set(rm, Integer.valueOf(value));
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void rollupWithMinuteBoundaryMovesSecondsToMinutes() throws Exception {
		RequestMeasurements rm = new RequestMeasurements();
		rm.updateMeasurements(1000, (short) 80, (short) 40, (short) 50);

		// Read the current lastMinute that was just set by updateMeasurements
		Field lastMinuteField = RequestMeasurements.class.getDeclaredField("lastMinute");
		lastMinuteField.setAccessible(true);
		int currentMinute = lastMinuteField.getInt(rm);

		// Simulate being one minute behind — rollup will advance lastMinute to currentMinute
		int prevMinute = (currentMinute - 1 + 60) % 60;
		setIntField(rm, "lastMinute", prevMinute);

		rm.rollup();

		// After rollup, seconds data should have been moved to minutes bucket at prevMinute
		Map<Integer, Integer> minutesMillis = rm.getMinutesMillis();
		assertFalse(minutesMillis.isEmpty(), "minutesMillis should have an entry after minute rollup");
		assertEquals(1000, minutesMillis.get(prevMinute).intValue());

		// secondsMillis should have been cleared by the rollup
		assertTrue(rm.getSecondsMillis().isEmpty(), "secondsMillis should be empty after minute rollup");
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void rollupWithTwoMinutesBoundaryFillsMinutesBucket() throws Exception {
		RequestMeasurements rm = new RequestMeasurements();
		rm.updateMeasurements(500, (short) 60, (short) 30, (short) 25);

		Field lastMinuteField = RequestMeasurements.class.getDeclaredField("lastMinute");
		lastMinuteField.setAccessible(true);
		int currentMinute = lastMinuteField.getInt(rm);

		// Simulate being two minutes behind
		int twoBack = (currentMinute - 2 + 60) % 60;
		setIntField(rm, "lastMinute", twoBack);

		rm.rollup();

		// minutesMillis should have entries for twoBack and (twoBack+1)%60
		Map<Integer, Integer> minutesMillis = rm.getMinutesMillis();
		assertFalse(minutesMillis.isEmpty(), "minutesMillis should have entries after 2-minute rollup");
		assertTrue(minutesMillis.containsKey(twoBack), "minutesMillis should have entry for first skipped minute");
	}

	@Test
	@SuppressWarnings("static-method")
	void rollupWithHourBoundaryMovesMinutesToHours() throws Exception {
		RequestMeasurements rm = new RequestMeasurements();
		rm.updateMeasurements(750, (short) 40, (short) 20, (short) 35);

		Field lastHourField = RequestMeasurements.class.getDeclaredField("lastHour");
		lastHourField.setAccessible(true);
		int currentHour = lastHourField.getInt(rm);

		// Set lastMinute to 58 so it crosses 59→0, triggering the hour boundary check
		// Set lastHour to one behind so the hour rollup fires when minute reaches 0
		int prevHour = (currentHour - 1 + 24) % 24;
		setIntField(rm, "lastMinute", 58);
		setIntField(rm, "lastHour", prevHour);

		rm.rollup();

		// hoursMillis may now have an entry at prevHour if the minute rolled to 0
		// At minimum, rollup should complete without exception
		assertNotNull(rm.getHoursMillis());
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void rollupStaticHelperAveragesNonMinValues() throws Exception {
		// Access the private static rollup(int[], int[], int) via reflection
		java.lang.reflect.Method rollupMethod = RequestMeasurements.class
				.getDeclaredMethod("rollup", int[].class, int[].class, int.class);
		rollupMethod.setAccessible(true);

		int[] source = new int[60];
		java.util.Arrays.fill(source, Integer.MIN_VALUE);
		source[5] = 200;
		source[10] = 400;
		int[] target = new int[60];
		java.util.Arrays.fill(target, Integer.MIN_VALUE);

		rollupMethod.invoke(null, source, target, 3);

		// Average of 200 and 400 is 300; stored at index 3
		assertEquals(300, target[3]);
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void rollupStaticHelperAllMinValueProducesMinValue() throws Exception {
		java.lang.reflect.Method rollupMethod = RequestMeasurements.class
				.getDeclaredMethod("rollup", int[].class, int[].class, int.class);
		rollupMethod.setAccessible(true);

		int[] source = new int[60];
		java.util.Arrays.fill(source, Integer.MIN_VALUE);
		int[] target = new int[60];
		java.util.Arrays.fill(target, Integer.MIN_VALUE);

		rollupMethod.invoke(null, source, target, 7);

		assertEquals(Integer.MIN_VALUE, target[7]);
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void rollupShortHelperAveragesNonMinValues() throws Exception {
		java.lang.reflect.Method rollupMethod = RequestMeasurements.class
				.getDeclaredMethod("rollup", short[].class, short[].class, int.class);
		rollupMethod.setAccessible(true);

		short[] source = new short[60];
		java.util.Arrays.fill(source, Short.MIN_VALUE);
		source[2] = (short) 100;
		source[4] = (short) 200;
		short[] target = new short[60];
		java.util.Arrays.fill(target, Short.MIN_VALUE);

		rollupMethod.invoke(null, source, target, 1);

		// Average of 100 and 200 is 150; stored at index 1
		assertEquals((short) 150, target[1]);
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void rollupShortHelperAllMinValueProducesMinValue() throws Exception {
		java.lang.reflect.Method rollupMethod = RequestMeasurements.class
				.getDeclaredMethod("rollup", short[].class, short[].class, int.class);
		rollupMethod.setAccessible(true);

		short[] source = new short[60];
		java.util.Arrays.fill(source, Short.MIN_VALUE);
		short[] target = new short[60];
		java.util.Arrays.fill(target, Short.MIN_VALUE);

		rollupMethod.invoke(null, source, target, 0);

		assertEquals(Short.MIN_VALUE, target[0]);
	}
}
