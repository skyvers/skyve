package org.skyve.util;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.time.temporal.ChronoField;
import java.util.Arrays;
import java.util.Map;
import java.util.TreeMap;

/**
 * RequestMeasurements collects and aggregates performance metrics for requests
 * over multiple time windows (seconds, minutes, hours, days, and weeks).
 * <p>
 * It tracks:
 * <ul>
 * <li>Elapsed request times (milliseconds)</li>
 * <li>CPU Time deltas (floating point values)</li>
 * <li>RAM usage deltas (percentages)</li>
 * </ul>
 *
 * Each metric is stored in fixed-length parallel arrays that represent rolling
 * windows of different granularities:
 * <ul>
 * <li>60 seconds</li>
 * <li>60 minutes</li>
 * <li>24 hours</li>
 * <li>7 days</li>
 * <li>52 weeks</li>
 * </ul>
 *
 * As time progresses, values are rolled up from finer-grained arrays to
 * coarser-grained arrays (e.g. seconds → minutes → hours → days → weeks).
 * Averages are stored in the coarser buckets.
 * <p>
 * This class is thread-safe: updates and string formatting are synchronized.
 * 
 * This class uses parallel arrays of values to minimise memory usage.
 */
public class RequestMeasurements implements Serializable {
	private static final long serialVersionUID = -595014460654883350L;

	// Parallel arrays of elapsed request time in millis
	private int[] secondsMillis = new int[60];
	private int[] minutesMillis = new int[60];
	private int[] hoursMillis = new int[24];
	private int[] daysMillis = new int[7];
	private int[] weeksMillis = new int[52];

	// Parallel arrays of CPU time deltas
	private int[] secondsCPUDelta = new int[60];
	private int[] minutesCPUDelta = new int[60];
	private int[] hoursCPUDelta = new int[24];
	private int[] daysCPUDelta = new int[7];
	private int[] weeksCPUDelta = new int[52];

	// Parallel arrays of RAM percentage used deltas
	private short[] secondsRAMDelta = new short[60];
	private short[] minutesRAMDelta = new short[60];
	private short[] hoursRAMDelta = new short[24];
	private short[] daysRAMDelta = new short[7];
	private short[] weeksRAMDelta = new short[52];

	// internal last indices
	private int lastSecond = Integer.MIN_VALUE;
	private int lastMinute = Integer.MIN_VALUE;
	private int lastHour = Integer.MIN_VALUE;
	private int lastDay = Integer.MIN_VALUE;
	private int lastWeek = Integer.MIN_VALUE;

	public Map<Integer, Integer> getSecondsMillis() {
		return getMap(secondsMillis);
	}

	public Map<Integer, Integer> getMinutesMillis() {
		return getMap(minutesMillis);
	}

	public Map<Integer, Integer> getHoursMillis() {
		return getMap(hoursMillis);
	}

	public Map<Integer, Integer> getDaysMillis() {
		return getMap(daysMillis);
	}

	public Map<Integer, Integer> getWeeksMillis() {
		return getMap(weeksMillis);
	}

	private static Map<Integer, Integer> getMap(int[] array) {
		TreeMap<Integer, Integer> result = new TreeMap<>();
		for (int i = 0, l = array.length; i < l; i++) {
			int value = array[i];
			if (value > 0) {
				result.put(Integer.valueOf(i), Integer.valueOf(value));
			}
		}
		
		return result;
	}
	public Map<Integer, Integer> getSecondsCPUTimeDelta() {
		return getMap(secondsCPUDelta);
	}

	public Map<Integer, Integer> getMinutesCPUTimeDelta() {
		return getMap(minutesCPUDelta);
	}

	public Map<Integer, Integer> getHoursCPUTimeDelta() {
		return getMap(hoursCPUDelta);
	}

	public Map<Integer, Integer> getDaysCPUTimeDelta() {
		return getMap(daysCPUDelta);
	}

	public Map<Integer, Integer> getWeeksCPUTimeDelta() {
		return getMap(weeksCPUDelta);
	}

	private static Map<Integer, Float> getMap(short[] array) {
		TreeMap<Integer, Float> result = new TreeMap<>();
		for (int i = 0, l = array.length; i < l; i++) {
			short value = array[i];
			if (value > 0) {
				result.put(Integer.valueOf(i), Float.valueOf(value / 100F));
			}
		}
		
		return result;
	}

	public Map<Integer, Float> getSecondsRAMPercentageDelta() {
		return getMap(secondsRAMDelta);
	}

	public Map<Integer, Float> getMinutesRAMPercentageDelta() {
		return getMap(minutesRAMDelta);
	}

	public Map<Integer, Float> getHoursRAMPercentageDelta() {
		return getMap(hoursRAMDelta);
	}

	public Map<Integer, Float> getDaysRAMPercentageDelta() {
		return getMap(daysRAMDelta);
	}

	public Map<Integer, Float> getWeeksRAMPercentageDelta() {
		return getMap(weeksRAMDelta);
	}

	/**
	 * Update the measurement arrays with a new data point at the specified time.
	 * <p>
	 * Rolls up values into coarser-grained arrays when minutes, hours, days, or
	 * weeks advance. For each roll-up, the average of all non-zero values in the
	 * finer array is written into the coarser array, and the finer array is
	 * cleared.
	 *
	 * @param currentDateTime the current timestamp (used to determine which index
	 *                        in the arrays should be updated and whether roll-up is
	 *                        required)
	 * @param millis          the elapsed request time in milliseconds
	 * @param cpuDelta        the CPU usage delta for this request
	 * @param ramDelta        the RAM usage delta for this request (percentage used)
	 */
	public synchronized void updateMeasurements(LocalDateTime currentDateTime, int millis, int cpuDelta, short ramDelta) {
		int second = currentDateTime.getSecond();
		int minute = currentDateTime.getMinute();
		int hour = currentDateTime.getHour();
		int day = currentDateTime.getDayOfWeek().getValue() - 1; // 0–6
		int week = currentDateTime.get(ChronoField.ALIGNED_WEEK_OF_YEAR) - 1; // 0–51

		// Initialise on first call
		if (lastSecond < 0) {
			lastSecond = second;
			lastMinute = minute;
			lastHour = hour;
			lastDay = day;
			lastWeek = week;
		}

		// roll forward skipped minutes/hours/days/weeks
		while (lastMinute != minute) {
			rollup(secondsMillis, minutesMillis, lastMinute);
			rollup(secondsCPUDelta, minutesCPUDelta, lastMinute);
			rollup(secondsRAMDelta, minutesRAMDelta, lastMinute);
			clear(secondsMillis, secondsCPUDelta, secondsRAMDelta);

			lastMinute = (lastMinute + 1) % 60;

			if (lastMinute == 0) { // hour tick
				rollup(minutesMillis, hoursMillis, lastHour);
				rollup(minutesCPUDelta, hoursCPUDelta, lastHour);
				rollup(minutesRAMDelta, hoursRAMDelta, lastHour);
				clear(minutesMillis, minutesCPUDelta, minutesRAMDelta);

				lastHour = (lastHour + 1) % 24;

				if (lastHour == 0) { // day tick
					rollup(hoursMillis, daysMillis, lastDay);
					rollup(hoursCPUDelta, daysCPUDelta, lastDay);
					rollup(hoursRAMDelta, daysRAMDelta, lastDay);
					clear(hoursMillis, hoursCPUDelta, hoursRAMDelta);

					lastDay = (lastDay + 1) % 7;

					if (lastDay == 0) { // week tick
						rollup(daysMillis, weeksMillis, lastWeek);
						rollup(daysCPUDelta, weeksCPUDelta, lastWeek);
						rollup(daysRAMDelta, weeksRAMDelta, lastWeek);
						clear(daysMillis, daysCPUDelta, daysRAMDelta);

						lastWeek = (lastWeek + 1) % 52;
					}
				}
			}
		}

		// --- record current second ---
		secondsMillis[second] = millis;
		secondsCPUDelta[second] = (short) cpuDelta;
		secondsRAMDelta[second] = ramDelta;

		lastSecond = second;
	}

	private static void rollup(int[] source, int[] target, int targetIndex) {
		int sum = 0;
		int count = 0;
		
		for (int v : source) {
			if (v != 0) {
				sum += v;
				count++;
			}
		}
		target[targetIndex] = (count > 0) ? (sum / count) : 0;
	}

	private static void rollup(short[] source, short[] target, int targetIndex) {
		int sum = 0;
		int count = 0;
		for (short v : source) {
			if (v > 0) {
				sum += v;
				count++;
			}
		}
		target[targetIndex] = (count > 0) ? (short) (sum / count) : 0;
	}
	
	private static void clear(int[] millis, int[] cpu, short[] ram) {
		Arrays.fill(millis, 0);
		Arrays.fill(cpu, 0);
		Arrays.fill(ram, (short) 0);
	}
	
	@Override
	public synchronized String toString() {
		StringBuilder result = new StringBuilder(256);
		result.append("RequestMeasurements:\n");

		// millis
		prettyPrint(result, "Seconds Millis", secondsMillis);
		prettyPrint(result, "Minutes Millis", minutesMillis);
		prettyPrint(result, "Hours   Millis", hoursMillis);
		prettyPrint(result, "Days    Millis", daysMillis);
		prettyPrint(result, "Weeks   Millis", weeksMillis);

		// cpu
		prettyPrint(result, "Seconds CPU", secondsCPUDelta);
		prettyPrint(result, "Minutes CPU", minutesCPUDelta);
		prettyPrint(result, "Hours   CPU", hoursCPUDelta);
		prettyPrint(result, "Days    CPU", daysCPUDelta);
		prettyPrint(result, "Weeks   CPU", weeksCPUDelta);

		// ram
		prettyPrint(result, "Seconds RAM", secondsRAMDelta);
		prettyPrint(result, "Minutes RAM", minutesRAMDelta);
		prettyPrint(result, "Hours   RAM", hoursRAMDelta);
		prettyPrint(result, "Days    RAM", daysRAMDelta);
		prettyPrint(result, "Weeks   RAM", weeksRAMDelta);

		return result.toString();
	}

	private static void prettyPrint(StringBuilder sb, String label, int[] values) {
		sb.append(label).append(": ");
		boolean any = false;
		for (int i = 0; i < values.length; i++) {
			if (values[i] != 0) {
				sb.append("[").append(i).append("=").append(values[i]).append("] ");
				any = true;
			}
		}
		if (!any) {
			sb.append("(empty)");
		}
		sb.append("\n");
	}

	private static void prettyPrint(StringBuilder sb, String label, short[] values) {
		sb.append(label).append(": ");
		boolean any = false;
		for (int i = 0; i < values.length; i++) {
			if (values[i] != 0f) {
				sb.append("[").append(i).append("=").append(String.format("%.2f", Float.valueOf(values[i] / 100F))).append("] ");
				any = true;
			}
		}
		if (!any) {
			sb.append("(empty)");
		}
		sb.append("\n");
	}
}