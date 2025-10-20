package org.skyve.util.monitoring;

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
 * <li>Request CPU Usage per thread (percentage)</li>
 * <li>Shared Heap RAM usage (percentage)</li>
 * <li>System Load CPU usage (percentage)</li>
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
	
	// When these measurements were last updated
	private long timeLastUpdate = System.currentTimeMillis();

	// Parallel arrays of elapsed request time in millis
	private int[] secondsMillis = new int[60];
	private int[] minutesMillis = new int[60];
	private int[] hoursMillis = new int[24];
	private int[] daysMillis = new int[7];
	private int[] weeksMillis = new int[52];

	// Parallel arrays of Thread CPU percentage utilisation
	private short[] secondsCpuUtilisation = new short[60];
	private short[] minutesCpuUtilisation = new short[60];
	private short[] hoursCpuUtilisation = new short[24];
	private short[] daysCpuUtilisation = new short[7];
	private short[] weeksCpuUtilisation = new short[52];

	// Parallel arrays of JVM RAM percentage used
	private short[] secondsHeapRamUsage = new short[60];
	private short[] minutesHeapRamUsage = new short[60];
	private short[] hoursHeapRamUsage = new short[24];
	private short[] daysHeapRamUsage = new short[7];
	private short[] weeksHeapRamUsage = new short[52];

	// Parallel arrays of System CPU percentage used
	private short[] secondsSystemCpuUsage = new short[60];
	private short[] minutesSystemCpuUsage = new short[60];
	private short[] hoursSystemCpuUsage = new short[24];
	private short[] daysSystemCpuUsage = new short[7];
	private short[] weeksSystemCpuUsage = new short[52];

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

	public Map<Integer, Float> getSecondsCpuUtilisation() {
		return getMap(secondsCpuUtilisation);
	}

	public Map<Integer, Float> getMinutesCpuUtilisation() {
		return getMap(minutesCpuUtilisation);
	}

	public Map<Integer, Float> getHoursCpuUtilisation() {
		return getMap(hoursCpuUtilisation);
	}

	public Map<Integer, Float> getDaysCpuUtilisation() {
		return getMap(daysCpuUtilisation);
	}

	public Map<Integer, Float> getWeeksCpuUtilisation() {
		return getMap(weeksCpuUtilisation);
	}

	public Map<Integer, Float> getSecondsHeapRamUsage() {
		return getMap(secondsHeapRamUsage);
	}

	public Map<Integer, Float> getMinutesHeapRamUsage() {
		return getMap(minutesHeapRamUsage);
	}

	public Map<Integer, Float> getHoursHeapRamUsage() {
		return getMap(hoursHeapRamUsage);
	}

	public Map<Integer, Float> getDaysHeapRamUsage() {
		return getMap(daysHeapRamUsage);
	}

	public Map<Integer, Float> getWeeksHeapRamUsage() {
		return getMap(weeksHeapRamUsage);
	}

	public Map<Integer, Float> getSecondsSystemCpuUsage() {
		return getMap(secondsSystemCpuUsage);
	}

	public Map<Integer, Float> getMinutesSystemCpuUsage() {
		return getMap(minutesSystemCpuUsage);
	}

	public Map<Integer, Float> getHoursSystemCpuUsage() {
		return getMap(hoursSystemCpuUsage);
	}

	public Map<Integer, Float> getDaysSystemCpuUsage() {
		return getMap(daysSystemCpuUsage);
	}

	public Map<Integer, Float> getWeeksSystemCpuUsage() {
		return getMap(weeksSystemCpuUsage);
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
	 * @param cpuUtilisation  the percentage of CPU time during the request for the thread
	 * @param systemCpuUsage  the CPU usage delta for this request (percentage used)
	 * @param heapRamUsage    the RAM usage delta for this request (percentage used)
	 */
	public synchronized void updateMeasurements(LocalDateTime currentDateTime,
													int millis,
													short cpuUtilisation,
													short systemCpuUsage,
													short heapRamUsage) {
		timeLastUpdate = System.currentTimeMillis();
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
		// Handle ALL boundary crossings, not just minutes
		while (lastMinute != minute || lastHour != hour || lastDay != day || lastWeek != week) {
			// Roll up current minute if we need to advance any time unit
			if (lastMinute != minute || lastHour != hour || lastDay != day || lastWeek != week) {
				rollup(secondsMillis, minutesMillis, lastMinute);
				rollup(secondsCpuUtilisation, minutesCpuUtilisation, lastMinute);
				rollup(secondsSystemCpuUsage, minutesSystemCpuUsage, lastMinute);
				rollup(secondsHeapRamUsage, minutesHeapRamUsage, lastMinute);
				clear(secondsMillis, secondsCpuUtilisation, secondsSystemCpuUsage, secondsHeapRamUsage);
				
				lastMinute = (lastMinute + 1) % 60; // 59 + 1 == 0
			}
			
			// Hour boundary crossing
			if (lastMinute == 0 && (lastHour != hour || lastDay != day || lastWeek != week)) {
				rollup(minutesMillis, hoursMillis, lastHour);
				rollup(minutesCpuUtilisation, hoursCpuUtilisation, lastHour);
				rollup(minutesSystemCpuUsage, hoursSystemCpuUsage, lastHour);
				rollup(minutesHeapRamUsage, hoursHeapRamUsage, lastHour);
				clear(minutesMillis, minutesCpuUtilisation, minutesSystemCpuUsage, minutesHeapRamUsage);
				
				lastHour = (lastHour + 1) % 24; // 23 + 1 == 0
			}
			
			// Day boundary crossing  
			if (lastHour == 0 && (lastDay != day || lastWeek != week)) {
				rollup(hoursMillis, daysMillis, lastDay);
				rollup(hoursCpuUtilisation, daysCpuUtilisation, lastDay);
				rollup(hoursSystemCpuUsage, daysSystemCpuUsage, lastDay);
				rollup(hoursHeapRamUsage, daysHeapRamUsage, lastDay);
				clear(hoursMillis, hoursCpuUtilisation, hoursSystemCpuUsage, hoursHeapRamUsage);
				
				lastDay = (lastDay + 1) % 7; // 6 + 1 == 0
			}
			
			// Week boundary crossing
			if (lastDay == 0 && lastWeek != week) {
				rollup(daysMillis, weeksMillis, lastWeek);
				rollup(daysCpuUtilisation, weeksCpuUtilisation, lastWeek);
				rollup(daysSystemCpuUsage, weeksSystemCpuUsage, lastWeek);
				rollup(daysHeapRamUsage, weeksHeapRamUsage, lastWeek);
				clear(daysMillis, daysCpuUtilisation, daysSystemCpuUsage, daysHeapRamUsage);
				
				lastWeek = (lastWeek + 1) % 52;  // 51 + 1 == 0
			}
		}

		// --- record current second ---
		secondsMillis[second] = millis;
		secondsCpuUtilisation[second] = cpuUtilisation;
		secondsSystemCpuUsage[second] = systemCpuUsage;
		secondsHeapRamUsage[second] = heapRamUsage;

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
	
	private static void clear(int[] millis, short[] cpu, short[] systemCpu, short[] heapRam) {
		Arrays.fill(millis, 0);
		Arrays.fill(cpu, (short)  0);
		Arrays.fill(systemCpu, (short)  0);
		Arrays.fill(heapRam, (short) 0);
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
		prettyPrint(result, "Seconds CPU", secondsCpuUtilisation);
		prettyPrint(result, "Minutes CPU", minutesCpuUtilisation);
		prettyPrint(result, "Hours   CPU", hoursCpuUtilisation);
		prettyPrint(result, "Days    CPU", daysCpuUtilisation);
		prettyPrint(result, "Weeks   CPU", weeksCpuUtilisation);

		// cpu
		prettyPrint(result, "Seconds System CPU", secondsSystemCpuUsage);
		prettyPrint(result, "Minutes System CPU", minutesSystemCpuUsage);
		prettyPrint(result, "Hours   System CPU", hoursSystemCpuUsage);
		prettyPrint(result, "Days    System CPU", daysSystemCpuUsage);
		prettyPrint(result, "Weeks   System CPU", weeksSystemCpuUsage);

		// ram
		prettyPrint(result, "Seconds Heap RAM", secondsHeapRamUsage);
		prettyPrint(result, "Minutes Heap RAM", minutesHeapRamUsage);
		prettyPrint(result, "Hours   Heap RAM", hoursHeapRamUsage);
		prettyPrint(result, "Days    Heap RAM", daysHeapRamUsage);
		prettyPrint(result, "Weeks   Heap RAM", weeksHeapRamUsage);

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

	public long getTimeLastUpdate() {
		return timeLastUpdate;
	}
}