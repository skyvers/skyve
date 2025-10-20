package org.skyve.util.monitoring;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.time.temporal.ChronoField;
import java.util.Arrays;
import java.util.Map;
import java.util.TreeMap;

/**
 * ResourceMeasurements collects and aggregates system resource usage metrics
 * (CPU and RAM Usage) across multiple rolling time windows.
 * <p>
 * It maintains fixed-length arrays representing measurements for:
 * <ul>
 * <li>60 seconds</li>
 * <li>60 minutes</li>
 * <li>24 hours</li>
 * <li>7 days</li>
 * <li>52 weeks</li>
 * </ul>
 *
 * As time advances, finer-grained measurements are rolled up into
 * coarser-grained buckets (seconds → minutes → hours → days → weeks). Each
 * roll-up stores the average of the non-zero values in the coarser bucket.
 * <p>
 * Thread-safe: both updates and string rendering are synchronized.
 * 
 * This class uses parallel arrays of values to minimise memory usage.
 */
public class ResourceMeasurements implements Serializable {
	private static final long serialVersionUID = -5470389789945833954L;

	// Parallel arrays of CPU load
	private short[] secondsSystemCpuUsage = new short[60];
	private short[] minutesSystemCpuUsage = new short[60];
	private short[] hoursSystemCpuUsage = new short[24];
	private short[] daysSystemCpuUsage = new short[7];
	private short[] weeksSystemCpuUsage = new short[52];

	// Parallel arrays of RAM percentage used
	private short[] secondsHeapRamUsage = new short[60];
	private short[] minutesHeapRamUsage = new short[60];
	private short[] hoursHeapRamUsage = new short[24];
	private short[] daysHeapRamUsage = new short[7];
	private short[] weeksHeapRamUsage = new short[52];

	// internal last indices
	private int lastSecond = Integer.MIN_VALUE;
	private int lastMinute = Integer.MIN_VALUE;
	private int lastHour = Integer.MIN_VALUE;
	private int lastDay = Integer.MIN_VALUE;
	private int lastWeek = Integer.MIN_VALUE;

	public Map<Integer, Float> getSecondsSystemCpuUsageUsage() {
		return getMap(secondsSystemCpuUsage);
	}

	public Map<Integer, Float> getMinutesSystemCpuUsageUsage() {
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

	/**
	 * Update the measurement arrays with a new resource data point.
	 * <p>
	 * Rolls forward any skipped minutes, hours, days, or weeks and performs
	 * roll-ups into coarser-grained arrays. Each roll-up stores the average of all
	 * non-zero values in the finer-grained array and clears the finer-grained array.
	 *
	 * @param currentDateTime the current timestamp (used to determine array indices
	 *        and when roll-ups should occur)
	 * @param percentageSystemLoad the average system load to record (percentage of all cores)
	 * @param percentageUsedMemory the Heap RAM usage to record (percentage used)
	 */
	public synchronized void updateMeasurements(LocalDateTime currentDateTime, float percentageSystemLoad, float percentageUsedMemory) {
		int second = currentDateTime.getSecond();
		int minute = currentDateTime.getMinute();
		int hour = currentDateTime.getHour();
		int day = currentDateTime.getDayOfWeek().getValue() - 1; // 0–6
		int week = currentDateTime.get(ChronoField.ALIGNED_WEEK_OF_YEAR) - 1; // 0–51

		if (lastSecond < 0) {
			lastSecond = second;
			lastMinute = minute;
			lastHour = hour;
			lastDay = day;
			lastWeek = week;
		}

		// roll skipped minutes/hours/days/weeks - handle all boundary crossings systematically
		while (lastMinute != minute || lastHour != hour || lastDay != day || lastWeek != week) {
			// Always roll up current minute if any boundary needs crossing
			if (lastMinute != minute || lastHour != hour || lastDay != day || lastWeek != week) {
				rollup(secondsSystemCpuUsage, minutesSystemCpuUsage, lastMinute);
				rollup(secondsHeapRamUsage, minutesHeapRamUsage, lastMinute);
				clear(secondsSystemCpuUsage, secondsHeapRamUsage);
				
				lastMinute = (lastMinute + 1) % 60; // 59 + 1 == 0
			}

			// Hour boundary crossing
			if (lastMinute == 0 && (lastHour != hour || lastDay != day || lastWeek != week)) {
				rollup(minutesSystemCpuUsage, hoursSystemCpuUsage, lastHour);
				rollup(minutesHeapRamUsage, hoursHeapRamUsage, lastHour);
				clear(minutesSystemCpuUsage, minutesHeapRamUsage);

				lastHour = (lastHour + 1) % 24; // 23 + 1 == 0
			}

			// Day boundary crossing
			if (lastHour == 0 && lastMinute == 0 && (lastDay != day || lastWeek != week)) {
				rollup(hoursSystemCpuUsage, daysSystemCpuUsage, lastDay);
				rollup(hoursHeapRamUsage, daysHeapRamUsage, lastDay);
				clear(hoursSystemCpuUsage, hoursHeapRamUsage);

				lastDay = (lastDay + 1) % 7; // 6 + 1 == 0
			}

			// Week boundary crossing
			if (lastDay == 0 && lastHour == 0 && lastMinute == 0 && lastWeek != week) {
				rollup(daysSystemCpuUsage, weeksSystemCpuUsage, lastWeek);
				rollup(daysHeapRamUsage, weeksHeapRamUsage, lastWeek);
				clear(daysHeapRamUsage, daysHeapRamUsage);

				lastWeek = (lastWeek + 1) % 52; // 51 + 1 == 0
			}
		}

		// record current second
		secondsSystemCpuUsage[second] = (short) (percentageSystemLoad * 100F);
		secondsHeapRamUsage[second] = (short) (percentageUsedMemory * 100F);

		lastSecond = second;
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
	
	private static void clear(short[] cpu, short[] ram) {
		Arrays.fill(cpu, (short) 0);
		Arrays.fill(ram, (short) 0);
	}

	@Override
	public synchronized String toString() {
		StringBuilder result = new StringBuilder(256);
		result.append("ResourceMeasurements:\n");

		prettyPrint(result, "Seconds CPU Usage", secondsSystemCpuUsage);
		prettyPrint(result, "Minutes CPU Usage", minutesSystemCpuUsage);
		prettyPrint(result, "Hours   CPU Usage", hoursSystemCpuUsage);
		prettyPrint(result, "Days    CPU Usage", daysSystemCpuUsage);
		prettyPrint(result, "Weeks   CPU Usage", weeksSystemCpuUsage);

		prettyPrint(result, "Seconds RAM Usage", secondsHeapRamUsage);
		prettyPrint(result, "Minutes RAM Usage", minutesHeapRamUsage);
		prettyPrint(result, "Hours   RAM Usage", hoursHeapRamUsage);
		prettyPrint(result, "Days    RAM Usage", daysHeapRamUsage);
		prettyPrint(result, "Weeks   RAM Usage", weeksHeapRamUsage);

		return result.toString();
	}

	private static void prettyPrint(StringBuilder sb, String label, short[] values) {
		sb.append(label).append(": ");
		boolean any = false;
		for (int i = 0; i < values.length; i++) {
			if (values[i] != 0f) {
				sb.append("[").append(i).append("=").append(String.format("%.2f", Float.valueOf(values[i] / 100.0F))).append("] ");
				any = true;
			}
		}
		if (! any) {
			sb.append("(empty)");
		}
		sb.append("\n");
	}
}
