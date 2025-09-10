package org.skyve.util;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.time.temporal.ChronoField;
import java.util.Arrays;
import java.util.Map;
import java.util.TreeMap;

/**
 * ResourceMeasurements collects and aggregates system resource usage metrics
 * (CPU and RAM deltas) across multiple rolling time windows.
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
	private short[] secondsCPUDelta = new short[60];
	private short[] minutesCPUDelta = new short[60];
	private short[] hoursCPUDelta = new short[24];
	private short[] daysCPUDelta = new short[7];
	private short[] weeksCPUDelta = new short[52];

	// Parallel arrays of RAM percentage used
	private int[] secondsRAMDelta = new int[60];
	private int[] minutesRAMDelta = new int[60];
	private int[] hoursRAMDelta = new int[24];
	private int[] daysRAMDelta = new int[7];
	private int[] weeksRAMDelta = new int[52];

	// internal last indices
	private int lastSecond = Integer.MIN_VALUE;
	private int lastMinute = Integer.MIN_VALUE;
	private int lastHour = Integer.MIN_VALUE;
	private int lastDay = Integer.MIN_VALUE;
	private int lastWeek = Integer.MIN_VALUE;

	public Map<Integer, Float> getSecondsCPUCoresDelta() {
		return getMap(secondsCPUDelta);
	}

	public Map<Integer, Float> getMinutesCPUCoresDelta() {
		return getMap(minutesCPUDelta);
	}

	public Map<Integer, Float> getHoursCPUCoresDelta() {
		return getMap(hoursCPUDelta);
	}

	public Map<Integer, Float> getDaysCPUCoresDelta() {
		return getMap(daysCPUDelta);
	}

	public Map<Integer, Float> getWeeksCPUCoresDelta() {
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

	public Map<Integer, Integer> getSecondsRAMPercentageDelta() {
		return getMap(secondsRAMDelta);
	}

	public Map<Integer, Integer> getMinutesRAMPercentageDelta() {
		return getMap(minutesRAMDelta);
	}

	public Map<Integer, Integer> getHoursRAMPercentageDelta() {
		return getMap(hoursRAMDelta);
	}

	public Map<Integer, Integer> getDaysRAMPercentageDelta() {
		return getMap(daysRAMDelta);
	}

	public Map<Integer, Integer> getWeeksRAMPercentageDelta() {
		return getMap(weeksRAMDelta);
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

	/**
	 * Update the measurement arrays with a new resource data point.
	 * <p>
	 * Rolls forward any skipped minutes, hours, days, or weeks and performs
	 * roll-ups into coarser-grained arrays. Each roll-up stores the average of all
	 * non-zero values in the finer-grained array and clears the finer-grained array.
	 *
	 * @param currentDateTime the current timestamp (used to determine array indices
	 *                        and when roll-ups should occur)
	 * @param cpuDelta        the CPU usage delta to record
	 * @param ramDelta        the RAM usage delta to record (percentage used)
	 */
	public synchronized void updateMeasurements(LocalDateTime currentDateTime, double cpuDelta, int ramDelta) {
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

		// roll skipped minutes/hours/days/weeks
		while (lastMinute != minute) {
			rollup(secondsCPUDelta, minutesCPUDelta, lastMinute);
			rollup(secondsRAMDelta, minutesRAMDelta, lastMinute);
			clear(secondsCPUDelta, secondsRAMDelta);

			lastMinute = (lastMinute + 1) % 60;

			if (lastMinute == 0) {
				rollup(minutesCPUDelta, hoursCPUDelta, lastHour);
				rollup(minutesRAMDelta, hoursRAMDelta, lastHour);
				clear(minutesCPUDelta, minutesRAMDelta);

				lastHour = (lastHour + 1) % 24;

				if (lastHour == 0) {
					rollup(hoursCPUDelta, daysCPUDelta, lastDay);
					rollup(hoursRAMDelta, daysRAMDelta, lastDay);
					clear(hoursCPUDelta, hoursRAMDelta);

					lastDay = (lastDay + 1) % 7;

					if (lastDay == 0) {
						rollup(daysCPUDelta, weeksCPUDelta, lastWeek);
						rollup(daysRAMDelta, weeksRAMDelta, lastWeek);
						clear(daysCPUDelta, daysRAMDelta);

						lastWeek = (lastWeek + 1) % 52;
					}
				}
			}
		}

		// record current second
		secondsCPUDelta[second] = (short) (cpuDelta * 100.0);
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

	private static void clear(short[] cpu, int[] ram) {
		Arrays.fill(cpu, (short) 0);
		Arrays.fill(ram, 0);
	}

	@Override
	public synchronized String toString() {
		StringBuilder result = new StringBuilder(256);
		result.append("ResourceMeasurements:\n");

		prettyPrint(result, "Seconds CPU", secondsCPUDelta);
		prettyPrint(result, "Minutes CPU", minutesCPUDelta);
		prettyPrint(result, "Hours   CPU", hoursCPUDelta);
		prettyPrint(result, "Days    CPU", daysCPUDelta);
		prettyPrint(result, "Weeks   CPU", weeksCPUDelta);

		prettyPrint(result, "Seconds RAM", secondsRAMDelta);
		prettyPrint(result, "Minutes RAM", minutesRAMDelta);
		prettyPrint(result, "Hours   RAM", hoursRAMDelta);
		prettyPrint(result, "Days    RAM", daysRAMDelta);
		prettyPrint(result, "Weeks   RAM", weeksRAMDelta);

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
}
