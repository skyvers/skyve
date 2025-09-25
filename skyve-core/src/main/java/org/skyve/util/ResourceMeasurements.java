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
	private short[] secondsCPULoad = new short[60];
	private short[] minutesCPULoad = new short[60];
	private short[] hoursCPULoad = new short[24];
	private short[] daysCPULoad = new short[7];
	private short[] weeksCPULoad = new short[52];

	// Parallel arrays of RAM percentage used
	private double[] secondsRAMUsage = new double[60];
	private double[] minutesRAMUsage = new double[60];
	private double[] hoursRAMUsage = new double[24];
	private double[] daysRAMUsage = new double[7];
	private double[] weeksRAMUsage = new double[52];

	// internal last indices
	private int lastSecond = Integer.MIN_VALUE;
	private int lastMinute = Integer.MIN_VALUE;
	private int lastHour = Integer.MIN_VALUE;
	private int lastDay = Integer.MIN_VALUE;
	private int lastWeek = Integer.MIN_VALUE;

	public Map<Integer, Float> getSecondsCPUCoresUsage() {
		return getMap(secondsCPULoad);
	}

	public Map<Integer, Float> getMinutesCPUCoresUsage() {
		return getMap(minutesCPULoad);
	}

	public Map<Integer, Float> getHoursCPUCoresUsage() {
		return getMap(hoursCPULoad);
	}

	public Map<Integer, Float> getDaysCPUCoresUsage() {
		return getMap(daysCPULoad);
	}

	public Map<Integer, Float> getWeeksCPUCoresUsage() {
		return getMap(weeksCPULoad);
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
	private static Map<Integer, Float> getMap(double[] array) {
		TreeMap<Integer, Float> result = new TreeMap<>();
		for (int i = 0, l = array.length; i < l; i++) {
			double value = array[i];
			if (value > 0) {
				result.put(Integer.valueOf(i), Float.valueOf((float) (value / 100F)));
			}
		}
		
		return result;
	}

	public Map<Integer, Float> getSecondsRAMPercentage() {
		return getMap(secondsRAMUsage);
	}

	public Map<Integer, Float> getMinutesRAMPercentage() {
		return getMap(minutesRAMUsage);
	}

	public Map<Integer, Float> getHoursRAMPercentage() {
		return getMap(hoursRAMUsage);
	}

	public Map<Integer, Float> getDaysRAMPercentage() {
		return getMap(daysRAMUsage);
	}

	public Map<Integer, Float> getWeeksRAMPercentage() {
		return getMap(weeksRAMUsage);
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
	 * @param sysLoad the avergae system load to record
	 * @param memPctPre the RAM usage to record (percentage used)
	 */
	public synchronized void updateMeasurements(LocalDateTime currentDateTime, double sysLoad, double memPctPre) {
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
			rollup(secondsCPULoad, minutesCPULoad, lastMinute);
			rollup(secondsRAMUsage, minutesRAMUsage, lastMinute);
			clear(secondsCPULoad, secondsRAMUsage);

			lastMinute = (lastMinute + 1) % 60;

			if (lastMinute == 0) {
				rollup(minutesCPULoad, hoursCPULoad, lastHour);
				rollup(minutesRAMUsage, hoursRAMUsage, lastHour);
				clear(minutesCPULoad, minutesRAMUsage);

				lastHour = (lastHour + 1) % 24;

				if (lastHour == 0) {
					rollup(hoursCPULoad, daysCPULoad, lastDay);
					rollup(hoursRAMUsage, daysRAMUsage, lastDay);
					clear(hoursCPULoad, hoursRAMUsage);

					lastDay = (lastDay + 1) % 7;

					if (lastDay == 0) {
						rollup(daysCPULoad, weeksCPULoad, lastWeek);
						rollup(daysRAMUsage, weeksRAMUsage, lastWeek);
						clear(daysCPULoad, daysRAMUsage);

						lastWeek = (lastWeek + 1) % 52;
					}
				}
			}
		}

		// record current second
		secondsCPULoad[second] = (short) (sysLoad * 100.0);
		secondsRAMUsage[second] = memPctPre;

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
	
	private static void rollup(double[] source, double[] target, int targetIndex) {
		int sum = 0;
		int count = 0;
		for (double v : source) {
			if (v > 0) {
				sum += v;
				count++;
			}
		}
		target[targetIndex] = (count > 0) ? (short) (sum / count) : 0;
	}

	private static void clear(short[] cpu, double[] ram) {
		Arrays.fill(cpu, (short) 0);
		Arrays.fill(ram, 0);
	}

	@Override
	public synchronized String toString() {
		StringBuilder result = new StringBuilder(256);
		result.append("ResourceMeasurements:\n");

		prettyPrint(result, "Seconds CPU", secondsCPULoad);
		prettyPrint(result, "Minutes CPU", minutesCPULoad);
		prettyPrint(result, "Hours   CPU", hoursCPULoad);
		prettyPrint(result, "Days    CPU", daysCPULoad);
		prettyPrint(result, "Weeks   CPU", weeksCPULoad);

		prettyPrint(result, "Seconds RAM", secondsRAMUsage);
		prettyPrint(result, "Minutes RAM", minutesRAMUsage);
		prettyPrint(result, "Hours   RAM", hoursRAMUsage);
		prettyPrint(result, "Days    RAM", daysRAMUsage);
		prettyPrint(result, "Weeks   RAM", weeksRAMUsage);

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

	private static void prettyPrint(StringBuilder sb, String label, double[] values) {
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
