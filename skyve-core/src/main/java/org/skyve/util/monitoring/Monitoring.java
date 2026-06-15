package org.skyve.util.monitoring;

import java.lang.management.ManagementFactory;
import java.lang.management.OperatingSystemMXBean;
import java.lang.management.ThreadMXBean;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * System monitoring functions.
 */
public class Monitoring {
	private static final Logger LOGGER = SkyveLoggerFactory.getLogger(Monitoring.class);
	private static final float MIB = 1024 * 1024.0F;

	private Monitoring() {
		// prevent instantiation
	}
	
	// Holds measurements for each thread
	private static final ThreadLocal<Measure> measurements = new ThreadLocal<>();

	// Determine CPU Time and MEM before
	/**
	 * Starts per-thread monitoring for the current request flow.
	 *
	 * <p>If monitoring has already been started on this thread, baseline values are reset.
	 */
	public static void start() {
		Measure measure = measurements.get();
		if (measure == null) {
			measure = new Measure();
			measurements.set(measure);
		}
		measure.startMillis = System.currentTimeMillis();
		measure.keyStartMillis = measure.startMillis;
		measure.startThreadCpuNanos = currentThreadCpuTime();
		measure.keyStartThreadCpuNanos = measure.startThreadCpuNanos;
		measure.startCpu = percentageSystemLoad();
		measure.keyStartCpu = measure.startCpu;
		measure.startMem = percentageUsedMemory();
		measure.keyStartMem = measure.startMem;
	}

	/**
	 * Captures a segment measurement for a logical request key since the previous
	 * {@link #start()} or {@code measure()} call on the same thread.
	 *
	 * @param key	logical request segment identifier; never {@code null}
	 */
	public static void measure(@Nonnull RequestKey key) {
		Measure measure = measurements.get();
		if (measure == null) {
			LOGGER.warn("Monitoring.measure() called out of sequence from", new Exception());
			start();
			measure = measurements.get();
		}
		
		long endMillis = System.currentTimeMillis();
		int millis = (int) (endMillis - measure.keyStartMillis);
		
		long endThreadCpuNanos = currentThreadCpuTime();
		float cpuUtilisation = 0F;
		// Only calculate CPU Utilisation if the request took more than 0 millis
		if (millis > 0) {
			// 10,000 = 1,000,000 (nanos to millis) / 100%
			cpuUtilisation = (endThreadCpuNanos - measure.keyStartThreadCpuNanos) / 10_000F / millis;
		}
		
		float endCpu = percentageSystemLoad();
		float cpuUsage = endCpu - measure.keyStartCpu;

		float endMem = percentageUsedMemory();
		float memUsage = endMem - measure.keyStartMem;

		RequestMeasurements rm = REQUEST_MEASUREMENTS.computeIfAbsent(key.toString(), k -> new RequestMeasurements());
		rm.updateMeasurements(millis, (short) (cpuUtilisation * 100F), (short) (cpuUsage * 100F), (short) (memUsage * 100F));
		
		measure.keyStartMillis = endMillis;
		measure.keyStartThreadCpuNanos = endThreadCpuNanos;
		measure.keyStartCpu = endCpu;
		measure.keyStartMem = endMem;
	}
	
	/**
	 * Finalises request monitoring for the current thread and returns the aggregate measure.
	 *
	 * <p>Side effects: updates process-level resource rolling metrics and clears the
	 * thread-local measurement context.
	 *
	 * @return completed request measure snapshot
	 */
	public static @Nonnull Measure end() {
		try {
			Measure result = measurements.get();
			if (result == null) {
				LOGGER.warn("Monitoring.end() called out of sequence from", new Exception());
				start();
				result = measurements.get();
			}
			else {
				result.endMillis = System.currentTimeMillis();
				result.endThreadCpuNanos = currentThreadCpuTime();
				result.endCpu = percentageSystemLoad();
				result.endMem = percentageUsedMemory();

				result.millis = (int) (result.endMillis - result.startMillis);
				// 10,000 = 1,000,000 (nanos to millis) / 100%
				result.threadCpuUtilisation = (result.endThreadCpuNanos - result.startThreadCpuNanos) / 10_000F / result.millis;
				result.cpuUsage = result.endCpu - result.startCpu;
				result.memUsage = result.endMem - result.startMem;

				RESOURCE_MEASUREMENTS.updateMeasurements(result.endCpu, result.endMem);
			}
			return result;
		}
		finally {
			measurements.remove();
		}
	}
	
	/**
	 * Returns CPU nanoseconds consumed by the current thread.
	 */
	public static long currentThreadCpuTime() {
		ThreadMXBean t = ManagementFactory.getThreadMXBean();
		return t.getCurrentThreadCpuTime();
	}

	/**
	 * Returns system load as a percentage of available processors.
	 */
	public static float percentageSystemLoad() {
		OperatingSystemMXBean os = ManagementFactory.getOperatingSystemMXBean();
		return (float) (os.getSystemLoadAverage() / os.getAvailableProcessors()) * 100F;
	}

	/**
	 * Returns currently allocated JVM heap in MiB.
	 */
	public static float totalMemoryInMiB() {
		Runtime runtime = Runtime.getRuntime();
		return runtime.totalMemory() / MIB;
	}

	/**
	 * Returns currently free JVM heap in MiB.
	 */
	public static float freeMemoryInMiB() {
		Runtime runtime = Runtime.getRuntime();
		return runtime.freeMemory() / MIB;
	}

	/**
	 * Returns maximum JVM heap limit in MiB.
	 */
	public static float maxMemoryInMiB() {
		Runtime runtime = Runtime.getRuntime();
		return runtime.maxMemory() / MIB;
	}

	/**
	 * Returns currently used heap as a percentage of allocated heap.
	 */
	public static float percentageUsedMemory() {
		Runtime runtime = Runtime.getRuntime();
		long total = runtime.totalMemory();
		long free = runtime.freeMemory();
		return (float) (((total - free) / (double) total) * 100d);
	}

	private static final ConcurrentHashMap<String, RequestMeasurements> REQUEST_MEASUREMENTS = new ConcurrentHashMap<>();
	private static final ResourceMeasurements RESOURCE_MEASUREMENTS = new ResourceMeasurements();

	// Reference start time for all measurements (in milliseconds since epoch)
	private static long MONITORING_START_TIME = System.currentTimeMillis();

	/**
	 * Clears all accumulated monitoring aggregates and resets the monitoring epoch.
	 */
	public static synchronized void purge() {
		MONITORING_START_TIME = System.currentTimeMillis();
		REQUEST_MEASUREMENTS.clear();
		RESOURCE_MEASUREMENTS.clear();
	}
	
	/**
	 * Returns process-level resource aggregates since the last {@link #purge()}.
	 */
	public static @Nonnull ResourceMeasurements getResourceMeasurements() {
		return RESOURCE_MEASUREMENTS;
	}

	/**
	 * Gets the monitoring start time in milliseconds since epoch.
	 * This can be used as the reference time for charting relative timestamps.
	 * 
	 * @return the start time when monitoring began collecting data
	 */
	public static synchronized long getMonitoringStartTime() {
		return MONITORING_START_TIME;
	}

	/**
	 * Returns all observed request key codes.
	 */
	public static @Nonnull Set<String> getRequestKeyCodes() {
		return REQUEST_MEASUREMENTS.keySet();
	}

	/**
	 * Returns accumulated measurements for a request key code.
	 */
	public static @Nullable RequestMeasurements getRequestMeasurements(String requestKeyCode) {
		return REQUEST_MEASUREMENTS.get(requestKeyCode);
	}

	/**
	 * Converts known request keys to domain values for UI selection lists.
	 */
	public static @Nonnull List<DomainValue> toDomainValues() {
		Set<String> keys = REQUEST_MEASUREMENTS.keySet();
		List<DomainValue> result = new ArrayList<>(keys.size());
		for (String key : keys) {
			result.add(RequestKey.fromString(key).toDomainValue());
		}

		return result;
	}
}
