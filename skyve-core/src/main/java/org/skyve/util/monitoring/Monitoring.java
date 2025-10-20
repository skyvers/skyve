package org.skyve.util.monitoring;

import java.lang.management.ManagementFactory;
import java.lang.management.OperatingSystemMXBean;
import java.lang.management.ThreadMXBean;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * System monitoring functions.
 */
public class Monitoring {
	private static final Logger LOGGER = LoggerFactory.getLogger(Monitoring.class);
	private static final float MiB = 1024 * 1024.0F;

	private Monitoring() {
		// prevent instantiation
	}
	
	// Holds measurements for each thread
	private static final ThreadLocal<Measure> measurements = new ThreadLocal<>();

	// Determine CPU Time and MEM before
	public static void start() {
		Measure measure = measurements.get();
		if (measure == null) {
			measure = new Measure();
			measurements.set(measure);
		}
		measure.startMillis = System.currentTimeMillis();
		measure.keyStartMillis = measure.startMillis;
		measure.startThreadCpuMillis = currentThreadCpuTime();
		measure.keyStartThreadCpuMillis = measure.startThreadCpuMillis;
		measure.startCpu = percentageSystemLoad();
		measure.keyStartCpu = measure.startCpu;
		measure.startMem = percentageUsedMemory();
		measure.keyStartMem = measure.startMem;
	}

	public static void measure(@Nonnull RequestKey key) {
		Measure measure = measurements.get();
		if (measure == null) {
			LOGGER.warn("Monitoring.measure() called out of sequence from");
			new Exception().printStackTrace();
			start();
			measure = measurements.get();
		}
		
		long endMillis = System.currentTimeMillis();
		int millis = (int) (endMillis - measure.keyStartMillis);
		
		long endThreadCpuMillis = currentThreadCpuTime();
		// 10,000 = 1,000,000 (nanos to millis) / 100%
		float cpuUtilisation = (endThreadCpuMillis - measure.keyStartThreadCpuMillis) / 10_000F / millis;

		float endCpu = percentageSystemLoad();
		float cpuUsage = endCpu - measure.keyStartCpu;

		float endMem = percentageUsedMemory();
		float memUsage = endMem - measure.keyStartMem;

		RequestMeasurements rm = REQUEST_MEASUREMENTS.computeIfAbsent(key.toString(), k -> new RequestMeasurements());
		LocalDateTime currentDateTime = LocalDateTime.now();
		rm.updateMeasurements(currentDateTime, millis, (short) (cpuUtilisation * 100F), (short) (cpuUsage * 100F), (short) (memUsage * 100F));
		
		measure.keyStartMillis = endMillis;
		measure.keyStartThreadCpuMillis = endThreadCpuMillis;
		measure.keyStartCpu = endCpu;
		measure.keyStartMem = endMem;
	}
	
	public static @Nonnull Measure end() {
		try {
			Measure result = measurements.get();
			if (result == null) {
				LOGGER.warn("Monitoring.end() called out of sequence from");
				new Exception().printStackTrace();
				start();
				result = measurements.get();
			}
			else {
				result.endMillis = System.currentTimeMillis();
				result.endThreadCpuMillis = currentThreadCpuTime();
				result.endCpu = percentageSystemLoad();
				result.endMem = percentageUsedMemory();

				result.millis = (int) (result.endMillis - result.startMillis);
				// 10,000 = 1,000,000 (nanos to millis) / 100%
				result.threadCpuUtilisation = (result.endThreadCpuMillis - result.startThreadCpuMillis) / 10_000F / result.millis;
				result.cpuUsage = result.endCpu - result.startCpu;
				result.memUsage = result.endMem - result.startMem;

				LocalDateTime currentDateTime = LocalDateTime.now();
				
				RESOURCE_MEASUREMENTS.updateMeasurements(currentDateTime, result.endCpu, result.endMem);
			}
			return result;
		}
		finally {
			measurements.remove();
		}
	}
	
	public static long currentThreadCpuTime() {
		ThreadMXBean t = ManagementFactory.getThreadMXBean();
		return t.getCurrentThreadCpuTime();
	}

	public static float percentageSystemLoad() {
		OperatingSystemMXBean os = ManagementFactory.getOperatingSystemMXBean();
		return (float) (os.getSystemLoadAverage() / os.getAvailableProcessors()) * 100F;
	}

	public static float totalMemoryInMiB() {
		Runtime runtime = Runtime.getRuntime();
		return runtime.totalMemory() / MiB;
	}

	public static float freeMemoryInMiB() {
		Runtime runtime = Runtime.getRuntime();
		return runtime.freeMemory() / MiB;
	}

	public static float maxMemoryInMiB() {
		Runtime runtime = Runtime.getRuntime();
		return runtime.maxMemory() / MiB;
	}

	public static float percentageUsedMemory() {
		Runtime runtime = Runtime.getRuntime();
		long total = runtime.totalMemory();
		long free = runtime.freeMemory();
		return (float) (((total - free) / (double) total) * 100d);
	}

	private static final ConcurrentHashMap<String, RequestMeasurements> REQUEST_MEASUREMENTS = new ConcurrentHashMap<>();
	private static final ResourceMeasurements RESOURCE_MEASUREMENTS = new ResourceMeasurements();

	// Reference start time for all measurements (in milliseconds since epoch)
	private static final long MONITORING_START_TIME = System.currentTimeMillis();

	public static @Nonnull ResourceMeasurements getResourceMeasurements() {
		return RESOURCE_MEASUREMENTS;
	}

	/**
	 * Gets the monitoring start time in milliseconds since epoch.
	 * This can be used as the reference time for charting relative timestamps.
	 * 
	 * @return the start time when monitoring began collecting data
	 */
	public static long getMonitoringStartTime() {
		return MONITORING_START_TIME;
	}

	public static @Nonnull Set<String> getRequestKeyCodes() {
		return REQUEST_MEASUREMENTS.keySet();
	}

	public static @Nullable RequestMeasurements getRequestMeasurements(String requestKeyCode) {
		return REQUEST_MEASUREMENTS.get(requestKeyCode);
	}

	public static @Nonnull List<DomainValue> toDomainValues() {
		Set<String> keys = REQUEST_MEASUREMENTS.keySet();
		List<DomainValue> result = new ArrayList<>(keys.size());
		for (String key : keys) {
			result.add(RequestKey.fromString(key).toDomainValue());
		}

		return result;
	}
}
