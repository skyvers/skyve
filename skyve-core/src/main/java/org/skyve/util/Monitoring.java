package org.skyve.util;

import java.lang.management.ManagementFactory;
import java.lang.management.OperatingSystemMXBean;

/**
 * System monitoring functions.
 */
public class Monitoring {
	private static float MiB = 1024 * 1024.0F;
	
	private Monitoring() {
		// prevent instantiation
	}
	
	public static double systemLoadAverage() {
		OperatingSystemMXBean os = ManagementFactory.getOperatingSystemMXBean();
		return os.getSystemLoadAverage();
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
	
	public static int percentageUsedMomory() {
		Runtime runtime = Runtime.getRuntime();
		long total = runtime.totalMemory();
		long free = runtime.freeMemory();
		return (int) ((total - free) / (double) total * 100d);
	}
}
