package org.skyve.util.monitoring;

/**
 * Represents a single completed request measurement snapshot.
 *
 * <p>Instances are populated by {@link org.skyve.util.monitoring.Monitoring} across a
 * {@code start() -> end()} lifecycle. The package-private fields are mutable while sampling,
 * then read through accessors after completion.
 *
 * <p>Units:
 * <ul>
 *   <li>Time values are milliseconds unless otherwise stated.</li>
 *   <li>CPU and memory values are percentages (for example {@code 12.5} means 12.5%).</li>
 * </ul>
 *
 * <p>Threading: instances are thread-confined to the request thread that produced them.
 */
public class Measure {
	long startMillis;
	long startThreadCpuNanos;
	float startCpu;
	float startMem;

	long endMillis;
	long endThreadCpuNanos;
	float endCpu;
	float endMem;
	
	RequestKey key;
	long keyStartMillis;
	long keyStartThreadCpuNanos;
	float keyStartCpu;
	float keyStartMem;

	int millis;
	float threadCpuUtilisation;
	float memUsage;
	float cpuUsage;

	/**
	 * Returns elapsed wall-clock time for the measured request.
	 *
	 * @return elapsed milliseconds between request start and end
	 */
	public int getMillis() {
		return millis;
	}

	/**
	 * Returns the JVM heap usage percentage captured at request start.
	 *
	 * @return starting heap usage percentage
	 */
	public float getStartMem() {
		return startMem;
	}

	/**
	 * Returns the JVM heap usage percentage captured at request end.
	 *
	 * @return ending heap usage percentage
	 */
	public float getEndMem() {
		return endMem;
	}

	/**
	 * Returns system CPU load percentage captured at request start.
	 *
	 * @return starting CPU load percentage
	 */
	public float getStartCpu() {
		return startCpu;
	}

	/**
	 * Returns system CPU load percentage captured at request end.
	 *
	 * @return ending CPU load percentage
	 */
	public float getEndCpu() {
		return endCpu;
	}

	/**
	 * Returns net heap usage change over the measured request.
	 *
	 * @return end minus start heap usage percentage
	 */
	public float getMemUsage() {
		return memUsage;
	}

	/**
	 * Returns net CPU load change over the measured request.
	 *
	 * @return end minus start CPU load percentage
	 */
	public float getCpuUsage() {
		return cpuUsage;
	}
}
