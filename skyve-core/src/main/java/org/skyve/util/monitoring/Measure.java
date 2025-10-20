package org.skyve.util.monitoring;

public class Measure {
	long startMillis;
	long startThreadCpuMillis;
	float startCpu;
	float startMem;

	long endMillis;
	long endThreadCpuMillis;
	float endCpu;
	float endMem;
	
	RequestKey key;
	long keyStartMillis;
	long keyStartThreadCpuMillis;
	float keyStartCpu;
	float keyStartMem;

	int millis;
	float threadCpuUtilisation;
	float memUsage;
	float cpuUsage;

	public int getMillis() {
		return millis;
	}
	public float getStartMem() {
		return startMem;
	}
	public float getEndMem() {
		return endMem;
	}
	public float getStartCpu() {
		return startCpu;
	}
	public float getEndCpu() {
		return endCpu;
	}
	public float getMemUsage() {
		return memUsage;
	}
	public float getCpuUsage() {
		return cpuUsage;
	}
}
