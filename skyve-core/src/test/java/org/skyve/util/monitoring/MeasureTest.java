package org.skyve.util.monitoring;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

public class MeasureTest {

	@Test
	@SuppressWarnings("static-method")
	public void getMillisDefaultZero() {
		Measure m = new Measure();
		assertEquals(0, m.getMillis());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getStartCpuDefaultZero() {
		Measure m = new Measure();
		assertEquals(0.0f, m.getStartCpu(), 0.001f);
	}

	@Test
	@SuppressWarnings("static-method")
	public void getEndCpuDefaultZero() {
		Measure m = new Measure();
		assertEquals(0.0f, m.getEndCpu(), 0.001f);
	}

	@Test
	@SuppressWarnings("static-method")
	public void getStartMemDefaultZero() {
		Measure m = new Measure();
		assertEquals(0.0f, m.getStartMem(), 0.001f);
	}

	@Test
	@SuppressWarnings("static-method")
	public void getEndMemDefaultZero() {
		Measure m = new Measure();
		assertEquals(0.0f, m.getEndMem(), 0.001f);
	}

	@Test
	@SuppressWarnings("static-method")
	public void getMemUsageDefaultZero() {
		Measure m = new Measure();
		assertEquals(0.0f, m.getMemUsage(), 0.001f);
	}

	@Test
	@SuppressWarnings("static-method")
	public void getCpuUsageDefaultZero() {
		Measure m = new Measure();
		assertEquals(0.0f, m.getCpuUsage(), 0.001f);
	}

	@Test
	@SuppressWarnings("static-method")
	public void fieldAssignmentAndGetters() {
		Measure m = new Measure();
		m.millis = 42;
		m.startCpu = 0.1f;
		m.endCpu = 0.9f;
		m.startMem = 0.2f;
		m.endMem = 0.8f;
		m.memUsage = 0.6f;
		m.cpuUsage = 0.8f;
		assertEquals(42, m.getMillis());
		assertEquals(0.1f, m.getStartCpu(), 0.001f);
		assertEquals(0.9f, m.getEndCpu(), 0.001f);
		assertEquals(0.2f, m.getStartMem(), 0.001f);
		assertEquals(0.8f, m.getEndMem(), 0.001f);
		assertEquals(0.6f, m.getMemUsage(), 0.001f);
		assertEquals(0.8f, m.getCpuUsage(), 0.001f);
	}
}
