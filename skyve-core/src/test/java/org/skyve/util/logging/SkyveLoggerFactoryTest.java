package org.skyve.util.logging;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.Marker;
import org.slf4j.MarkerFactory;
import org.slf4j.spi.LocationAwareLogger;

/**
 * Verifies that {@link SkyveLoggerFactory} strips log-injection characters from
 * messages and {@link String} arguments before forwarding to the delegate logger.
 *
 * <p>A {@link RecordingLogger} acting as the delegate captures exactly what
 * {@code SkyveLoggerFactory} passes after sanitisation, without going through a
 * real logging backend.
 */
class SkyveLoggerFactoryTest {
	private static final int TRACE = LocationAwareLogger.TRACE_INT;
	private static final int DEBUG = LocationAwareLogger.DEBUG_INT;
	private static final int INFO = LocationAwareLogger.INFO_INT;
	private static final int WARN = LocationAwareLogger.WARN_INT;
	private static final int ERROR = LocationAwareLogger.ERROR_INT;

	private RecordingLogger recorder;
	private Logger logger;

	@BeforeEach
	void setUp() {
		recorder = new RecordingLogger();
		logger = SkyveLoggerFactory.wrap(recorder);
	}

	// ---- message sanitisation ----

	@Test
	void sanitisesCarriageReturnInMessage() {
		logger.info("Prefix\rForged: INFO injected-line");
		assertEquals("Prefix_Forged: INFO injected-line", recorder.lastMsg);
	}

	@Test
	void sanitisesLineFeedInMessage() {
		logger.info("Line1\nLine2");
		assertEquals("Line1_Line2", recorder.lastMsg);
	}

	@Test
	void sanitisesCrLfSequenceInMessage() {
		// Classic log-forging: append CR+LF then a fake log line
		logger.info("Head\r\nINFO  fake-component - fake-log-line");
		assertEquals("Head__INFO  fake-component - fake-log-line", recorder.lastMsg);
	}

	@Test
	void sanitisesTabInMessage() {
		logger.info("Col1\tCol2");
		assertEquals("Col1_Col2", recorder.lastMsg);
	}

	@Test
	void sanitisesNulCharInMessage() {
		logger.info("Before\u0000After");
		assertEquals("Before_After", recorder.lastMsg);
	}

	@Test
	void sanitisesDelCharInMessage() {
		logger.info("Before\u007fAfter");
		assertEquals("Before_After", recorder.lastMsg);
	}

	@Test
	void sanitisesOtherAsciiControlCharsInMessage() {
		// 0x01 (SOH) and 0x1f (US) are both in the stripped range
		logger.info("A\u0001B\u001fC");
		assertEquals("A_B_C", recorder.lastMsg);
	}

	@Test
	void doesNotModifyCleanMessage() {
		logger.info("This is a perfectly normal log message.");
		assertEquals("This is a perfectly normal log message.", recorder.lastMsg);
	}

	@Test
	void handlesNullMessage() {
		logger.info((String) null);
		assertNull(recorder.lastMsg);
	}

	// ---- argument sanitisation ----

	@Test
	void sanitisesStringArgument() {
		logger.info("User: {}", "admin\nINFO  fake-component - injected");
		assertNotNull(recorder.lastArgs);
		assertEquals("admin_INFO  fake-component - injected", recorder.lastArgs[0]);
	}

	@Test
	void sanitisesMultipleStringArguments() {
		logger.info("{} logged in from {}", "alice\r\nbob", "192.168.1.1\nforged");
		assertNotNull(recorder.lastArgs);
		assertEquals("alice__bob", recorder.lastArgs[0]);
		assertEquals("192.168.1.1_forged", recorder.lastArgs[1]);
	}

	@Test
	void passesNonStringArgumentUnmodified() {
		Integer count = Integer.valueOf(42);
		logger.info("Count: {}", count);
		assertNotNull(recorder.lastArgs);
		assertSame(count, recorder.lastArgs[0]);
	}

	@Test
	void onlyStringElementsSanitisedInMixedArgArray() {
		Integer number = Integer.valueOf(99);
		logger.info("{} {}", "inject\nme", number);
		assertNotNull(recorder.lastArgs);
		assertEquals("inject_me", recorder.lastArgs[0]);
		assertSame(number, recorder.lastArgs[1]);
	}

	// ---- throwable passthrough ----

	@Test
	void preservesThrowable() {
		RuntimeException cause = new RuntimeException("boom");
		logger.warn("Something failed", cause);
		assertSame(cause, recorder.lastThrowable);
	}

	@Test
	void sanitisesMessageAndPreservesThrowable() {
		RuntimeException cause = new RuntimeException("boom");
		logger.error("Failure\nin component", cause);
		assertEquals("Failure_in component", recorder.lastMsg);
		assertSame(cause, recorder.lastThrowable);
	}

	@Test
	@SuppressWarnings("static-method")
	void usesLocationAwareLoggerWhenDelegateSupportsCallerBoundary() {
		LocationAwareRecordingLogger locationAwareRecorder = new LocationAwareRecordingLogger();
		Logger locationAwareLogger = SkyveLoggerFactory.wrap(locationAwareRecorder);
		RuntimeException cause = new RuntimeException("boom");

		locationAwareLogger.warn("Failure\nfor {}", "user\rname", cause);

		assertEquals(LocationAwareLogger.WARN_INT, locationAwareRecorder.lastLevelInt);
		assertEquals("org.skyve.util.logging.SkyveLoggerFactory$SanitisingLogger", locationAwareRecorder.lastFqcn);
		assertEquals("Failure_for {}", locationAwareRecorder.lastMsg);
		assertNotNull(locationAwareRecorder.lastArgs);
		assertEquals(1, locationAwareRecorder.lastArgs.length);
		assertEquals("user_name", locationAwareRecorder.lastArgs[0]);
		assertSame(cause, locationAwareRecorder.lastThrowable);
	}

	@Test
	@SuppressWarnings("static-method")
	void mapsAllLevelsForLocationAwareLogger() {
		LocationAwareRecordingLogger locationAwareRecorder = new LocationAwareRecordingLogger();
		Logger locationAwareLogger = SkyveLoggerFactory.wrap(locationAwareRecorder);

		locationAwareLogger.trace("trace");
		assertEquals(LocationAwareLogger.TRACE_INT, locationAwareRecorder.lastLevelInt);

		locationAwareLogger.debug("debug");
		assertEquals(LocationAwareLogger.DEBUG_INT, locationAwareRecorder.lastLevelInt);

		locationAwareLogger.info("info");
		assertEquals(LocationAwareLogger.INFO_INT, locationAwareRecorder.lastLevelInt);

		locationAwareLogger.error("error");
		assertEquals(LocationAwareLogger.ERROR_INT, locationAwareRecorder.lastLevelInt);
	}

	@Test
	void delegatesEnabledChecks() {
		assertTrue(logger.isTraceEnabled());
		assertTrue(logger.isTraceEnabled(null));
		assertTrue(logger.isDebugEnabled());
		assertTrue(logger.isDebugEnabled(null));
		assertTrue(logger.isInfoEnabled());
		assertTrue(logger.isInfoEnabled(null));
		assertTrue(logger.isWarnEnabled());
		assertTrue(logger.isWarnEnabled(null));
		assertTrue(logger.isErrorEnabled());
		assertTrue(logger.isErrorEnabled(null));
	}

	@Test
	void passesMarkerThrough() {
		Marker marker = MarkerFactory.getMarker("audit");
		logger.info(marker, "Marker\nmessage");

		assertSame(marker, recorder.lastMarker);
		assertEquals("Marker_message", recorder.lastMsg);
	}

	// ---- all five levels sanitise ----

	@Test
	void sanitisesTraceLevel() {
		logger.trace("Trace\ninjection");
		assertEquals("Trace_injection", recorder.lastMsg);
		assertEquals(TRACE, recorder.lastLevel);
	}

	@Test
	void sanitisesDebugLevel() {
		logger.debug("Debug\ninjection");
		assertEquals("Debug_injection", recorder.lastMsg);
		assertEquals(DEBUG, recorder.lastLevel);
	}

	@Test
	void sanitisesInfoLevel() {
		logger.info("Info\ninjection");
		assertEquals("Info_injection", recorder.lastMsg);
		assertEquals(INFO, recorder.lastLevel);
	}

	@Test
	void sanitisesWarnLevel() {
		logger.warn("Warn\ninjection");
		assertEquals("Warn_injection", recorder.lastMsg);
		assertEquals(WARN, recorder.lastLevel);
	}

	@Test
	void sanitisesErrorLevel() {
		logger.error("Error\ninjection");
		assertEquals("Error_injection", recorder.lastMsg);
		assertEquals(ERROR, recorder.lastLevel);
	}

	// ---- factory methods return non-null sanitizing loggers ----

	@Test
	@SuppressWarnings("static-method")
	void getLoggerByClassReturnsNonNull() {
		assertNotNull(SkyveLoggerFactory.getLogger(SkyveLoggerFactoryTest.class));
	}

	@Test
	@SuppressWarnings("static-method")
	void getLoggerByStringReturnsNonNull() {
		assertNotNull(SkyveLoggerFactory.getLogger("test.logger.name"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getLoggerByCategoryReturnsNonNull() {
		assertNotNull(SkyveLoggerFactory.getLogger(Category.SECURITY));
	}

	// ---- recording delegate ----

	/**
	 * A minimal {@link Logger} implementation that records the last normalised
	 * logging call made to it. All levels are enabled so that {@code SkyveLoggerFactory}
	 * always forwards calls down to this recorder.
	 */
	private static class RecordingLogger implements Logger {
		String lastMsg;
		Object[] lastArgs;
		Throwable lastThrowable;
		Marker lastMarker;
		int lastLevel;

		@Override
		public String getName() {
			return "recording";
		}

		protected void capture(int level, Marker marker, String msg, Object[] arguments, Throwable throwable) {
			this.lastLevel = level;
			this.lastMarker = marker;
			this.lastMsg = msg;
			this.lastArgs = arguments;
			this.lastThrowable = throwable;
		}

		@Override public boolean isTraceEnabled() { return true; }
		@Override public boolean isTraceEnabled(Marker marker) { return true; }
		@Override public void trace(String msg) { capture(TRACE, null, msg, null, null); }
		@Override public void trace(String format, Object arg) { capture(TRACE, null, format, new Object[] {arg}, null); }
		@Override public void trace(String format, Object arg1, Object arg2) { capture(TRACE, null, format, new Object[] {arg1, arg2}, null); }
		@Override public void trace(String format, Object... arguments) { capture(TRACE, null, format, arguments, null); }
		@Override public void trace(String msg, Throwable t) { capture(TRACE, null, msg, null, t); }
		@Override public void trace(Marker marker, String msg) { capture(TRACE, marker, msg, null, null); }
		@Override public void trace(Marker marker, String format, Object arg) { capture(TRACE, marker, format, new Object[] {arg}, null); }
		@Override public void trace(Marker marker, String format, Object arg1, Object arg2) { capture(TRACE, marker, format, new Object[] {arg1, arg2}, null); }
		@Override public void trace(Marker marker, String format, Object... argArray) { capture(TRACE, marker, format, argArray, null); }
		@Override public void trace(Marker marker, String msg, Throwable t) { capture(TRACE, marker, msg, null, t); }

		@Override public boolean isDebugEnabled() { return true; }
		@Override public boolean isDebugEnabled(Marker marker) { return true; }
		@Override public void debug(String msg) { capture(DEBUG, null, msg, null, null); }
		@Override public void debug(String format, Object arg) { capture(DEBUG, null, format, new Object[] {arg}, null); }
		@Override public void debug(String format, Object arg1, Object arg2) { capture(DEBUG, null, format, new Object[] {arg1, arg2}, null); }
		@Override public void debug(String format, Object... arguments) { capture(DEBUG, null, format, arguments, null); }
		@Override public void debug(String msg, Throwable t) { capture(DEBUG, null, msg, null, t); }
		@Override public void debug(Marker marker, String msg) { capture(DEBUG, marker, msg, null, null); }
		@Override public void debug(Marker marker, String format, Object arg) { capture(DEBUG, marker, format, new Object[] {arg}, null); }
		@Override public void debug(Marker marker, String format, Object arg1, Object arg2) { capture(DEBUG, marker, format, new Object[] {arg1, arg2}, null); }
		@Override public void debug(Marker marker, String format, Object... arguments) { capture(DEBUG, marker, format, arguments, null); }
		@Override public void debug(Marker marker, String msg, Throwable t) { capture(DEBUG, marker, msg, null, t); }

		@Override public boolean isInfoEnabled() { return true; }
		@Override public boolean isInfoEnabled(Marker marker) { return true; }
		@Override public void info(String msg) { capture(INFO, null, msg, null, null); }
		@Override public void info(String format, Object arg) { capture(INFO, null, format, new Object[] {arg}, null); }
		@Override public void info(String format, Object arg1, Object arg2) { capture(INFO, null, format, new Object[] {arg1, arg2}, null); }
		@Override public void info(String format, Object... arguments) { capture(INFO, null, format, arguments, null); }
		@Override public void info(String msg, Throwable t) { capture(INFO, null, msg, null, t); }
		@Override public void info(Marker marker, String msg) { capture(INFO, marker, msg, null, null); }
		@Override public void info(Marker marker, String format, Object arg) { capture(INFO, marker, format, new Object[] {arg}, null); }
		@Override public void info(Marker marker, String format, Object arg1, Object arg2) { capture(INFO, marker, format, new Object[] {arg1, arg2}, null); }
		@Override public void info(Marker marker, String format, Object... arguments) { capture(INFO, marker, format, arguments, null); }
		@Override public void info(Marker marker, String msg, Throwable t) { capture(INFO, marker, msg, null, t); }

		@Override public boolean isWarnEnabled() { return true; }
		@Override public boolean isWarnEnabled(Marker marker) { return true; }
		@Override public void warn(String msg) { capture(WARN, null, msg, null, null); }
		@Override public void warn(String format, Object arg) { capture(WARN, null, format, new Object[] {arg}, null); }
		@Override public void warn(String format, Object... arguments) { capture(WARN, null, format, arguments, null); }
		@Override public void warn(String format, Object arg1, Object arg2) { capture(WARN, null, format, new Object[] {arg1, arg2}, null); }
		@Override public void warn(String msg, Throwable t) { capture(WARN, null, msg, null, t); }
		@Override public void warn(Marker marker, String msg) { capture(WARN, marker, msg, null, null); }
		@Override public void warn(Marker marker, String format, Object arg) { capture(WARN, marker, format, new Object[] {arg}, null); }
		@Override public void warn(Marker marker, String format, Object arg1, Object arg2) { capture(WARN, marker, format, new Object[] {arg1, arg2}, null); }
		@Override public void warn(Marker marker, String format, Object... arguments) { capture(WARN, marker, format, arguments, null); }
		@Override public void warn(Marker marker, String msg, Throwable t) { capture(WARN, marker, msg, null, t); }

		@Override public boolean isErrorEnabled() { return true; }
		@Override public boolean isErrorEnabled(Marker marker) { return true; }
		@Override public void error(String msg) { capture(ERROR, null, msg, null, null); }
		@Override public void error(String format, Object arg) { capture(ERROR, null, format, new Object[] {arg}, null); }
		@Override public void error(String format, Object arg1, Object arg2) { capture(ERROR, null, format, new Object[] {arg1, arg2}, null); }
		@Override public void error(String format, Object... arguments) { capture(ERROR, null, format, arguments, null); }
		@Override public void error(String msg, Throwable t) { capture(ERROR, null, msg, null, t); }
		@Override public void error(Marker marker, String msg) { capture(ERROR, marker, msg, null, null); }
		@Override public void error(Marker marker, String format, Object arg) { capture(ERROR, marker, format, new Object[] {arg}, null); }
		@Override public void error(Marker marker, String format, Object arg1, Object arg2) { capture(ERROR, marker, format, new Object[] {arg1, arg2}, null); }
		@Override public void error(Marker marker, String format, Object... arguments) { capture(ERROR, marker, format, arguments, null); }
		@Override public void error(Marker marker, String msg, Throwable t) { capture(ERROR, marker, msg, null, t); }
	}

	private static final class LocationAwareRecordingLogger extends RecordingLogger implements LocationAwareLogger {
		String lastFqcn;
		int lastLevelInt;

		@Override
		public void log(Marker marker, String fqcn, int level, String message, Object[] argArray, Throwable t) {
			this.lastFqcn = fqcn;
			this.lastLevelInt = level;
			this.lastMsg = message;
			this.lastArgs = argArray;
			this.lastThrowable = t;
		}
	}
}
