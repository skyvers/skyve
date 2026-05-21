package org.skyve.util.logging;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.Marker;
import org.slf4j.event.Level;
import org.slf4j.helpers.AbstractLogger;

/**
 * Verifies that {@link SkyveLoggerFactory} strips log-injection characters from
 * messages and {@link String} arguments before forwarding to the delegate logger.
 *
 * <p>A {@link RecordingLogger} acting as the delegate captures exactly what
 * {@code SkyveLoggerFactory} passes after sanitisation, without going through a
 * real logging backend.
 */
class SkyveLoggerFactoryTest {

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

	// ---- all five levels sanitise ----

	@Test
	void sanitisesTraceLevel() {
		logger.trace("Trace\ninjection");
		assertEquals("Trace_injection", recorder.lastMsg);
		assertEquals(Level.TRACE, recorder.lastLevel);
	}

	@Test
	void sanitisesDebugLevel() {
		logger.debug("Debug\ninjection");
		assertEquals("Debug_injection", recorder.lastMsg);
		assertEquals(Level.DEBUG, recorder.lastLevel);
	}

	@Test
	void sanitisesInfoLevel() {
		logger.info("Info\ninjection");
		assertEquals("Info_injection", recorder.lastMsg);
		assertEquals(Level.INFO, recorder.lastLevel);
	}

	@Test
	void sanitisesWarnLevel() {
		logger.warn("Warn\ninjection");
		assertEquals("Warn_injection", recorder.lastMsg);
		assertEquals(Level.WARN, recorder.lastLevel);
	}

	@Test
	void sanitisesErrorLevel() {
		logger.error("Error\ninjection");
		assertEquals("Error_injection", recorder.lastMsg);
		assertEquals(Level.ERROR, recorder.lastLevel);
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
	 * A minimal {@link AbstractLogger} subclass that records the last normalised
	 * logging call made to it. All levels are enabled so that {@code SkyveLoggerFactory}
	 * always forwards calls down to this recorder.
	 */
	private static final class RecordingLogger extends AbstractLogger {

		private static final long serialVersionUID = 1L;

		String lastMsg;
		Object[] lastArgs;
		Throwable lastThrowable;
		Level lastLevel;

		RecordingLogger() {
			this.name = "recording";
		}

		@Override
		protected String getFullyQualifiedCallerName() {
			return null;
		}

		@Override
		protected void handleNormalizedLoggingCall(Level level, Marker marker, String msg, Object[] arguments, Throwable throwable) {
			this.lastLevel = level;
			this.lastMsg = msg;
			this.lastArgs = arguments;
			this.lastThrowable = throwable;
		}

		@Override public boolean isTraceEnabled() { return true; }
		@Override public boolean isTraceEnabled(Marker marker) { return true; }
		@Override public boolean isDebugEnabled() { return true; }
		@Override public boolean isDebugEnabled(Marker marker) { return true; }
		@Override public boolean isInfoEnabled() { return true; }
		@Override public boolean isInfoEnabled(Marker marker) { return true; }
		@Override public boolean isWarnEnabled() { return true; }
		@Override public boolean isWarnEnabled(Marker marker) { return true; }
		@Override public boolean isErrorEnabled() { return true; }
		@Override public boolean isErrorEnabled(Marker marker) { return true; }
	}
}
