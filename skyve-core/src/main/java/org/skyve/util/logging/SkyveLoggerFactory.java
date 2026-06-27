package org.skyve.util.logging;

import org.skyve.util.OWASP;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.Marker;
import org.slf4j.spi.LocationAwareLogger;

/**
 * Factory for sanitising {@link Logger} wrappers that strip CR, LF, and other
 * ASCII control characters from log messages and {@link String} arguments before
 * delegating to the underlying SLF4J logger.
 *
 * <p>Non-{@link String} arguments are passed through unmodified; callers remain
 * responsible for sanitising objects whose {@link Object#toString()} output may
 * contain control characters.
 *
 * <p>Threading: returned loggers are thread-safe when the underlying SLF4J logger
 * is thread-safe. The wrapper holds no mutable state beyond the immutable delegate
 * reference.
 *
 * <p>The wrapper only depends on the SLF4J 1.7 {@link Logger} API surface so it can
 * run inside Maven 3 plugin classloaders, which commonly expose SLF4J 1.7 before
 * plugin dependencies.
 *
 * @implNote When the delegate implements {@link LocationAwareLogger}, the wrapper
 *           forwards through {@link LocationAwareLogger#log(Marker, String, int, String, Object[], Throwable)}
 *           with a caller boundary so compatible backends can report the original
 *           logging call site. Other delegates use the normal {@link Logger}
 *           overloads.
 */
public final class SkyveLoggerFactory {
	/**
	 * Prevents construction of this factory.
	 */
	private SkyveLoggerFactory() {
		// factory class
	}

	/**
	 * Returns a sanitising logger named after {@code clazz}.
	 *
	 * @param clazz the class to name the logger after; must not be {@code null}
	 * @return a sanitising {@link Logger}; never {@code null}
	 */
	public static Logger getLogger(Class<?> clazz) {
		return new SanitisingLogger(LoggerFactory.getLogger(clazz));
	}

	/**
	 * Wraps an existing {@link Logger} in the sanitising decorator.
	 *
	 * <p>Side effects: none; this method does not configure the delegate or resolve
	 * a backend through {@link LoggerFactory}.
	 *
	 * @param delegate the logger to wrap; must not be {@code null}
	 * @return a sanitising {@link Logger}; never {@code null}
	 */
	static Logger wrap(Logger delegate) {
		return new SanitisingLogger(delegate);
	}

	/**
	 * Returns a sanitising logger with the given name, matching the signature of
	 * {@link LoggerFactory#getLogger(String)}.
	 *
	 * @param name the logger name; must not be {@code null}
	 * @return a sanitising {@link Logger}; never {@code null}
	 */
	public static Logger getLogger(String name) {
		return new SanitisingLogger(LoggerFactory.getLogger(name));
	}

	/**
	 * Returns a sanitising logger for the given {@link Category}.
	 *
	 * @param category the framework logging category; must not be {@code null}
	 * @return a sanitising {@link Logger}; never {@code null}
	 */
	public static Logger getLogger(Category category) {
		return new SanitisingLogger(LoggerFactory.getLogger(category.getName()));
	}

	/**
	 * Sanitises SLF4J 1.7 logging calls before forwarding them to a delegate.
	 *
	 * <p>Invariant: the delegate is never replaced after construction. Messages and
	 * {@link String} arguments are sanitised for every logging level and marker
	 * overload; enabled checks are forwarded directly and do not allocate.
	 *
	 * <p>Threading: immutable and thread-safe when the delegate is thread-safe.
	 *
	 * @implNote This class intentionally implements {@link Logger} directly instead
	 *           of extending SLF4J helper classes. Maven 3 plugin classloaders often
	 *           expose SLF4J 1.7 before plugin dependencies, so references to SLF4J
	 *           2.x helpers can fail during generator startup.
	 */
	private static final class SanitisingLogger implements Logger {
		private static final String CALLER_BOUNDARY_FQCN = SanitisingLogger.class.getName();

		private final Logger delegate;

		/**
		 * Creates a wrapper around {@code delegate}.
		 *
		 * @param delegate the logger to receive sanitised calls; must not be {@code null}
		 */
		private SanitisingLogger(Logger delegate) {
			this.delegate = delegate;
		}

		@Override
		public String getName() {
			return delegate.getName();
		}

		@Override
		public boolean isTraceEnabled() {
			return delegate.isTraceEnabled();
		}

		@Override
		public void trace(String msg) {
			log(null, LocationAwareLogger.TRACE_INT, msg, null, null, () -> delegate.trace(sanitise(msg)));
		}

		@Override
		public void trace(String format, Object arg) {
			log(null, LocationAwareLogger.TRACE_INT, format, new Object[] {arg}, null, () -> delegate.trace(sanitise(format), sanitiseArg(arg)));
		}

		@Override
		public void trace(String format, Object arg1, Object arg2) {
			log(null, LocationAwareLogger.TRACE_INT, format, new Object[] {arg1, arg2}, null, () -> delegate.trace(sanitise(format), sanitiseArg(arg1), sanitiseArg(arg2)));
		}

		@Override
		public void trace(String format, Object... arguments) {
			log(null, LocationAwareLogger.TRACE_INT, format, arguments, null, () -> delegate.trace(sanitise(format), sanitiseArgs(arguments)));
		}

		@Override
		public void trace(String msg, Throwable t) {
			log(null, LocationAwareLogger.TRACE_INT, msg, null, t, () -> delegate.trace(sanitise(msg), t));
		}

		@Override
		public boolean isTraceEnabled(Marker marker) {
			return delegate.isTraceEnabled(marker);
		}

		@Override
		public void trace(Marker marker, String msg) {
			log(marker, LocationAwareLogger.TRACE_INT, msg, null, null, () -> delegate.trace(marker, sanitise(msg)));
		}

		@Override
		public void trace(Marker marker, String format, Object arg) {
			log(marker, LocationAwareLogger.TRACE_INT, format, new Object[] {arg}, null, () -> delegate.trace(marker, sanitise(format), sanitiseArg(arg)));
		}

		@Override
		public void trace(Marker marker, String format, Object arg1, Object arg2) {
			log(marker, LocationAwareLogger.TRACE_INT, format, new Object[] {arg1, arg2}, null, () -> delegate.trace(marker, sanitise(format), sanitiseArg(arg1), sanitiseArg(arg2)));
		}

		@Override
		public void trace(Marker marker, String format, Object... argArray) {
			log(marker, LocationAwareLogger.TRACE_INT, format, argArray, null, () -> delegate.trace(marker, sanitise(format), sanitiseArgs(argArray)));
		}

		@Override
		public void trace(Marker marker, String msg, Throwable t) {
			log(marker, LocationAwareLogger.TRACE_INT, msg, null, t, () -> delegate.trace(marker, sanitise(msg), t));
		}

		@Override
		public boolean isDebugEnabled() {
			return delegate.isDebugEnabled();
		}

		@Override
		public void debug(String msg) {
			log(null, LocationAwareLogger.DEBUG_INT, msg, null, null, () -> delegate.debug(sanitise(msg)));
		}

		@Override
		public void debug(String format, Object arg) {
			log(null, LocationAwareLogger.DEBUG_INT, format, new Object[] {arg}, null, () -> delegate.debug(sanitise(format), sanitiseArg(arg)));
		}

		@Override
		public void debug(String format, Object arg1, Object arg2) {
			log(null, LocationAwareLogger.DEBUG_INT, format, new Object[] {arg1, arg2}, null, () -> delegate.debug(sanitise(format), sanitiseArg(arg1), sanitiseArg(arg2)));
		}

		@Override
		public void debug(String format, Object... arguments) {
			log(null, LocationAwareLogger.DEBUG_INT, format, arguments, null, () -> delegate.debug(sanitise(format), sanitiseArgs(arguments)));
		}

		@Override
		public void debug(String msg, Throwable t) {
			log(null, LocationAwareLogger.DEBUG_INT, msg, null, t, () -> delegate.debug(sanitise(msg), t));
		}

		@Override
		public boolean isDebugEnabled(Marker marker) {
			return delegate.isDebugEnabled(marker);
		}

		@Override
		public void debug(Marker marker, String msg) {
			log(marker, LocationAwareLogger.DEBUG_INT, msg, null, null, () -> delegate.debug(marker, sanitise(msg)));
		}

		@Override
		public void debug(Marker marker, String format, Object arg) {
			log(marker, LocationAwareLogger.DEBUG_INT, format, new Object[] {arg}, null, () -> delegate.debug(marker, sanitise(format), sanitiseArg(arg)));
		}

		@Override
		public void debug(Marker marker, String format, Object arg1, Object arg2) {
			log(marker, LocationAwareLogger.DEBUG_INT, format, new Object[] {arg1, arg2}, null, () -> delegate.debug(marker, sanitise(format), sanitiseArg(arg1), sanitiseArg(arg2)));
		}

		@Override
		public void debug(Marker marker, String format, Object... arguments) {
			log(marker, LocationAwareLogger.DEBUG_INT, format, arguments, null, () -> delegate.debug(marker, sanitise(format), sanitiseArgs(arguments)));
		}

		@Override
		public void debug(Marker marker, String msg, Throwable t) {
			log(marker, LocationAwareLogger.DEBUG_INT, msg, null, t, () -> delegate.debug(marker, sanitise(msg), t));
		}

		@Override
		public boolean isInfoEnabled() {
			return delegate.isInfoEnabled();
		}

		@Override
		public void info(String msg) {
			log(null, LocationAwareLogger.INFO_INT, msg, null, null, () -> delegate.info(sanitise(msg)));
		}

		@Override
		public void info(String format, Object arg) {
			log(null, LocationAwareLogger.INFO_INT, format, new Object[] {arg}, null, () -> delegate.info(sanitise(format), sanitiseArg(arg)));
		}

		@Override
		public void info(String format, Object arg1, Object arg2) {
			log(null, LocationAwareLogger.INFO_INT, format, new Object[] {arg1, arg2}, null, () -> delegate.info(sanitise(format), sanitiseArg(arg1), sanitiseArg(arg2)));
		}

		@Override
		public void info(String format, Object... arguments) {
			log(null, LocationAwareLogger.INFO_INT, format, arguments, null, () -> delegate.info(sanitise(format), sanitiseArgs(arguments)));
		}

		@Override
		public void info(String msg, Throwable t) {
			log(null, LocationAwareLogger.INFO_INT, msg, null, t, () -> delegate.info(sanitise(msg), t));
		}

		@Override
		public boolean isInfoEnabled(Marker marker) {
			return delegate.isInfoEnabled(marker);
		}

		@Override
		public void info(Marker marker, String msg) {
			log(marker, LocationAwareLogger.INFO_INT, msg, null, null, () -> delegate.info(marker, sanitise(msg)));
		}

		@Override
		public void info(Marker marker, String format, Object arg) {
			log(marker, LocationAwareLogger.INFO_INT, format, new Object[] {arg}, null, () -> delegate.info(marker, sanitise(format), sanitiseArg(arg)));
		}

		@Override
		public void info(Marker marker, String format, Object arg1, Object arg2) {
			log(marker, LocationAwareLogger.INFO_INT, format, new Object[] {arg1, arg2}, null, () -> delegate.info(marker, sanitise(format), sanitiseArg(arg1), sanitiseArg(arg2)));
		}

		@Override
		public void info(Marker marker, String format, Object... arguments) {
			log(marker, LocationAwareLogger.INFO_INT, format, arguments, null, () -> delegate.info(marker, sanitise(format), sanitiseArgs(arguments)));
		}

		@Override
		public void info(Marker marker, String msg, Throwable t) {
			log(marker, LocationAwareLogger.INFO_INT, msg, null, t, () -> delegate.info(marker, sanitise(msg), t));
		}

		@Override
		public boolean isWarnEnabled() {
			return delegate.isWarnEnabled();
		}

		@Override
		public void warn(String msg) {
			log(null, LocationAwareLogger.WARN_INT, msg, null, null, () -> delegate.warn(sanitise(msg)));
		}

		@Override
		public void warn(String format, Object arg) {
			log(null, LocationAwareLogger.WARN_INT, format, new Object[] {arg}, null, () -> delegate.warn(sanitise(format), sanitiseArg(arg)));
		}

		@Override
		public void warn(String format, Object... arguments) {
			log(null, LocationAwareLogger.WARN_INT, format, arguments, null, () -> delegate.warn(sanitise(format), sanitiseArgs(arguments)));
		}

		@Override
		public void warn(String format, Object arg1, Object arg2) {
			log(null, LocationAwareLogger.WARN_INT, format, new Object[] {arg1, arg2}, null, () -> delegate.warn(sanitise(format), sanitiseArg(arg1), sanitiseArg(arg2)));
		}

		@Override
		public void warn(String msg, Throwable t) {
			log(null, LocationAwareLogger.WARN_INT, msg, null, t, () -> delegate.warn(sanitise(msg), t));
		}

		@Override
		public boolean isWarnEnabled(Marker marker) {
			return delegate.isWarnEnabled(marker);
		}

		@Override
		public void warn(Marker marker, String msg) {
			log(marker, LocationAwareLogger.WARN_INT, msg, null, null, () -> delegate.warn(marker, sanitise(msg)));
		}

		@Override
		public void warn(Marker marker, String format, Object arg) {
			log(marker, LocationAwareLogger.WARN_INT, format, new Object[] {arg}, null, () -> delegate.warn(marker, sanitise(format), sanitiseArg(arg)));
		}

		@Override
		public void warn(Marker marker, String format, Object arg1, Object arg2) {
			log(marker, LocationAwareLogger.WARN_INT, format, new Object[] {arg1, arg2}, null, () -> delegate.warn(marker, sanitise(format), sanitiseArg(arg1), sanitiseArg(arg2)));
		}

		@Override
		public void warn(Marker marker, String format, Object... arguments) {
			log(marker, LocationAwareLogger.WARN_INT, format, arguments, null, () -> delegate.warn(marker, sanitise(format), sanitiseArgs(arguments)));
		}

		@Override
		public void warn(Marker marker, String msg, Throwable t) {
			log(marker, LocationAwareLogger.WARN_INT, msg, null, t, () -> delegate.warn(marker, sanitise(msg), t));
		}

		@Override
		public boolean isErrorEnabled() {
			return delegate.isErrorEnabled();
		}

		@Override
		public void error(String msg) {
			log(null, LocationAwareLogger.ERROR_INT, msg, null, null, () -> delegate.error(sanitise(msg)));
		}

		@Override
		public void error(String format, Object arg) {
			log(null, LocationAwareLogger.ERROR_INT, format, new Object[] {arg}, null, () -> delegate.error(sanitise(format), sanitiseArg(arg)));
		}

		@Override
		public void error(String format, Object arg1, Object arg2) {
			log(null, LocationAwareLogger.ERROR_INT, format, new Object[] {arg1, arg2}, null, () -> delegate.error(sanitise(format), sanitiseArg(arg1), sanitiseArg(arg2)));
		}

		@Override
		public void error(String format, Object... arguments) {
			log(null, LocationAwareLogger.ERROR_INT, format, arguments, null, () -> delegate.error(sanitise(format), sanitiseArgs(arguments)));
		}

		@Override
		public void error(String msg, Throwable t) {
			log(null, LocationAwareLogger.ERROR_INT, msg, null, t, () -> delegate.error(sanitise(msg), t));
		}

		@Override
		public boolean isErrorEnabled(Marker marker) {
			return delegate.isErrorEnabled(marker);
		}

		@Override
		public void error(Marker marker, String msg) {
			log(marker, LocationAwareLogger.ERROR_INT, msg, null, null, () -> delegate.error(marker, sanitise(msg)));
		}

		@Override
		public void error(Marker marker, String format, Object arg) {
			log(marker, LocationAwareLogger.ERROR_INT, format, new Object[] {arg}, null, () -> delegate.error(marker, sanitise(format), sanitiseArg(arg)));
		}

		@Override
		public void error(Marker marker, String format, Object arg1, Object arg2) {
			log(marker, LocationAwareLogger.ERROR_INT, format, new Object[] {arg1, arg2}, null, () -> delegate.error(marker, sanitise(format), sanitiseArg(arg1), sanitiseArg(arg2)));
		}

		@Override
		public void error(Marker marker, String format, Object... arguments) {
			log(marker, LocationAwareLogger.ERROR_INT, format, arguments, null, () -> delegate.error(marker, sanitise(format), sanitiseArgs(arguments)));
		}

		@Override
		public void error(Marker marker, String msg, Throwable t) {
			log(marker, LocationAwareLogger.ERROR_INT, msg, null, t, () -> delegate.error(marker, sanitise(msg), t));
		}

		/**
		 * Forwards a sanitised logging call through the best delegate path available.
		 *
		 * <p>If the delegate is location-aware, this method performs the same throwable
		 * normalisation that SLF4J helper classes normally provide before invoking the
		 * lower-level API. Otherwise it runs {@code fallback}, which uses the matching
		 * ordinary {@link Logger} overload.
		 *
		 * <p>Complexity: O(n) time and space where n is the number of logging arguments.
		 *
		 * @param marker    the SLF4J marker for marker overloads, or {@code null}
		 * @param level     one of the {@link LocationAwareLogger} integer level constants
		 * @param msg       the message or format string; may be {@code null}
		 * @param arguments formatting arguments; may be {@code null}
		 * @param throwable explicit throwable argument, or {@code null}
		 * @param fallback  the ordinary SLF4J call to use when the delegate is not
		 *                  location-aware; must not be {@code null}
		 */
		private void log(Marker marker, int level, String msg, Object[] arguments, Throwable throwable, Runnable fallback) {
			if (delegate instanceof LocationAwareLogger law) {
				Throwable normalisedThrowable = normaliseThrowable(arguments, throwable);
				Object[] normalisedArguments = normaliseArgs(arguments, normalisedThrowable, throwable);
				law.log(marker, CALLER_BOUNDARY_FQCN, level, sanitise(msg), normalisedArguments, normalisedThrowable);
				return;
			}
			fallback.run();
		}

		/**
		 * Returns {@code value} with log-injection control characters replaced.
		 *
		 * @param value the string to sanitise; may be {@code null}
		 * @return a sanitised string, or {@code null} when {@code value} is {@code null}
		 */
		private static String sanitise(String value) {
			return OWASP.sanitiseLog(value);
		}

		/**
		 * Returns a copy of {@code args} with each {@link String} element sanitised.
		 * Non-{@link String} elements are passed through unchanged.
		 *
		 * @param args the argument array; may be {@code null}
		 * @return a sanitised copy, or an empty array if {@code args} was {@code null}
		 */
		private static Object[] sanitiseArgs(Object[] args) {
			if (args == null) {
				return new Object[0];
			}
			int n = args.length;
			Object[] cleaned = new Object[n];
			for (int i = 0; i < n; i++) {
				cleaned[i] = sanitiseArg(args[i]);
			}
			return cleaned;
		}

		/**
		 * Returns {@code arg} sanitised only when it is a {@link String}.
		 *
		 * @param arg the logging argument; may be {@code null}
		 * @return the sanitised string, or the original argument for non-string values
		 */
		private static Object sanitiseArg(Object arg) {
			return (arg instanceof String string) ? sanitise(string) : arg;
		}

		/**
		 * Resolves the throwable to pass to {@link LocationAwareLogger}.
		 *
		 * <p>If the caller used an overload with an explicit {@link Throwable}, that
		 * throwable wins. Otherwise, a trailing throwable in the argument array is
		 * treated as the throwable according to SLF4J's normal varargs convention.
		 *
		 * @param arguments the formatting arguments; may be {@code null}
		 * @param throwable the explicit throwable, or {@code null}
		 * @return the throwable to forward, or {@code null}
		 */
		private static Throwable normaliseThrowable(Object[] arguments, Throwable throwable) {
			if ((throwable != null) || (arguments == null) || (arguments.length == 0)) {
				return throwable;
			}
			Object candidate = arguments[arguments.length - 1];
			return (candidate instanceof Throwable t) ? t : null;
		}

		/**
		 * Returns formatting arguments with any trailing implicit throwable removed.
		 *
		 * <p>Complexity: O(n) time and space where n is the number of arguments.
		 *
		 * @param arguments           the original formatting arguments; may be {@code null}
		 * @param normalisedThrowable the throwable selected for forwarding, or {@code null}
		 * @param explicitThrowable   the throwable supplied through an explicit throwable
		 *                            overload, or {@code null}
		 * @return sanitised formatting arguments; never {@code null}
		 */
		private static Object[] normaliseArgs(Object[] arguments, Throwable normalisedThrowable, Throwable explicitThrowable) {
			if ((arguments == null) || (normalisedThrowable == explicitThrowable)) {
				return sanitiseArgs(arguments);
			}
			int n = arguments.length - 1;
			Object[] normalisedArguments = new Object[n];
			System.arraycopy(arguments, 0, normalisedArguments, 0, n);
			return sanitiseArgs(normalisedArguments);
		}
	}
}
