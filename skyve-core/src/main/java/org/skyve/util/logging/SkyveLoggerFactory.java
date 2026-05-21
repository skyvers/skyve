package org.skyve.util.logging;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.Marker;
import org.slf4j.event.Level;
import org.slf4j.helpers.AbstractLogger;
import org.skyve.util.OWASP;

/**
 * A sanitising {@link Logger} wrapper that strips CR, LF, and other ASCII control
 * characters from log messages and {@link String} arguments before delegating to the
 * underlying SLF4J logger, preventing log injection attacks regardless of the
 * configured logging backend.
 *
 * <p>Non-{@link String} arguments are passed through unmodified; callers are responsible
 * for sanitizing objects whose {@link Object#toString()} output may contain control
 * characters.
 *
 * <p>Usage — drop-in replacement for {@link LoggerFactory#getLogger}:
 * <pre>
 *     private static final Logger LOGGER = SkyveLoggerFactory.getLogger(MyClass.class);
 *     private static final Logger LOGGER = SkyveLoggerFactory.getLogger(Category.SECURITY);
 * </pre>
 *
 * <p>{@link Category#logger()} also returns a {@code SkyveLogger}-wrapped instance, so
 * category loggers obtained through that convenience method are automatically sanitised.
 *
 * @implNote Because this class adds a delegation frame, caller-location information in
 *           log output (class name, method, line number) reflects the internal delegation
 *           call rather than the original call site. This is an inherent limitation of
 *           the wrapper pattern and an acceptable trade-off for backend-agnostic
 *           sanitisation.
 */
public final class SkyveLoggerFactory extends AbstractLogger {
	private static final long serialVersionUID = 6403958415244412320L;

	/**
	 * Transient because {@link Logger} implementations are not reliably serializable.
	 * {@code SkyveLogger} instances are not intended to be serialized; any
	 * deserialized instance will have a {@code null} delegate and must not be used.
	 */
	private final transient Logger delegate;

	private SkyveLoggerFactory(Logger delegate) {
		this.delegate = delegate;
		this.name = delegate.getName();
	}

	/**
	 * Returns a sanitising logger named after {@code clazz}.
	 *
	 * @param clazz the class to name the logger after; must not be {@code null}
	 * @return a sanitising {@link Logger}; never {@code null}
	 */
	public static Logger getLogger(Class<?> clazz) {
		return new SkyveLoggerFactory(LoggerFactory.getLogger(clazz));
	}

	/**
	 * Wraps an existing {@link Logger} in the sanitising decorator.
	 * Package-private test seam — allows unit tests to supply a recording
	 * delegate without going through {@link LoggerFactory}.
	 */
	static Logger wrap(Logger delegate) {
		return new SkyveLoggerFactory(delegate);
	}

	/**
	 * Returns a sanitising logger with the given name, matching the signature of
	 * {@link LoggerFactory#getLogger(String)}.
	 *
	 * @param name the logger name; must not be {@code null}
	 * @return a sanitising {@link Logger}; never {@code null}
	 */
	public static Logger getLogger(String name) {
		return new SkyveLoggerFactory(LoggerFactory.getLogger(name));
	}

	/**
	 * Returns a sanitising logger for the given {@link Category}.
	 *
	 * <p>Prefer using {@link Category#logger()} as the call site; this factory method
	 * exists for completeness and for cases where a {@link Category} reference is
	 * already at hand.
	 *
	 * @param category the framework logging category; must not be {@code null}
	 * @return a sanitising {@link Logger}; never {@code null}
	 */
	public static Logger getLogger(Category category) {
		return new SkyveLoggerFactory(LoggerFactory.getLogger(category.getName()));
	}

	@Override
	protected String getFullyQualifiedCallerName() {
		return null;
	}

	@Override
	protected void handleNormalizedLoggingCall(Level level, Marker marker, String msg, Object[] arguments, Throwable throwable) {
		String cleanMsg = OWASP.sanitiseLog(msg);
		Object[] cleanArgs = sanitiseArgs(arguments);
		Object[] delegateArgs = throwable != null ? appendThrowable(cleanArgs, throwable) : cleanArgs;
		switch (level) {
			case TRACE: delegate.trace(marker, cleanMsg, delegateArgs); break;
			case DEBUG: delegate.debug(marker, cleanMsg, delegateArgs); break;
			case INFO:  delegate.info(marker, cleanMsg, delegateArgs);  break;
			case WARN:  delegate.warn(marker, cleanMsg, delegateArgs);  break;
			case ERROR: delegate.error(marker, cleanMsg, delegateArgs); break;
			default:    delegate.info(marker, cleanMsg, delegateArgs);  break;
		}
	}

	@Override
	public boolean isTraceEnabled() {
		return delegate.isTraceEnabled();
	}

	@Override
	public boolean isTraceEnabled(Marker marker) {
		return delegate.isTraceEnabled(marker);
	}

	@Override
	public boolean isDebugEnabled() {
		return delegate.isDebugEnabled();
	}

	@Override
	public boolean isDebugEnabled(Marker marker) {
		return delegate.isDebugEnabled(marker);
	}

	@Override
	public boolean isInfoEnabled() {
		return delegate.isInfoEnabled();
	}

	@Override
	public boolean isInfoEnabled(Marker marker) {
		return delegate.isInfoEnabled(marker);
	}

	@Override
	public boolean isWarnEnabled() {
		return delegate.isWarnEnabled();
	}

	@Override
	public boolean isWarnEnabled(Marker marker) {
		return delegate.isWarnEnabled(marker);
	}

	@Override
	public boolean isErrorEnabled() {
		return delegate.isErrorEnabled();
	}

	@Override
	public boolean isErrorEnabled(Marker marker) {
		return delegate.isErrorEnabled(marker);
	}

	/**
	 * Returns a copy of {@code args} with each {@link String} element sanitised.
	 * Non-{@link String} elements are passed through unchanged.
	 *
	 * @param args the argument array; may be {@code null}
	 * @return a sanitised copy, or {@code null} if {@code args} was {@code null}
	 */
	private static Object[] sanitiseArgs(Object[] args) {
		if (args == null) {
			return new Object[0];
		}
		int n = args.length;
		Object[] cleaned = new Object[n];
		for (int i = 0; i < n; i++) {
			Object arg = args[i];
			cleaned[i] = arg instanceof String s ? OWASP.sanitiseLog(s) : arg;
		}
		return cleaned;
	}

	/**
	 * Returns a new array containing all elements of {@code args} followed by
	 * {@code throwable}. This allows the delegate logger to process the throwable
	 * through its normal varargs handling.
	 *
	 * @param args      the existing argument array; may be {@code null}
	 * @param throwable the throwable to append; must not be {@code null}
	 * @return a new array with {@code throwable} as the last element
	 */
	private static Object[] appendThrowable(Object[] args, Throwable throwable) {
		int n = args == null ? 0 : args.length;
		Object[] result = new Object[n + 1];
		if (args != null) {
			System.arraycopy(args, 0, result, 0, n);
		}
		result[n] = throwable;
		return result;
	}
}
