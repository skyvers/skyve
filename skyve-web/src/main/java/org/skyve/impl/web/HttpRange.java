package org.skyve.impl.web;

import java.time.DateTimeException;
import java.time.Instant;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoField;
import java.util.Locale;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Parses and represents one HTTP byte range against a known content length.
 *
 * <p>Supports the single-range forms used by browser media elements:
 * {@code bytes=0-99}, {@code bytes=100-}, and {@code bytes=-500}. Unsupported
 * units, malformed values, and multi-range requests resolve to a full response so
 * callers can keep backward-compatible {@code 200} behaviour. A syntactically
 * valid range outside the known length resolves to an unsatisfiable state for
 * {@code 416 Requested Range Not Satisfiable}.
 *
 * <p>Precondition: callers must only use parsed offsets for untransformed bytes
 * whose total length is known. Ranges over arbitrary {@link java.io.InputStream}
 * instances require a known length plus reliable seek or reopen semantics; this
 * helper deliberately does not provide those semantics.
 *
 * <p>Threading: immutable and thread-safe.
 */
final class HttpRange {
	/**
	 * Identifies the response shape selected for a parsed range header.
	 */
	enum State {
		/**
		 * Indicates that callers should ignore the range header and send the complete entity body.
		 */
		FULL,

		/**
		 * Indicates that callers should send a single inclusive byte range with {@code 206 Partial Content}.
		 */
		PARTIAL,

		/**
		 * Indicates that callers should send {@code 416 Requested Range Not Satisfiable}.
		 */
		UNSATISFIABLE
	}

	private static final String BYTES_PREFIX = "bytes=";
	private static final DateTimeFormatter RFC_850_DATE_TIME = new DateTimeFormatterBuilder()
			.parseCaseInsensitive()
			.appendPattern("EEEE, dd-MMM-yy HH:mm:ss zzz")
			.toFormatter(Locale.US);
	private static final DateTimeFormatter ASCTIME_DATE_TIME = new DateTimeFormatterBuilder()
			.parseCaseInsensitive()
			.appendPattern("EEE MMM ")
			.optionalStart()
			.appendValue(ChronoField.DAY_OF_MONTH, 2)
			.optionalEnd()
			.optionalStart()
			.appendLiteral(' ')
			.appendValue(ChronoField.DAY_OF_MONTH, 1)
			.optionalEnd()
			.appendPattern(" HH:mm:ss yyyy")
			.toFormatter(Locale.US)
			.withZone(ZoneOffset.UTC);

	private final State state;
	private final long contentLength;
	private final long start;
	private final long end;

	private HttpRange(@Nonnull State state, long contentLength, long start, long end) {
		this.state = state;
		this.contentLength = contentLength;
		this.start = start;
		this.end = end;
	}

	/**
	 * Parses a single HTTP {@code Range} header value for a resource with {@code contentLength} bytes.
	 *
	 * <p>Invalid, unsupported, absent, multi-range, or non-byte range values return a full range rather
	 * than throwing. A syntactically valid byte range whose first byte lies beyond the content length
	 * returns an unsatisfiable range.
	 *
	 * <p>Complexity: O(n) time where n is the header length, and O(n) temporary space for parsed
	 * substrings.
	 *
	 * @param header the raw {@code Range} header value; may be {@code null}
	 * @param contentLength total byte length of the untransformed entity; non-positive lengths cannot be
	 *        ranged and return a full response
	 * @return an immutable range decision; never {@code null}
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	static @Nonnull HttpRange parse(@Nullable String header, long contentLength) {
		if ((contentLength <= 0) || (header == null)) {
			return full(contentLength);
		}

		String value = header.trim();
		if (! value.startsWith(BYTES_PREFIX)) {
			return full(contentLength);
		}

		String rangeSpec = value.substring(BYTES_PREFIX.length()).trim();
		if ((rangeSpec.isEmpty()) || rangeSpec.contains(",")) {
			return full(contentLength);
		}

		int dashIndex = rangeSpec.indexOf('-');
		if ((dashIndex < 0) || (rangeSpec.indexOf('-', dashIndex + 1) >= 0)) {
			return full(contentLength);
		}

		String first = rangeSpec.substring(0, dashIndex).trim();
		String last = rangeSpec.substring(dashIndex + 1).trim();
		if (first.isEmpty() && last.isEmpty()) {
			return full(contentLength);
		}

		try {
			if (first.isEmpty()) {
				return suffixRange(last, contentLength);
			}

			long start = Long.parseLong(first);
			if (start < 0) {
				return full(contentLength);
			}

			long end = contentLength - 1;
			if (! last.isEmpty()) {
				end = Long.parseLong(last);
				if (end < 0) {
					return full(contentLength);
				}
			}

			if ((start > end) || (start >= contentLength)) {
				return unsatisfiable(contentLength);
			}

			return partial(contentLength, start, Math.min(end, contentLength - 1));
		}
		catch (@SuppressWarnings("unused") NumberFormatException e) {
			return full(contentLength);
		}
	}

	/**
	 * Parses a {@code Range} header only when an optional {@code If-Range} validator
	 * still matches the current entity.
	 *
	 * <p>If {@code ifRangeHeader} is present and does not match {@code entityTag} or
	 * {@code lastModifiedMillis}, this returns a full response decision as required by
	 * RFC 9110 resume-download semantics.
	 *
	 * @param rangeHeader the raw {@code Range} header value; may be {@code null}
	 * @param contentLength total byte length of the untransformed entity
	 * @param ifRangeHeader the raw {@code If-Range} header value; may be {@code null}
	 * @param lastModifiedMillis last-modified timestamp in epoch milliseconds, or a negative value when
	 *        unknown
	 * @param entityTag current entity tag, or {@code null} when none is available
	 * @return an immutable range decision; never {@code null}
	 */
	static @Nonnull HttpRange parse(@Nullable String rangeHeader,
										long contentLength,
										@Nullable String ifRangeHeader,
										long lastModifiedMillis,
										@Nullable String entityTag) {
		if (! isIfRangeSatisfied(ifRangeHeader, lastModifiedMillis, entityTag)) {
			return full(contentLength);
		}
		return parse(rangeHeader, contentLength);
	}

	private static boolean isIfRangeSatisfied(@Nullable String ifRangeHeader,
												long lastModifiedMillis,
												@Nullable String entityTag) {
		if (ifRangeHeader == null) {
			return true;
		}

		String validator = ifRangeHeader.trim();
		if (validator.isEmpty()) {
			return false;
		}

		if (validator.startsWith("\"") || validator.startsWith("W/\"")) {
			return isStrongEntityTagMatch(validator, entityTag);
		}

		Long ifRangeMillis = parseHttpDateMillis(validator);
		if ((ifRangeMillis == null) || (lastModifiedMillis < 0L)) {
			return false;
		}

		long lastModifiedSeconds = lastModifiedMillis - (lastModifiedMillis % 1_000L);
		return ifRangeMillis.longValue() >= lastModifiedSeconds;
	}

	private static boolean isStrongEntityTagMatch(@Nonnull String ifRangeTag, @Nullable String entityTag) {
		if ((entityTag == null) || ifRangeTag.startsWith("W/")) {
			return false;
		}

		String currentTag = entityTag.trim();
		return (! currentTag.startsWith("W/")) && ifRangeTag.equals(currentTag);
	}

	private static @Nullable Long parseHttpDateMillis(@Nonnull String value) {
		try {
			return Long.valueOf(ZonedDateTime.parse(value, DateTimeFormatter.RFC_1123_DATE_TIME).toInstant().toEpochMilli());
		}
		catch (@SuppressWarnings("unused") DateTimeParseException e) {
			// Try the obsolete HTTP-date formats recipients are expected to accept.
		}

		try {
			return Long.valueOf(ZonedDateTime.parse(value, RFC_850_DATE_TIME).toInstant().toEpochMilli());
		}
		catch (@SuppressWarnings("unused") DateTimeParseException e) {
			// Try ANSI C asctime() below.
		}

		try {
			return Long.valueOf(Instant.from(ASCTIME_DATE_TIME.parse(value)).toEpochMilli());
		}
		catch (@SuppressWarnings("unused") DateTimeException e) {
			return null;
		}
	}

	/**
	 * Parses a suffix-byte-range-spec against {@code contentLength}.
	 *
	 * @param last the suffix length text after {@code bytes=-}; must not be {@code null}
	 * @param contentLength total byte length of the untransformed entity; expected to be positive
	 * @return a partial, full, or unsatisfiable range decision; never {@code null}
	 */
	private static @Nonnull HttpRange suffixRange(@Nonnull String last, long contentLength) {
		try {
			long suffixLength = Long.parseLong(last);
			if (suffixLength < 0) {
				return full(contentLength);
			}
			if (suffixLength == 0) {
				return unsatisfiable(contentLength);
			}

			long start = Math.max(contentLength - suffixLength, 0);
			return partial(contentLength, start, contentLength - 1);
		}
		catch (@SuppressWarnings("unused") NumberFormatException e) {
			return full(contentLength);
		}
	}

	/**
	 * Returns a decision for sending the complete entity body.
	 *
	 * @param contentLength total byte length of the entity; may be non-positive when the length is not
	 *        rangeable
	 * @return a full range decision; never {@code null}
	 */
	private static @Nonnull HttpRange full(long contentLength) {
		return new HttpRange(State.FULL, contentLength, 0, Math.max(contentLength - 1, 0));
	}

	/**
	 * Returns a decision for sending bytes from {@code start} through {@code end}, inclusive.
	 *
	 * @param contentLength total byte length of the entity
	 * @param start first byte offset to send; must be non-negative and less than {@code contentLength}
	 * @param end last byte offset to send; must be greater than or equal to {@code start}
	 * @return a partial range decision; never {@code null}
	 */
	private static @Nonnull HttpRange partial(long contentLength, long start, long end) {
		return new HttpRange(State.PARTIAL, contentLength, start, end);
	}

	/**
	 * Returns a decision for a syntactically valid range that cannot be satisfied.
	 *
	 * @param contentLength total byte length of the entity
	 * @return an unsatisfiable range decision; never {@code null}
	 */
	private static @Nonnull HttpRange unsatisfiable(long contentLength) {
		return new HttpRange(State.UNSATISFIABLE, contentLength, 0, 0);
	}

	/**
	 * Returns the response shape selected for this range decision.
	 *
	 * @return the state; never {@code null}
	 */
	@Nonnull State getState() {
		return state;
	}

	/**
	 * Returns whether callers should send the complete entity body.
	 *
	 * @return {@code true} when the state is {@link State#FULL}
	 */
	boolean isFull() {
		return state == State.FULL;
	}

	/**
	 * Returns whether callers should send a single inclusive byte range.
	 *
	 * @return {@code true} when the state is {@link State#PARTIAL}
	 */
	boolean isPartial() {
		return state == State.PARTIAL;
	}

	/**
	 * Returns whether callers should respond with {@code 416 Requested Range Not Satisfiable}.
	 *
	 * @return {@code true} when the state is {@link State#UNSATISFIABLE}
	 */
	boolean isUnsatisfiable() {
		return state == State.UNSATISFIABLE;
	}

	/**
	 * Returns the first byte offset to send.
	 *
	 * @return the inclusive start offset for partial responses, or {@code 0} for full and unsatisfiable
	 *         responses
	 */
	long getStart() {
		return start;
	}

	/**
	 * Returns the last byte offset to send.
	 *
	 * @return the inclusive end offset for full and partial responses, or {@code 0} for unsatisfiable
	 *         responses
	 */
	long getEnd() {
		return end;
	}

	/**
	 * Returns the entity length used when parsing the range.
	 *
	 * @return the original content length supplied to {@link #parse(String, long)}
	 */
	long getContentLength() {
		return contentLength;
	}

	/**
	 * Returns the number of response body bytes to write for this decision.
	 *
	 * @return the partial range length for partial responses, the non-negative entity length for full
	 *         responses, or {@code 0} for unsatisfiable responses
	 */
	long getBytesToSend() {
		if (state == State.PARTIAL) {
			return end - start + 1;
		}
		if (state == State.FULL) {
			return Math.max(contentLength, 0);
		}
		return 0;
	}

	/**
	 * Builds the HTTP {@code Content-Range} header value for responses that require one.
	 *
	 * @return a {@code bytes start-end/length} value for partial responses, an unsatisfied byte-range
	 *         value for unsatisfiable responses, or {@code null} for full responses
	 */
	@Nullable String getContentRange() {
		if (state == State.PARTIAL) {
			return "bytes " + start + '-' + end + '/' + contentLength;
		}
		if (state == State.UNSATISFIABLE) {
			return "bytes */" + contentLength;
		}
		return null;
	}
}
