package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.EOFException;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class HttpRangeTest {
	@Test
	void parseClosedRange() {
		HttpRange range = HttpRange.parse("bytes=0-99", 1_000);

		assertTrue(range.isPartial());
		assertEquals(HttpRange.State.PARTIAL, range.getState());
		assertEquals(0, range.getStart());
		assertEquals(99, range.getEnd());
		assertEquals(100, range.getBytesToSend());
		assertEquals(1_000, range.getContentLength());
		assertEquals("bytes 0-99/1000", range.getContentRange());
	}

	@Test
	void parseClosedRangeClampsEndToContentLength() {
		HttpRange range = HttpRange.parse("bytes=950-1200", 1_000);

		assertTrue(range.isPartial());
		assertEquals(950, range.getStart());
		assertEquals(999, range.getEnd());
		assertEquals(50, range.getBytesToSend());
		assertEquals("bytes 950-999/1000", range.getContentRange());
	}

	@Test
	void parseOpenEndedRange() {
		HttpRange range = HttpRange.parse("bytes=100-", 1_000);

		assertTrue(range.isPartial());
		assertEquals(100, range.getStart());
		assertEquals(999, range.getEnd());
		assertEquals(900, range.getBytesToSend());
		assertEquals("bytes 100-999/1000", range.getContentRange());
	}

	@Test
	void parseBrowserInitialMediaRange() {
		HttpRange range = HttpRange.parse("bytes=0-", 1_000);

		assertTrue(range.isPartial());
		assertEquals(0, range.getStart());
		assertEquals(999, range.getEnd());
		assertEquals(1_000, range.getBytesToSend());
		assertEquals("bytes 0-999/1000", range.getContentRange());
	}

	@Test
	void parseBrowserSeekMediaRange() {
		HttpRange range = HttpRange.parse("bytes=12345-", 20_000);

		assertTrue(range.isPartial());
		assertEquals(12_345, range.getStart());
		assertEquals(19_999, range.getEnd());
		assertEquals(7_655, range.getBytesToSend());
		assertEquals("bytes 12345-19999/20000", range.getContentRange());
	}

	@Test
	void parseSuffixRange() {
		HttpRange range = HttpRange.parse("bytes=-500", 1_000);

		assertTrue(range.isPartial());
		assertEquals(500, range.getStart());
		assertEquals(999, range.getEnd());
		assertEquals(500, range.getBytesToSend());
		assertEquals("bytes 500-999/1000", range.getContentRange());
	}

	@Test
	void parseSuffixRangeLongerThanContent() {
		HttpRange range = HttpRange.parse("bytes=-2000", 1_000);

		assertTrue(range.isPartial());
		assertEquals(0, range.getStart());
		assertEquals(999, range.getEnd());
		assertEquals(1_000, range.getBytesToSend());
		assertEquals("bytes 0-999/1000", range.getContentRange());
	}

	@Test
	void parseRangeWithWhitespace() {
		HttpRange range = HttpRange.parse("  bytes= 2 - 4  ", 10);

		assertTrue(range.isPartial());
		assertEquals(2, range.getStart());
		assertEquals(4, range.getEnd());
		assertEquals(3, range.getBytesToSend());
		assertEquals("bytes 2-4/10", range.getContentRange());
	}

	@Test
	void missingHeaderProducesFullResponse() {
		HttpRange range = HttpRange.parse(null, 1_000);

		assertTrue(range.isFull());
		assertFalse(range.isPartial());
		assertFalse(range.isUnsatisfiable());
		assertEquals(HttpRange.State.FULL, range.getState());
		assertEquals(0, range.getStart());
		assertEquals(999, range.getEnd());
		assertEquals(1_000, range.getBytesToSend());
		assertNull(range.getContentRange());
	}

	@Test
	void invalidHeaderProducesFullResponse() {
		HttpRange range = HttpRange.parse("items=0-99", 1_000);

		assertTrue(range.isFull());
		assertEquals(1_000, range.getBytesToSend());
		assertNull(range.getContentRange());
	}

	@Test
	void emptyRangeSpecProducesFullResponse() {
		HttpRange range = HttpRange.parse("bytes=", 1_000);

		assertTrue(range.isFull());
		assertEquals(1_000, range.getBytesToSend());
		assertNull(range.getContentRange());
	}

	@Test
	void malformedHeaderProducesFullResponse() {
		HttpRange range = HttpRange.parse("bytes=abc-def", 1_000);

		assertTrue(range.isFull());
		assertEquals(1_000, range.getBytesToSend());
		assertNull(range.getContentRange());
	}

	@Test
	void rangeWithoutDashProducesFullResponse() {
		HttpRange range = HttpRange.parse("bytes=99", 1_000);

		assertTrue(range.isFull());
		assertEquals(1_000, range.getBytesToSend());
		assertNull(range.getContentRange());
	}

	@Test
	void rangeWithTooManyDashesProducesFullResponse() {
		HttpRange range = HttpRange.parse("bytes=0-1-2", 1_000);

		assertTrue(range.isFull());
		assertEquals(1_000, range.getBytesToSend());
		assertNull(range.getContentRange());
	}

	@Test
	void multipleRangesProduceFullResponse() {
		HttpRange range = HttpRange.parse("bytes=0-99,200-299", 1_000);

		assertTrue(range.isFull());
		assertEquals(1_000, range.getBytesToSend());
		assertNull(range.getContentRange());
	}

	@Test
	void startGreaterThanEndProducesUnsatisfiableResponse() {
		HttpRange range = HttpRange.parse("bytes=100-99", 1_000);

		assertTrue(range.isUnsatisfiable());
		assertEquals(HttpRange.State.UNSATISFIABLE, range.getState());
		assertEquals(0, range.getBytesToSend());
		assertEquals("bytes */1000", range.getContentRange());
	}

	@Test
	void startBeyondContentLengthProducesUnsatisfiableResponse() {
		HttpRange range = HttpRange.parse("bytes=1000-", 1_000);

		assertTrue(range.isUnsatisfiable());
		assertEquals(0, range.getBytesToSend());
		assertEquals("bytes */1000", range.getContentRange());
	}

	@Test
	void zeroSuffixRangeProducesUnsatisfiableResponse() {
		HttpRange range = HttpRange.parse("bytes=-0", 1_000);

		assertTrue(range.isUnsatisfiable());
		assertEquals("bytes */1000", range.getContentRange());
	}

	@Test
	void negativeLookingRangeProducesFullResponse() {
		HttpRange range = HttpRange.parse("bytes=-10-20", 1_000);

		assertTrue(range.isFull());
		assertEquals(1_000, range.getBytesToSend());
		assertNull(range.getContentRange());
	}

	@Test
	void overflowingStartProducesFullResponse() {
		HttpRange range = HttpRange.parse("bytes=999999999999999999999-", 1_000);

		assertTrue(range.isFull());
		assertEquals(1_000, range.getBytesToSend());
		assertNull(range.getContentRange());
	}

	@Test
	void zeroContentLengthDisablesRangeSupport() {
		HttpRange range = HttpRange.parse("bytes=0-99", 0);

		assertTrue(range.isFull());
		assertEquals(0, range.getBytesToSend());
		assertNull(range.getContentRange());
	}

	@Test
	void parseIfRangeAllowsRangeWhenDateMatchesLastModifiedSecond() {
		long lastModified = 1_700_000_000_777L;

		HttpRange range = HttpRange.parse("bytes=1-3", 10, httpDate(lastModified), lastModified, null);

		assertTrue(range.isPartial());
		assertEquals(1, range.getStart());
		assertEquals(3, range.getEnd());
	}

	@Test
	void parseIfRangeIgnoresRangeWhenDateIsStale() {
		long lastModified = 1_700_000_000_000L;

		HttpRange range = HttpRange.parse("bytes=1-3", 10, httpDate(lastModified - 1_000L), lastModified, null);

		assertTrue(range.isFull());
		assertEquals(10, range.getBytesToSend());
		assertNull(range.getContentRange());
	}

	@Test
	void parseIfRangeRequiresStrongEntityTagMatch() {
		HttpRange matching = HttpRange.parse("bytes=1-3", 10, "\"abc\"", -1L, "\"abc\"");
		HttpRange stale = HttpRange.parse("bytes=1-3", 10, "\"abc\"", -1L, "\"def\"");
		HttpRange weak = HttpRange.parse("bytes=1-3", 10, "W/\"abc\"", -1L, "\"abc\"");
		HttpRange missingCurrentTag = HttpRange.parse("bytes=1-3", 10, "\"abc\"", -1L, null);
		HttpRange weakCurrentTag = HttpRange.parse("bytes=1-3", 10, "\"abc\"", -1L, "W/\"abc\"");

		assertTrue(matching.isPartial());
		assertTrue(stale.isFull());
		assertTrue(weak.isFull());
		assertTrue(missingCurrentTag.isFull());
		assertTrue(weakCurrentTag.isFull());
	}

	@Test
	void parseIfRangeIgnoresRangeWhenValidatorIsInvalid() {
		HttpRange range = HttpRange.parse("bytes=1-3", 10, "not a validator", 1_700_000_000_000L, null);

		assertTrue(range.isFull());
		assertEquals(10, range.getBytesToSend());
	}

	@Test
	void parseIfRangeIgnoresRangeWhenValidatorIsBlank() {
		HttpRange range = HttpRange.parse("bytes=1-3", 10, "   ", 1_700_000_000_000L, null);

		assertTrue(range.isFull());
		assertEquals(10, range.getBytesToSend());
	}

	@Test
	void parseIfRangeIgnoresValidDateWhenLastModifiedIsUnknown() {
		HttpRange range = HttpRange.parse("bytes=1-3", 10, httpDate(1_700_000_000_000L), -1L, null);

		assertTrue(range.isFull());
		assertEquals(10, range.getBytesToSend());
	}

	@Test
	void parseIfRangeAcceptsObsoleteRfc850Date() {
		long lastModified = 1_699_999_200_000L;

		HttpRange range = HttpRange.parse("bytes=1-3", 10, "Tuesday, 14-Nov-23 22:13:20 GMT", lastModified, null);

		assertTrue(range.isPartial());
		assertEquals(1, range.getStart());
		assertEquals(3, range.getEnd());
	}

	@Test
	void parseIfRangeAcceptsObsoleteAsctimeDate() {
		long lastModified = 1_699_999_200_000L;

		HttpRange range = HttpRange.parse("bytes=1-3", 10, "Tue Nov 14 22:13:20 2023", lastModified, null);

		assertTrue(range.isPartial());
		assertEquals(1, range.getStart());
		assertEquals(3, range.getEnd());
	}

	@Test
	void negativeContentLengthDisablesRangeSupport() {
		HttpRange range = HttpRange.parse("bytes=0-99", -1);

		assertTrue(range.isFull());
		assertEquals(0, range.getBytesToSend());
		assertNull(range.getContentRange());
	}

	@Test
	void copySkipsAndCopiesExactlyRequestedBytes() throws IOException {
		ByteArrayInputStream in = new ByteArrayInputStream("abcdefghijklmnopqrstuvwxyz".getBytes(StandardCharsets.UTF_8));
		ByteArrayOutputStream out = new ByteArrayOutputStream();

		StreamResponse.copy(in, out, 5, 10);

		assertEquals("fghijklmno", out.toString(StandardCharsets.UTF_8));
	}

	@Test
	void copyHandlesSkipReturningZero() throws IOException {
		ZeroSkipInputStream in = new ZeroSkipInputStream("0123456789".getBytes(StandardCharsets.UTF_8));
		ByteArrayOutputStream out = new ByteArrayOutputStream();

		StreamResponse.copy(in, out, 3, 4);

		assertEquals("3456", out.toString(StandardCharsets.UTF_8));
	}

	@Test
	@SuppressWarnings("resource")
	void copyDoesNotCloseStreams() throws IOException {
		TrackingInputStream in = new TrackingInputStream("abcdef".getBytes(StandardCharsets.UTF_8));
		TrackingOutputStream out = new TrackingOutputStream();

		StreamResponse.copy(in, out, 1, 3);

		assertFalse(in.closed);
		assertFalse(out.closed);
		assertEquals("bcd", out.toString(StandardCharsets.UTF_8));
	}

	@Test
	void copyThrowsWhenOffsetCannotBeReached() {
		ByteArrayInputStream in = new ByteArrayInputStream("abc".getBytes(StandardCharsets.UTF_8));
		ByteArrayOutputStream out = new ByteArrayOutputStream();

		assertThrows(EOFException.class, () -> StreamResponse.copy(in, out, 10, 1));
	}

	@Test
	void copyThrowsWhenRequestedBytesCannotBeCopied() {
		ByteArrayInputStream in = new ByteArrayInputStream("abc".getBytes(StandardCharsets.UTF_8));
		ByteArrayOutputStream out = new ByteArrayOutputStream();

		assertThrows(EOFException.class, () -> StreamResponse.copy(in, out, 1, 10));
	}

	@Test
	void copyRejectsNegativeArguments() {
		ByteArrayInputStream in = new ByteArrayInputStream("abc".getBytes(StandardCharsets.UTF_8));
		ByteArrayOutputStream out = new ByteArrayOutputStream();

		assertThrows(IllegalArgumentException.class, () -> StreamResponse.copy(in, out, -1, 1));
		assertThrows(IllegalArgumentException.class, () -> StreamResponse.copy(in, out, 1, -1));
	}

	@Test
	void copyLargeRangeUsesMultipleBufferReads() throws IOException {
		byte[] bytes = new byte[(64 * 1024) + 17];
		for (int i = 0, l = bytes.length; i < l; i++) {
			bytes[i] = (byte) (i % 251);
		}
		ByteArrayInputStream in = new ByteArrayInputStream(bytes);
		ByteArrayOutputStream out = new ByteArrayOutputStream();

		StreamResponse.copy(in, out, 9, bytes.length - 17);

		byte[] expected = new byte[bytes.length - 17];
		System.arraycopy(bytes, 9, expected, 0, expected.length);
		assertArrayEquals(expected, out.toByteArray());
	}

	private static final class ZeroSkipInputStream extends ByteArrayInputStream {
		ZeroSkipInputStream(byte[] buf) {
			super(buf);
		}

		@Override
		public synchronized long skip(long n) {
			return 0;
		}
	}

	private static String httpDate(long epochMillis) {
		return DateTimeFormatter.RFC_1123_DATE_TIME.format(Instant.ofEpochMilli(epochMillis).atZone(ZoneOffset.UTC));
	}

	private static final class TrackingInputStream extends ByteArrayInputStream {
		private boolean closed;

		TrackingInputStream(byte[] buf) {
			super(buf);
		}

		@Override
		public void close() {
			closed = true;
		}
	}

	private static final class TrackingOutputStream extends ByteArrayOutputStream {
		private boolean closed;

		@Override
		public void close() throws IOException {
			closed = true;
			super.close();
		}
	}
}
