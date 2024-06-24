package org.skyve.impl.util;

import java.security.SecureRandom;
import java.time.Instant;
import java.util.UUID;

/**
 * Adapted from: <a href="https://github.com/f4b6a3/uuid-creator">f4b6a3/uuid-creator</a>
 * 
 * <pre>
 * MIT License
 * 
 * Copyright (c) 2018-2023 Fabio Lima
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 * </pre>
 * 
 * @see <a href="https://www.rfc-editor.org/rfc/rfc9562.html#name-uuid-version-7">UUIDv7 RFC</a>
 */
public class UUIDv7 {

    private static SecureRandom random = new SecureRandom();

    private UUIDv7() {
    }

    /**
     * Create a unix epoch (ms) time based UUID (Version 7, variant 0b10) using the current time.
     * 
     * @return a time base UUID
     */
    public static UUID create() {
        return create(Instant.now());
    }

    /**
     * Create a unix epoch (ms) time based UUID (version 7, variant 0b10) using the
     * provided instant. Most likely only useful for testing.
     * 
     * @return a time base UUID
     */
    public static UUID create(Instant instant) {
        final long time = instant.toEpochMilli();
        final long msb = (time << 16) | (random.nextLong() & 0x0fffL) | 0x7000L;
        final long lsb = (random.nextLong() & 0x3fffffffffffffffL) | 0x8000000000000000L;
        return new UUID(msb, lsb);
    }

    /**
     * Extract the timestamp component from the provided UUID. No version or variant
     * checking is performed (eg: providing a UUIDv4 will produce nonsense times).
     * 
     * @param uuid The UUID to convert
     * @return the instant the provided UUID was created.
     */
    public static Instant toInstant(UUID uuid) {

        long msb = uuid.getMostSignificantBits();

        // Get the 48 bits of the timestamp, masking out any extra bits
        long time = (msb >> 16) & 0x0000ffffffffffffl;

        return Instant.ofEpochMilli(time);
    }
}