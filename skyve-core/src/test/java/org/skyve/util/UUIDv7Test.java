package org.skyve.util;

import static java.util.stream.Collectors.toSet;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Set;
import java.util.TreeSet;
import java.util.UUID;

import org.junit.Test;
import org.skyve.impl.util.UUIDv7;

public class UUIDv7Test {

    private final int iterationCount = 100_000;

	@Test
    @SuppressWarnings("boxing")
    public void duplicateTest() {

        Set<String> generated = new TreeSet<>();

        for (int i = iterationCount; i > 0; --i) {
            generated.add(UUIDv7.create()
                                .toString());
        }

        assertThat(generated.size(), is(iterationCount));
    }

    @SuppressWarnings("boxing")
	@Test
    public void sameTimeTest() {

        Instant now = Instant.now();

        Set<String> generated = new TreeSet<>();

        for (int i = iterationCount; i > 0; --i) {
            generated.add(UUIDv7.create(now)
                                .toString());
        }

        // Even generating UUIDs in the same millisecond leaves
        // 72 bits of randomness; duplicates are very unlikely
        assertThat(generated.size(), is(iterationCount));

        // The first 48 bits (13 characters in the string representation)
        // should be identical between all of these UUIDs as they were created
        // using the same millisecond
        Set<String> timePrefix = generated.stream()
                                          .map(s -> s.substring(0, 13))
                                          .limit(2000)
                                          .collect(toSet());

        assertThat(timePrefix.size(), is(1));
    }

 	@Test
    @SuppressWarnings("static-method")
 	public void testToInstant() {

        Instant now = Instant.now();

        UUID uuid = UUIDv7.create(now);

        Instant result = UUIDv7.toInstant(uuid);

        assertThat(result, is(now.truncatedTo(ChronoUnit.MILLIS)));
    }

}
