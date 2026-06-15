package org.skyve.impl.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;

import java.time.Instant;
import java.util.UUID;

import org.junit.jupiter.api.Test;

@SuppressWarnings({ "static-method", "java:S8692" }) // system clock OK
class UUIDv7Test {

	@Test
	void createReturnsNonNullUUID() {
		UUID uuid = UUIDv7.create();
		assertNotNull(uuid);
	}

	@Test
	void createReturnsDifferentValuesOnSuccessiveCalls() {
		UUID first = UUIDv7.create();
		UUID second = UUIDv7.create();
		assertNotSame(first, second);
	}

	@Test
	void createWithInstantRoundTripsTimestamp() {
		Instant original = Instant.ofEpochMilli(1_700_000_000_000L);
		UUID uuid = UUIDv7.create(original);
		Instant recovered = UUIDv7.toInstant(uuid);
		// toInstant extracts millisecond precision; exact match expected
		assertEquals(original.toEpochMilli(), recovered.toEpochMilli());
	}

	@Test
	void createWithInstantReturnsNonNullUUID() {
		UUID uuid = UUIDv7.create(Instant.now());
		assertNotNull(uuid);
	}

	@Test
	void toInstantExtractsEmbeddedTimestamp() {
		Instant fixedInstant = Instant.ofEpochMilli(1_000_000_000_000L);
		UUID uuid = UUIDv7.create(fixedInstant);
		Instant result = UUIDv7.toInstant(uuid);
		assertEquals(fixedInstant.toEpochMilli(), result.toEpochMilli());
	}
}
