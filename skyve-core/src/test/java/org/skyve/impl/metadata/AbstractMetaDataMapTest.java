package org.skyve.impl.metadata;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.metadata.SerializableMetaData;

class AbstractMetaDataMapTest {

	/** Minimal concrete SerializableMetaData implementation for testing. */
	private static class StubMetaData implements SerializableMetaData {
		private static final long serialVersionUID = 1L;
	}

	/** Concrete subclass that exposes the protected methods for testing. */
	private static class TestableMap extends AbstractMetaDataMap {
		private static final long serialVersionUID = 1L;

		public SerializableMetaData get(String id) {
			return getMetaData(id);
		}

		void put(String id, SerializableMetaData metaData) {
			putMetaData(id, metaData);
		}

		public <T extends SerializableMetaData> List<T> ofType(Class<T> type) {
			return getMetaDataOfType(type);
		}
	}

	private TestableMap map;

	@BeforeEach
	void setUp() {
		map = new TestableMap();
	}

	@Test
	void getReturnsNullForMissingKey() {
		assertNull(map.get("unknown"));
	}

	@Test
	void putAndGetRoundtrip() {
		StubMetaData md = new StubMetaData();
		map.put("first", md);
		assertNotNull(map.get("first"));
	}

	@Test
	void putDuplicateKeyThrowsIllegalStateException() {
		StubMetaData metaData = new StubMetaData();
		map.put("key", metaData);
		assertThrows(IllegalStateException.class, () -> map.put("key", metaData));
	}

	@Test
	void getMetaDataOfTypeReturnsEmptyListWhenNoEntries() {
		List<StubMetaData> result = map.ofType(StubMetaData.class);
		assertThat(result, empty());
	}

	@Test
	void getMetaDataOfTypeReturnsMatchingEntries() {
		StubMetaData a = new StubMetaData();
		StubMetaData b = new StubMetaData();
		map.put("a", a);
		map.put("b", b);
		List<StubMetaData> result = map.ofType(StubMetaData.class);
		assertThat(result, containsInAnyOrder(is(a), is(b)));
	}

	@Test
	void getMetaDataOfTypeFiltersCorrectly() {
		/** Alternative MetaData type. */
		class OtherMetaData implements SerializableMetaData {
			private static final long serialVersionUID = 1L;
		}
		map.put("stub", new StubMetaData());
		map.put("other", new OtherMetaData());
		List<StubMetaData> result = map.ofType(StubMetaData.class);
		assertEquals(1, result.size());
	}
}
