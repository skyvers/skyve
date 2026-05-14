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
import org.skyve.metadata.MetaData;

public class AbstractMetaDataMapTest {

	/** Minimal concrete MetaData implementation for testing. */
	private static class StubMetaData implements MetaData {
		@SuppressWarnings("unused")
		private static final long serialVersionUID = 1L;
	}

	/** Concrete subclass that exposes the protected methods for testing. */
	private static class TestableMap extends AbstractMetaDataMap {
		private static final long serialVersionUID = 1L;

		public MetaData get(String id) {
			return getMetaData(id);
		}

		public void put(String id, MetaData metaData) {
			putMetaData(id, metaData);
		}

		public <T extends MetaData> List<T> ofType(Class<T> type) {
			return getMetaDataOfType(type);
		}
	}

	private TestableMap map;

	@BeforeEach
	public void setUp() {
		map = new TestableMap();
	}

	@Test
	public void getReturnsNullForMissingKey() {
		assertNull(map.get("unknown"));
	}

	@Test
	public void putAndGetRoundtrip() {
		StubMetaData md = new StubMetaData();
		map.put("first", md);
		assertNotNull(map.get("first"));
	}

	@Test
	public void putDuplicateKeyThrowsIllegalStateException() {
		map.put("key", new StubMetaData());
		assertThrows(IllegalStateException.class, () -> map.put("key", new StubMetaData()));
	}

	@Test
	public void getMetaDataOfTypeReturnsEmptyListWhenNoEntries() {
		List<StubMetaData> result = map.ofType(StubMetaData.class);
		assertThat(result, empty());
	}

	@Test
	public void getMetaDataOfTypeReturnsMatchingEntries() {
		StubMetaData a = new StubMetaData();
		StubMetaData b = new StubMetaData();
		map.put("a", a);
		map.put("b", b);
		List<StubMetaData> result = map.ofType(StubMetaData.class);
		assertThat(result, containsInAnyOrder(is(a), is(b)));
	}

	@Test
	public void getMetaDataOfTypeFiltersCorrectly() {
		/** Alternative MetaData type. */
		class OtherMetaData implements MetaData {
			@SuppressWarnings("unused")
			private static final long serialVersionUID = 1L;
		}
		map.put("stub", new StubMetaData());
		map.put("other", new OtherMetaData());
		List<StubMetaData> result = map.ofType(StubMetaData.class);
		assertEquals(1, result.size());
	}
}
