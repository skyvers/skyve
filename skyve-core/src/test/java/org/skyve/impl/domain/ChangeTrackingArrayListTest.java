package org.skyve.impl.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class ChangeTrackingArrayListTest {

	/** Minimal concrete AbstractBean for testing purposes. */
	private static class TestBean extends AbstractTransientBean {
		private static final long serialVersionUID = 1L;

		@Override
		public String getBizKey() {
			return "TestBean";
		}

		@Override
		public String getBizModule() {
			return "test";
		}

		@Override
		public String getBizDocument() {
			return "TestBean";
		}
	}

	private TestBean owner;
	private ChangeTrackingArrayList<String> list;

	@BeforeEach
	public void setUp() {
		owner = new TestBean();
		list = new ChangeTrackingArrayList<>("items", owner);
	}

	@Test
	public void addRecordsOriginalValues() {
		list.add("a");
		// original values should now have the empty state captured
		assertTrue(owner.originalValues().containsKey("items"));
	}

	@Test
	public void addAtIndexRecordsOriginalValues() {
		list.add("a");
		// clear original values to test add(int, E)
		owner.originalValues().clear();
		list.add(0, "b");
		assertTrue(owner.originalValues().containsKey("items"));
	}

	@Test
	public void removeByIndexRecordsOriginalValues() {
		list.add("a");
		owner.originalValues().clear();
		list.remove(0);
		assertTrue(owner.originalValues().containsKey("items"));
	}

	@Test
	public void removeByObjectRecordsOriginalValues() {
		list.add("a");
		owner.originalValues().clear();
		list.remove("a");
		assertTrue(owner.originalValues().containsKey("items"));
	}

	@Test
	public void addAllRecordsOriginalValues() {
		list.addAll(Arrays.asList("x", "y"));
		assertTrue(owner.originalValues().containsKey("items"));
	}

	@Test
	public void addAllAtIndexRecordsOriginalValues() {
		list.add("a");
		owner.originalValues().clear();
		list.addAll(0, Arrays.asList("x", "y"));
		assertTrue(owner.originalValues().containsKey("items"));
	}

	@Test
	public void removeAllRecordsOriginalValues() {
		list.add("a");
		list.add("b");
		owner.originalValues().clear();
		list.removeAll(Arrays.asList("a"));
		assertTrue(owner.originalValues().containsKey("items"));
	}

	@Test
	public void removeIfRecordsOriginalValues() {
		list.add("a");
		list.add("b");
		owner.originalValues().clear();
		list.removeIf(s -> s.equals("a"));
		assertTrue(owner.originalValues().containsKey("items"));
	}

	@Test
	public void setRecordsOriginalValues() {
		list.add("a");
		owner.originalValues().clear();
		list.set(0, "new");
		assertTrue(owner.originalValues().containsKey("items"));
	}

	@Test
	public void retainAllRecordsOriginalValues() {
		list.add("a");
		list.add("b");
		owner.originalValues().clear();
		list.retainAll(List.of("a"));
		assertTrue(owner.originalValues().containsKey("items"));
	}

	@Test
	public void replaceAllRecordsOriginalValues() {
		list.add("a");
		owner.originalValues().clear();
		list.replaceAll(String::toUpperCase);
		assertTrue(owner.originalValues().containsKey("items"));
		assertEquals("A", list.get(0));
	}

	@Test
	public void presetCaputuresNonEmptyOriginalList() {
		list.add("original1");
		list.add("original2");
		owner.originalValues().clear();
		// trigger preset via add
		list.add("new");
		// original values should contain the previous 2-element list
		@SuppressWarnings("unchecked")
		List<String> captured = (List<String>) owner.originalValues().get("items");
		assertEquals(2, captured.size());
	}
}
