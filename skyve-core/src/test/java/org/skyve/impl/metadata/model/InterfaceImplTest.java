package org.skyve.impl.metadata.model;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

/**
 * Tests for {@link InterfaceImpl}.
 */
@SuppressWarnings("static-method")
class InterfaceImplTest {

	@Test
	void setAndGetInterfaceName() {
		InterfaceImpl impl = new InterfaceImpl();
		impl.setInterfaceName("com.example.MyInterface");
		assertEquals("com.example.MyInterface", impl.getInterfaceName());
	}

	@Test
	void equalsReturnsTrueForSameName() {
		InterfaceImpl a = new InterfaceImpl();
		a.setInterfaceName("com.example.A");
		InterfaceImpl b = new InterfaceImpl();
		b.setInterfaceName("com.example.A");
		assertEquals(a, b);
	}

	@Test
	void equalsReturnsFalseForDifferentName() {
		InterfaceImpl a = new InterfaceImpl();
		a.setInterfaceName("com.example.A");
		InterfaceImpl b = new InterfaceImpl();
		b.setInterfaceName("com.example.B");
		assertNotEquals(a, b);
	}

	@Test
	@SuppressWarnings("java:S2159")
	void equalsReturnsFalseForNonInterfaceObject() {
		InterfaceImpl a = new InterfaceImpl();
		a.setInterfaceName("com.example.A");
		assertNotEquals("com.example.A", a);
	}

	@Test
	void equalsReturnsFalseWhenArgIsNotAnInterface() {
		InterfaceImpl a = new InterfaceImpl();
		a.setInterfaceName("com.example.A");
		boolean result = a.equals(null);
		assertFalse(result);
	}

	@Test
	void hashCodeEqualForSameName() {
		InterfaceImpl a = new InterfaceImpl();
		a.setInterfaceName("com.example.A");
		InterfaceImpl b = new InterfaceImpl();
		b.setInterfaceName("com.example.A");
		assertEquals(a.hashCode(), b.hashCode());
	}

	@Test
	void hashCodeDifferentForDifferentNames() {
		InterfaceImpl a = new InterfaceImpl();
		a.setInterfaceName("com.example.A");
		InterfaceImpl b = new InterfaceImpl();
		b.setInterfaceName("com.example.B");
		assertNotEquals(a.hashCode(), b.hashCode());
	}

	@Test
	void toStringContainsName() {
		InterfaceImpl impl = new InterfaceImpl();
		impl.setInterfaceName("com.example.MyInterface");
		assertTrue(impl.toString().contains("com.example.MyInterface"));
	}
}
