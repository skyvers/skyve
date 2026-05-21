package org.skyve.bizport;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class SheetKeyTest {

	@Test
	@SuppressWarnings("static-method")
	void twoArgConstructorSetsFields() {
		SheetKey key = new SheetKey("admin", "User");
		assertThat(key.getModuleName(), is("admin"));
		assertThat(key.getDocumentName(), is("User"));
		assertNull(key.getCollectionBinding());
	}

	@Test
	@SuppressWarnings("static-method")
	void threeArgConstructorSetsAllFields() {
		SheetKey key = new SheetKey("admin", "User", "roles");
		assertThat(key.getModuleName(), is("admin"));
		assertThat(key.getDocumentName(), is("User"));
		assertThat(key.getCollectionBinding(), is("roles"));
	}

	@Test
	@SuppressWarnings("static-method")
	void toStringIncludesModuleAndDocument() {
		SheetKey key = new SheetKey("admin", "User");
		String s = key.toString();
		assertTrue(s.contains("admin"));
		assertTrue(s.contains("User"));
	}

	@Test
	@SuppressWarnings("static-method")
	void toStringWithCollectionIncludesBinding() {
		SheetKey key = new SheetKey("admin", "User", "roles");
		assertTrue(key.toString().contains("roles"));
	}

	@Test
	@SuppressWarnings("static-method")
	void equalSameModuleDocument() {
		SheetKey k1 = new SheetKey("admin", "User");
		SheetKey k2 = new SheetKey("admin", "User");
		assertTrue(k1.equals(k2));
	}

	@Test
	@SuppressWarnings("static-method")
	void notEqualDifferentDocument() {
		SheetKey k1 = new SheetKey("admin", "User");
		SheetKey k2 = new SheetKey("admin", "Group");
		assertFalse(k1.equals(k2));
	}

	@Test
	@SuppressWarnings("static-method")
	void notEqualDifferentModule() {
		SheetKey k1 = new SheetKey("admin", "User");
		SheetKey k2 = new SheetKey("crm", "User");
		assertFalse(k1.equals(k2));
	}

	@Test
	@SuppressWarnings("static-method")
	void equalWithSameCollectionBinding() {
		SheetKey k1 = new SheetKey("admin", "User", "roles");
		SheetKey k2 = new SheetKey("admin", "User", "roles");
		assertTrue(k1.equals(k2));
	}

	@Test
	@SuppressWarnings("static-method")
	void notEqualDifferentCollectionBinding() {
		SheetKey k1 = new SheetKey("admin", "User", "roles");
		SheetKey k2 = new SheetKey("admin", "User", "groups");
		assertFalse(k1.equals(k2));
	}

	@Test
	@SuppressWarnings("static-method")
	void notEqualOneHasCollectionOneDoesNot() {
		SheetKey k1 = new SheetKey("admin", "User");
		SheetKey k2 = new SheetKey("admin", "User", "roles");
		assertFalse(k1.equals(k2));
		assertFalse(k2.equals(k1));
	}

	@Test
	@SuppressWarnings({"static-method", "unlikely-arg-type"})
	void notEqualToNonSheetKey() {
		SheetKey k1 = new SheetKey("admin", "User");
		assertFalse(k1.equals("not a SheetKey"));
	}

	@Test
	@SuppressWarnings("static-method")
	void hashCodeEqualForEqualKeys() {
		SheetKey k1 = new SheetKey("admin", "User");
		SheetKey k2 = new SheetKey("admin", "User");
		assertEquals(k1.hashCode(), k2.hashCode());
	}

	@Test
	@SuppressWarnings("static-method")
	void hashCodeDifferentForDifferentKeys() {
		SheetKey k1 = new SheetKey("admin", "User");
		SheetKey k2 = new SheetKey("admin", "Group");
		assertNotEquals(k1.hashCode(), k2.hashCode());
	}

	@Test
	@SuppressWarnings("static-method")
	void compareToNullReturnsPositive() {
		SheetKey k = new SheetKey("admin", "User");
		assertTrue(k.compareTo(null) > 0);
	}

	@Test
	@SuppressWarnings("static-method")
	void compareToSameKeyReturnsZero() {
		SheetKey k1 = new SheetKey("admin", "User");
		SheetKey k2 = new SheetKey("admin", "User");
		assertEquals(0, k1.compareTo(k2));
	}

	@Test
	@SuppressWarnings("static-method")
	void compareToDocumentSheetBeforeCollectionSheet() {
		SheetKey k1 = new SheetKey("admin", "User");
		SheetKey k2 = new SheetKey("admin", "User", "roles");
		// k1 has no collection → comes before k2
		assertTrue(k1.compareTo(k2) < 0);
		assertTrue(k2.compareTo(k1) > 0);
	}

        @Test
        @SuppressWarnings("static-method")
        void hashCodeWithCollectionBindingIsConsistent() {
                SheetKey k1 = new SheetKey("admin", "User", "roles");
                SheetKey k2 = new SheetKey("admin", "User", "roles");
                assertEquals(k1.hashCode(), k2.hashCode());
        }
}
