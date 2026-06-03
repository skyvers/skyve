package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.view.TextOutput.Sanitisation;

@SuppressWarnings("static-method")
class ViewBindingsTest {
	@Test
	void viewBindingMergePromotesFlagsAndSanitisation() {
		ViewBinding binding = new ViewBinding(false, false, Sanitisation.relaxed, false);

		binding.merge(false, false, Sanitisation.basic, false);
		binding.merge(true, false, Sanitisation.none, true);
		binding.merge(false, true, null, false);

		assertTrue(binding.isMutable());
		assertTrue(binding.isEscape());
		assertEquals(Sanitisation.basic, binding.getSanitise());
		assertTrue(binding.isInstantiate());
	}

	@Test
	void viewBindingMergeSetsSanitiseWhenCurrentIsNullAndIgnoresDowngrade() {
		ViewBinding binding = new ViewBinding(true, true, null, true);

		binding.merge(false, false, Sanitisation.simple, false);
		assertEquals(Sanitisation.simple, binding.getSanitise());

		binding.merge(false, false, Sanitisation.none, false);
		assertEquals(Sanitisation.simple, binding.getSanitise());
		assertTrue(binding.isMutable());
		assertTrue(binding.isEscape());
		assertTrue(binding.isInstantiate());
	}

	@Test
	void viewBindingsSeedCoreBindingsAndMergePutBinding() {
		Document nonPersistentDocument = mock(Document.class);
		when(nonPersistentDocument.getPersistent()).thenReturn(null);

		ViewBindings bindings = new ViewBindings(nonPersistentDocument);
		assertNotNull(bindings.getBinding(Bean.DOCUMENT_ID));
		assertNull(bindings.getBinding(PersistentBean.LOCK_NAME));

		bindings.putBinding("status", false, true, Sanitisation.none, false);
		bindings.putBinding("status", true, false, Sanitisation.text, true);

		ViewBinding merged = bindings.getBinding("status");
		assertNotNull(merged);
		assertTrue(merged.isMutable());
		assertTrue(merged.isEscape());
		assertEquals(Sanitisation.text, merged.getSanitise());
		assertTrue(merged.isInstantiate());
	}

	@Test
	void viewBindingsChildHierarchyAndFullyQualifiedPrefixes() {
		Document rootDocument = mock(Document.class);
		when(rootDocument.getPersistent()).thenReturn(null);
		Document persistentChildDocument = mock(Document.class);
		when(persistentChildDocument.getPersistent()).thenReturn(new Persistent());

		ViewBindings root = new ViewBindings(rootDocument);
		ViewBindings child = root.putOrGetChild("line", persistentChildDocument);
		ViewBindings sameChild = root.putOrGetChild("line", persistentChildDocument);
		ViewBindings grandChild = child.putOrGetChild("item", rootDocument);

		assertSame(child, sameChild);
		assertSame(root, child.getParent());
		assertEquals("line", child.getBindingPrefix());
		assertEquals("line", child.getFullyQualifiedBindingPrefix());
		assertEquals("line.item", grandChild.getFullyQualifiedBindingPrefix());
		assertTrue(root.getChildren().contains("line"));
		assertNotNull(child.getBinding(Bean.DOCUMENT_ID));
		assertNotNull(child.getBinding(PersistentBean.LOCK_NAME));
		assertFalse(grandChild.getChildren().contains("line"));
	}
}
