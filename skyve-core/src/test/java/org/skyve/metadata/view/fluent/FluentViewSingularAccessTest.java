package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.view.access.ViewSingularUserAccessMetaData;

/**
 * Tests for {@link FluentViewSingularAccess}.
 */
@SuppressWarnings("static-method")
class FluentViewSingularAccessTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertNotNull(new FluentViewSingularAccess().get());
	}

	@Test
	void wrappingConstructorPreservesInstance() {
		ViewSingularUserAccessMetaData md = new ViewSingularUserAccessMetaData();
		FluentViewSingularAccess fluent = new FluentViewSingularAccess(md);
		assertEquals(md, fluent.get());
	}

	@Test
	void documentNameSetsValue() {
		FluentViewSingularAccess fluent = new FluentViewSingularAccess().documentName("MyDoc");
		assertEquals("MyDoc", fluent.get().getDocumentName());
	}

	@Test
	void fromSetsDocumentNameAndUxUis() {
		Set<String> uxuis = new HashSet<>(Arrays.asList("desktop", "phone"));
		FluentViewSingularAccess fluent = new FluentViewSingularAccess();
		FluentViewSingularAccess result = fluent.from("Contact", uxuis);
		assertNotNull(result);
		assertEquals("Contact", fluent.get().getDocumentName());
		// check that uxuis were added
		assertEquals(2, fluent.get().getUxuis().size());
	}
}
