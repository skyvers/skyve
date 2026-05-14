package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.view.access.ViewUserAccessUxUiMetadata;

/**
 * Tests for the abstract base {@link FluentViewUserAccess} via the concrete
 * {@link FluentViewSingularAccess} subclass.
 */
@SuppressWarnings("static-method")
class FluentViewUserAccessTest {

	@Test
	void addUxUiByObjectAppendsEntry() {
		FluentViewSingularAccess fa = new FluentViewSingularAccess();
		ViewUserAccessUxUiMetadata uxui = new ViewUserAccessUxUiMetadata();
		uxui.setName("desktop");
		fa.addUxUi(uxui);
		assertEquals(1, fa.get().getUxuis().size());
		assertEquals("desktop", fa.get().getUxuis().get(0).getName());
	}

	@Test
	void addUxUiByStringAppendsEntry() {
		FluentViewSingularAccess fa = new FluentViewSingularAccess();
		fa.addUxUi("mobile");
		assertEquals(1, fa.get().getUxuis().size());
		assertEquals("mobile", fa.get().getUxuis().get(0).getName());
	}

	@Test
	void addMultipleUxUisBuildsCorrectList() {
		FluentViewSingularAccess fa = new FluentViewSingularAccess();
		fa.addUxUi("desktop").addUxUi("mobile");
		assertEquals(2, fa.get().getUxuis().size());
	}

	@Test
	void removeUxUiDeletesByName() {
		FluentViewSingularAccess fa = new FluentViewSingularAccess();
		fa.addUxUi("desktop").addUxUi("mobile");
		fa.removeUxUi("desktop");
		assertEquals(1, fa.get().getUxuis().size());
		assertEquals("mobile", fa.get().getUxuis().get(0).getName());
	}

	@Test
	void clearUxUisEmptiesList() {
		FluentViewSingularAccess fa = new FluentViewSingularAccess();
		fa.addUxUi("desktop").addUxUi("mobile");
		fa.clearUxUis();
		assertTrue(fa.get().getUxuis().isEmpty());
	}

	@Test
	void getReturnedObjectIsNotNull() {
		assertNotNull(new FluentViewSingularAccess().get());
	}
}
