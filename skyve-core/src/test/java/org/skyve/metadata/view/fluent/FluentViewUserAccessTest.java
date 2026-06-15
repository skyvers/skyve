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

	// ---- lambda$from$0 coverage: forEach in each access class ----

	@Test
	void fluentViewContentAccessFromWithNonEmptyUxUisCoversLambda() {
		FluentViewContentAccess a = new FluentViewContentAccess();
		a.from("contactPhoto", java.util.Set.of("desktop"));
		assertEquals(1, a.get().getUxuis().size());
	}

	@Test
	void fluentViewDocumentAggregateAccessFromWithNonEmptyUxUisCoversLambda() {
		FluentViewDocumentAggregateAccess a = new FluentViewDocumentAggregateAccess();
		a.from("Contact", java.util.Set.of("mobile"));
		assertEquals(1, a.get().getUxuis().size());
	}

	@Test
	void fluentViewDynamicImageAccessFromWithNonEmptyUxUisCoversLambda() {
		FluentViewDynamicImageAccess a = new FluentViewDynamicImageAccess();
		a.from("chartImage", java.util.Set.of("desktop"));
		assertEquals(1, a.get().getUxuis().size());
	}

	@Test
	void fluentViewModelAggregateAccessFromWithNonEmptyUxUisCoversLambda() {
		FluentViewModelAggregateAccess a = new FluentViewModelAggregateAccess();
		a.from("ContactModel", java.util.Set.of("tablet"));
		assertEquals(1, a.get().getUxuis().size());
	}

	@Test
	void fluentViewPreviousCompleteAccessFromWithNonEmptyUxUisCoversLambda() {
		FluentViewPreviousCompleteAccess a = new FluentViewPreviousCompleteAccess();
		a.from("parent", java.util.Set.of("desktop"));
		assertEquals(1, a.get().getUxuis().size());
	}

	@Test
	void fluentViewQueryAggregateAccessFromWithNonEmptyUxUisCoversLambda() {
		FluentViewQueryAggregateAccess a = new FluentViewQueryAggregateAccess();
		a.from("qContacts", java.util.Set.of("mobile"));
		assertEquals(1, a.get().getUxuis().size());
	}

	@Test
	void fluentViewReportAccessFromWithNonEmptyUxUisCoversLambda() {
		FluentViewReportAccess a = new FluentViewReportAccess();
		a.from("admin", "Contact", "ContactReport", java.util.Set.of("desktop"));
		assertEquals(1, a.get().getUxuis().size());
	}
}
