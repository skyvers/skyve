package org.skyve.metadata.model.document.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.field.Markup;
import org.skyve.impl.metadata.model.document.field.Memo;

/**
 * Tests for {@link FluentMarkup} and {@link FluentMemo}: constructors
 * and {@code from()} delegation.
 */
@SuppressWarnings("static-method")
class FluentMarkupAndMemoTest {

	// --- FluentMarkup ---

	@Test
	void markupDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentMarkup().get());
	}

	@Test
	void markupWrappingConstructorPreservesInstance() {
		Markup m = new Markup();
		FluentMarkup fm = new FluentMarkup(m);
		assertEquals(m, fm.get());
	}

	@Test
	void markupFromDelegatesAndReturnsThis() {
		Markup source = new Markup();
		source.setName("m1");
		FluentMarkup copy = new FluentMarkup().from(source);
		assertEquals("m1", copy.get().getName());
	}

	// --- FluentMemo ---

	@Test
	void memoDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentMemo().get());
	}

	@Test
	void memoWrappingConstructorPreservesInstance() {
		Memo m = new Memo();
		FluentMemo fm = new FluentMemo(m);
		assertEquals(m, fm.get());
	}

	@Test
	void memoFromDelegatesAndReturnsThis() {
		Memo source = new Memo();
		source.setName("m2");
		FluentMemo copy = new FluentMemo().from(source);
		assertEquals("m2", copy.get().getName());
	}
}
