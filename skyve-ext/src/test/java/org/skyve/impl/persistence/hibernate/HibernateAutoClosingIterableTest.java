package org.skyve.impl.persistence.hibernate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.hibernate.ScrollableResults;
import org.junit.jupiter.api.Test;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.messages.DomainException;

@SuppressWarnings({"static-method", "resource"})
class HibernateAutoClosingIterableTest {

	@Test
	@SuppressWarnings("boxing")
	void testIteratorYieldsScalarThenClosesResults() {
		ScrollableResults results = mock(ScrollableResults.class);
		when(results.next()).thenReturn(true, false);
		when(results.get()).thenReturn(new Object[] {"first"});

		HibernateAutoClosingIterable<String> iterable = new HibernateAutoClosingIterable<>(results, true, false);
		var iterator = iterable.iterator();
		assertTrue(iterator.hasNext());
		assertEquals("first", iterator.next());
		assertFalse(iterator.hasNext());
		verify(results).close();
	}

	@Test
	@SuppressWarnings("boxing")
	void testIteratorProjectedYieldsDynamicBean() {
		ScrollableResults results = mock(ScrollableResults.class);
		when(results.next()).thenReturn(true, false);
		when(results.get()).thenReturn(new Object[] {"Bob", "ACTIVE"});

		HibernateAutoClosingIterable<Object> iterable = new HibernateAutoClosingIterable<>(
				"admin",
				"Contact",
				results,
				new String[] {"name", "status"},
				false,
				false);
		var iterator = iterable.iterator();
		assertTrue(iterator.hasNext());
		Object value = iterator.next();
		assertInstanceOf(DynamicBean.class, value);
		DynamicBean bean = (DynamicBean) value;
		assertEquals("admin", bean.getBizModule());
		assertEquals("Contact", bean.getBizDocument());
		assertEquals("Bob", bean.get("name"));
		assertEquals("ACTIVE", bean.get("status"));
		assertFalse(iterator.hasNext());
	}

	@Test
	@SuppressWarnings("boxing")
	void testIteratorThrowsWhenAssertSingleFails() {
		ScrollableResults results = mock(ScrollableResults.class);
		when(results.next()).thenReturn(true);
		when(results.get()).thenReturn(new Object[] {"one", "two"});

		HibernateAutoClosingIterable<String> iterable = new HibernateAutoClosingIterable<>(results, true, false);
		var iterator = iterable.iterator();
		assertTrue(iterator.hasNext());
		assertThrows(IllegalStateException.class, iterator::next);
	}

	@Test
	void testCloseWrapsCloseFailureInDomainException() {
		ScrollableResults results = mock(ScrollableResults.class);
		RuntimeException closeFailure = new RuntimeException("boom");
		doThrow(closeFailure).when(results).close();
		HibernateAutoClosingIterable<String> iterable = new HibernateAutoClosingIterable<>(results, false, false);

		DomainException thrown = assertThrows(DomainException.class, iterable::close);
		assertEquals("Could not close the iterator.", thrown.getMessage());
	}
}
