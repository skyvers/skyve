package org.skyve.impl.persistence;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ManyResultsException;
import org.skyve.domain.messages.NoResultsException;

class AbstractQueryTest {

	// Minimal concrete subclass for testing non-static methods
	private static class StubQuery extends AbstractQuery {
		@Override
		public String toQueryString() {
			return "stub";
		}
	}

	// ---- assertOneResult ----
	@SuppressWarnings("static-method")
	void assertOneResultReturnsSingleElement() {
		List<String> results = Collections.singletonList("hello");
		String result = AbstractQuery.assertOneResult(results);
		assertThat(result, is("hello"));
	}

	@Test
	@SuppressWarnings("static-method")
	void assertOneResultThrowsNoResultsExceptionForEmptyList() {
		List<String> results = Collections.emptyList();
		assertThrows(NoResultsException.class, () -> AbstractQuery.assertOneResult(results));
	}

	@Test
	@SuppressWarnings("static-method")
	void assertOneResultThrowsManyResultsExceptionForMultipleElements() {
		List<String> results = Arrays.asList("a", "b");
		assertThrows(ManyResultsException.class, () -> AbstractQuery.assertOneResult(results));
	}

	// ---- returnOneResult ----

	@Test
	@SuppressWarnings("static-method")
	void returnOneResultReturnsFirstElement() {
		List<String> results = Arrays.asList("first", "second");
		String result = AbstractQuery.returnOneResult(results);
		assertThat(result, is("first"));
	}

	@Test
	@SuppressWarnings("static-method")
	void returnOneResultReturnsNullForEmptyList() {
		List<String> results = Collections.emptyList();
		assertNull(AbstractQuery.returnOneResult(results));
	}

	@Test
	@SuppressWarnings("static-method")
	void returnOneResultReturnsSingleElement() {
		List<String> results = Collections.singletonList("single");
		assertThat(AbstractQuery.returnOneResult(results), is("single"));
	}

	// ---- instance methods ----

	@Test
	@SuppressWarnings("static-method")
	void getParameterNamesEmptyByDefault() {
		assertTrue(new StubQuery().getParameterNames().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getParameterReturnsNullWhenAbsent() {
		assertNull(new StubQuery().getParameter("missing"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getParameterReturnsStoredValue() {
		StubQuery q = new StubQuery();
		q.parameters.put("p1", "value");
		assertThat(q.getParameter("p1"), is("value"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getParameterNamesContainsStoredKey() {
		StubQuery q = new StubQuery();
		q.parameters.put("p1", "value");
		assertTrue(q.getParameterNames().contains("p1"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getDrivingModuleNameNullByDefault() {
		assertNull(new StubQuery().getDrivingModuleName());
	}

	@Test
	@SuppressWarnings("static-method")
	void getDrivingDocumentNameNullByDefault() {
		assertNull(new StubQuery().getDrivingDocumentName());
	}

	@Test
	@SuppressWarnings("static-method")
	void getTimeoutInSecondsDefaultsToZero() {
		assertEquals(0, new StubQuery().getTimeoutInSeconds());
	}

	@Test
	@SuppressWarnings("static-method")
	void setTimeoutInSecondsRoundTrip() {
		StubQuery q = new StubQuery();
		q.setTimeoutInSeconds(30);
		assertEquals(30, q.getTimeoutInSeconds());
	}
}
