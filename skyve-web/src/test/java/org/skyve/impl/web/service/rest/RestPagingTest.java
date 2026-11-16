package org.skyve.impl.web.service.rest;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

/**
 * Unit tests for {@link RestPaging} clamp helpers used by REST list/query endpoints.
 */
class RestPagingTest {

	@Test
	@SuppressWarnings("static-method")
	void safeStartClampsNegativeToZero() {
		assertEquals(0, RestPaging.safeStart(-1));
		assertEquals(0, RestPaging.safeStart(-100));
		assertEquals(0, RestPaging.safeStart(0));
		assertEquals(10, RestPaging.safeStart(10));
	}

	@Test
	@SuppressWarnings("static-method")
	void documentListDefaultsWhenMissingOrZeroSized() {
		// JAX-RS defaults missing start/end to 0 → stock maxResults = -1 (unbounded)
		assertEquals(RestPaging.MAX_REST_PAGE_SIZE, RestPaging.safeDocumentListMaxResults(0, 0));
	}

	@Test
	@SuppressWarnings("static-method")
	void documentListDefaultsWhenStartGreaterThanEnd() {
		assertEquals(RestPaging.MAX_REST_PAGE_SIZE, RestPaging.safeDocumentListMaxResults(10, 9));
	}

	@Test
	@SuppressWarnings("static-method")
	void documentListCapsOversizedPage() {
		// end - start - 1 = 200
		assertEquals(RestPaging.MAX_REST_PAGE_SIZE, RestPaging.safeDocumentListMaxResults(0, 201));
	}

	@Test
	@SuppressWarnings("static-method")
	void documentListPreservesValidSmallPage() {
		// start=0&end=9 → maxResults = 8
		assertEquals(8, RestPaging.safeDocumentListMaxResults(0, 9));
	}

	@Test
	@SuppressWarnings("static-method")
	void documentListPreservesExactMaxPage() {
		// end - start - 1 = 100
		assertEquals(RestPaging.MAX_REST_PAGE_SIZE, RestPaging.safeDocumentListMaxResults(0, 101));
	}

	@Test
	@SuppressWarnings("static-method")
	void documentListUsesClampedStartForNegativeStart() {
		// safeStart(-5)=0 → maxResults = 10 - 0 - 1 = 9
		assertEquals(9, RestPaging.safeDocumentListMaxResults(-5, 10));
		assertEquals(0, RestPaging.safeStart(-5));
	}

	@Test
	@SuppressWarnings("static-method")
	void queryListDefaultsWhenMissingOrZeroSized() {
		// start=0&end=0 → page size 0 → default end = 100
		assertEquals(RestPaging.MAX_REST_PAGE_SIZE, RestPaging.safeQueryEndRow(0, 0));
	}

	@Test
	@SuppressWarnings("static-method")
	void queryListDefaultsWhenStartGreaterThanEnd() {
		// start=10&end=9 → inverted → end = 10 + 100
		assertEquals(110, RestPaging.safeQueryEndRow(10, 9));
	}

	@Test
	@SuppressWarnings("static-method")
	void queryListCapsOversizedPage() {
		assertEquals(RestPaging.MAX_REST_PAGE_SIZE, RestPaging.safeQueryEndRow(0, 500));
		assertEquals(150, RestPaging.safeQueryEndRow(50, 500));
	}

	@Test
	@SuppressWarnings("static-method")
	void queryListPreservesValidSmallPage() {
		assertEquals(9, RestPaging.safeQueryEndRow(0, 9));
	}

	@Test
	@SuppressWarnings("static-method")
	void queryListPreservesExactMaxPage() {
		assertEquals(RestPaging.MAX_REST_PAGE_SIZE, RestPaging.safeQueryEndRow(0, 100));
		assertEquals(150, RestPaging.safeQueryEndRow(50, 150));
	}

	@Test
	@SuppressWarnings("static-method")
	void queryListUsesClampedStartForNegativeStart() {
		// safeStart(-5)=0 → page size from end=10 is 10 → endRow=10
		assertEquals(10, RestPaging.safeQueryEndRow(-5, 10));
		// inverted after clamp: end=-1, safeStart=0 → default page → endRow=100
		assertEquals(RestPaging.MAX_REST_PAGE_SIZE, RestPaging.safeQueryEndRow(-5, -1));
	}
}
