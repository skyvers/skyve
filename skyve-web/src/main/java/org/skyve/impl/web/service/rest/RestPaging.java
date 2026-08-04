package org.skyve.impl.web.service.rest;

/**
 * Clamps REST list/query paging so omitted or invalid {@code start}/{@code end}
 * cannot yield unbounded result sets.
 *
 * <p>Hibernate only applies {@code maxResults} when it is {@code > 0}. JAX-RS
 * defaults missing query params to {@code 0}, so requests with no paging (or
 * inverted/zero-sized ranges) previously loaded the full result set and could
 * OOM the JVM during {@code populateFully} and JSON marshalling.
 *
 * <p>This type is package-visible and intentionally has no mutable state.
 */
final class RestPaging {

	/** Hard cap on rows returned by built-in REST document-list and query endpoints. */
	static final int MAX_REST_PAGE_SIZE = 100;

	private RestPaging() {
		// utility
	}

	/**
	 * Returns a non-negative first-row index.
	 *
	 * @param start requested start row (may be negative)
	 * @return {@code start} when {@code >= 0}, otherwise {@code 0}
	 */
	static int safeStart(int start) {
		return (start < 0) ? 0 : start;
	}

	/**
	 * Returns a safe {@code maxResults} for the document-list formula
	 * {@code end - start - 1}.
	 *
	 * <p>When the derived size is {@code <= 0} or greater than
	 * {@link #MAX_REST_PAGE_SIZE}, returns {@link #MAX_REST_PAGE_SIZE}.
	 * Otherwise returns the derived size unchanged.
	 *
	 * @param start requested start row (clamped before deriving size)
	 * @param end requested end row
	 * @return clamped max results in {@code 1..MAX_REST_PAGE_SIZE}
	 */
	static int safeDocumentListMaxResults(int start, int end) {
		int maxResults = end - safeStart(start) - 1;
		if ((maxResults <= 0) || (maxResults > MAX_REST_PAGE_SIZE)) {
			return MAX_REST_PAGE_SIZE;
		}
		return maxResults;
	}

	/**
	 * Returns a safe exclusive end row for the query-list formula
	 * {@code pageSize = end - start}.
	 *
	 * <p>When the derived page size is {@code <= 0} or greater than
	 * {@link #MAX_REST_PAGE_SIZE}, the page size becomes
	 * {@link #MAX_REST_PAGE_SIZE}. Otherwise the requested end is preserved
	 * relative to the clamped start.
	 *
	 * @param start requested start row (clamped before deriving size)
	 * @param end requested end row
	 * @return {@code safeStart + clampedPageSize}
	 */
	static int safeQueryEndRow(int start, int end) {
		int safe = safeStart(start);
		int pageSize = end - safe;
		if ((pageSize <= 0) || (pageSize > MAX_REST_PAGE_SIZE)) {
			pageSize = MAX_REST_PAGE_SIZE;
		}
		return safe + pageSize;
	}
}
