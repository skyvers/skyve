package org.skyve.impl.persistence;

import java.util.Collections;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.AutoClosingIterableAdpater;
import org.skyve.persistence.BizQL;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;

import jakarta.annotation.Nonnull;

/**
 * Abstract implementation of {@link org.skyve.persistence.BizQL} providing
 * parameter binding and timeout management for Skyve BizQL string queries.
 *
 * <p>Subclasses supply query execution by translating the BizQL string into a
 * back-end-specific query (typically HQL via Hibernate).
 *
 * @see org.skyve.persistence.BizQL
 */
public class AbstractBizQL extends AbstractQuery implements BizQL {
	private String query;
	private String resolvedQuery;
	
	private static final Logger QUERY_LOGGER = Category.QUERY.logger();

	/**
	 * Creates a BizQL query wrapper around the supplied Skyve query string.
	 *
	 * <p>The query may contain {@code {module.Document}} tokens that are resolved to
	 * persistence entity names when {@link #toQueryString()} is called.
	 *
	 * @param query the BizQL query text; must not be {@code null}
	 */
	public AbstractBizQL(@Nonnull String query) {
		this.query = query;
	}

	/**
	 * Disables execution timeout enforcement for this query instance.
	 *
	 * <p>Side effects: mutates this query object's timeout state.
	 *
	 * @return this query for fluent chaining
	 */
	@Override
	public AbstractBizQL noTimeout() {
		this.timeoutInSeconds = Integer.MIN_VALUE;
		return this;
	}
	
	/**
	 * Binds a named parameter used by this BizQL query.
	 *
	 * <p>Side effects: stores the parameter in this query instance and may emit a
	 * trace log entry when query tracing is enabled.
	 *
	 * @param name the parameter name without the leading colon
	 * @param value the parameter value, which may be {@code null}
	 * @return this query for fluent chaining
	 */
	@Override
	public AbstractBizQL putParameter(String name, Object value) {
		parameters.put(name, value);
		if (UtilImpl.QUERY_TRACE) {
		    QUERY_LOGGER.info("    SET PARAM {} = {}", name, value);
		}
		return this;
	}
	
	/**
	 * Returns the resolved executable query string.
	 *
	 * <p>Side effects: lazily resolves and caches document tokens on first call.
	 *
	 * @return the resolved query text ready for execution
	 */
	@Override
	public String toQueryString() {
		return toQueryString(true);
	}

	@Nonnull String toQueryString(boolean checkForMalformation) {
		if (resolvedQuery == null) {
			try {
				resolveDocuments(checkForMalformation);
			}
			catch (Exception e) {
				throw new IllegalStateException("Could not resolve and secure query " + query, e);
			}
		}

		return resolvedQuery;
	}
	
	/**
	 * Resolves all {@code {module.Document}} tokens in the query to entity names.
	 *
	 * <p>Side effects: updates this instance's driving module/document fields and
	 * stores the resolved query string for reuse.
	 *
	 * @param checkForMalformation whether to enforce brace-delimited token checks
	 * @throws DomainException if the query contains malformed document tokens
	 */
	protected final void resolveDocuments(boolean checkForMalformation) {
		AbstractPersistence persistence = AbstractPersistence.get();

		StringBuilder result = new StringBuilder(query);
		
		int openBraceIndex = result.indexOf("{");
		if (checkForMalformation && (openBraceIndex < 0)) {
			throw new DomainException("Malformed select statement - no opening curly brace to delimit a document.  Use 'select bean from {admin.User} as bean'");
		}
		int closeBraceIndex = result.indexOf("}");

		while (openBraceIndex >= 0) {
			if (closeBraceIndex < 0) {
				throw new DomainException("Malformed select statement - no closing curly brace to delimit a document.  Use 'select bean from {admin.User} as bean'");
			}

			String moduleDotDocument = result.substring(openBraceIndex + 1, closeBraceIndex);
			int dotIndex = moduleDotDocument.indexOf('.');
			if (dotIndex < 0) {
				throw new DomainException("A document needs to be of the form <module>.<document>");
			}
			String moduleName = moduleDotDocument.substring(0, dotIndex);
			if (drivingModuleName == null) {
				drivingModuleName = moduleName;
			}
			String documentName = moduleDotDocument.substring(dotIndex + 1);
			if (drivingDocumentName == null) {
				drivingDocumentName = documentName;
			}
			result.replace(openBraceIndex, closeBraceIndex + 1, persistence.getDocumentEntityName(moduleName, documentName));

			openBraceIndex = result.indexOf("{");
			closeBraceIndex = result.indexOf("}");
		}

		resolvedQuery = result.toString();
	}

	/**
	 * Returns the corresponding query result using the current query state.
	 *
	 * <p>This base implementation returns an empty result set.
	 */
	@Override
	public <T extends Bean> List<T> beanResults() {
		// No-op
		return Collections.emptyList();
	}

	/**
	 * Returns the corresponding query result using the current query state.
	 */
	@Override
	public final <T extends Bean> T retrieveBean() {
		List<T> results = beanResults();
		return AbstractQuery.assertOneResult(results);
	}

	/**
	 * Returns the corresponding query result using the current query state.
	 */
	@Override
	public final <T extends Bean> T beanResult() {
		List<T> results = beanResults();
		return AbstractQuery.returnOneResult(results);
	}

	/**
	 * Returns the corresponding query result using the current query state.
	 *
	 * <p>This base implementation returns an empty iterable.
	 */
	@Override
	@SuppressWarnings("resource")
	public <T extends Bean> AutoClosingIterable<T> beanIterable() {
		// No-op
		return new AutoClosingIterableAdpater<>(Collections.emptyList());
	}

	/**
	 * Returns the corresponding query result using the current query state.
	 *
	 * <p>This base implementation returns an empty result set.
	 */
	@Override
	public <T extends Bean> List<T> projectedResults() {
		// No-op
		return Collections.emptyList();
	}

	/**
	 * Returns the corresponding query result using the current query state.
	 */
	@Override
	public final <T extends Bean> T projectedResult() {
		List<T> results = projectedResults();
		return AbstractQuery.returnOneResult(results);
	}

	/**
	 * Returns the corresponding query result using the current query state.
	 */
	@Override
	public final <T extends Bean> T retrieveProjected() {
		List<T> results = projectedResults();
		return AbstractQuery.assertOneResult(results);
	}

	/**
	 * Returns the corresponding query result using the current query state.
	 *
	 * <p>This base implementation returns an empty iterable.
	 */
	@Override
	@SuppressWarnings("resource")
	public <T extends Bean> AutoClosingIterable<T> projectedIterable() {
		// No-op
		return new AutoClosingIterableAdpater<>(Collections.emptyList());
	}

	/**
	 * Returns the corresponding query result using the current query state.
	 *
	 * <p>This base implementation returns an empty result set.
	 */
	@Override
	public <T> List<T> scalarResults(Class<T> type) {
		// No-op
		return Collections.emptyList();
	}

	/**
	 * Returns the corresponding query result using the current query state.
	 */
	@Override
	public final <T> T retrieveScalar(Class<T> type) {
		List<T> results = scalarResults(type);
		return AbstractQuery.assertOneResult(results);
	}

	/**
	 * Returns the corresponding query result using the current query state.
	 */
	@Override
	public final <T> T scalarResult(Class<T> type) {
		List<T> results = scalarResults(type);
		return AbstractQuery.returnOneResult(results);
	}

	/**
	 * Returns the corresponding query result using the current query state.
	 *
	 * <p>This base implementation returns an empty iterable.
	 */
	@Override
	@SuppressWarnings("resource")
	public <T> AutoClosingIterable<T> scalarIterable(Class<T> type) {
		// No-op
		return new AutoClosingIterableAdpater<>(Collections.emptyList());
	}

	/**
	 * Returns the corresponding query result using the current query state.
	 *
	 * <p>This base implementation returns an empty result set.
	 */
	@Override
	public List<Object[]> tupleResults() {
		// No-op
		return Collections.emptyList();
	}

	/**
	 * Returns the corresponding query result using the current query state.
	 */
	@Override
	public final Object[] retrieveTuple() {
		List<Object[]> results = tupleResults();
		return AbstractQuery.assertOneResult(results);
	}

	/**
	 * Returns the corresponding query result using the current query state.
	 */
	@Override
	public final Object[] tupleResult() {
		List<Object[]> results = tupleResults();
		return AbstractQuery.returnOneResult(results);
	}

	/**
	 * Returns the corresponding query result using the current query state.
	 *
	 * <p>This base implementation returns an empty iterable.
	 */
	@Override
	@SuppressWarnings("resource")
	public AutoClosingIterable<Object[]> tupleIterable() {
		// No-op
		return new AutoClosingIterableAdpater<>(Collections.emptyList());
	}

	/**
	 * Executes the current statement and returns the affected-row count.
	 *
	 * <p>This base implementation performs no update and returns {@code 0}.
	 */
	@Override
	public int execute() {
		// No-op
		return 0;
	}

	/**
	 * Sets this value on the current object and returns it for fluent chaining.
	 *
	 * <p>This base implementation is a no-op and returns this query unchanged.
	 */
	@Override
	public BizQL setFirstResult(int first) {
		// No-op
		return this;
	}

	/**
	 * Sets this value on the current object and returns it for fluent chaining.
	 *
	 * <p>This base implementation is a no-op and returns this query unchanged.
	 */
	@Override
	public BizQL setMaxResults(int max) {
		// No-op
		return this;
	}
}
