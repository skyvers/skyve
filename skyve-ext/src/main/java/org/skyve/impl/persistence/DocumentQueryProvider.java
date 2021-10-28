package org.skyve.impl.persistence;

import java.util.Collection;

import javax.inject.Inject;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.persistence.Persistence;

/**
 * Base class that can be used by service layer classes to reduce boilerplate database retrieval code.
 */
public abstract class DocumentQueryProvider<T extends PersistentBean> {

	protected Document document;

	protected DocumentQueryProvider(String moduleName, String documentName) {
		final Customer c = CORE.getUser().getCustomer();
		final Module m = c.getModule(moduleName);
		document = m.getDocument(c, documentName);
	}

	@Inject
	protected DocumentQueryProvider(Document document) {
		this.document = document;
	}

	protected abstract Persistence getPersistence();

	/**
	 * @param bizId The bizId of the bean to retrieve.
	 * @return The bean with the given bizId.
	 */
	public T get(String bizId) {
		final DocumentQuery query = getDocumentQuery();
		query.getFilter().addEquals(Bean.DOCUMENT_ID, bizId);
		return query.beanResult();
	}

	/**
	 * @param maxResults The maximum number of results to retrieve.
	 * @return A collection of the first maxResults records.
	 */
	public Collection<T> getAll(int maxResults) {
		final DocumentQuery query = getDocumentQuery();
		query.setMaxResults(maxResults);
		return query.beanResults();
	}

	/**
	 * @return All records.
	 */
	public Collection<T> getAll() {
		return getDocumentQuery().beanResults();
	}

	/**
	 * @return The number of records.
	 */
	public long count() {
		final DocumentQuery query = getDocumentQuery();
		query.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "count");
		Long result = query.scalarResult(Long.class);
		return (result == null) ? 0 : result.longValue();
	}

	/**
	 * @param bizId The bizId of the bean to check.
	 * @return True if a bean with the specified bizId exists, otherwise false.
	 */
	public boolean exists(String bizId) {
		return get(bizId) != null;
	}

	/**
	 * @param bizId The bizId of the bean to delete.
	 */
	public void delete(String bizId) {
		final T bean = get(bizId);
		if (bean != null) {
			getPersistence().delete(bean);
		}
	}

	public DocumentQuery getDocumentQuery() {
		return getPersistence().newDocumentQuery(document);
	}
}
