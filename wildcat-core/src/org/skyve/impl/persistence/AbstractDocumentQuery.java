package org.skyve.impl.persistence;

import java.util.LinkedHashMap;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.util.Binder;
import org.skyve.impl.persistence.AbstractBizQL;
import org.skyve.impl.persistence.AbstractDocumentQuery;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.AbstractQuery;

public abstract class AbstractDocumentQuery extends AbstractQuery implements DocumentQuery {
	/**
	 * Used to get metadata about the query's driving document
	 */
	protected Document drivingDocument;

	/**
	 * Indicates if the query project is distinct or not
	 */
	private boolean distinct;
	private StringBuilder projectionClause = new StringBuilder(128);
	private StringBuilder fromClause = new StringBuilder(128);
	private DocumentFilter filter;
	private LinkedHashMap<String, SortDirection> insertedOrderings = new LinkedHashMap<>();
	private LinkedHashMap<String, SortDirection> appendedOrderings = new LinkedHashMap<>();
	private StringBuilder groupClause = new StringBuilder(32);

	public AbstractDocumentQuery(String moduleName, String documentName) 
	throws MetaDataException {
		AbstractPersistence persistence = AbstractPersistence.get();
		Customer customer = AbstractPersistence.get().getUser().getCustomer();
		Module module = customer.getModule(moduleName);
		drivingDocument = module.getDocument(customer, documentName);
		postConstruct(persistence, null);
	}

	public AbstractDocumentQuery(Document document) {
		drivingDocument = document;
		postConstruct(AbstractPersistence.get(), null);
	}

	public AbstractDocumentQuery(Document document, String fromClause, String filterClause) {
		drivingDocument = document;
		postConstruct(AbstractPersistence.get(), filterClause);
		if (fromClause != null) {
			this.fromClause.setLength(0);
			this.fromClause.append(new AbstractBizQL(fromClause).toQueryString(false));
		}
	}

	public AbstractDocumentQuery(Bean queryByExampleBean) throws Exception {
		this(queryByExampleBean.getBizModule(), queryByExampleBean.getBizDocument());

		for (Attribute attribute : drivingDocument.getAttributes()) {
			if (! (attribute instanceof Relation)) {
				String attributeName = attribute.getName();
				Object value = Binder.get(queryByExampleBean, attributeName);
				boolean isString = attribute.getAttributeType().getImplementingType().equals(String.class);
				if (isString) {
					value = UtilImpl.processStringValue((String) value);
				}
				if (value != null) {
					if (isString) {
						String string = (String) value;
						StringBuilder operand = new StringBuilder(string.length() + 2);
						operand.append('%').append(string).append('%');
						filter.addLike(attributeName, operand.toString());
					}
					else {
						filter.addEquals(attributeName, value);
					}
				}
			}
		}
	}
	
	private void postConstruct(AbstractPersistence persistence, String filterClause) {
		drivingModuleName = drivingDocument.getOwningModuleName();
		drivingDocumentName = drivingDocument.getName();
		filter = new org.skyve.impl.persistence.DocumentFilterImpl(this, filterClause);
		fromClause.append(persistence.getDocumentEntityName(drivingModuleName, drivingDocumentName));
		fromClause.append(" as ").append(THIS_ALIAS);
	}

	@Override
	public AbstractDocumentQuery putParameter(String name, Object value) {
		parameters.put(name, value);
		return this;
	}
	
	@Override
	public DocumentFilter newDocumentFilter() {
		return new org.skyve.impl.persistence.DocumentFilterImpl(this);
	}

	@Override
	public boolean isDistinct() {
		return distinct;
	}

	@Override
	public DocumentQuery setDistinct(boolean distinct) {
		this.distinct = distinct;
		return this;
	}

	@Override
	public DocumentQuery addThisProjection() {
		if (projectionClause.length() > 0) {
			projectionClause.append(", ");
		}
		projectionClause.append(THIS_ALIAS).append(" as ").append(THIS_ALIAS);
		return this;
	}

	@Override
	public DocumentQuery addBoundProjection(String binding) {
		addBoundProjection(binding, binding);
		return this;
	}

	@Override
	public DocumentQuery addBoundProjection(String binding, String alias) {
		if (projectionClause.length() > 0) {
			projectionClause.append(", ");
		}
		projectionClause.append(THIS_ALIAS).append('.').append(binding);
		projectionClause.append(" as ").append(alias);
		return this;
	}

	@Override
	public DocumentQuery addExpressionProjection(String expression, String alias) {
		if (projectionClause.length() > 0) {
			projectionClause.append(", ");
		}
		projectionClause.append(expression).append(" as ").append(alias);
		return this;
	}

	@Override
	public DocumentQuery addAggregateProjection(AggregateFunction function, String binding, String alias) {
		if (projectionClause.length() > 0) {
			projectionClause.append(", ");
		}
		projectionClause.append(function.toString().toLowerCase()).append('(');
		// Handle SQLServer which will not automatically cast to a broader type of number and just breaks
		if ((AggregateFunction.Avg.equals(function) || 
					AggregateFunction.Sum.equals(function)) && 
				UtilImpl.DIALECT.contains("SQLServer")) {
			projectionClause.append("cast(").append(THIS_ALIAS).append('.').append(binding).append(" as big_decimal)");
		}
		else {
			projectionClause.append(THIS_ALIAS).append('.').append(binding);
		}
		projectionClause.append(") as ").append(alias);
		return this;
	}

	public void clearProjections() {
		projectionClause.setLength(0);
	}

	public void clearOrderings() {
		insertedOrderings.clear();
		appendedOrderings.clear();
	}

	@Override
	public DocumentFilter getFilter() {
		return filter;
	}

	@Override
	public DocumentQuery addOrdering(String binding) {
		addOrdering(binding, SortDirection.ascending);
		return this;
	}

	@Override
	public DocumentQuery addOrdering(String binding, SortDirection order) {
		appendedOrderings.put(binding, order);
		return this;
	}

	@Override
	public DocumentQuery insertOrdering(String binding, SortDirection order) {
		insertedOrderings.put(binding, order);
		return this;
	}

	@Override
	public DocumentQuery addGrouping(String binding) {
		if (groupClause.length() > 0) {
			groupClause.append(", ");
		}
		groupClause.append(THIS_ALIAS).append('.').append(binding);
		return this;
	}

	@Override
	public DocumentQuery addInnerJoin(String associationBinding) {
		fromClause.append(" INNER JOIN ").append(THIS_ALIAS).append('.').append(associationBinding);
		return this;
	}

	@Override
	public DocumentQuery addLeftOuterJoin(String associationBinding) {
		fromClause.append(" LEFT OUTER JOIN ").append(THIS_ALIAS).append('.').append(associationBinding);
		return this;
	}

	@Override
	public DocumentQuery addRightOuterJoin(String associationBinding) {
		fromClause.append(" RIGHT OUTER JOIN ").append(THIS_ALIAS).append('.').append(associationBinding);
		return this;
	}

	@Override
	public String toQueryString() {
		StringBuilder result = new StringBuilder(256);

		result.append("SELECT ").append(distinct ? "DISTINCT " : "");
		if (projectionClause.length() == 0) {
			addThisProjection();
		}
		result.append(projectionClause);
		result.append(" FROM ").append(fromClause);
		String filterClause = filter.toString();
		if (filterClause != null) {
			result.append(" WHERE ").append(filterClause);
		}
		if (groupClause.length() > 0) {
			result.append(" GROUP BY ").append(groupClause);
		}

		if ((! insertedOrderings.isEmpty()) || (! appendedOrderings.isEmpty())) {
			// append the inserted orderings first
			result.append(" ORDER BY ");
			for (String binding : insertedOrderings.keySet()) {
				SortDirection direction = insertedOrderings.get(binding);
				result.append(THIS_ALIAS).append('.').append(binding).append(' ');
				result.append(SortDirection.descending.equals(direction) ? "desc, " : "asc, ");
			}

			// append the appended orderings if the binding is not in the inserted orderings
			for (String binding : appendedOrderings.keySet()) {
				if (! insertedOrderings.containsKey(binding)) {
					SortDirection direction = appendedOrderings.get(binding);
					result.append(THIS_ALIAS).append('.').append(binding).append(' ');
					result.append(SortDirection.descending.equals(direction) ? "desc, " : "asc, ");
				}
			}

			result.setLength(result.length() - 2); // remove the last comma
		}

		return result.toString();
	}

	@Override
	public final Document getDrivingDocument() {
		return drivingDocument;
	}
	
	@Override
	public final <T extends Bean> T beanResult() throws DomainException {
		List<T> results = beanResults();
		return AbstractQuery.returnOneResult(results);
	}

	@Override
	public final <T extends Bean> T retrieveBean() throws DomainException {
		List<T> results = beanResults();
		return AbstractQuery.assertOneResult(results);
	}

	@Override
	public final <T extends Bean> T projectedResult() throws DomainException {
		List<T> results = projectedResults();
		return AbstractQuery.returnOneResult(results);
	}

	@Override
	public final <T extends Bean> T retrieveProjected() throws DomainException {
		List<T> results = projectedResults();
		return AbstractQuery.assertOneResult(results);
	}

	@Override
	public final <T> T scalarResult(Class<T> type) throws DomainException {
		List<T> results = scalarResults(type);
		return AbstractQuery.returnOneResult(results);
	}

	@Override
	public final <T> T retrieveScalar(Class<T> type) throws DomainException {
		List<T> results = scalarResults(type);
		return AbstractQuery.assertOneResult(results);
	}

	@Override
	public final Object[] tupleResult() throws DomainException {
		List<Object[]> results = tupleResults();
		return AbstractQuery.returnOneResult(results);
	}

	@Override
	public final Object[] retrieveTuple() throws DomainException {
		List<Object[]> results = tupleResults();
		return AbstractQuery.assertOneResult(results);
	}
}
