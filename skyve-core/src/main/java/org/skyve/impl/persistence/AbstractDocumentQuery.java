package org.skyve.impl.persistence;

import java.util.LinkedHashMap;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect.RDBMS;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.util.Binder;

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
	// projection (bean.<binding> or projectedAlias) -> order
	private LinkedHashMap<String, SortDirection> insertedOrderings = new LinkedHashMap<>();
	// projection (bean.<binding> or projectedAlias) -> order
	private LinkedHashMap<String, SortDirection> appendedOrderings = new LinkedHashMap<>();
	private StringBuilder groupClause = new StringBuilder(32);
	// Used to change the query built slightly for each RDBMS
	private RDBMS rdbms;

	protected AbstractDocumentQuery(String moduleName, String documentName, RDBMS rdbms) {
		AbstractPersistence persistence = AbstractPersistence.get();
		Customer customer = persistence.getUser().getCustomer();
		Module module = customer.getModule(moduleName);
		drivingDocument = module.getDocument(customer, documentName);
		postConstruct(persistence, rdbms, null);
	}

	protected AbstractDocumentQuery(Document document, RDBMS rdbms) {
		drivingDocument = document;
		postConstruct(AbstractPersistence.get(), rdbms, null);
	}

	protected AbstractDocumentQuery(Document document, RDBMS rdbms, String fromClause, String filterClause) {
		drivingDocument = document;
		postConstruct(AbstractPersistence.get(), rdbms, filterClause);
		if (fromClause != null) {
			this.fromClause.setLength(0);
			this.fromClause.append(new AbstractBizQL(fromClause).toQueryString(false));
		}
	}

	protected AbstractDocumentQuery(Bean queryByExampleBean, RDBMS rdbms) throws Exception {
		this(queryByExampleBean.getBizModule(), queryByExampleBean.getBizDocument(), rdbms);

		for (Attribute attribute : drivingDocument.getAttributes()) {
			if (attribute.isPersistent() && (! (attribute instanceof Relation))) {
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
	
	private void postConstruct(AbstractPersistence persistence, @SuppressWarnings("hiding") RDBMS rdbms, String filterClause) {
		drivingModuleName = drivingDocument.getOwningModuleName();
		drivingDocumentName = drivingDocument.getName();
		filter = new DocumentFilterImpl(this, rdbms, filterClause);
		fromClause.append(persistence.getDocumentEntityName(drivingModuleName, drivingDocumentName));
		fromClause.append(" as ").append(THIS_ALIAS);
		this.rdbms = rdbms;
	}

	@Override
	public AbstractDocumentQuery noTimeout() {
		this.timeoutInSeconds = Integer.MIN_VALUE;
		return this;
	}
	
	@Override
	public AbstractDocumentQuery putParameter(String name, Object value) {
		parameters.put(name, value);
		if (UtilImpl.QUERY_TRACE) {
			UtilImpl.LOGGER.info("    SET PARAM " + name + " = " + value);
		}
		return this;
	}
	
	@Override
	public DocumentFilter newDocumentFilter() {
		return new DocumentFilterImpl(this, rdbms);
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
		return addBoundProjection(binding, binding);
	}

	@Override
	public DocumentQuery addBoundProjection(String binding, String projectedAlias) {
		return addBoundProjection(THIS_ALIAS, binding, projectedAlias);
	}

	@Override
	public DocumentQuery addBoundProjection(String entityAlias, String binding, String projectedAlias) {
		if (projectionClause.length() > 0) {
			projectionClause.append(", ");
		}
		projectionClause.append(entityAlias).append('.').append(binding);
		projectionClause.append(" as ").append(BindUtil.sanitiseBinding(projectedAlias));
		return this;
	}

	@Override
	public DocumentQuery addExpressionProjection(String expression, String projectedAlias) {
		if (projectionClause.length() > 0) {
			projectionClause.append(", ");
		}
		projectionClause.append(expression).append(" as ").append(BindUtil.sanitiseBinding(projectedAlias));
		return this;
	}

	@Override
	public DocumentQuery addAggregateProjection(AggregateFunction function, String binding, String projectedAlias) {
		return addAggregateProjection(function, THIS_ALIAS, binding, projectedAlias);
	}
	
	@Override
	public DocumentQuery addAggregateProjection(AggregateFunction function, String entityAlias, String binding, String projectedAlias) {
		if (projectionClause.length() > 0) {
			projectionClause.append(", ");
		}
		projectionClause.append(function.toString().toLowerCase()).append('(');
		// Handle SQLServer which will not automatically cast to a broader type of number and just breaks
		if ((AggregateFunction.Avg.equals(function) || 
					AggregateFunction.Sum.equals(function)) && 
				UtilImpl.DATA_STORE.getDialectClassName().contains("SQLServer")) {
			projectionClause.append("cast(").append(entityAlias).append('.').append(binding).append(" as big_decimal)");
		}
		else {
			projectionClause.append(entityAlias).append('.').append(binding);
		}
		projectionClause.append(") as ").append(BindUtil.sanitiseBinding(projectedAlias));
		return this;
	}

	public void clearProjections() {
		projectionClause.setLength(0);
	}

	public void clearOrderings() {
		insertedOrderings.clear();
		appendedOrderings.clear();
	}
	
	public void clearGroups() {
		groupClause.setLength(0);
	}

	@Override
	public DocumentFilter getFilter() {
		return filter;
	}

	@Override
	public DocumentQuery addBoundOrdering(String binding) {
		return addBoundOrdering(THIS_ALIAS, binding);
	}
	
	@Override
	public DocumentQuery addBoundOrdering(String entityAlias, String binding) {
		return addBoundOrdering(entityAlias, binding, SortDirection.ascending);
	}

	@Override
	public DocumentQuery addBoundOrdering(String binding, SortDirection order) {
		return addBoundOrdering(THIS_ALIAS, binding, order);
	}
	
	@Override
	public DocumentQuery addBoundOrdering(String entityAlias, String binding, SortDirection order) {
		appendedOrderings.put(String.format("%s.%s", entityAlias, binding), order);
		return this;
	}

	@Override
	public DocumentQuery insertBoundOrdering(String binding, SortDirection order) {
		return insertBoundOrdering(THIS_ALIAS, binding, order);
	}
	
	@Override
	public DocumentQuery insertBoundOrdering(String entityAlias, String binding, SortDirection order) {
		insertedOrderings.put(String.format("%s.%s", entityAlias, binding), order);
		return this;
	}

	@Override
	public DocumentQuery addBoundGrouping(String binding) {
		return addBoundGrouping(THIS_ALIAS, binding);
	}
	
	@Override
	public DocumentQuery addBoundGrouping(String entityAlias, String binding) {
		if (groupClause.length() > 0) {
			groupClause.append(", ");
		}
		groupClause.append(entityAlias).append('.').append(binding);
		return this;
	}

	@Override
	public DocumentQuery addExpressionOrdering(String expression) {
		return addExpressionOrdering(expression, SortDirection.ascending);
	}

	@Override
	public DocumentQuery addExpressionOrdering(String expression, SortDirection order) {
		appendedOrderings.put(expression, order);
		return this;
	}

	@Override
	public DocumentQuery insertExpressionOrdering(String expression, SortDirection order) {
		insertedOrderings.put(expression, order);
		return this;
	}

	@Override
	public DocumentQuery addExpressionGrouping(String expression) {
		if (groupClause.length() > 0) {
			groupClause.append(", ");
		}
		groupClause.append(expression);
		return this;
	}

	@Override
	public DocumentQuery addInnerJoin(String referenceBinding) {
		return addInnerJoinFromEntity(THIS_ALIAS, referenceBinding);
	}

	@Override
	public DocumentQuery addInnerJoinFromEntity(String entityAlias, String referenceBinding) {
		fromClause.append(" INNER JOIN ").append(entityAlias).append('.').append(referenceBinding);
		return this;
	}

	@Override
	public DocumentQuery addLeftOuterJoin(String referenceBinding) {
		return addLeftOuterJoinFromEntity(THIS_ALIAS, referenceBinding);
	}
	
	@Override
	public DocumentQuery addLeftOuterJoinFromEntity(String entityAlias, String referenceBinding) {
		fromClause.append(" LEFT OUTER JOIN ").append(entityAlias).append('.').append(referenceBinding);
		return this;
	}

	@Override
	public DocumentQuery addRightOuterJoin(String referenceBinding) {
		return addRightOuterJoinFromEntity(THIS_ALIAS, referenceBinding);
	}
	
	@Override
	public DocumentQuery addRightOuterJoinFromEntity(String entityAlias, String referenceBinding) {
		fromClause.append(" RIGHT OUTER JOIN ").append(entityAlias).append('.').append(referenceBinding);
		return this;
	}

	@Override
	public DocumentQuery addFetchedInnerJoin(String referenceBinding) {
		return addFetchedInnerJoinFromEntity(THIS_ALIAS, referenceBinding);
	}
	
	@Override
	public DocumentQuery addFetchedInnerJoinFromEntity(String entityAlias, String referenceBinding) {
		fromClause.append(" INNER JOIN FETCH ").append(entityAlias).append('.').append(referenceBinding);
		return this;
	}

	@Override
	public DocumentQuery addFetchedLeftOuterJoin(String referenceBinding) {
		return addFetchedLeftOuterJoinFromEntity(THIS_ALIAS, referenceBinding);
	}
	
	@Override
	public DocumentQuery addFetchedLeftOuterJoinFromEntity(String entityAlias, String referenceBinding) {
		fromClause.append(" LEFT OUTER JOIN FETCH ").append(entityAlias).append('.').append(referenceBinding);
		return this;
	}

	@Override
	public DocumentQuery addFetchedRightOuterJoin(String referenceBinding) {
		return addFetchedRightOuterJoinFromEntity(THIS_ALIAS, referenceBinding);
	}
	
	@Override
	public DocumentQuery addFetchedRightOuterJoinFromEntity(String entityAlias, String referenceBinding) {
		fromClause.append(" RIGHT OUTER JOIN FETCH ").append(entityAlias).append('.').append(referenceBinding);
		return this;
	}

	@Override
	public DocumentQuery addInnerJoin(String referenceBinding, String joinAlias) {
		return addInnerJoinFromEntity(THIS_ALIAS, referenceBinding, joinAlias);
	}
	
	@Override
	public DocumentQuery addInnerJoinFromEntity(String entityAlias, String referenceBinding, String joinAlias) {
		fromClause.append(" INNER JOIN ").append(entityAlias).append('.').append(referenceBinding);
		fromClause.append(" as ").append(BindUtil.sanitiseBinding(joinAlias));
		return this;
	}

	@Override
	public DocumentQuery addLeftOuterJoin(String referenceBinding, String joinAlias) {
		return addLeftOuterJoinFromEntity(THIS_ALIAS, referenceBinding, joinAlias);
	}
	
	@Override
	public DocumentQuery addLeftOuterJoinFromEntity(String entityAlias, String referenceBinding, String joinAlias) {
		fromClause.append(" LEFT OUTER JOIN ").append(entityAlias).append('.').append(referenceBinding);
		fromClause.append(" as ").append(BindUtil.sanitiseBinding(joinAlias));
		return this;
	}

	@Override
	public DocumentQuery addRightOuterJoin(String referenceBinding, String joinAlias) {
		return addRightOuterJoinFromEntity(THIS_ALIAS, referenceBinding, joinAlias);
	}
	
	@Override
	public DocumentQuery addRightOuterJoinFromEntity(String entityAlias, String referenceBinding, String joinAlias) {
		fromClause.append(" RIGHT OUTER JOIN ").append(entityAlias).append('.').append(referenceBinding);
		fromClause.append(" as ").append(BindUtil.sanitiseBinding(joinAlias));
		return this;
	}

	@Override
	public DocumentQuery addFetchedInnerJoin(String referenceBinding, String joinAlias) {
		return addFetchedInnerJoinFromEntity(THIS_ALIAS, referenceBinding, joinAlias);
	}
	
	@Override
	public DocumentQuery addFetchedInnerJoinFromEntity(String entityAlias, String referenceBinding, String joinAlias) {
		fromClause.append(" INNER JOIN FETCH ").append(entityAlias).append('.').append(referenceBinding);
		fromClause.append(" as ").append(BindUtil.sanitiseBinding(joinAlias));
		return this;
	}

	@Override
	public DocumentQuery addFetchedLeftOuterJoin(String referenceBinding, String joinAlias) {
		return addFetchedLeftOuterJoinFromEntity(THIS_ALIAS, referenceBinding, joinAlias);
	}
	
	@Override
	public DocumentQuery addFetchedLeftOuterJoinFromEntity(String entityAlias, String referenceBinding, String joinAlias) {
		fromClause.append(" LEFT OUTER JOIN FETCH ").append(entityAlias).append('.').append(referenceBinding);
		fromClause.append(" as ").append(BindUtil.sanitiseBinding(joinAlias));
		return this;
	}

	@Override
	public DocumentQuery addFetchedRightOuterJoin(String referenceBinding, String joinAlias) {
		return addFetchedRightOuterJoinFromEntity(THIS_ALIAS, referenceBinding, joinAlias);
	}
	
	@Override
	public DocumentQuery addFetchedRightOuterJoinFromEntity(String entityAlias, String referenceBinding, String joinAlias) {
		fromClause.append(" RIGHT OUTER JOIN FETCH ").append(entityAlias).append('.').append(referenceBinding);
		fromClause.append(" as ").append(BindUtil.sanitiseBinding(joinAlias));
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
			for (String projection : insertedOrderings.keySet()) {
				SortDirection direction = insertedOrderings.get(projection);
				result.append(projection).append(' ');
				result.append(SortDirection.descending.equals(direction) ? "desc, " : "asc, ");
			}

			// append the appended orderings if the projection is not in the inserted orderings
			for (String projection : appendedOrderings.keySet()) {
				if (! insertedOrderings.containsKey(projection)) {
					SortDirection direction = appendedOrderings.get(projection);
					result.append(projection).append(' ');
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
	public final <T extends Bean> T beanResult() {
		List<T> results = beanResults();
		return AbstractQuery.returnOneResult(results);
	}

	@Override
	public final <T extends Bean> T retrieveBean() {
		List<T> results = beanResults();
		return AbstractQuery.assertOneResult(results);
	}

	@Override
	public final <T extends Bean> T projectedResult() {
		List<T> results = projectedResults();
		return AbstractQuery.returnOneResult(results);
	}

	@Override
	public final <T extends Bean> T retrieveProjected() {
		List<T> results = projectedResults();
		return AbstractQuery.assertOneResult(results);
	}

	@Override
	public final <T> T scalarResult(Class<T> type) {
		List<T> results = scalarResults(type);
		return AbstractQuery.returnOneResult(results);
	}

	@Override
	public final <T> T retrieveScalar(Class<T> type) {
		List<T> results = scalarResults(type);
		return AbstractQuery.assertOneResult(results);
	}

	@Override
	public final Object[] tupleResult() {
		List<Object[]> results = tupleResults();
		return AbstractQuery.returnOneResult(results);
	}

	@Override
	public final Object[] retrieveTuple() {
		List<Object[]> results = tupleResults();
		return AbstractQuery.assertOneResult(results);
	}
}
