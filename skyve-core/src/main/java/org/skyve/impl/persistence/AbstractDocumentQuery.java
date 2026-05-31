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
import org.skyve.util.logging.Category;
import org.slf4j.Logger;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Abstract implementation of {@link DocumentQuery} providing common document-query
 * infrastructure: projection, filter composition, ordering, paging, and tagging.
 *
 * <p>Subclasses supply the ORM-specific query execution via
 * {@link #projectedResults}, {@link #beanResults}, and related methods.
 *
 * @see org.skyve.persistence.DocumentQuery
 */
public abstract class AbstractDocumentQuery extends AbstractQuery implements DocumentQuery {

    private static final Logger QUERY_LOGGER = Category.QUERY.logger();

	/**
	 * Metadata for the document that anchors the query's from-clause.
	 */
	protected Document drivingDocument;

	/**
	 * Indicates whether the query renders a {@code SELECT DISTINCT} projection.
	 */
	private boolean distinct;
	private StringBuilder projectionClause = new StringBuilder(128);
	private StringBuilder fromClause = new StringBuilder(128);
	private DocumentFilter filter = null;
	// projection (bean.<binding> or projectedAlias) -> order
	private LinkedHashMap<String, SortDirection> insertedOrderings = new LinkedHashMap<>();
	// projection (bean.<binding> or projectedAlias) -> order
	private LinkedHashMap<String, SortDirection> appendedOrderings = new LinkedHashMap<>();
	private StringBuilder groupClause = new StringBuilder(32);
	private String orderClause = null;
	// Used to change the query built slightly for each RDBMS
	private RDBMS rdbms;

	/**
	 * Creates a document query from module and document names.
	 *
	 * @param moduleName the module owning the driving document
	 * @param documentName the driving document name
	 * @param rdbms optional dialect hint used for filter generation
	 */
	protected AbstractDocumentQuery(@Nonnull String moduleName, @Nonnull String documentName, @Nullable RDBMS rdbms) {
		AbstractPersistence persistence = AbstractPersistence.get();
		Customer customer = persistence.getUser().getCustomer();
		Module module = customer.getModule(moduleName);
		drivingDocument = module.getDocument(customer, documentName);
		postConstruct(persistence, rdbms, null, null, null);
	}

	/**
	 * Creates a document query from an already-resolved driving document.
	 *
	 * @param document the driving document metadata
	 * @param rdbms optional dialect hint used for filter generation
	 */
	protected AbstractDocumentQuery(@Nonnull Document document, @Nullable RDBMS rdbms) {
		drivingDocument = document;
		postConstruct(AbstractPersistence.get(), rdbms, null, null, null);
	}

	/**
	 * Creates a document query with pre-seeded from/filter/group/order clauses.
	 *
	 * <p>Side effects: resolves BizQL-style document tokens in the supplied
	 * {@code fromClause} when present.
	 *
	 * @param document the driving document metadata
	 * @param rdbms optional dialect hint used for filter generation
	 * @param fromClause optional from-clause text, supporting BizQL document tokens
	 * @param filterClause optional where-clause expression
	 * @param groupClause optional group-by expression list
	 * @param orderClause optional order-by expression list
	 */
	protected AbstractDocumentQuery(@Nonnull Document document,
										@Nullable RDBMS rdbms,
										@Nullable String fromClause,
										@Nullable String filterClause,
										@Nullable String groupClause,
										@Nullable String orderClause) {
		drivingDocument = document;
		postConstruct(AbstractPersistence.get(), rdbms, filterClause, groupClause, orderClause);
		if (fromClause != null) {
			this.fromClause.setLength(0);
			this.fromClause.append(new AbstractBizQL(fromClause).toQueryString(false));
		}
	}

	/**
	 * Creates a query-by-example query from non-null scalar attribute values.
	 *
	 * <p>String values are normalised and matched using {@code like}; other scalar
	 * persistent values are matched with equality predicates.
	 *
	 * @param queryByExampleBean bean providing criteria values
	 * @param rdbms optional dialect hint used for filter generation
	 */
	protected AbstractDocumentQuery(@Nonnull Bean queryByExampleBean, @Nullable RDBMS rdbms) {
		this(queryByExampleBean.getBizModule(), queryByExampleBean.getBizDocument(), rdbms);

		for (Attribute attribute : drivingDocument.getAttributes()) {
			if (attribute.isPersistent() && (! (attribute instanceof Relation))) {
				String attributeName = attribute.getName();
				Object value = Binder.get(queryByExampleBean, attributeName);
				boolean isString = attribute.getImplementingType().equals(String.class);
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
	
	/**
	 * Initialises common query state after constructor resolution.
	 *
	 * <p>Side effects: creates a dialect-aware {@link DocumentFilterImpl}, resolves
	 * the default entity name for the driving document, and seeds optional group/order
	 * clauses.
	 *
	 * @param persistence active persistence context used for entity-name resolution
	 * @param rdbms optional dialect hint used for filter generation
	 * @param filterClause optional where-clause expression to seed into the filter
	 * @param groupClause optional group-by expression list
	 * @param orderClause optional order-by expression list
	 */
	private void postConstruct(@Nonnull AbstractPersistence persistence,
								@SuppressWarnings("hiding") @Nullable RDBMS rdbms,
								@Nullable String filterClause,
								@SuppressWarnings("hiding") @Nullable String groupClause,
								@SuppressWarnings("hiding") @Nullable String orderClause) {
		drivingModuleName = drivingDocument.getOwningModuleName();
		drivingDocumentName = drivingDocument.getName();
		filter = new DocumentFilterImpl(this, rdbms, filterClause);
		fromClause.append(persistence.getDocumentEntityName(drivingModuleName, drivingDocumentName));
		fromClause.append(" as ").append(THIS_ALIAS);
		if (groupClause != null) {
			this.groupClause.append(groupClause);
		}
		this.orderClause = orderClause;
		this.rdbms = rdbms;
	}

	/**
	 * Disables execution timeout enforcement for this query instance.
	 *
	 * @return this query for fluent chaining
	 */
	@Override
	public AbstractDocumentQuery noTimeout() {
		this.timeoutInSeconds = Integer.MIN_VALUE;
		return this;
	}
	
	/**
	 * Binds a named parameter used by this document query.
	 *
	 * <p>Side effects: stores the parameter in this query instance and may emit a
	 * trace log entry when query tracing is enabled.
	 *
	 * @param name the parameter name without the leading colon
	 * @param value the value to bind, which may be {@code null}
	 * @return this query for fluent chaining
	 */
	@Override
	public AbstractDocumentQuery putParameter(String name, Object value) {
		parameters.put(name, value);
		if (UtilImpl.QUERY_TRACE) {
		    QUERY_LOGGER.info("    SET PARAM {} = {}", name, value);
		}
		return this;
	}
	
	/**
	 * Creates a new mutable filter compatible with this query's dialect hint.
	 *
	 * @return a new empty filter
	 */
	@Override
	public DocumentFilter newDocumentFilter() {
		return new DocumentFilterImpl(this, rdbms);
	}

	/**
	 * Indicates whether this query currently renders with {@code DISTINCT}.
	 * @return the resulting value
	 */
	@Override
	public boolean isDistinct() {
		return distinct;
	}

	/**
	 * Sets whether this query should render a {@code SELECT DISTINCT} clause.
	 *
	 * @param distinct {@code true} to force distinct projection rows
	 * @return this query for fluent chaining
	 */
	@Override
	public DocumentQuery setDistinct(boolean distinct) {
		this.distinct = distinct;
		return this;
	}

	/**
	 * Adds the driving bean projection ({@code this as this}) to the select clause.
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addThisProjection() {
		if (projectionClause.length() > 0) {
			projectionClause.append(", ");
		}
		projectionClause.append(THIS_ALIAS).append(" as ").append(THIS_ALIAS);
		return this;
	}

	/**
	 * Adds a projection for the given driving-document binding using the same alias.
	 * @param binding the binding value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addBoundProjection(String binding) {
		return addBoundProjection(binding, binding);
	}

	/**
	 * Adds a projection for the given driving-document binding using a custom alias.
	 * @param binding the binding value
	 * @param projectedAlias the projectedAlias value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addBoundProjection(String binding, String projectedAlias) {
		return addBoundProjection(THIS_ALIAS, binding, projectedAlias);
	}

	/**
	 * Adds a projection for the specified entity alias and binding.
	 * @param entityAlias the entityAlias value
	 * @param binding the binding value
	 * @param projectedAlias the projectedAlias value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addBoundProjection(String entityAlias, String binding, String projectedAlias) {
		if (projectionClause.length() > 0) {
			projectionClause.append(", ");
		}
		projectionClause.append(entityAlias).append('.').append(binding);
		projectionClause.append(" as ").append(BindUtil.sanitiseBinding(projectedAlias));
		return this;
	}

	/**
	 * Adds a raw expression projection with the provided projected alias.
	 * @param expression the expression value
	 * @param projectedAlias the projectedAlias value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addExpressionProjection(String expression, String projectedAlias) {
		if (projectionClause.length() > 0) {
			projectionClause.append(", ");
		}
		projectionClause.append(expression).append(" as ").append(BindUtil.sanitiseBinding(projectedAlias));
		return this;
	}

	/**
	 * Adds an aggregate projection for a driving-document binding.
	 * @param function the function value
	 * @param binding the binding value
	 * @param projectedAlias the projectedAlias value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addAggregateProjection(AggregateFunction function, String binding, String projectedAlias) {
		return addAggregateProjection(function, THIS_ALIAS, binding, projectedAlias);
	}
	
	/**
	 * Adds an aggregate projection expression to the select clause.
	 *
	 * <p>On SQL Server, {@code avg} and {@code sum} are cast to
	 * {@code big_decimal} to avoid dialect type-width issues.
	 *
	 * @param function aggregate function to apply
	 * @param entityAlias entity alias containing {@code binding}
	 * @param binding projected binding/expression
	 * @param projectedAlias projected column alias
	 * @return this query for fluent chaining
	 */
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

	/**
	 * Removes all configured projections from this query.
	 */
	public void clearProjections() {
		projectionClause.setLength(0);
	}

	/**
	 * Removes all configured orderings and seeded order clauses.
	 */
	public void clearOrderings() {
		insertedOrderings.clear();
		appendedOrderings.clear();
		orderClause = null;
	}
	
	/**
	 * Removes all configured grouping expressions.
	 */
	public void clearGroups() {
		groupClause.setLength(0);
	}

	/**
	 * Returns the mutable filter currently attached to this query.
	 * @return the resulting value
	 */
	@Override
	public DocumentFilter getFilter() {
		return filter;
	}

	/**
	 * Appends an ascending ordering for a driving-document binding.
	 * @param binding the binding value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addBoundOrdering(String binding) {
		return addBoundOrdering(THIS_ALIAS, binding);
	}
	
	/**
	 * Appends an ascending ordering for the specified entity alias and binding.
	 * @param entityAlias the entityAlias value
	 * @param binding the binding value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addBoundOrdering(String entityAlias, String binding) {
		return addBoundOrdering(entityAlias, binding, SortDirection.ascending);
	}

	/**
	 * Appends an ordering for a driving-document binding with the given direction.
	 * @param binding the binding value
	 * @param order the order value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addBoundOrdering(String binding, SortDirection order) {
		return addBoundOrdering(THIS_ALIAS, binding, order);
	}
	
	/**
	 * Appends an ordering for the specified entity alias and binding.
	 * @param entityAlias the entityAlias value
	 * @param binding the binding value
	 * @param order the order value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addBoundOrdering(String entityAlias, String binding, SortDirection order) {
		appendedOrderings.put(String.format("%s.%s", entityAlias, binding), order);
		return this;
	}

	/**
	 * Inserts a driving-document ordering ahead of appended orderings.
	 * @param binding the binding value
	 * @param order the order value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery insertBoundOrdering(String binding, SortDirection order) {
		return insertBoundOrdering(THIS_ALIAS, binding, order);
	}
	
	/**
	 * Inserts an ordering for the specified entity alias and binding ahead of appended orderings.
	 * @param entityAlias the entityAlias value
	 * @param binding the binding value
	 * @param order the order value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery insertBoundOrdering(String entityAlias, String binding, SortDirection order) {
		insertedOrderings.put(String.format("%s.%s", entityAlias, binding), order);
		return this;
	}

	/**
	 * Appends a group-by item for a driving-document binding.
	 * @param binding the binding value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addBoundGrouping(String binding) {
		return addBoundGrouping(THIS_ALIAS, binding);
	}
	
	/**
	 * Appends a group-by item for the specified entity alias and binding.
	 * @param entityAlias the entityAlias value
	 * @param binding the binding value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addBoundGrouping(String entityAlias, String binding) {
		if (groupClause.length() > 0) {
			groupClause.append(", ");
		}
		groupClause.append(entityAlias).append('.').append(binding);
		return this;
	}

	/**
	 * Appends an ascending ordering using a raw expression.
	 * @param expression the expression value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addExpressionOrdering(String expression) {
		return addExpressionOrdering(expression, SortDirection.ascending);
	}

	/**
	 * Appends an ordering using a raw expression.
	 * @param expression the expression value
	 * @param order the order value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addExpressionOrdering(String expression, SortDirection order) {
		appendedOrderings.put(expression, order);
		return this;
	}

	/**
	 * Inserts an ordering expression ahead of appended orderings.
	 * @param expression the expression value
	 * @param order the order value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery insertExpressionOrdering(String expression, SortDirection order) {
		insertedOrderings.put(expression, order);
		return this;
	}

	/**
	 * Appends a raw expression to the group-by clause.
	 * @param expression the expression value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addExpressionGrouping(String expression) {
		if (groupClause.length() > 0) {
			groupClause.append(", ");
		}
		groupClause.append(expression);
		return this;
	}

	/**
	 * Adds an inner join from the driving entity using the reference binding.
	 * @param referenceBinding the referenceBinding value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addInnerJoin(String referenceBinding) {
		return addInnerJoinFromEntity(THIS_ALIAS, referenceBinding);
	}

	/**
	 * Adds an inner join from the given entity alias using the reference binding.
	 * @param entityAlias the entityAlias value
	 * @param referenceBinding the referenceBinding value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addInnerJoinFromEntity(String entityAlias, String referenceBinding) {
		fromClause.append(" INNER JOIN ").append(entityAlias).append('.').append(referenceBinding);
		return this;
	}

	/**
	 * Adds a left outer join from the driving entity using the reference binding.
	 * @param referenceBinding the referenceBinding value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addLeftOuterJoin(String referenceBinding) {
		return addLeftOuterJoinFromEntity(THIS_ALIAS, referenceBinding);
	}
	
	/**
	 * Adds a left outer join from the given entity alias using the reference binding.
	 * @param entityAlias the entityAlias value
	 * @param referenceBinding the referenceBinding value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addLeftOuterJoinFromEntity(String entityAlias, String referenceBinding) {
		fromClause.append(" LEFT OUTER JOIN ").append(entityAlias).append('.').append(referenceBinding);
		return this;
	}

	/**
	 * Adds a right outer join from the driving entity using the reference binding.
	 * @param referenceBinding the referenceBinding value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addRightOuterJoin(String referenceBinding) {
		return addRightOuterJoinFromEntity(THIS_ALIAS, referenceBinding);
	}
	
	/**
	 * Adds a right outer join from the given entity alias using the reference binding.
	 * @param entityAlias the entityAlias value
	 * @param referenceBinding the referenceBinding value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addRightOuterJoinFromEntity(String entityAlias, String referenceBinding) {
		fromClause.append(" RIGHT OUTER JOIN ").append(entityAlias).append('.').append(referenceBinding);
		return this;
	}

	/**
	 * Adds an inner fetch join from the driving entity using the reference binding.
	 * @param referenceBinding the referenceBinding value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addFetchedInnerJoin(String referenceBinding) {
		return addFetchedInnerJoinFromEntity(THIS_ALIAS, referenceBinding);
	}
	
	/**
	 * Adds an inner fetch join from the given entity alias using the reference binding.
	 * @param entityAlias the entityAlias value
	 * @param referenceBinding the referenceBinding value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addFetchedInnerJoinFromEntity(String entityAlias, String referenceBinding) {
		fromClause.append(" INNER JOIN FETCH ").append(entityAlias).append('.').append(referenceBinding);
		return this;
	}

	/**
	 * Adds a left outer fetch join from the driving entity using the reference binding.
	 * @param referenceBinding the referenceBinding value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addFetchedLeftOuterJoin(String referenceBinding) {
		return addFetchedLeftOuterJoinFromEntity(THIS_ALIAS, referenceBinding);
	}
	
	/**
	 * Adds a left outer fetch join from the given entity alias using the reference binding.
	 * @param entityAlias the entityAlias value
	 * @param referenceBinding the referenceBinding value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addFetchedLeftOuterJoinFromEntity(String entityAlias, String referenceBinding) {
		fromClause.append(" LEFT OUTER JOIN FETCH ").append(entityAlias).append('.').append(referenceBinding);
		return this;
	}

	/**
	 * Adds a right outer fetch join from the driving entity using the reference binding.
	 * @param referenceBinding the referenceBinding value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addFetchedRightOuterJoin(String referenceBinding) {
		return addFetchedRightOuterJoinFromEntity(THIS_ALIAS, referenceBinding);
	}
	
	/**
	 * Adds a right outer fetch join from the given entity alias using the reference binding.
	 * @param entityAlias the entityAlias value
	 * @param referenceBinding the referenceBinding value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addFetchedRightOuterJoinFromEntity(String entityAlias, String referenceBinding) {
		fromClause.append(" RIGHT OUTER JOIN FETCH ").append(entityAlias).append('.').append(referenceBinding);
		return this;
	}

	/**
	 * Adds an aliased inner join from the driving entity.
	 * @param referenceBinding the referenceBinding value
	 * @param joinAlias the joinAlias value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addInnerJoin(String referenceBinding, String joinAlias) {
		return addInnerJoinFromEntity(THIS_ALIAS, referenceBinding, joinAlias);
	}
	
	/**
	 * Adds an aliased inner join from the given entity alias.
	 * @param entityAlias the entityAlias value
	 * @param referenceBinding the referenceBinding value
	 * @param joinAlias the joinAlias value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addInnerJoinFromEntity(String entityAlias, String referenceBinding, String joinAlias) {
		fromClause.append(" INNER JOIN ").append(entityAlias).append('.').append(referenceBinding);
		fromClause.append(" as ").append(BindUtil.sanitiseBinding(joinAlias));
		return this;
	}

	/**
	 * Adds an aliased left outer join from the driving entity.
	 * @param referenceBinding the referenceBinding value
	 * @param joinAlias the joinAlias value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addLeftOuterJoin(String referenceBinding, String joinAlias) {
		return addLeftOuterJoinFromEntity(THIS_ALIAS, referenceBinding, joinAlias);
	}
	
	/**
	 * Adds an aliased left outer join from the given entity alias.
	 * @param entityAlias the entityAlias value
	 * @param referenceBinding the referenceBinding value
	 * @param joinAlias the joinAlias value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addLeftOuterJoinFromEntity(String entityAlias, String referenceBinding, String joinAlias) {
		fromClause.append(" LEFT OUTER JOIN ").append(entityAlias).append('.').append(referenceBinding);
		fromClause.append(" as ").append(BindUtil.sanitiseBinding(joinAlias));
		return this;
	}

	/**
	 * Adds an aliased right outer join from the driving entity.
	 * @param referenceBinding the referenceBinding value
	 * @param joinAlias the joinAlias value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addRightOuterJoin(String referenceBinding, String joinAlias) {
		return addRightOuterJoinFromEntity(THIS_ALIAS, referenceBinding, joinAlias);
	}
	
	/**
	 * Adds an aliased right outer join from the given entity alias.
	 * @param entityAlias the entityAlias value
	 * @param referenceBinding the referenceBinding value
	 * @param joinAlias the joinAlias value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addRightOuterJoinFromEntity(String entityAlias, String referenceBinding, String joinAlias) {
		fromClause.append(" RIGHT OUTER JOIN ").append(entityAlias).append('.').append(referenceBinding);
		fromClause.append(" as ").append(BindUtil.sanitiseBinding(joinAlias));
		return this;
	}

	/**
	 * Adds an aliased inner fetch join from the driving entity.
	 * @param referenceBinding the referenceBinding value
	 * @param joinAlias the joinAlias value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addFetchedInnerJoin(String referenceBinding, String joinAlias) {
		return addFetchedInnerJoinFromEntity(THIS_ALIAS, referenceBinding, joinAlias);
	}
	
	/**
	 * Adds an aliased inner fetch join from the given entity alias.
	 * @param entityAlias the entityAlias value
	 * @param referenceBinding the referenceBinding value
	 * @param joinAlias the joinAlias value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addFetchedInnerJoinFromEntity(String entityAlias, String referenceBinding, String joinAlias) {
		fromClause.append(" INNER JOIN FETCH ").append(entityAlias).append('.').append(referenceBinding);
		fromClause.append(" as ").append(BindUtil.sanitiseBinding(joinAlias));
		return this;
	}

	/**
	 * Adds an aliased left outer fetch join from the driving entity.
	 * @param referenceBinding the referenceBinding value
	 * @param joinAlias the joinAlias value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addFetchedLeftOuterJoin(String referenceBinding, String joinAlias) {
		return addFetchedLeftOuterJoinFromEntity(THIS_ALIAS, referenceBinding, joinAlias);
	}
	
	/**
	 * Adds an aliased left outer fetch join from the given entity alias.
	 * @param entityAlias the entityAlias value
	 * @param referenceBinding the referenceBinding value
	 * @param joinAlias the joinAlias value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addFetchedLeftOuterJoinFromEntity(String entityAlias, String referenceBinding, String joinAlias) {
		fromClause.append(" LEFT OUTER JOIN FETCH ").append(entityAlias).append('.').append(referenceBinding);
		fromClause.append(" as ").append(BindUtil.sanitiseBinding(joinAlias));
		return this;
	}

	/**
	 * Adds an aliased right outer fetch join from the driving entity.
	 * @param referenceBinding the referenceBinding value
	 * @param joinAlias the joinAlias value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addFetchedRightOuterJoin(String referenceBinding, String joinAlias) {
		return addFetchedRightOuterJoinFromEntity(THIS_ALIAS, referenceBinding, joinAlias);
	}
	
	/**
	 * Adds an aliased right outer fetch join from the given entity alias.
	 * @param entityAlias the entityAlias value
	 * @param referenceBinding the referenceBinding value
	 * @param joinAlias the joinAlias value
	 * @return the resulting value
	 */
	@Override
	public DocumentQuery addFetchedRightOuterJoinFromEntity(String entityAlias, String referenceBinding, String joinAlias) {
		fromClause.append(" RIGHT OUTER JOIN FETCH ").append(entityAlias).append('.').append(referenceBinding);
		fromClause.append(" as ").append(BindUtil.sanitiseBinding(joinAlias));
		return this;
	}

	/**
	 * Renders the current query state into an executable query string.
	 *
	 * <p>Side effects: if no projection is configured, this method adds the
	 * implicit {@code this} projection before rendering.
	 *
	 * @return rendered query text including select/from/filter/group/order clauses
	 */
	@Override
	public String toQueryString() {
		StringBuilder result = new StringBuilder(256);

		result.append("SELECT ").append(distinct ? "DISTINCT " : "");
		if (projectionClause.isEmpty()) {
			addThisProjection();
		}
		result.append(projectionClause);
		result.append(" FROM ").append(fromClause);
		if (! filter.isEmpty()) {
			result.append(" WHERE ").append(filter.toString());
		}
		if (! groupClause.isEmpty()) {
			result.append(" GROUP BY ").append(groupClause);
		}

		boolean hasOrderings = (! insertedOrderings.isEmpty()) || (! appendedOrderings.isEmpty());
		if ((orderClause != null) || hasOrderings) {
			result.append(" ORDER BY ");
			if (orderClause != null) {
				result.append(orderClause);
				if (hasOrderings) {
					result.append(", ");
				}
			}

			if (hasOrderings) {
				// append the inserted orderings first
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
		}

		return result.toString();
	}

	/**
	 * Returns the metadata document driving this query.
	 * @return the resulting value
	 */
	@Override
	public final Document getDrivingDocument() {
		return drivingDocument;
	}
	
	/**
	 * Returns the single bean result when present, or {@code null} when no rows match.
	 * @return the resulting value
	 */
	@Override
	public final <T extends Bean> T beanResult() {
		List<T> results = beanResults();
		return AbstractQuery.returnOneResult(results);
	}

	/**
	 * Returns exactly one bean result and fails when zero or multiple rows are returned.
	 * @return the resulting value
	 */
	@Override
	public final <T extends Bean> T retrieveBean() {
		List<T> results = beanResults();
		return AbstractQuery.assertOneResult(results);
	}

	/**
	 * Returns the single projected bean result when present, or {@code null} when no rows match.
	 * @return the resulting value
	 */
	@Override
	public final <T extends Bean> T projectedResult() {
		List<T> results = projectedResults();
		return AbstractQuery.returnOneResult(results);
	}

	/**
	 * Returns exactly one projected bean result and fails when zero or multiple rows are returned.
	 * @return the resulting value
	 */
	@Override
	public final <T extends Bean> T retrieveProjected() {
		List<T> results = projectedResults();
		return AbstractQuery.assertOneResult(results);
	}

	/**
	 * Returns the single scalar result when present, or {@code null} when no rows match.
	 * @param type the type value
	 * @return the resulting value
	 */
	@Override
	public final <T> T scalarResult(Class<T> type) {
		List<T> results = scalarResults(type);
		return AbstractQuery.returnOneResult(results);
	}

	/**
	 * Returns exactly one scalar result and fails when zero or multiple rows are returned.
	 * @param type the type value
	 * @return the resulting value
	 */
	@Override
	public final <T> T retrieveScalar(Class<T> type) {
		List<T> results = scalarResults(type);
		return AbstractQuery.assertOneResult(results);
	}

	/**
	 * Returns the single tuple result when present, or {@code null} when no rows match.
	 * @return the resulting value
	 */
	@Override
	public final Object[] tupleResult() {
		List<Object[]> results = tupleResults();
		return AbstractQuery.returnOneResult(results);
	}

	/**
	 * Returns exactly one tuple result and fails when zero or multiple rows are returned.
	 * @return the resulting value
	 */
	@Override
	public final Object[] retrieveTuple() {
		List<Object[]> results = tupleResults();
		return AbstractQuery.assertOneResult(results);
	}
}
