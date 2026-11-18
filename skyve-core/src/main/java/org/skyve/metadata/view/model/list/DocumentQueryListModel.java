package org.skyve.metadata.view.model.list;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.model.document.InverseOne;
import org.skyve.impl.persistence.AbstractDocumentQuery;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.web.SortParameter;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public class DocumentQueryListModel <T extends Bean> extends ListModel<T> {
	private String description;
	
	private Customer queryCustomer;
	private Module module;
	private Document drivingDocument;
	private MetaDataQueryDefinition query;

	public DocumentQueryListModel(@Nonnull MetaDataQueryDefinition query) {
		this.query = query;
	}

	/**
	 * Use this constructor when the driving document is explicitly set in postConstruct call.
	 */
	protected DocumentQueryListModel() {
		// nothing to see here
	}
	
	/**
	 * Used to set the query in postConstruct() of subclasses.
	 * @param query
	 */
	protected void setQuery(@Nonnull MetaDataQueryDefinition query) {
		this.query = query;
	}

	@Override
	public void postConstruct(@Nonnull Customer customer, boolean runtime) {
		queryCustomer = customer;
		description = query.getDescription();
		columns = query.getColumns();
		module = query.getDocumentModule(customer);
		
		projections.put(Bean.DOCUMENT_ID, null);
		projections.put(PersistentBean.LOCK_NAME, null);
		projections.put(PersistentBean.TAGGED_NAME, null);
		projections.put(PersistentBean.FLAG_COMMENT_NAME, null);
		projections.put(Bean.BIZ_KEY, null);

		drivingDocument = module.getDocument(customer, query.getDocumentName());
		for (MetaDataQueryColumn column : query.getColumns()) {
			MetaDataQueryProjectedColumn projectedColumn = (column instanceof MetaDataQueryProjectedColumn c) ? c : null;
			@SuppressWarnings("java:S1125")
			boolean projected = (projectedColumn != null) ? projectedColumn.isProjected() : true;
			if (projected) {
				String binding = column.getBinding();
				// if this binding is to an association, 
				// add the association as the column value and bizKey as the column displayValue
				if (binding != null) {
					TargetMetaData target = Binder.getMetaDataForBinding(customer,
																			module,
																			drivingDocument,
																			binding);
					if (target.getAttribute() instanceof Association) {
						projections.put(String.format("%s.%s", binding, Bean.BIZ_KEY), null);
					}
					projections.put(binding, null);
				}
				else if (projectedColumn != null) {
					projections.put(column.getName(), projectedColumn.getExpression());
				}
			}
		}
	}
	
	@Override
	public @Nonnull String getDescription() {
		return description;
	}

	@Override
	public @Nonnull Document getDrivingDocument() {
		return drivingDocument;
	}

	private List<MetaDataQueryColumn> columns;
	
	@Override
	public @Nonnull List<MetaDataQueryColumn> getColumns() {
		return columns;
	}

	// column binding/alias -> null (if binding) or expression (if alias)
	private Map<String, String> projections = new TreeMap<>();
	
	@Override
	public @Nonnull Set<String> getProjections() {
		return projections.keySet();
	}

	private DocumentQuery detailQuery;
	private DocumentQuery summaryQuery;
	private DocumentQueryFilter filter;
	
	@Override
	public @Nonnull Filter getFilter() {
		if (filter == null) {
			establishQueries();
			filter = new DocumentQueryFilter(detailQuery.getFilter(), summaryQuery.getFilter(), this::leftOuterJoinAssociations);
		}
		return filter;
	}

	@Override
	public @Nonnull Filter newFilter() {
		establishQueries();
		return new DocumentQueryFilter(detailQuery.newDocumentFilter(), summaryQuery.newDocumentFilter(), this::leftOuterJoinAssociations);
	}

	/**
	 * Left outer joins the associations and inverse ones on the path of a binding that a filter puts a condition on.
	 * Such a path in a filter is an implicit inner join in HQL, which would leave out the rows with nothing on it: a
	 * negation also matches them, and under Match Any so does any other criterion. The joins are to-one, so they add
	 * no rows, and a condition through the path still excludes the rows with nothing on it unless it matches no value.
	 * The query's own columns may have joined them already. An embedded association is on the row, so it needs none.
	 * <p>A query whose from clause declares several entities (a metadata query's {@code <from>} of
	 * {@code {a.X} as bean, {a.Y} as y}) gets no joins: appended after the last entity, a join's on clause sees bean only
	 * where the dialect renders the comma as a cross join (Hibernate 5.6's H2, MySQL, PostgreSQL and SQL Server dialects
	 * do; its DB2 and Oracle 8i and 9i dialects do not). Such a query keeps the implicit inner joins, so its filters
	 * through an optional association leave out the rows with none, even a filter that matches no value.
	 * <p>Side effects: adds the joins to the detail and the summary query.
	 * @param binding	the binding of the condition
	 */
	private void leftOuterJoinAssociations(@Nonnull String binding) {
		// postConstruct may not have been called: the model can fetch with only its query
		if (queryCustomer == null) {
			return;
		}
		int dot = binding.indexOf('.');
		while (dot > 0) {
			String path = binding.substring(0, dot);
			TargetMetaData target = Binder.getMetaDataForBinding(queryCustomer, module, drivingDocument, path);
			Attribute attribute = target.getAttribute();
			if (! ((attribute instanceof Association) || (attribute instanceof InverseOne))) {
				return;
			}
			if (! ((attribute instanceof Association association) && (association.getType() == AssociationType.embedded))) {
				leftOuterJoin(detailQuery, path);
				leftOuterJoin(summaryQuery, path);
			}
			dot = binding.indexOf('.', dot + 1);
		}
	}

	private static void leftOuterJoin(@Nullable DocumentQuery query, @Nonnull String path) {
		if ((query instanceof AbstractDocumentQuery documentQuery) &&
				(! documentQuery.isFromSeveralEntities()) &&
				(! documentQuery.isLeftOuterJoined(path))) {
			documentQuery.addLeftOuterJoin(path);
		}
	}
	
	@Override
	public void putParameter(@Nonnull String name, @Nullable Object value) {
		establishQueries();
		detailQuery.putParameter(name, value);
		summaryQuery.putParameter(name, value);
	}

	@Override
	public @Nonnull Page fetch() throws Exception {
		establishQueries();
		
		if (getSummary() == null) {
			AbstractDocumentQuery internalSummaryQuery = (AbstractDocumentQuery) summaryQuery;
			internalSummaryQuery.clearProjections();
			internalSummaryQuery.clearOrderings();
		}

		// This needs to be the ID to satisfy the client data source definitions
		summaryQuery.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, Bean.DOCUMENT_ID);
		
		// Only page if this isn't an aggregate query
		if (! query.isAggregate()) {
			int startRow = getStartRow();
			int endRow = getEndRow();
			detailQuery.setFirstResult(startRow);
			detailQuery.setMaxResults(endRow - startRow);
		}
		
		SortParameter[] sorts = getSortParameters();
		if (sorts != null) {
			for (SortParameter sort : sorts) {
				String by = sort.getBy();
				if (by == null) {
					throw new IllegalStateException("A sort parameter for query " + query.getName() + " has no binding to sort by");
				}
				// No direction means ascending - the query treats anything but descending as ascending
				SortDirection direction = sort.getDirection();
				if (direction == null) {
					direction = SortDirection.ascending;
				}
				String expression = projections.get(by);
				if (expression == null) {
					detailQuery.insertBoundOrdering(by, direction);
				}
				else {
					detailQuery.insertExpressionOrdering(expression, direction);
				}
			}
		}
		
		Page result = new Page();
		List<Bean> rows = detailQuery.projectedResults();
		Bean summaryBean = null;
		if (query.isAggregate()) {
			Map<String, Object> properties = new TreeMap<>();
			properties.put(Bean.DOCUMENT_ID, Long.valueOf(rows.size()));
			summaryBean = new DynamicBean(module.getName(), drivingDocument.getName(), properties);
		}
		else {
			summaryBean = summaryQuery.projectedResult();
		}
		// No summary row means no rows were counted
		Number totalRows = (summaryBean == null) ? null : (Number) BindUtil.get(summaryBean, Bean.DOCUMENT_ID);
		result.setTotalRows((totalRows == null) ? 0L : totalRows.longValue());
		result.setRows(rows);
		result.setSummary(summaryBean);
		
		return result;
	}

	@Override
	public @Nonnull AutoClosingIterable<Bean> iterate() throws Exception {
		establishQueries();
		return detailQuery.projectedIterable();
	}
	
	@Override
	public @Nullable Bean update(@Nonnull String bizId, @Nonnull SortedMap<String, Object> values) 
	throws Exception {
		return update(bizId, 
						values, 
						drivingDocument, 
						query,
						getSelectedTagId());
	}

	public static @Nullable Bean update(@Nonnull String bizId, 
											@Nonnull SortedMap<String, Object> properties, 
											@Nonnull Document drivingDocument,
											@Nonnull MetaDataQueryDefinition query,
											@Nullable String selectedTagId) {
		Persistence p = CORE.getPersistence();
		PersistentBean bean = p.retrieveAndLock(drivingDocument, bizId);
		BindUtil.populateProperties(p.getUser(), bean, properties, true);
		bean = p.save(drivingDocument, bean);
		
		DocumentQuery q = query.constructDocumentQuery(null, selectedTagId);
		q.getFilter().addEquals(Bean.DOCUMENT_ID, bizId);
		return q.projectedResult();
	}
	
	@Override
	public void remove(@Nonnull String bizId) throws Exception {
		remove(bizId, drivingDocument);
	}

	public static void remove(@Nonnull String bizId, @Nonnull Document drivingDocument)
	throws Exception {
		Persistence p = CORE.getPersistence();
		PersistentBean bean = p.retrieveAndLock(drivingDocument, bizId);
		p.delete(drivingDocument, bean);
	}
	
	/**
	 * For sub-classes to override to establish the internal queries, 
	 * or call super.establishQueries() and manipulate the stanadard queries in some manner.
	 */
	protected void establishQueries() {
		if (detailQuery == null) {
			detailQuery = query.constructDocumentQuery(null, getSelectedTagId());
		}
		if (summaryQuery == null) {
			AggregateFunction summary = getSummary();
			summaryQuery = query.constructDocumentQuery((summary == null) ? AggregateFunction.Count : summary, getSelectedTagId());
		}
	}
	
	/**
	 * For Sub-classes to get hold of the internal detail query being used.
	 */
	protected @Nullable DocumentQuery getDetailQuery() {
		return detailQuery;
	}
	
	/**
	 * For Sub-classes to get hold of the internal summary query being used.
	 */
	protected @Nullable DocumentQuery getSummaryQuery() {
		return summaryQuery;
	}
}
