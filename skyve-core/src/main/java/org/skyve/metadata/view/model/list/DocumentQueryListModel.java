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
import org.skyve.impl.persistence.AbstractDocumentQuery;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Association;
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

public class DocumentQueryListModel <T extends Bean> extends ListModel<T> {
	private String description;
	
	private Module module;
	private Document drivingDocument;
	private MetaDataQueryDefinition query;

	public DocumentQueryListModel(MetaDataQueryDefinition query) {
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
	protected void setQuery(MetaDataQueryDefinition query) {
		this.query = query;
	}

	@Override
	public void postConstruct(Customer customer, boolean runtime) {
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
			MetaDataQueryProjectedColumn projectedColumn = (column instanceof MetaDataQueryProjectedColumn) ?
																(MetaDataQueryProjectedColumn) column :
																null;
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
	public String getDescription() {
		return description;
	}

	@Override
	public Document getDrivingDocument() {
		return drivingDocument;
	}

	private List<MetaDataQueryColumn> columns;
	
	@Override
	public List<MetaDataQueryColumn> getColumns() {
		return columns;
	}

	// column binding/alias -> null (if binding) or expression (if alias)
	private Map<String, String> projections = new TreeMap<>();
	
	@Override
	public Set<String> getProjections() {
		return projections.keySet();
	}

	private DocumentQuery detailQuery;
	private DocumentQuery summaryQuery;
	private DocumentQueryFilter filter;
	
	@Override
	public Filter getFilter() {
		if (filter == null) {
			establishQueries();
			filter = new DocumentQueryFilter(detailQuery.getFilter(), summaryQuery.getFilter());
		}
		return filter;
	}

	@Override
	public Filter newFilter() {
		establishQueries();
		return new DocumentQueryFilter(detailQuery.newDocumentFilter(), summaryQuery.newDocumentFilter());
	}
	
	@Override
	public void putParameter(String name, Object value) {
		establishQueries();
		detailQuery.putParameter(name, value);
		summaryQuery.putParameter(name, value);
	}

	@Override
	public Page fetch() throws Exception {
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
				SortDirection direction = sort.getDirection();
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
		Number totalRows = (Number) BindUtil.get(summaryBean, Bean.DOCUMENT_ID);
		result.setTotalRows((totalRows == null) ? 0L : totalRows.longValue());
		result.setRows(rows);
		result.setSummary(summaryBean);
		
		return result;
	}

	@Override
	public AutoClosingIterable<Bean> iterate() throws Exception {
		establishQueries();
		return detailQuery.projectedIterable();
	}
	
	@Override
	public Bean update(String bizId, SortedMap<String, Object> values) 
	throws Exception {
		return update(bizId, 
						values, 
						drivingDocument, 
						query,
						getSelectedTagId());
	}

	public static Bean update(String bizId, 
								SortedMap<String, Object> properties, 
								Document drivingDocument,
								MetaDataQueryDefinition query,
								String selectedTagId)
	throws Exception {
		Persistence p = CORE.getPersistence();
		PersistentBean bean = p.retrieveAndLock(drivingDocument, bizId);
		BindUtil.populateProperties(p.getUser(), bean, properties, true);
		bean = p.save(drivingDocument, bean);
		
		DocumentQuery q = query.constructDocumentQuery(null, selectedTagId);
		q.getFilter().addEquals(Bean.DOCUMENT_ID, bizId);
		return q.projectedResult();
	}
	
	@Override
	public void remove(String bizId) throws Exception {
		remove(bizId, drivingDocument);
	}

	public static void remove(String bizId, Document drivingDocument)
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
	protected DocumentQuery getDetailQuery() {
		return detailQuery;
	}
	
	/**
	 * For Sub-classes to get hold of the internal summary query being used.
	 */
	protected DocumentQuery getSummaryQuery() {
		return summaryQuery;
	}
}
