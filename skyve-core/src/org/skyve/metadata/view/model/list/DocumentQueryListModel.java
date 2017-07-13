package org.skyve.metadata.view.model.list;

import java.util.List;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeSet;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.persistence.AbstractDocumentQuery;
import org.skyve.impl.web.SortParameter;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.DocumentQueryDefinition;
import org.skyve.metadata.module.query.QueryColumn;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;

public class DocumentQueryListModel extends ListModel<Bean> {
	private static final long serialVersionUID = 8905939302545321358L;

	private String description;
	
	private Customer customer;
	private Module module;
	private Document drivingDocument;
	private DocumentQueryDefinition query;

	public void setQuery(DocumentQueryDefinition query) {
		customer = CORE.getUser().getCustomer();
		this.query = query;
		description = query.getDescription();
		columns = query.getColumns();
		module = query.getDocumentModule(customer);
		
		projections.add(Bean.DOCUMENT_ID);
		projections.add(PersistentBean.LOCK_NAME);
		projections.add(PersistentBean.TAGGED_NAME);
		projections.add(PersistentBean.FLAG_COMMENT_NAME);
		projections.add(Bean.BIZ_KEY);

		drivingDocument = module.getDocument(customer, query.getDocumentName());
		for (QueryColumn column : query.getColumns()) {
			if (column.isProjected()) {
				String binding = column.getBinding();
				// if this binding is to an association, 
				// add the bizId as the column value and bizKey as the column displayValue
				if (binding != null) {
					TargetMetaData target = Binder.getMetaDataForBinding(customer,
																			module,
																			drivingDocument,
																			binding);
					
					if (target.getAttribute() instanceof Association) {
						StringBuilder sb = new StringBuilder(64);
						sb.append(binding).append('.').append(Bean.BIZ_KEY);
						projections.add(sb.toString());
					}
					projections.add(binding);
				}
				else {
					projections.add(column.getName());
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

	private List<QueryColumn> columns;
	
	@Override
	public List<QueryColumn> getColumns() {
		return columns;
	}

	private Set<String> projections = new TreeSet<>();
	
	@Override
	public Set<String> getProjections() {
		return projections;
	}

	private DocumentQuery detailQuery;
	private DocumentQuery summaryQuery;
	private DocumentQueryFilter filter;
	
	@Override
	public Filter getFilter() throws Exception {
		if (filter == null) {
			establishQueries();
			filter = new DocumentQueryFilter(detailQuery.getFilter(), summaryQuery.getFilter());
		}
		return filter;
	}

	@Override
	public Filter newFilter() throws Exception {
		establishQueries();
		return new DocumentQueryFilter(detailQuery.newDocumentFilter(), summaryQuery.newDocumentFilter());
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
		summaryQuery.addAggregateProjection(AggregateFunction.Min, PersistentBean.FLAG_COMMENT_NAME, PersistentBean.FLAG_COMMENT_NAME);
		
		int startRow = getStartRow();
		int endRow = getEndRow();
		detailQuery.setFirstResult(startRow);
		detailQuery.setMaxResults(endRow - startRow);
		
		SortParameter[] sorts = getSortParameters();
		if (sorts != null) {
			for (SortParameter sort : sorts) {
				String binding = sort.getBinding();
				SortDirection direction = sort.getDirection();
				detailQuery.insertOrdering(binding, direction);
			}
		}
		
		Page result = new Page();
		Bean summaryBean = summaryQuery.projectedResult();
		result.setTotalRows(((Number) BindUtil.get(summaryBean, Bean.DOCUMENT_ID)).longValue());
		result.setRows(detailQuery.projectedResults());
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
								DocumentQueryDefinition query,
								String selectedTagId)
	throws Exception {
		Persistence p = CORE.getPersistence();
		PersistentBean bean = p.retrieve(drivingDocument, bizId, true);
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
		PersistentBean bean = p.retrieve(drivingDocument, bizId, true);
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
