package org.skyve.metadata.view.model.list;

import org.skyve.domain.Bean;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.persistence.SQL;

public class SQLDocumentQueryListModel <T extends Bean> extends DocumentQueryListModel<T> {
	private SQL inputSQL;
	private SQL detailSQL;
	private SQL summarySQL;
	private SQLFilter filter;
	
	protected SQLDocumentQueryListModel(SQL sql) {
		super(null); // this is wrong
		this.inputSQL = sql;
	}

	@Override
	public Filter getFilter() {
		if (filter == null) {
			establishQueries();
			filter = new SQLFilter(detailSQL, summarySQL);
		}
		return filter;
	}

	@Override
	public Filter newFilter() {
		establishQueries();
		return new SQLFilter(detailSQL, summarySQL);
	}

	@Override
	public Page fetch() throws Exception {
		establishQueries();
/*		
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
		Bean summaryBean = summaryQuery.projectedResult();
		result.setTotalRows(((Number) BindUtil.get(summaryBean, Bean.DOCUMENT_ID)).longValue());
		result.setRows(detailQuery.projectedResults());
		result.setSummary(summaryBean);
		
		return result;
*/
		return null;
	}

	@Override
	public AutoClosingIterable<Bean> iterate() throws Exception {
		establishQueries();
//		return detailQuery.projectedIterable();
		return null;
	}
	
	/**
	 * For sub-classes to override to establish the internal queries, 
	 * or call super.establishQueries() and manipulate the stanadard queries in some manner.
	 */
	protected void establishSQLs() {
		if (detailSQL == null) {
			detailSQL = inputSQL;//query.constructDocumentQuery(null, getSelectedTagId());
		}
		if (summarySQL == null) {
			@SuppressWarnings("unused")
			AggregateFunction summary = getSummary();
			summarySQL = inputSQL;//query.constructDocumentQuery((summary == null) ? AggregateFunction.Count : summary, getSelectedTagId());
		}
	}
}
