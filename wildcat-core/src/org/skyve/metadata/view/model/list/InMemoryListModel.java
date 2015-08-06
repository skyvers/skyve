package org.skyve.metadata.view.model.list;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.QueryColumn;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.wildcat.domain.MapBean;
import org.skyve.wildcat.metadata.model.document.Collection.Ordering;
import org.skyve.wildcat.web.SortParameter;

public abstract class InMemoryListModel extends ListModel<Bean> {
	private static final long serialVersionUID = -4488883647065013017L;

	private Customer customer;
	private Module module;
	private Document document;
	private List<Bean> rows;
	
	/**
	 * The model is destructive to the collection of rows so ensure you send in a copy.
	 * 
	 * @param rows
	 * @param module
	 * @param document
	 * @throws Exception
	 */
	public void setRows(List<Bean> rows, Module module, Document document)
	throws Exception {
		customer = CORE.getUser().getCustomer();
		this.module = module;
		this.document = document;
		this.rows = rows;
		
		projections.add(Bean.DOCUMENT_ID);
		projections.add(PersistentBean.LOCK_NAME);
		projections.add(PersistentBean.TAGGED_NAME);
		projections.add(PersistentBean.FLAG_COMMENT_NAME);
		projections.add(Bean.BIZ_KEY);

		for (QueryColumn column : getColumns()) {
			if (column.isProjected()) {
				String binding = column.getBinding();
				// if this binding is to an association, 
				// add the bizId as the column value and bizKey as the column displayValue
				TargetMetaData target = Binder.getMetaDataForBinding(customer,
																		module,
																		document,
																		binding);
				
				if (target.getAttribute() instanceof Association) {
					StringBuilder sb = new StringBuilder(64);
					sb.append(binding).append('.').append(Bean.BIZ_KEY);
					projections.add(sb.toString());
				}
				projections.add((binding != null) ? binding : column.getName());
			}
		}
	}
	
	private Set<String> projections = new TreeSet<>();
	
	@Override
	public Set<String> getProjections() {
		return projections;
	}

	private InMemoryFilter filter;
	
	@Override
	public Filter getFilter() throws Exception {
		if (filter == null) {
			filter = new InMemoryFilter();
		}
		return filter;
	}

	@Override
	public Filter newFilter() throws Exception {
		return new InMemoryFilter();
	}

	@Override
	public Page fetch() throws Exception {
		if (getSummary() == null) {
			// TODO need to do summary processing here
		}

		if (filter != null) {
			filter.filter(rows);
		}
		
		SortParameter[] sorts = getSortParameters();
		if (sorts != null) {
			Ordering[] order = new Ordering[sorts.length];
			int i = 0;
			for (SortParameter sort : sorts) {
				String binding = sort.getBinding();
				SortDirection direction = sort.getDirection();
				order[i++] = new Ordering(binding, direction);
			}
			Binder.sortCollectionByOrdering(rows, order);
		}
		
		int startRow = getStartRow();
		int endRow = getEndRow();
		
		Page result = new Page();
		int totalRows = rows.size();
		result.setTotalRows(totalRows);
		// NB get destructive on the list
		rows.retainAll(rows.subList(startRow, Math.min(endRow + 1, totalRows)));
		result.setRows(rows);

		Map<String, Object> summaryData = new TreeMap<>();
		// This needs to be the ID to satisfy the client data source definitions
		summaryData.put(Bean.DOCUMENT_ID, Long.valueOf(rows.size()));
		summaryData.put(PersistentBean.FLAG_COMMENT_NAME, "");
		result.setSummary(new MapBean(module.getName(), document.getName(), summaryData));
		
		return result;
	}

	@Override
	public AutoClosingIterable<Bean> iterate() throws Exception {
		// TODO 
		return null;
	}
}
