package org.skyve.impl.web.faces.models;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.primefaces.model.LazyDataModel;
import org.primefaces.model.SortMeta;
import org.primefaces.model.SortOrder;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.MapBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.SkyveException;
import org.skyve.impl.metadata.view.widget.bound.FilterParameterImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.SortParameterImpl;
import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.DocumentQueryDefinition;
import org.skyve.metadata.view.model.list.DocumentQueryListModel;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.list.Page;
import org.skyve.metadata.view.widget.bound.FilterParameter;
import org.skyve.web.SortParameter;

public class SkyveLazyDataModel extends LazyDataModel<BeanMapAdapter<Bean>> {
	private static final long serialVersionUID = -2161288261538038204L;

	private FacesView<? extends Bean> view;
	private String moduleName;
	private String documentName;
	private String queryName;
	private String modelName;
	private List<FilterParameter> filterParameters;
	
	public SkyveLazyDataModel(FacesView<? extends Bean> view,
								String moduleName, 
								String documentName, 
								String queryName,
								String modelName,
								List<FilterParameter> filterParameters) {
		this.view = view;
		this.moduleName = moduleName;
		this.documentName = documentName;
		this.queryName = queryName;
		this.modelName = modelName;
		this.filterParameters = filterParameters;
	}
	
	@Override
	public List<BeanMapAdapter<Bean>> load(int first,
											int pageSize,
											List<SortMeta> multiSortMeta,
											Map<String, Object> filters) {
		Customer c = CORE.getUser().getCustomer();
		Module m = c.getModule(moduleName);
		Document d = m.getDocument(c, documentName);
		DocumentQueryDefinition query = null;
		ListModel<Bean> model = null;

		// model type of request
		if (modelName != null) {
			model = CORE.getRepository().getListModel(c, d, modelName, true);
			if (model == null) {
				throw new MetaDataException(modelName + " is not a valid ListModel");
			}
		}
		// query type of request
		else {
			if (queryName != null) {
				query = m.getDocumentQuery(queryName);
				if (query == null) {
					query = m.getDocumentDefaultQuery(c, documentName);
				}
				if (query == null) {
					throw new MetaDataException(queryName + " is not a valid document query.");
				}
			}
			else {
				query = m.getDocumentDefaultQuery(c, documentName);
				if (query == null) {
					throw new MetaDataException(documentName + " is not a valid document for a default query.");
				}
			}
	        DocumentQueryListModel<Bean> queryModel = new DocumentQueryListModel<>();
	        queryModel.setQuery(query);
	        model = queryModel;
		}

		if (view != null) {
			BeanMapAdapter<?> currentBean = view.getCurrentBean();
			if (currentBean != null) {
				model.setBean(currentBean.getBean());
			}
		}
		
		if (UtilImpl.COMMAND_TRACE) UtilImpl.LOGGER.info(String.format("LOAD %s : %s", String.valueOf(first), String.valueOf(pageSize)));
		model.setStartRow(first);
		model.setEndRow(first + pageSize);

		if (multiSortMeta != null) {
			int l = multiSortMeta.size();
			SortParameter[] sortParameters = new SortParameter[l];
			for (int i = 0; i < l; i++) {
				SortMeta sm = multiSortMeta.get(i);
				if (UtilImpl.COMMAND_TRACE) UtilImpl.LOGGER.info(String.format("    SORT by %s %s", sm.getSortField(), sm.getSortOrder()));
				SortParameter sp = new SortParameterImpl();
				sp.setBy(sm.getSortField());
				sp.setDirection((SortOrder.DESCENDING.equals(sm.getSortOrder())) ? SortDirection.descending : null);
				sortParameters[i] = sp;
			}

			model.setSortParameters(sortParameters);
		}
		List<FilterParameter> consolidatedFilterParameters = null;
		if ((filters != null) || (filterParameters != null)) {
			consolidatedFilterParameters = new ArrayList<>(((filters != null) ? filters.size() : 0) + 
															(filterParameters != null ? filterParameters.size() : 0));
			if (filterParameters != null) {
				consolidatedFilterParameters.addAll(filterParameters);
			}
			if (filters != null) {
				for (String key : filters.keySet()) {
					Object value = filters.get(key);
					if (UtilImpl.COMMAND_TRACE) UtilImpl.LOGGER.info(String.format("    FILTER %s with %s", key, value));
					FilterParameterImpl fp = new FilterParameterImpl();
					fp.setName(key);
					fp.setValue((value == null) ? "" : String.format("%%%s%%", value.toString()));
					fp.setOperator(FilterOperator.like);
					consolidatedFilterParameters.add(fp);
				}
			}
		}
		
		Page page;
		try {
			if (consolidatedFilterParameters != null) {
				model.addFilterParameters(d, consolidatedFilterParameters);
			}
			page = model.fetch();
		}
		catch (Exception e) {
			if (e instanceof SkyveException) {
				throw (SkyveException) e;
			}
			throw new DomainException(e);
		}
		setRowCount((int) page.getTotalRows());
		
		List<Bean> beans = page.getRows();
		List<BeanMapAdapter<Bean>> result = new ArrayList<>(beans.size());
		for (Bean bean : beans) {
			result.add(new BeanMapAdapter<>(bean));
		}
		return result;
	}

	@Override
	public List<BeanMapAdapter<Bean>> load(int first,
											int pageSize,
											String sortField,
											SortOrder sortOrder,
											Map<String, Object> filters) {
		List<SortMeta> sorts = null;
		if (sortField != null) {
			sorts = Collections.singletonList(new SortMeta(null, sortField, sortOrder, null));
		}
		return load(first, pageSize, sorts, filters);
	}

	/**
	 * Called when encoding the rows of a data table or data list.
	 */
	@Override
	public Object getRowKey(BeanMapAdapter<Bean> bean) {
		return bean.getBean().getBizId();
	}
	
	/**
	 * Called when a table or list row is selected.
	 */
	@Override
	public BeanMapAdapter<Bean> getRowData(String rowKey) {
		Map<String, Object> properties = new TreeMap<>();
		properties.put(Bean.DOCUMENT_ID, rowKey);
		MapBean bean = new MapBean(moduleName, documentName, properties);
		return new BeanMapAdapter<>(bean);
	}
}
