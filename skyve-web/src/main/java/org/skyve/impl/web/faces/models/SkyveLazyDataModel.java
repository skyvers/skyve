package org.skyve.impl.web.faces.models;

import java.util.ArrayList;
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
import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.DocumentQueryDefinition;
import org.skyve.metadata.view.model.list.DocumentQueryListModel;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.list.Page;
import org.skyve.metadata.view.widget.bound.FilterParameter;

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
		
		model.setStartRow(first);
		model.setEndRow(first + pageSize);
		
		Page page;
		try {
			if (filterParameters != null) {
				model.addFilterParameters(d, filterParameters);
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
		return load(first, pageSize, null, filters);
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
