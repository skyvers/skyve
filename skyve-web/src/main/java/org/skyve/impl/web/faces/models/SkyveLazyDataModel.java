package org.skyve.impl.web.faces.models;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
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
import org.skyve.domain.types.Decimal;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.domain.messages.SecurityException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.SortParameterImpl;
import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.DocumentQueryListModel;
import org.skyve.metadata.view.model.list.Filter;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.list.Page;
import org.skyve.metadata.view.widget.bound.FilterParameter;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.web.SortParameter;

import com.vividsolutions.jts.geom.Geometry;

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
		User u = CORE.getUser();
		Customer c = u.getCustomer();
		Module m = c.getModule(moduleName);
		Document d = null;
		MetaDataQueryDefinition query = null;
		ListModel<Bean> model = null;

		// model type of request
		if (modelName != null) {
			d = m.getDocument(c, documentName);
			model = CORE.getRepository().getListModel(c, d, modelName, true);
			if (model == null) {
				throw new MetaDataException(modelName + " is not a valid ListModel");
			}
		}
		// query type of request
		else {
			if (queryName != null) {
				query = m.getMetaDataQuery(queryName);
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
			d = m.getDocument(c, query.getDocumentName());
	        DocumentQueryListModel<Bean> queryModel = new DocumentQueryListModel<>();
	        queryModel.setQuery(query);
	        model = queryModel;
		}

		if (! u.canReadDocument(model.getDrivingDocument())) {
			throw new SecurityException(model.getDrivingDocument().getName() + " in module " + model.getDrivingDocument().getOwningModuleName(), u.getName());
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
			sort(multiSortMeta, model);
		}

		Page page;
		try {
			if (filterParameters != null) {
				model.addFilterParameters(d, filterParameters);
			}
			if (filters != null) {
				filter(filters, model, c);
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
	
	private static void sort(List<SortMeta> multiSortMeta, ListModel<Bean> model) {
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
	
	private static void filter(Map<String, Object> filters, ListModel<Bean> model, Customer customer)
	throws Exception {
		Document drivingDocument = model.getDrivingDocument();
		Module drivingModule = customer.getModule(drivingDocument.getOwningModuleName());
		Filter modelFilter = model.getFilter();
		for (String key : filters.keySet()) {
			Object value = filters.get(key);
			if ((value == null) || ("".equals(value))) {
				continue;
			}

			TargetMetaData target = BindUtil.getMetaDataForBinding(customer, drivingModule, drivingDocument, key);
			boolean contains = false;
			if (target != null) {
				Attribute attribute = target.getAttribute();
				if (attribute != null) {
					AttributeType type = attribute.getAttributeType();
					// Use "like" if its textual and not a constant domain type
					if (AttributeType.colour.equals(type) ||
							AttributeType.markup.equals(type) ||
							AttributeType.memo.equals(type) ||
							AttributeType.text.equals(type)) {
						if (! DomainType.constant.equals(attribute.getDomainType())) {
							contains = true;
						}
					}
				}
			}
			if (UtilImpl.COMMAND_TRACE) UtilImpl.LOGGER.info(String.format("    FILTER %s %s %s", key, contains ? "contains" : "=", value));
			if (contains) {
				modelFilter.addContains(key, (String) value);
			}
			else {
				if (value instanceof Boolean) {
					modelFilter.addEquals(key, (Boolean) value);
				}
				else if (value instanceof Date) {
					modelFilter.addEquals(key, (Date) value);
				}
				else if (value instanceof Decimal) {
					modelFilter.addEquals(key, (Decimal) value);
				}
				else if (value instanceof Enum<?>) {
					modelFilter.addEquals(key, (Enum<?>) value);
				}
				else if (value instanceof Geometry) {
					modelFilter.addEquals(key, (Geometry) value);
				}
				else if (value instanceof Integer) {
					modelFilter.addEquals(key, (Integer) value);
				}
				else if (value instanceof Long) {
					modelFilter.addEquals(key, (Long) value);
				}
				else if (value instanceof String) {
					modelFilter.addEquals(key, (String) value);
				}
				else {
					throw new IllegalArgumentException(value + " is not a valid value for param " + key);
				}
			}
		}
	}
}
