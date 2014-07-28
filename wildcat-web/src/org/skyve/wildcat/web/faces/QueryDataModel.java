package org.skyve.wildcat.web.faces;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.primefaces.model.LazyDataModel;
import org.primefaces.model.SortMeta;
import org.primefaces.model.SortOrder;
import org.skyve.domain.Bean;
import org.skyve.wildcat.web.faces.actions.GetBeansAction;

public class QueryDataModel extends LazyDataModel<BeanMapAdapter<Bean>> {
	private static final long serialVersionUID = -2161288261538038204L;

	private String moduleName;
	private String queryName;
	
	public QueryDataModel(String moduleName, String queryName) {
		this.moduleName = moduleName;
		this.queryName = queryName;
		setRowCount(1);
	}
	
	@Override
	public List<BeanMapAdapter<Bean>> load(int first,
											int pageSize,
											List<SortMeta> multiSortMeta,
											Map<String, Object> filters) {
		List<Bean> beans = new GetBeansAction(moduleName, queryName, null).execute(); 
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
}
