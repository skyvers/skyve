package org.skyve.metadata.view.model.list;

import java.util.List;
import java.util.SortedMap;

import org.skyve.domain.Bean;
import org.skyve.metadata.module.query.QueryColumn;
import org.skyve.persistence.DocumentQuery;

public class InMemoryDocumentQueryListModel<T extends Bean> extends InMemoryListModel<T> {
	private static final long serialVersionUID = 7072903544810000605L;

	private DocumentQuery query;
	
	public InMemoryDocumentQueryListModel(DocumentQuery query) {
		this.query = query;
	}
	
	@Override
	public List<Bean> getRows() throws Exception {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getDescription() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<QueryColumn> getColumns() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Bean update(String bizId, SortedMap<String, Object> properties) throws Exception {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void remove(String bizId) throws Exception {
		// TODO Auto-generated method stub
		
	}

}
