package org.skyve.metadata.view.model.list;

import java.util.List;
import java.util.SortedMap;

import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.persistence.DocumentQuery;

public class InMemoryDocumentQueryListModel<T extends Bean> extends InMemoryListModel<T> {
	@SuppressWarnings("unused")
	private DocumentQuery query;
	
	public InMemoryDocumentQueryListModel(Module module, Document drivingDocument, DocumentQuery query) {
		super(module, drivingDocument);
		this.query = query;
	}
	
	@Override
	public void postConstruct(Customer customer, boolean runtime) {
		// TODO Auto-generated method stub
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
	public List<MetaDataQueryColumn> getColumns() {
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
