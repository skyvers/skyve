package org.skyve.metadata.view.model.list;

import java.util.ArrayList;
import java.util.List;
import java.util.SortedMap;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicPersistentBean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.Persistence;

public class RDBMSDynamicPersistenceListModel<T extends Bean> extends InMemoryListModel<T> {
	private String description;
	private List<MetaDataQueryColumn> columns;
	private Module module;
	private Document document;
	
	public void setQuery(MetaDataQueryDefinition query) throws Exception {
		description = query.getDescription();
		columns = query.getColumns();
		
		Customer c = CORE.getCustomer();
		module = query.getDocumentModule(c);
		document = module.getDocument(c, query.getDocumentName());

		setDrivingDocument(module, document);
	}
	
	@Override
	public List<Bean> getRows() throws Exception {
		List<Bean> result = new ArrayList<>(128);
		
		Persistence p = CORE.getPersistence();
		try (AutoClosingIterable<String> i = p.newSQL("select bizId from ADM_DynamicEntity where moduleName = :moduleName and documentName = :documentName")
												.putParameter("moduleName", module.getName(), false)
												.putParameter("documentName", document.getName(), false)
												.scalarIterable(String.class)) {
			for (String bizId : i) {
				DynamicPersistentBean b = p.retrieve(document, bizId);
				result.add(b);
			}
		}
		
		return result;
	}

	@Override
	public String getDescription() {
		return description;
	}

	@Override
	public List<MetaDataQueryColumn> getColumns() {
		return columns;
	}

	@Override
	public Bean update(String bizId, SortedMap<String, Object> properties) throws Exception {
		throw new UnsupportedOperationException("Update Not Supported");
	}

	@Override
	public void remove(String bizId) throws Exception {
		throw new UnsupportedOperationException("Remove Not Supported");
	}
}
