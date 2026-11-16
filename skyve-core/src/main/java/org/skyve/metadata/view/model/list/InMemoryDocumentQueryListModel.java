package org.skyve.metadata.view.model.list;

import java.util.Collections;
import java.util.List;
import java.util.SortedMap;

import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.persistence.DocumentQuery;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public class InMemoryDocumentQueryListModel<T extends Bean> extends InMemoryListModel<T> {
	@SuppressWarnings("unused")
	private DocumentQuery query;
	
	public InMemoryDocumentQueryListModel(@Nonnull Module module, @Nonnull Document drivingDocument, @Nonnull DocumentQuery query) {
		super(module, drivingDocument);
		this.query = query;
	}
	
	@Override
	public void postConstruct(@Nonnull Customer customer, boolean runtime) {
		// TODO Auto-generated method stub
	}
	
	@Override
	public @Nullable List<Bean> getRows() throws Exception {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public @Nullable String getDescription() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public @Nonnull List<MetaDataQueryColumn> getColumns() {
		// TODO Auto-generated method stub
		return Collections.emptyList();
	}

	@Override
	public @Nullable Bean update(@Nonnull String bizId, @Nonnull SortedMap<String, Object> properties) throws Exception {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void remove(@Nonnull String bizId) throws Exception {
		// TODO Auto-generated method stub
		
	}

}
