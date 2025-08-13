package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.module.query.MetaDataQueryReferenceImpl;
import org.skyve.impl.metadata.repository.module.MetaDataQueryReferenceMetaData;

public class FluentMetaDataQueryReference extends FluentQueryReference<FluentMetaDataQueryReference> {
	private MetaDataQueryReferenceMetaData query = null;
	
	public FluentMetaDataQueryReference() {
		query = new MetaDataQueryReferenceMetaData();
	}
	
	public FluentMetaDataQueryReference(MetaDataQueryReferenceMetaData query) {
		this.query = query;
	}
	
	public FluentMetaDataQueryReference from(@SuppressWarnings("hiding") MetaDataQueryReferenceImpl query) {
		super.from(query);
		return this;
	}
	
	@Override
	public MetaDataQueryReferenceMetaData get() {
		return query;
	}
}
