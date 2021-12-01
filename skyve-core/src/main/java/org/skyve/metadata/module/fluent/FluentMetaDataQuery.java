package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.MetaDataQueryMetaData;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryContentColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;

public class FluentMetaDataQuery extends FluentQuery<FluentMetaDataQuery> {
	private MetaDataQueryMetaData query = null;
	
	public FluentMetaDataQuery() {
		query = new MetaDataQueryMetaData();
	}
	
	public FluentMetaDataQuery(MetaDataQueryMetaData query) {
		this.query = query;
	}
	
	public FluentMetaDataQuery from(@SuppressWarnings("hiding") MetaDataQueryDefinition query) {
		super.from(query);
		documentName(query.getDocumentName());
		polymorphic(Boolean.TRUE.equals(query.getPolymorphic()) ? true : false);
		aggregate(query.isAggregate());
		from(query.getFromClause());
		filter(query.getFilterClause());
		
		for (MetaDataQueryColumn column : query.getColumns()) {
			if (column instanceof MetaDataQueryProjectedColumn) {
				addProjectedColumn(new FluentMetaDataQueryProjectedColumn().from((MetaDataQueryProjectedColumn) column));
			}
			else {
				addContentColumn(new FluentMetaDataQueryContentColumn().from((MetaDataQueryContentColumn) column));
			}
		}
		return this;
	}
	
	public FluentMetaDataQuery documentName(String documentName) {
		query.setDocumentName(documentName);
		return this;
	}

	public FluentMetaDataQuery polymorphic(boolean polymorphic) {
		query.setPolymorphic(polymorphic ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentMetaDataQuery aggregate(boolean aggregate) {
		query.setAggregate(aggregate ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentMetaDataQuery from(String from) {
		query.setFrom(from);
		return this;
	}

	public FluentMetaDataQuery filter(String filter) {
		query.setFilter(filter);
		return this;
	}

	public FluentMetaDataQuery addProjectedColumn(FluentMetaDataQueryProjectedColumn column) {
		query.getColumns().add(column.get());
		return this;
	}

	public FluentMetaDataQuery addContentColumn(FluentMetaDataQueryContentColumn column) {
		query.getColumns().add(column.get());
		return this;
	}

	@Override
	public MetaDataQueryMetaData get() {
		return query;
	}
}
