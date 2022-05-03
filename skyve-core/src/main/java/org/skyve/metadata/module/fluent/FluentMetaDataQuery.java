package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.MetaDataQueryContentColumnMetaData;
import org.skyve.impl.metadata.repository.module.MetaDataQueryMetaData;
import org.skyve.impl.metadata.repository.module.MetaDataQueryProjectedColumnMetaData;
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

	public FluentMetaDataQueryProjectedColumn findProjectedColumnByName(String name) {
		MetaDataQueryProjectedColumnMetaData result = (MetaDataQueryProjectedColumnMetaData) query.getColumns().stream().filter(c -> name.equals(c.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentMetaDataQueryProjectedColumn(result);
		}
		return null;
	}

	public FluentMetaDataQueryProjectedColumn findProjectedColumnByBinding(String binding) {
		MetaDataQueryProjectedColumnMetaData result = (MetaDataQueryProjectedColumnMetaData) query.getColumns().stream().filter(c -> binding.equals(c.getBinding())).findAny().orElse(null);
		if (result != null) {
			return new FluentMetaDataQueryProjectedColumn(result);
		}
		return null;
	}

	public FluentMetaDataQueryProjectedColumn findProjectedColumnByDisplayName(String displayName) {
		MetaDataQueryProjectedColumnMetaData result = (MetaDataQueryProjectedColumnMetaData) query.getColumns().stream().filter(c -> displayName.equals(c.getDisplayName())).findAny().orElse(null);
		if (result != null) {
			return new FluentMetaDataQueryProjectedColumn(result);
		}
		return null;
	}

	public FluentMetaDataQuery addContentColumn(FluentMetaDataQueryContentColumn column) {
		query.getColumns().add(column.get());
		return this;
	}

	public FluentMetaDataQueryContentColumn findContentColumnByName(String name) {
		MetaDataQueryContentColumnMetaData result = (MetaDataQueryContentColumnMetaData) query.getColumns().stream().filter(c -> name.equals(c.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentMetaDataQueryContentColumn(result);
		}
		return null;
	}

	public FluentMetaDataQueryContentColumn findContentColumnByBinding(String binding) {
		MetaDataQueryContentColumnMetaData result = (MetaDataQueryContentColumnMetaData) query.getColumns().stream().filter(c -> binding.equals(c.getBinding())).findAny().orElse(null);
		if (result != null) {
			return new FluentMetaDataQueryContentColumn(result);
		}
		return null;
	}

	public FluentMetaDataQueryContentColumn findContentColumnByDisplayName(String displayName) {
		MetaDataQueryContentColumnMetaData result = (MetaDataQueryContentColumnMetaData) query.getColumns().stream().filter(c -> displayName.equals(c.getDisplayName())).findAny().orElse(null);
		if (result != null) {
			return new FluentMetaDataQueryContentColumn(result);
		}
		return null;
	}

	public FluentMetaDataQuery removeColumnByName(String name) {
		query.getColumns().removeIf(c -> name.equals(c.getName()));
		return this;
	}

	public FluentMetaDataQuery removeColumnByBinding(String binding) {
		query.getColumns().removeIf(c -> binding.equals(c.getBinding()));
		return this;
	}

	public FluentMetaDataQuery removeColumnByDisplayName(String displayName) {
		query.getColumns().removeIf(c -> displayName.equals(c.getDisplayName()));
		return this;
	}

	public FluentMetaDataQuery clearColumns() {
		query.getColumns().clear();
		return this;
	}

	@Override
	public MetaDataQueryMetaData get() {
		return query;
	}
}
