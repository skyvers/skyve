package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.MetaDataQueryContentColumnMetaData;
import org.skyve.impl.metadata.repository.module.MetaDataQueryMetaData;
import org.skyve.impl.metadata.repository.module.MetaDataQueryProjectedColumnMetaData;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryContentColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Builds metadata query definitions including projected and content columns.
 */
public class FluentMetaDataQuery extends FluentQueryDefinition<FluentMetaDataQuery> {
	private MetaDataQueryMetaData query = null;
	
	/**
	 * Creates a fluent wrapper with a new metadata instance.
	 */
	public FluentMetaDataQuery() {
		query = new MetaDataQueryMetaData();
	}
	
	/**
	 * Creates a fluent wrapper around an existing metadata instance.
	 *
	 * @param query The metadata to mutate.
	 */
	public FluentMetaDataQuery(@Nonnull MetaDataQueryMetaData query) {
		this.query = query;
	}
	
	/**
	 * Copies metadata query fields and columns from an existing query definition.
	 *
	 * @param query The source metadata query definition.
	 * @return this fluent instance.
	 */
	public @Nonnull FluentMetaDataQuery from(@SuppressWarnings("hiding") @Nonnull MetaDataQueryDefinition query) {
		super.from(query);
		documentName(query.getDocumentName());
		polymorphic(Boolean.TRUE.equals(query.getPolymorphic()));
		aggregate(query.isAggregate());
		from(query.getFromClause());
		filter(query.getFilterClause());
		grouping(query.getGroupClause());
		ordering(query.getOrderClause());
		
		for (MetaDataQueryColumn column : query.getColumns()) {
			if (column instanceof MetaDataQueryProjectedColumn projected) {
				addProjectedColumn(new FluentMetaDataQueryProjectedColumn().from(projected));
			}
			else {
				addContentColumn(new FluentMetaDataQueryContentColumn().from((MetaDataQueryContentColumn) column));
			}
		}
		return this;
	}
	
	/**
	 * Sets the target document name for this metadata query.
	 *
	 * @param documentName The document name.
	 * @return this fluent instance.
	 */
	public @Nonnull FluentMetaDataQuery documentName(@Nonnull String documentName) {
		query.setDocumentName(documentName);
		return this;
	}

	/**
	 * Sets whether polymorphic document types are included.
	 *
	 * @param polymorphic {@code true} to include polymorphic types.
	 * @return this fluent instance.
	 */
	public @Nonnull FluentMetaDataQuery polymorphic(boolean polymorphic) {
		query.setPolymorphic(polymorphic ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether this query performs aggregation.
	 *
	 * @param aggregate {@code true} when aggregate query semantics apply.
	 * @return this fluent instance.
	 */
	public @Nonnull FluentMetaDataQuery aggregate(boolean aggregate) {
		query.setAggregate(aggregate ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets the FROM clause used by this query definition.
	 *
	 * @param from The FROM clause.
	 * @return this fluent instance.
	 */
	public @Nonnull FluentMetaDataQuery from(@Nullable String from) {
		query.setFrom(from);
		return this;
	}

	/**
	 * Sets the filter clause used by this query definition.
	 *
	 * @param filter The filter clause.
	 * @return this fluent instance.
	 */
	public @Nonnull FluentMetaDataQuery filter(@Nullable String filter) {
		query.setFilter(filter);
		return this;
	}

	/**
	 * Sets the GROUP BY clause used by this query definition.
	 *
	 * @param grouping The grouping clause.
	 * @return this fluent instance.
	 */
	public @Nonnull FluentMetaDataQuery grouping(@Nullable String grouping) {
		query.setGrouping(grouping);
		return this;
	}
	
	/**
	 * Sets the ORDER BY clause used by this query definition.
	 *
	 * @param ordering The ordering clause.
	 * @return this fluent instance.
	 */
	public @Nonnull FluentMetaDataQuery ordering(@Nullable String ordering) {
		query.setOrdering(ordering);
		return this;
	}

	/**
	 * Adds a projected column definition.
	 *
	 * @param column The projected column wrapper.
	 * @return this fluent instance.
	 */
	public @Nonnull FluentMetaDataQuery addProjectedColumn(@Nonnull FluentMetaDataQueryProjectedColumn column) {
		query.getColumns().add(column.get());
		return this;
	}

	/**
	 * Finds a projected column by name.
	 *
	 * @param name The projected column name.
	 * @return A fluent wrapper for the matched column, or {@code null} when not found.
	 */
	public @Nullable FluentMetaDataQueryProjectedColumn findProjectedColumnByName(@Nonnull String name) {
		MetaDataQueryProjectedColumnMetaData result = (MetaDataQueryProjectedColumnMetaData) query.getColumns().stream().filter(c -> name.equals(c.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentMetaDataQueryProjectedColumn(result);
		}
		return null;
	}

	/**
	 * Finds a projected column by binding.
	 *
	 * @param binding The projected column binding.
	 * @return A fluent wrapper for the matched column, or {@code null} when not found.
	 */
	public @Nullable FluentMetaDataQueryProjectedColumn findProjectedColumnByBinding(@Nonnull String binding) {
		MetaDataQueryProjectedColumnMetaData result = (MetaDataQueryProjectedColumnMetaData) query.getColumns().stream().filter(c -> binding.equals(c.getBinding())).findAny().orElse(null);
		if (result != null) {
			return new FluentMetaDataQueryProjectedColumn(result);
		}
		return null;
	}

	/**
	 * Finds a projected column by display name.
	 *
	 * @param displayName The projected column display name.
	 * @return A fluent wrapper for the matched column, or {@code null} when not found.
	 */
	public @Nullable FluentMetaDataQueryProjectedColumn findProjectedColumnByDisplayName(@Nonnull String displayName) {
		MetaDataQueryProjectedColumnMetaData result = (MetaDataQueryProjectedColumnMetaData) query.getColumns().stream().filter(c -> displayName.equals(c.getDisplayName())).findAny().orElse(null);
		if (result != null) {
			return new FluentMetaDataQueryProjectedColumn(result);
		}
		return null;
	}

	/**
	 * Adds a content column definition.
	 *
	 * @param column The content column wrapper.
	 * @return this fluent instance.
	 */
	public @Nonnull FluentMetaDataQuery addContentColumn(@Nonnull FluentMetaDataQueryContentColumn column) {
		query.getColumns().add(column.get());
		return this;
	}

	/**
	 * Finds a content column by name.
	 *
	 * @param name The content column name.
	 * @return A fluent wrapper for the matched column, or {@code null} when not found.
	 */
	public @Nullable FluentMetaDataQueryContentColumn findContentColumnByName(@Nonnull String name) {
		MetaDataQueryContentColumnMetaData result = (MetaDataQueryContentColumnMetaData) query.getColumns().stream().filter(c -> name.equals(c.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentMetaDataQueryContentColumn(result);
		}
		return null;
	}

	/**
	 * Finds a content column by binding.
	 *
	 * @param binding The content column binding.
	 * @return A fluent wrapper for the matched column, or {@code null} when not found.
	 */
	public @Nullable FluentMetaDataQueryContentColumn findContentColumnByBinding(@Nonnull String binding) {
		MetaDataQueryContentColumnMetaData result = (MetaDataQueryContentColumnMetaData) query.getColumns().stream().filter(c -> binding.equals(c.getBinding())).findAny().orElse(null);
		if (result != null) {
			return new FluentMetaDataQueryContentColumn(result);
		}
		return null;
	}

	/**
	 * Finds a content column by display name.
	 *
	 * @param displayName The content column display name.
	 * @return A fluent wrapper for the matched column, or {@code null} when not found.
	 */
	public @Nullable FluentMetaDataQueryContentColumn findContentColumnByDisplayName(@Nonnull String displayName) {
		MetaDataQueryContentColumnMetaData result = (MetaDataQueryContentColumnMetaData) query.getColumns().stream().filter(c -> displayName.equals(c.getDisplayName())).findAny().orElse(null);
		if (result != null) {
			return new FluentMetaDataQueryContentColumn(result);
		}
		return null;
	}

	/**
	 * Removes columns matching the supplied name.
	 *
	 * @param name The logical column name.
	 * @return this fluent instance.
	 */
	public @Nonnull FluentMetaDataQuery removeColumnByName(@Nonnull String name) {
		query.getColumns().removeIf(c -> name.equals(c.getName()));
		return this;
	}

	/**
	 * Removes columns matching the supplied binding expression.
	 *
	 * @param binding The column binding.
	 * @return this fluent instance.
	 */
	public @Nonnull FluentMetaDataQuery removeColumnByBinding(@Nonnull String binding) {
		query.getColumns().removeIf(c -> binding.equals(c.getBinding()));
		return this;
	}

	/**
	 * Removes columns matching the supplied display name.
	 *
	 * @param displayName The display name.
	 * @return this fluent instance.
	 */
	public @Nonnull FluentMetaDataQuery removeColumnByDisplayName(@Nonnull String displayName) {
		query.getColumns().removeIf(c -> displayName.equals(c.getDisplayName()));
		return this;
	}

	/**
	 * Removes all configured metadata query columns.
	 *
	 * @return this fluent instance.
	 */
	public @Nonnull FluentMetaDataQuery clearColumns() {
		query.getColumns().clear();
		return this;
	}

	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The metadata query metadata instance.
	 */
	@Override
	public @Nonnull MetaDataQueryMetaData get() {
		return query;
	}
}
