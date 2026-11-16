package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.BizQLMetaData;
import org.skyve.metadata.module.query.BizQLDefinition;

import jakarta.annotation.Nonnull;

/**
 * Builds {@link BizQLMetaData} query definitions.
 */
public class FluentBizQL extends FluentQueryDefinition<FluentBizQL> {
	private BizQLMetaData bizql = null;
	
	/**
	 * Creates a fluent wrapper with a new metadata instance.
	 */
	public FluentBizQL() {
		bizql = new BizQLMetaData();
	}
	
	/**
	 * Creates a fluent wrapper around an existing metadata instance.
	 *
	 * @param bizql The metadata to mutate.
	 */
	public FluentBizQL(@Nonnull BizQLMetaData bizql) {
		this.bizql = bizql;
	}
	
	/**
	 * Copies all definition fields from an existing BizQL definition.
	 *
	 * @param bizql The source definition.
	 * @return this fluent instance.
	 */
	public @Nonnull FluentBizQL from(@SuppressWarnings("hiding") @Nonnull BizQLDefinition bizql) {
		super.from(bizql);
		query(bizql.getQuery());
		return this;
	}
	
	/**
	 * Sets the BizQL statement text.
	 *
	 * @param query The BizQL statement.
	 * @return this fluent instance.
	 */
	public @Nonnull FluentBizQL query(@Nonnull String query) {
		bizql.setQuery(query);
		return this;
	}

	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The BizQL metadata instance.
	 */
	@Override
	public @Nonnull BizQLMetaData get() {
		return bizql;
	}
}
