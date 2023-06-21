package org.skyve.metadata.view.fluent;

import java.util.Set;

import org.skyve.impl.metadata.repository.view.access.ViewQueryAggregateUserAccessMetaData;

/**
 * A fluent helper builder class to construct and manipulate the {@link ViewQueryAggregateUserAccessMetaData} metadata.
 * 
 * @author brandon-klar
 */
public class FluentViewQueryAggregateAccess extends FluentViewUserAccess<FluentViewQueryAggregateAccess, ViewQueryAggregateUserAccessMetaData> {
	/**
	 * Creates a new FluentViewQueryAggregateAccess
	 */
	public FluentViewQueryAggregateAccess() {
		access = new ViewQueryAggregateUserAccessMetaData();
	}

	/**
	 * Creates a new FluentViewQueryAggregateAccess from the specified ViewQueryAggregateUserAccessMetaData.
	 */
	public FluentViewQueryAggregateAccess(ViewQueryAggregateUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Returns a FluentViewQueryAggregateAccess from a runtime metadata.
	 */
	protected FluentViewQueryAggregateAccess from(Set<String> uxuis) {
		uxuis.forEach(u -> addUxUi(u));
		return this;
	}

	/**
	 * Specifies the query name for this FluentViewQueryAggregateAccess.
	 */
	public FluentViewQueryAggregateAccess queryName(final String queryName) {
		access.setQueryName(queryName);
		return this;
	}

	/**
	 * Returns the underlying view user access metadata from this builder.
	 */
	@Override
	public ViewQueryAggregateUserAccessMetaData get() {
		return access;
	}
}
