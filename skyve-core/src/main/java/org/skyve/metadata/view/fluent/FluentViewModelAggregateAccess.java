package org.skyve.metadata.view.fluent;

import java.util.Set;

import org.skyve.impl.metadata.repository.view.access.ViewModelAggregateUserAccessMetaData;

/**
 * A fluent helper builder class to construct and manipulate the {@link ViewModelAggregateUserAccessMetaData} metadata.
 * 
 * @author brandon-klar
 */
public class FluentViewModelAggregateAccess extends FluentViewUserAccess<FluentViewModelAggregateAccess, ViewModelAggregateUserAccessMetaData> {
	/**
	 * Creates a new FluentViewModelAggregateAccess
	 */
	public FluentViewModelAggregateAccess() {
		access = new ViewModelAggregateUserAccessMetaData();
	}

	/**
	 * Creates a new FluentViewModelAggregateAccess from the specified ViewModelAggregateUserAccessMetaData.
	 */
	public FluentViewModelAggregateAccess(ViewModelAggregateUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Returns a FluentViewModelAggregateAccess from a runtime metadata.
	 */
	protected FluentViewModelAggregateAccess from(String modelName, Set<String> uxuis) {
		modelName(modelName);
		uxuis.forEach(u -> addUxUi(u));
		return this;
	}

	/**
	 * Specifies the model name for this FluentViewModelAggregateAccess.
	 */
	public FluentViewModelAggregateAccess modelName(final String modelName) {
		access.setModelName(modelName);
		return this;
	}

	/**
	 * Returns the underlying model aggregate user access metadata from this builder.
	 */
	@Override
	public ViewModelAggregateUserAccessMetaData get() {
		return access;
	}
}
