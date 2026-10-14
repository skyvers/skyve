package org.skyve.metadata.view.fluent;

import java.util.Set;

import org.skyve.impl.metadata.repository.view.access.ViewModelAggregateUserAccessMetaData;

import jakarta.annotation.Nonnull;

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
	public FluentViewModelAggregateAccess(@Nonnull ViewModelAggregateUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Returns a FluentViewModelAggregateAccess from a runtime metadata.
	 */
	protected @Nonnull FluentViewModelAggregateAccess from(@Nonnull String modelName, @Nonnull Set<String> uxuis) {
		modelName(modelName);
		uxuis.forEach(u -> addUxUi(u));
		return this;
	}

	/**
	 * Specifies the model name for this FluentViewModelAggregateAccess.
	 */
	public @Nonnull FluentViewModelAggregateAccess modelName(final @Nonnull String modelName) {
		access.setModelName(modelName);
		return this;
	}

	/**
	 * Returns the underlying model aggregate user access metadata from this builder.
	 */
	@Override
	public @Nonnull ViewModelAggregateUserAccessMetaData get() {
		return access;
	}
}
