package org.skyve.metadata.view.fluent;

import java.util.Set;

import org.skyve.impl.metadata.repository.view.access.ViewContentUserAccessMetaData;

import jakarta.annotation.Nonnull;

/**
 * A fluent helper builder class to construct and manipulate the {@link ViewContentUserAccessMetaData} metadata.
 * 
 * @author mike
 */
public class FluentViewContentAccess extends FluentViewUserAccess<FluentViewContentAccess, ViewContentUserAccessMetaData> {
	/**
	 * Creates a new FluentViewContentAccess
	 */
	public FluentViewContentAccess() {
		access = new ViewContentUserAccessMetaData();
	}

	/**
	 * Creates a new FluentViewContentAccess from the specified ViewContentUserAccessMetaData.
	 */
	public FluentViewContentAccess(@Nonnull ViewContentUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Returns a FluentViewContentAccess from a runtime metadata.
	 */
	protected @Nonnull FluentViewContentAccess from(@Nonnull String binding, @Nonnull Set<String> uxuis) {
		binding(binding);
		uxuis.forEach(u -> addUxUi(u));
		return this;
	}

	/**
	 * Specifies the binding for this FluentViewContentAccess.
	 */
	public @Nonnull FluentViewContentAccess binding(final @Nonnull String binding) {
		access.setBinding(binding);
		return this;
	}

	/**
	 * Returns the underlying view user access metadata from this builder.
	 */
	@Override
	public @Nonnull ViewContentUserAccessMetaData get() {
		return access;
	}
}
