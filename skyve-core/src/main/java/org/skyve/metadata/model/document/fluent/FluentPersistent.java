package org.skyve.metadata.model.document.fluent;

import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Provides a fluent builder for FluentPersistent metadata.
 */
public class FluentPersistent {
	private Persistent persistent = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentPersistent() {
		persistent = new Persistent();
	}
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentPersistent(@Nonnull Persistent persistent) {
		this.persistent = persistent;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public @Nonnull FluentPersistent from(@SuppressWarnings("hiding") @Nonnull Persistent persistent) {
		name(persistent.getName());
		schema(persistent.getSchema());
		catalog(persistent.getCatalog());
		strategy(persistent.getStrategy());
		discriminator(persistent.getDiscriminator());
		cacheName(persistent.getCacheName());
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentPersistent name(@Nullable String name) {
		persistent.setName(name);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentPersistent schema(@Nullable String schema) {
		persistent.setSchema(schema);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentPersistent catalog(@Nullable String catalog) {
		persistent.setCatalog(catalog);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentPersistent strategy(@Nullable ExtensionStrategy strategy) {
		persistent.setStrategy(strategy);
		return this;
	}
	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentPersistent discriminator(@Nullable String discriminator) {
		persistent.setDiscriminator(discriminator);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentPersistent cacheName(@Nullable String cacheName) {
		persistent.setCacheName(cacheName);
		return this;
	}
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	public @Nonnull Persistent get() {
		return persistent;
	}
}
