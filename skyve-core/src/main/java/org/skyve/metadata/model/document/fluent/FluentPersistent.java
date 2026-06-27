package org.skyve.metadata.model.document.fluent;

import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;

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
	public FluentPersistent(Persistent persistent) {
		this.persistent = persistent;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentPersistent from(@SuppressWarnings("hiding") Persistent persistent) {
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
	public FluentPersistent name(String name) {
		persistent.setName(name);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentPersistent schema(String schema) {
		persistent.setSchema(schema);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentPersistent catalog(String catalog) {
		persistent.setCatalog(catalog);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentPersistent strategy(ExtensionStrategy strategy) {
		persistent.setStrategy(strategy);
		return this;
	}
	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentPersistent discriminator(String discriminator) {
		persistent.setDiscriminator(discriminator);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentPersistent cacheName(String cacheName) {
		persistent.setCacheName(cacheName);
		return this;
	}
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	public Persistent get() {
		return persistent;
	}
}
