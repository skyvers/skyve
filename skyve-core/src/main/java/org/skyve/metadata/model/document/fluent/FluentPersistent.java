package org.skyve.metadata.model.document.fluent;

import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;

public class FluentPersistent {
	Persistent persistent = new Persistent();
	
	public FluentPersistent() {
		// nothing to see
	}

	public FluentPersistent(Persistent persistent) {
		name(persistent.getName());
		schema(persistent.getSchema());
		catalog(persistent.getCatalog());
		strategy(persistent.getStrategy());
		discriminator(persistent.getDiscriminator());
		cacheName(persistent.getCacheName());
	}
	
	public FluentPersistent name(String name) {
		persistent.setName(name);
		return this;
	}

	public FluentPersistent schema(String schema) {
		persistent.setSchema(schema);
		return this;
	}

	public FluentPersistent catalog(String catalog) {
		persistent.setCatalog(catalog);
		return this;
	}

	public FluentPersistent strategy(ExtensionStrategy strategy) {
		persistent.setStrategy(strategy);
		return this;
	}
	public FluentPersistent discriminator(String discriminator) {
		persistent.setDiscriminator(discriminator);
		return this;
	}

	public FluentPersistent cacheName(String cacheName) {
		persistent.setCacheName(cacheName);
		return this;
	}

	public Persistent get() {
		return persistent;
	}
}
