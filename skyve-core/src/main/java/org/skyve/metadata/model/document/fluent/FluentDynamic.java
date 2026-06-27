package org.skyve.metadata.model.document.fluent;

import org.skyve.metadata.model.Dynamic;

/**
 * Provides a fluent builder for FluentDynamic metadata.
 */
public class FluentDynamic {
	private Dynamic dynamic = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentDynamic() {
		dynamic = new Dynamic();
	}
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentDynamic(Dynamic dynamic) {
		this.dynamic = dynamic;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentDynamic from(@SuppressWarnings("hiding") Dynamic dynamic) {
		bizletClassName(dynamic.getBizletClassName());
		dataFactoryClassName(dynamic.getDataFactoryClassName());
		dynamic.getActions().forEach(this::addAction);
		dynamic.getImages().forEach(this::addImage);
		dynamic.getModels().forEach(this::addModel);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentDynamic bizletClassName(String bizletClassName) {
		dynamic.setBizletClassName(bizletClassName);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentDynamic dataFactoryClassName(String dataFactoryClassName) {
		dynamic.setDataFactoryClassName(dataFactoryClassName);
		return this;
	}

	/**
	 * Registers or replaces a dynamic action implementation mapping.
	 */
	public FluentDynamic addAction(String name, String fullyQualifiedClassName) {
		dynamic.getActions().put(name, fullyQualifiedClassName);
		return this;
	}

	/**
	 * Removes a dynamic action mapping.
	 */
	public FluentDynamic removeAction(String name) {
		dynamic.getActions().remove(name);
		return this;
	}

	/**
	 * Registers or replaces a dynamic image implementation mapping.
	 */
	public FluentDynamic addImage(String name, String fullyQualifiedClassName) {
		dynamic.getImages().put(name, fullyQualifiedClassName);
		return this;
	}

	/**
	 * Removes a dynamic image mapping.
	 */
	public FluentDynamic removeImage(String name) {
		dynamic.getImages().remove(name);
		return this;
	}

	/**
	 * Registers or replaces a dynamic model implementation mapping.
	 */
	public FluentDynamic addModel(String name, String fullyQualifiedClassName) {
		dynamic.getModels().put(name, fullyQualifiedClassName);
		return this;
	}

	/**
	 * Removes a dynamic model mapping.
	 */
	public FluentDynamic removeModel(String name) {
		dynamic.getModels().remove(name);
		return this;
	}

	/**
	 * Returns the mutable dynamic metadata instance being built.
	 */
	public Dynamic get() {
		return dynamic;
	}
}
