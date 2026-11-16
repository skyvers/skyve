package org.skyve.metadata.model.document.fluent;

import org.skyve.metadata.model.Dynamic;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
	public FluentDynamic(@Nonnull Dynamic dynamic) {
		this.dynamic = dynamic;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public @Nonnull FluentDynamic from(@SuppressWarnings("hiding") @Nonnull Dynamic dynamic) {
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
	public @Nonnull FluentDynamic bizletClassName(@Nullable String bizletClassName) {
		dynamic.setBizletClassName(bizletClassName);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentDynamic dataFactoryClassName(@Nullable String dataFactoryClassName) {
		dynamic.setDataFactoryClassName(dataFactoryClassName);
		return this;
	}

	/**
	 * Registers or replaces a dynamic action implementation mapping.
	 */
	public @Nonnull FluentDynamic addAction(@Nonnull String name, @Nonnull String fullyQualifiedClassName) {
		dynamic.getActions().put(name, fullyQualifiedClassName);
		return this;
	}

	/**
	 * Removes a dynamic action mapping.
	 */
	public @Nonnull FluentDynamic removeAction(@Nonnull String name) {
		dynamic.getActions().remove(name);
		return this;
	}

	/**
	 * Registers or replaces a dynamic image implementation mapping.
	 */
	public @Nonnull FluentDynamic addImage(@Nonnull String name, @Nonnull String fullyQualifiedClassName) {
		dynamic.getImages().put(name, fullyQualifiedClassName);
		return this;
	}

	/**
	 * Removes a dynamic image mapping.
	 */
	public @Nonnull FluentDynamic removeImage(@Nonnull String name) {
		dynamic.getImages().remove(name);
		return this;
	}

	/**
	 * Registers or replaces a dynamic model implementation mapping.
	 */
	public @Nonnull FluentDynamic addModel(@Nonnull String name, @Nonnull String fullyQualifiedClassName) {
		dynamic.getModels().put(name, fullyQualifiedClassName);
		return this;
	}

	/**
	 * Removes a dynamic model mapping.
	 */
	public @Nonnull FluentDynamic removeModel(@Nonnull String name) {
		dynamic.getModels().remove(name);
		return this;
	}

	/**
	 * Returns the mutable dynamic metadata instance being built.
	 */
	public @Nonnull Dynamic get() {
		return dynamic;
	}
}
