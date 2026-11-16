package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Provides a fluent builder for FluentEnumeration metadata.
 */
public class FluentEnumeration extends FluentConstrainableField<FluentEnumeration> {
	private Enumeration enumeration = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentEnumeration() {
		enumeration = new Enumeration();
	}

	/**
	 * Creates a fluent builder instance.
	 */
	public FluentEnumeration(@Nonnull Enumeration enumeration) {
		this.enumeration = enumeration;
	}

	/**
	 * Copies enumeration metadata, including all configured values and reference links.
	 */
	public @Nonnull FluentEnumeration from(@SuppressWarnings("hiding") @Nonnull Enumeration enumeration) {
		super.from(enumeration);
		typeName(enumeration.getTypeName());
		implementingEnumClassName(enumeration.getImplementingEnumClassName());
		moduleRef(enumeration.getModuleRef());
		documentRef(enumeration.getDocumentRef());
		attributeRef(enumeration.getAttributeRef());
		
		for (EnumeratedValue value : enumeration.getXmlValues()) {
			addValue(new FluentEnumeratedValue().from(value));
		}
		
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentEnumeration typeName(@Nullable String typeName) {
		enumeration.setXmlTypeName(typeName);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentEnumeration implementingEnumClassName(@Nullable String implementingEnumClassName) {
		enumeration.setXmlImplementingEnumClassName(implementingEnumClassName);
		return this;
	}

	/**
	 * Appends an enumerated value.
	 */
	public @Nonnull FluentEnumeration addValue(@Nonnull FluentEnumeratedValue value) {
		enumeration.getXmlValues().add(value.get());
		return this;
	}

	/**
	 * Removes values that match {@code name}.
	 */
	public @Nonnull FluentEnumeration removeValueByName(@Nonnull String name) {
		enumeration.getXmlValues().removeIf(v -> name.equals(v.getName()));
		return this;
	}

	/**
	 * Finds the first value with the supplied {@code name}.
	 *
	 * @return a fluent wrapper around the matching value, or {@code null} when absent
	 */
	public @Nullable FluentEnumeratedValue findValueByName(@Nonnull String name) {
		EnumeratedValue result = enumeration.getXmlValues().stream().filter(v -> name.equals(v.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentEnumeratedValue(result);
		}
		return null;
	}
	/**
	 * Removes values that match {@code code}.
	 */
	public @Nonnull FluentEnumeration removeValueByCode(@Nonnull String code) {
		enumeration.getXmlValues().removeIf(v -> code.equals(v.getCode()));
		return this;
	}

	/**
	 * Finds the first value with the supplied {@code code}.
	 *
	 * @return a fluent wrapper around the matching value, or {@code null} when absent
	 */
	public @Nullable FluentEnumeratedValue findValueByCode(@Nonnull String code) {
		EnumeratedValue result = enumeration.getXmlValues().stream().filter(v -> code.equals(v.getCode())).findAny().orElse(null);
		if (result != null) {
			return new FluentEnumeratedValue(result);
		}
		return null;
	}
	/**
	 * Removes values that match {@code description}.
	 */
	public @Nonnull FluentEnumeration removeValueByDescription(@Nonnull String description) {
		enumeration.getXmlValues().removeIf(v -> description.equals(v.getDescription()));
		return this;
	}

	/**
	 * Finds the first value with the supplied {@code description}.
	 *
	 * @return a fluent wrapper around the matching value, or {@code null} when absent
	 */
	public @Nullable FluentEnumeratedValue findValueByDescription(@Nonnull String description) {
		EnumeratedValue result = enumeration.getXmlValues().stream().filter(v -> description.equals(v.getDescription())).findAny().orElse(null);
		if (result != null) {
			return new FluentEnumeratedValue(result);
		}
		return null;
	}
	/**
	 * Removes all enumerated values.
	 */
	public @Nonnull FluentEnumeration clearValues() {
		enumeration.getXmlValues().clear();
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentEnumeration moduleRef(@Nullable String moduleRef) {
		enumeration.setModuleRef(moduleRef);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentEnumeration documentRef(@Nullable String documentRef) {
		enumeration.setDocumentRef(documentRef);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentEnumeration attributeRef(@Nullable String attributeRef) {
		enumeration.setAttributeRef(attributeRef);
		return this;
	}
	
	// TODO public FluentEnumeration Document owningDocument; - set this when its added to a document
	// TODO call setRepository when the document is added to the repository or loaded into the repository

	/**
	 * Returns the mutable enumeration metadata instance being built.
	 */
	@Override
	public @Nonnull Enumeration get() {
		return enumeration;
	}
}
