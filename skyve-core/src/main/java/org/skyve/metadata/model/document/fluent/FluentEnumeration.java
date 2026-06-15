package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;

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
	public FluentEnumeration(Enumeration enumeration) {
		this.enumeration = enumeration;
	}

	/**
	 * Copies enumeration metadata, including all configured values and reference links.
	 */
	public FluentEnumeration from(@SuppressWarnings("hiding") Enumeration enumeration) {
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
	public FluentEnumeration typeName(String typeName) {
		enumeration.setXmlTypeName(typeName);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentEnumeration implementingEnumClassName(String implementingEnumClassName) {
		enumeration.setXmlImplementingEnumClassName(implementingEnumClassName);
		return this;
	}

	/**
	 * Appends an enumerated value.
	 */
	public FluentEnumeration addValue(FluentEnumeratedValue value) {
		enumeration.getXmlValues().add(value.get());
		return this;
	}

	/**
	 * Removes values that match {@code name}.
	 */
	public FluentEnumeration removeValueByName(String name) {
		enumeration.getXmlValues().removeIf(v -> name.equals(v.getName()));
		return this;
	}

	/**
	 * Finds the first value with the supplied {@code name}.
	 *
	 * @return a fluent wrapper around the matching value, or {@code null} when absent
	 */
	public FluentEnumeratedValue findValueByName(String name) {
		EnumeratedValue result = enumeration.getXmlValues().stream().filter(v -> name.equals(v.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentEnumeratedValue(result);
		}
		return null;
	}
	/**
	 * Removes values that match {@code code}.
	 */
	public FluentEnumeration removeValueByCode(String code) {
		enumeration.getXmlValues().removeIf(v -> code.equals(v.getCode()));
		return this;
	}

	/**
	 * Finds the first value with the supplied {@code code}.
	 *
	 * @return a fluent wrapper around the matching value, or {@code null} when absent
	 */
	public FluentEnumeratedValue findValueByCode(String code) {
		EnumeratedValue result = enumeration.getXmlValues().stream().filter(v -> code.equals(v.getCode())).findAny().orElse(null);
		if (result != null) {
			return new FluentEnumeratedValue(result);
		}
		return null;
	}
	/**
	 * Removes values that match {@code description}.
	 */
	public FluentEnumeration removeValueByDescription(String description) {
		enumeration.getXmlValues().removeIf(v -> description.equals(v.getDescription()));
		return this;
	}

	/**
	 * Finds the first value with the supplied {@code description}.
	 *
	 * @return a fluent wrapper around the matching value, or {@code null} when absent
	 */
	public FluentEnumeratedValue findValueByDescription(String description) {
		EnumeratedValue result = enumeration.getXmlValues().stream().filter(v -> description.equals(v.getDescription())).findAny().orElse(null);
		if (result != null) {
			return new FluentEnumeratedValue(result);
		}
		return null;
	}
	/**
	 * Removes all enumerated values.
	 */
	public FluentEnumeration clearValues() {
		enumeration.getXmlValues().clear();
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentEnumeration moduleRef(String moduleRef) {
		enumeration.setModuleRef(moduleRef);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentEnumeration documentRef(String documentRef) {
		enumeration.setDocumentRef(documentRef);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentEnumeration attributeRef(String attributeRef) {
		enumeration.setAttributeRef(attributeRef);
		return this;
	}
	
	// TODO public FluentEnumeration Document owningDocument; - set this when its added to a document
	// TODO call setRepository when the document is added to the repository or loaded into the repository

	/**
	 * Returns the mutable enumeration metadata instance being built.
	 */
	@Override
	public Enumeration get() {
		return enumeration;
	}
}
