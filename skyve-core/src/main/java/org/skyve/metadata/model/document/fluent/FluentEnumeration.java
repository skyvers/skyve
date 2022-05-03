package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;

public class FluentEnumeration extends FluentConstrainableField<FluentEnumeration> {
	private Enumeration enumeration = null;
	
	public FluentEnumeration() {
		enumeration = new Enumeration();
	}

	public FluentEnumeration(Enumeration enumeration) {
		this.enumeration = enumeration;
	}

	public FluentEnumeration from(@SuppressWarnings("hiding") Enumeration enumeration) {
		super.from(enumeration);
		typeName(enumeration.getTypeName());
		implementingEnumClassName(enumeration.getImplementingEnumClassName());
		moduleRef(enumeration.getModuleRef());
		documentRef(enumeration.getDocumentRef());
		attributeRef(enumeration.getAttributeRef());
		
		for (EnumeratedValue value : enumeration.getValues()) {
			addValue(new FluentEnumeratedValue().from(value));
		}
		
		return this;
	}
	
	public FluentEnumeration typeName(String typeName) {
		enumeration.setXmlTypeName(typeName);
		return this;
	}

	public FluentEnumeration implementingEnumClassName(String implementingEnumClassName) {
		enumeration.setXmlImplementingEnumClassName(implementingEnumClassName);
		return this;
	}

	public FluentEnumeration addValue(FluentEnumeratedValue value) {
		enumeration.getXmlValues().add(value.get());
		return this;
	}

	public FluentEnumeration removeValueByName(String name) {
		enumeration.getXmlValues().removeIf(v -> name.equals(v.getName()));
		return this;
	}

	public FluentEnumeratedValue findValueByName(String name) {
		EnumeratedValue result = enumeration.getXmlValues().stream().filter(v -> name.equals(v.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentEnumeratedValue(result);
		}
		return null;
	}
	
	public FluentEnumeration removeValueByCode(String code) {
		enumeration.getXmlValues().removeIf(v -> code.equals(v.getCode()));
		return this;
	}

	public FluentEnumeratedValue findValueByCode(String code) {
		EnumeratedValue result = enumeration.getXmlValues().stream().filter(v -> code.equals(v.getCode())).findAny().orElse(null);
		if (result != null) {
			return new FluentEnumeratedValue(result);
		}
		return null;
	}
	
	public FluentEnumeration removeValueByDescription(String description) {
		enumeration.getXmlValues().removeIf(v -> description.equals(v.getDescription()));
		return this;
	}

	public FluentEnumeratedValue findValueByDescription(String description) {
		EnumeratedValue result = enumeration.getXmlValues().stream().filter(v -> description.equals(v.getDescription())).findAny().orElse(null);
		if (result != null) {
			return new FluentEnumeratedValue(result);
		}
		return null;
	}

	public FluentEnumeration clearValues() {
		enumeration.getXmlValues().clear();
		return this;
	}
	
	public FluentEnumeration moduleRef(String moduleRef) {
		enumeration.setModuleRef(moduleRef);
		return this;
	}

	public FluentEnumeration documentRef(String documentRef) {
		enumeration.setDocumentRef(documentRef);
		return this;
	}

	public FluentEnumeration attributeRef(String attributeRef) {
		enumeration.setAttributeRef(attributeRef);
		return this;
	}
	
	// TODO public FluentEnumeration Document owningDocument; - set this when its added to a document
	// TODO call setRepository when the document is added to the repository or loaded into the repository

	@Override
	public Enumeration get() {
		return enumeration;
	}
}
