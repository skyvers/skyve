package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;

public class FluentEnumeration extends FluentConstrainableField<FluentEnumeration> {
	private Enumeration enumeration = new Enumeration();
	
	public FluentEnumeration() {
		// nothing to see
	}

	public FluentEnumeration(Enumeration enumeration) {
		super(enumeration);
		typeName(enumeration.getTypeName());
		implementingEnumClassName(enumeration.getImplementingEnumClassName());
		moduleRef(enumeration.getModuleRef());
		documentRef(enumeration.getDocumentRef());
		attributeRef(enumeration.getAttributeRef());
		
		for (EnumeratedValue value : enumeration.getValues()) {
			addValue(new FluentEnumeratedValue(value));
		}
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
