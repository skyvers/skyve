package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.ModuleDocumentMetaData;
import org.skyve.metadata.module.Module.DocumentRef;

public class FluentModuleDocument {
	private ModuleDocumentMetaData document = null;
	
	public FluentModuleDocument() {
		document = new ModuleDocumentMetaData();
	}

	public FluentModuleDocument(ModuleDocumentMetaData document) {
		this.document = document;
	}

	public FluentModuleDocument from(String documentName, DocumentRef ref) {
		moduleRef(ref.getReferencedModuleName());
		ref(documentName);
		defaultQueryName(ref.getDefaultQueryName());
		return this;
	}
	
	public FluentModuleDocument moduleRef(String moduleName) {
		document.setModuleRef(moduleName);
		return this;
	}

	public FluentModuleDocument ref(String documentName) {
		document.setRef(documentName);
		return this;
	}
	
	public FluentModuleDocument defaultQueryName(String queryName) {
		document.setDefaultQueryName(queryName);
		return this;
	}

	public ModuleDocumentMetaData get() {
		return document;
	}
}
