package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.ModuleDocument;
import org.skyve.metadata.module.Module.DocumentRef;

public class FluentModuleDocument {
	private ModuleDocument document = null;
	
	public FluentModuleDocument() {
		document = new ModuleDocument();
	}

	public FluentModuleDocument(ModuleDocument document) {
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

	public ModuleDocument get() {
		return document;
	}
}
