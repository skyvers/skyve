package org.skyve.metadata.module.fluent;

import java.util.Map.Entry;

import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.query.BizQLDefinition;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.module.query.SQLDefinition;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.view.View.ViewType;

public class FluentModule {
	private ModuleMetaData module = new ModuleMetaData();
	
	public FluentModule() {
		// nothing to see here
	}
	
	public FluentModule(Module module) {
		name(module.getName());
		title(module.getTitle());
		documentation(module.getDocumentation());
		prototype(module.isPrototype());
		homeRef(module.getHomeRef());
		homeDocument(module.getHomeDocumentName());

		// Populate Jobs
		for (JobMetaData job : module.getJobs()) {
			addJob(new FluentJob(job));
		}
		
		// Populate document refs
		for (Entry<String, DocumentRef> ref : module.getDocumentRefs().entrySet()) {
			addDocument(new FluentModuleDocument(ref.getKey(), ref.getValue()));
		}
		
		// Populate queries
		for (QueryDefinition query : module.getMetadataQueries()) {
			if (query instanceof MetaDataQueryDefinition) {
				addMetaDataQuery(new FluentMetaDataQuery((MetaDataQueryDefinition) query));
			}
			else if (query instanceof SQLDefinition) {
				addSQL(new FluentSQL((SQLDefinition) query));
			}
			else {
				addBizQL(new FluentBizQL((BizQLDefinition) query));
			}
		}
		
		// Populate Roles
		for (Role role : module.getRoles()) {
			addRole(new FluentModuleRole(role));
		}

		// Populate the menu
		menu(new FluentMenu(module.getMenu()));
	}
	
	public FluentModule name(String name) {
		module.setName(name);
		return this;
	}
	
	public FluentModule title(String title) {
		module.setTitle(title);
		return this;
	}
	
	public FluentModule documentation(String documentation) {
		module.setDocumentation(documentation);
		return this;
	}

	public FluentModule prototype(boolean prototype) {
		module.setPrototype(prototype ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentModule homeRef(ViewType homeRef) {
		module.setHomeRef(homeRef);
		return this;
	}
	
	public FluentModule homeDocument(String documentName) {
		module.setHomeDocument(documentName);
		return this;
	}
	
	public FluentModule addJob(FluentJob job) {
		module.getJobs().add(job.get());
		return this;
	}
	
	public FluentModule addDocument(FluentModuleDocument document) {
		module.getDocuments().add(document.get());
		return this;
	}

	public FluentModule addMetaDataQuery(FluentMetaDataQuery query) {
		module.getQueries().add(query.get());
		return this;
	}

	public FluentModule addSQL(FluentSQL sql) {
		module.getQueries().add(sql.get());
		return this;
	}
	
	public FluentModule addBizQL(FluentBizQL bizql) {
		module.getQueries().add(bizql.get());
		return this;
	}
	
	public FluentModule addRole(FluentModuleRole role) {
		module.getRoles().add(role.get());
		return this;
	}
	
	public FluentModule menu(FluentMenu menu) {
		module.setMenu(menu.get());
		return this;
	}
	
	public ModuleMetaData get() {
		return module;
	}
}
