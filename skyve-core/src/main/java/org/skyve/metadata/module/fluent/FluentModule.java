package org.skyve.metadata.module.fluent;

import java.util.Map.Entry;

import org.skyve.impl.metadata.module.JobMetaDataImpl;
import org.skyve.impl.metadata.repository.module.BizQLMetaData;
import org.skyve.impl.metadata.repository.module.MetaDataQueryMetaData;
import org.skyve.impl.metadata.repository.module.ModuleDocumentMetaData;
import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.impl.metadata.repository.module.ModuleRoleMetaData;
import org.skyve.impl.metadata.repository.module.SQLMetaData;
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
	private ModuleMetaData module = null;
	
	public FluentModule() {
		module = new ModuleMetaData();
	}

	public FluentModule(ModuleMetaData module) {
		this.module = module;
	}

	public FluentModule from(@SuppressWarnings("hiding") Module module) {
		name(module.getName());
		title(module.getTitle());
		documentation(module.getDocumentation());
		prototype(module.isPrototype());
		homeRef(module.getHomeRef());
		homeDocument(module.getHomeDocumentName());

		// Populate Jobs
		for (JobMetaData job : module.getJobs()) {
			addJob(new FluentJob().from(job));
		}
		
		// Populate document refs
		for (Entry<String, DocumentRef> ref : module.getDocumentRefs().entrySet()) {
			addDocument(new FluentModuleDocument().from(ref.getKey(), ref.getValue()));
		}
		
		// Populate queries
		for (QueryDefinition query : module.getMetadataQueries()) {
			if (query instanceof MetaDataQueryDefinition) {
				addMetaDataQuery(new FluentMetaDataQuery().from((MetaDataQueryDefinition) query));
			}
			else if (query instanceof SQLDefinition) {
				addSQL(new FluentSQL().from((SQLDefinition) query));
			}
			else {
				addBizQL(new FluentBizQL().from((BizQLDefinition) query));
			}
		}
		
		// Populate Roles
		for (Role role : module.getRoles()) {
			addRole(new FluentModuleRole().from(role));
		}

		// Populate the menu
		menu(new FluentMenu().from(module.getMenu()));
		
		return this;
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
	
	public FluentModule removeJob(String name) {
		module.getJobs().removeIf(j -> name.equals(j.getName()));
		return this;
	}
	
	public FluentModule clearJobs() {
		module.getJobs().clear();
		return this;
	}

	public FluentJob findJob(String name) {
		JobMetaDataImpl result = module.getJobs().stream().filter(j -> name.equals(j.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentJob(result);
		}
		return null;
	}
	
	public FluentModule addDocument(FluentModuleDocument document) {
		module.getDocuments().add(document.get());
		return this;
	}

	public FluentModule removeDocument(String name) {
		module.getDocuments().removeIf(d -> name.equals(d.getRef()));
		// TOD remove document privileges?
		return this;
	}

	public FluentModule clearDocuments() {
		module.getDocuments().clear();
		return this;
	}

	public FluentModuleDocument findDocument(String name) {
		ModuleDocumentMetaData result = module.getDocuments().stream().filter(d -> name.equals(d.getRef())).findAny().orElse(null);
		if (result != null) {
			return new FluentModuleDocument(result);
		}
		return null;
	}
	
	public FluentModule addMetaDataQuery(FluentMetaDataQuery query) {
		module.getQueries().add(query.get());
		return this;
	}

	public FluentMetaDataQuery findMetaDataQuery(String name) {
		MetaDataQueryMetaData result = (MetaDataQueryMetaData) module.getQueries().stream().filter(q -> name.equals(q.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentMetaDataQuery(result);
		}
		return null;
	}
	
	public FluentModule addSQL(FluentSQL sql) {
		module.getQueries().add(sql.get());
		return this;
	}
	
	public FluentSQL findSQL(String name) {
		SQLMetaData result = (SQLMetaData) module.getQueries().stream().filter(q -> name.equals(q.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentSQL(result);
		}
		return null;
	}
	
	public FluentModule addBizQL(FluentBizQL bizql) {
		module.getQueries().add(bizql.get());
		return this;
	}
	
	public FluentBizQL findBizQL(String name) {
		BizQLMetaData result = (BizQLMetaData) module.getQueries().stream().filter(q -> name.equals(q.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentBizQL(result);
		}
		return null;
	}

	public FluentModule removeQuery(String name) {
		module.getQueries().removeIf(q -> name.equals(q.getName()));
		return this;
	}

	public FluentModule clearQueries() {
		module.getQueries().clear();
		return this;
	}

	public FluentModule addRole(FluentModuleRole role) {
		module.getRoles().add(role.get());
		return this;
	}

	public FluentModule removeRole(String name) {
		module.getRoles().removeIf(r -> name.equals(r.getName()));
		return this;
	}
	
	public FluentModule clearRoles() {
		module.getRoles().clear();
		return this;
	}

	public FluentModuleRole findRole(String name) {
		ModuleRoleMetaData role = module.getRoles().stream().filter(r -> r.getName().equals(name)).findAny().orElse(null);
		if (role != null) {
			return new FluentModuleRole(role);
		}
		return null;
	}

	public FluentModule menu(FluentMenu menu) {
		module.setMenu(menu.get());
		return this;
	}
	
	public ModuleMetaData get() {
		return module;
	}
}
