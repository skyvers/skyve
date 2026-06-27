package org.skyve.metadata.module.fluent;

import java.util.Map.Entry;

import org.skyve.impl.metadata.module.JobMetaDataImpl;
import org.skyve.impl.metadata.repository.module.BizQLMetaData;
import org.skyve.impl.metadata.repository.module.MetaDataQueryMetaData;
import org.skyve.impl.metadata.repository.module.ModuleDocumentMetaData;
import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.impl.metadata.repository.module.ModuleRoleMetaData;
import org.skyve.impl.metadata.repository.module.SQLMetaData;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.query.BizQLDefinition;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.module.query.SQLDefinition;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.view.View.ViewType;

/**
 * Builds module metadata including documents, queries, roles, jobs, and menu structure.
 */
public class FluentModule {
	private ModuleMetaData module = null;
	
	/**
	 * Creates a fluent wrapper with a new metadata instance.
	 */
	public FluentModule() {
		module = new ModuleMetaData();
	}

	/**
	 * Creates a fluent wrapper around an existing metadata instance.
	 *
	 * @param module The metadata to mutate.
	 */
	public FluentModule(ModuleMetaData module) {
		this.module = module;
	}

	/**
	 * Copies module state from an existing module definition.
	 *
	 * <p>Side effects: appends converted jobs, document refs, queries, roles, and menu entries
	 * to this wrapper's backing module metadata.
	 *
	 * @param module The source module definition.
	 * @return this fluent instance.
	 */
	public FluentModule from(@SuppressWarnings("hiding") Module module) {
		name(module.getName());
		title(module.getTitle());
		documentation(module.getDocumentation());
		prototype(module.isPrototype());
		formLabelLayout(module.getFormLabelLayout());
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
			if (query instanceof MetaDataQueryDefinition meta) {
				addMetaDataQuery(new FluentMetaDataQuery().from(meta));
			}
			else if (query instanceof SQLDefinition sql) {
				addSQL(new FluentSQL().from(sql));
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
	
	/**
	 * Sets the module name.
	 *
	 * @param name The module name.
	 * @return this fluent instance.
	 */
	public FluentModule name(String name) {
		module.setName(name);
		return this;
	}
	
	/**
	 * Sets the module title.
	 *
	 * @param title The module title.
	 * @return this fluent instance.
	 */
	public FluentModule title(String title) {
		module.setTitle(title);
		return this;
	}
	
	/**
	 * Sets module documentation text.
	 *
	 * @param documentation The module documentation.
	 * @return this fluent instance.
	 */
	public FluentModule documentation(String documentation) {
		module.setDocumentation(documentation);
		return this;
	}

	/**
	 * Sets whether this module is a prototype.
	 *
	 * @param prototype {@code true} to mark as prototype.
	 * @return this fluent instance.
	 */
	public FluentModule prototype(boolean prototype) {
		module.setPrototype(prototype ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets the default form label layout.
	 *
	 * @param layout The form label layout.
	 * @return this fluent instance.
	 */
	public FluentModule formLabelLayout(FormLabelLayout layout) {
		module.setFormLabelLayout(layout);
		return this;
	}
	
	/**
	 * Sets the home view type reference.
	 *
	 * @param homeRef The home view type.
	 * @return this fluent instance.
	 */
	public FluentModule homeRef(ViewType homeRef) {
		module.setHomeRef(homeRef);
		return this;
	}
	
	/**
	 * Sets the home document name.
	 *
	 * @param documentName The home document name.
	 * @return this fluent instance.
	 */
	public FluentModule homeDocument(String documentName) {
		module.setHomeDocument(documentName);
		return this;
	}
	
	/**
	 * Adds a scheduled job definition.
	 *
	 * @param job The job wrapper.
	 * @return this fluent instance.
	 */
	public FluentModule addJob(FluentJob job) {
		module.getJobs().add(job.get());
		return this;
	}
	
	/**
	 * Removes jobs matching the supplied name.
	 *
	 * @param name The job name.
	 * @return this fluent instance.
	 */
	public FluentModule removeJob(String name) {
		module.getJobs().removeIf(j -> name.equals(j.getName()));
		return this;
	}
	
	/**
	 * Removes all configured jobs.
	 *
	 * @return this fluent instance.
	 */
	public FluentModule clearJobs() {
		module.getJobs().clear();
		return this;
	}

	/**
	 * Finds a job by name.
	 *
	 * @param name The job name.
	 * @return A fluent wrapper for the matched job, or {@code null} when not found.
	 */
	public FluentJob findJob(String name) {
		JobMetaDataImpl result = module.getJobs().stream().filter(j -> name.equals(j.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentJob(result);
		}
		return null;
	}
	
	/**
	 * Adds a module document reference.
	 *
	 * @param document The document wrapper.
	 * @return this fluent instance.
	 */
	public FluentModule addDocument(FluentModuleDocument document) {
		module.getDocuments().add(document.get());
		return this;
	}

	/**
	 * Removes document references matching the supplied name.
	 *
	 * @param name The document name.
	 * @return this fluent instance.
	 */
	public FluentModule removeDocument(String name) {
		module.getDocuments().removeIf(d -> name.equals(d.getRef()));
		// TOD remove document privileges?
		return this;
	}

	/**
	 * Removes all configured document references.
	 *
	 * @return this fluent instance.
	 */
	public FluentModule clearDocuments() {
		module.getDocuments().clear();
		return this;
	}

	/**
	 * Finds a document reference by name.
	 *
	 * @param name The referenced document name.
	 * @return A fluent wrapper for the matched module document, or {@code null} when not found.
	 */
	public FluentModuleDocument findDocument(String name) {
		ModuleDocumentMetaData result = module.getDocuments().stream().filter(d -> name.equals(d.getRef())).findAny().orElse(null);
		if (result != null) {
			return new FluentModuleDocument(result);
		}
		return null;
	}
	
	/**
	 * Adds a metadata query definition.
	 *
	 * @param query The metadata query wrapper.
	 * @return this fluent instance.
	 */
	public FluentModule addMetaDataQuery(FluentMetaDataQuery query) {
		module.getQueries().add(query.get());
		return this;
	}

	/**
	 * Finds a metadata query by name.
	 *
	 * @param name The metadata query name.
	 * @return A fluent wrapper for the matched metadata query, or {@code null} when not found.
	 */
	public FluentMetaDataQuery findMetaDataQuery(String name) {
		MetaDataQueryMetaData result = (MetaDataQueryMetaData) module.getQueries().stream().filter(q -> name.equals(q.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentMetaDataQuery(result);
		}
		return null;
	}
	
	/**
	 * Adds an SQL query definition.
	 *
	 * @param sql The SQL query wrapper.
	 * @return this fluent instance.
	 */
	public FluentModule addSQL(FluentSQL sql) {
		module.getQueries().add(sql.get());
		return this;
	}
	
	/**
	 * Finds an SQL query by name.
	 *
	 * @param name The SQL query name.
	 * @return A fluent wrapper for the matched SQL query, or {@code null} when not found.
	 */
	public FluentSQL findSQL(String name) {
		SQLMetaData result = (SQLMetaData) module.getQueries().stream().filter(q -> name.equals(q.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentSQL(result);
		}
		return null;
	}
	
	/**
	 * Adds a BizQL query definition.
	 *
	 * @param bizql The BizQL query wrapper.
	 * @return this fluent instance.
	 */
	public FluentModule addBizQL(FluentBizQL bizql) {
		module.getQueries().add(bizql.get());
		return this;
	}
	
	/**
	 * Finds a BizQL query by name.
	 *
	 * @param name The BizQL query name.
	 * @return A fluent wrapper for the matched BizQL query, or {@code null} when not found.
	 */
	public FluentBizQL findBizQL(String name) {
		BizQLMetaData result = (BizQLMetaData) module.getQueries().stream().filter(q -> name.equals(q.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentBizQL(result);
		}
		return null;
	}

	/**
	 * Removes query definitions matching the supplied name.
	 *
	 * @param name The query name.
	 * @return this fluent instance.
	 */
	public FluentModule removeQuery(String name) {
		module.getQueries().removeIf(q -> name.equals(q.getName()));
		return this;
	}

	/**
	 * Removes all configured query definitions.
	 *
	 * @return this fluent instance.
	 */
	public FluentModule clearQueries() {
		module.getQueries().clear();
		return this;
	}

	/**
	 * Adds a role definition.
	 *
	 * @param role The role wrapper.
	 * @return this fluent instance.
	 */
	public FluentModule addRole(FluentModuleRole role) {
		module.getRoles().add(role.get());
		return this;
	}

	/**
	 * Removes role definitions matching the supplied name.
	 *
	 * @param name The role name.
	 * @return this fluent instance.
	 */
	public FluentModule removeRole(String name) {
		module.getRoles().removeIf(r -> name.equals(r.getName()));
		return this;
	}
	
	/**
	 * Removes all configured role definitions.
	 *
	 * @return this fluent instance.
	 */
	public FluentModule clearRoles() {
		module.getRoles().clear();
		return this;
	}

	/**
	 * Finds a role by name.
	 *
	 * @param name The role name.
	 * @return A fluent wrapper for the matched role, or {@code null} when not found.
	 */
	public FluentModuleRole findRole(String name) {
		ModuleRoleMetaData role = module.getRoles().stream().filter(r -> r.getName().equals(name)).findAny().orElse(null);
		if (role != null) {
			return new FluentModuleRole(role);
		}
		return null;
	}

	/**
	 * Sets the module menu definition.
	 *
	 * @param menu The menu wrapper.
	 * @return this fluent instance.
	 */
	public FluentModule menu(FluentMenu menu) {
		module.setMenu(menu.get());
		return this;
	}
	
	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The module metadata instance.
	 */
	public ModuleMetaData get() {
		return module;
	}
}
