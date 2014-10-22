package org.skyve.wildcat.metadata.module;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Job;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.menu.Menu;
import org.skyve.metadata.module.query.Query;
import org.skyve.metadata.module.query.QueryColumn;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.wildcat.metadata.AbstractMetaDataMap;
import org.skyve.wildcat.metadata.model.document.field.Field;
import org.skyve.wildcat.metadata.module.query.QueryImpl;
import org.skyve.wildcat.metadata.repository.AbstractRepository;
import org.skyve.wildcat.metadata.user.RoleImpl;

public class ModuleImpl extends AbstractMetaDataMap implements Module {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -5291187014833234045L;

	private String name;

	private String title;
	
	private Map<String, DocumentRef> documentRefs = new TreeMap<>();

	/**
	 * The list of jobs for ths module in the order they are
	 */
	private List<Job> jobs = new ArrayList<>();
	
	/**
	 * The list of queries for this module in the order they are added.
	 */
	private List<Query> queries = new ArrayList<>();

	/**
	 * The list of roles for this module in the order they are added.
	 */
	private List<Role> roles = new ArrayList<>();

	private ViewType homeRef;

	private String homeDocumentName;

	private Menu menu;

	private String documentation; 

	@Override
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	@Override
	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	@Override
	public Map<String, DocumentRef> getDocumentRefs() {
		return documentRefs;
	}

	@Override
	public Query getDocumentDefaultQuery(Customer customer, String documentName) 
	throws MetaDataException {
		Query result = null;

		DocumentRef documentRef = documentRefs.get(documentName);
		if (documentRef != null) {
			String queryName = documentRef.getDefaultQueryName();
			if (queryName != null) {
				result = getQuery(queryName);
				if (result == null) {
					throw new MetaDataException("The default query of " + queryName + 
													" does not exist for document " + documentName);
				}
			}
			else {
				Document document = getDocument(customer, documentName);
				Persistent persistent = document.getPersistent();
				if ((persistent == null) || (persistent.getName() == null)) {
					throw new MetaDataException("Cannot create a query for transient Document " + document.getOwningModuleName() + "." + document.getName());
				}
				QueryImpl query = new QueryImpl();

				String queryTitle = "All " + document.getPluralAlias();
				query.setDisplayName(queryTitle);
				query.setDescription(queryTitle);
				query.setName(documentName);
				query.setDocumentName(documentName);
				query.setOwningModule(this);
				
				result = query;

/*
				QueryColumn column = new QueryColumn();
				column.setBinding(Bean.BIZ_KEY);
				column.setEditable(false);
				column.setHidden(true);
				column.setDisplayName(document.getSingularAlias());
				column.setSortOrder(SortDirection.ascending);
				
				result.getColumns().add(column);
*/
				
				processColumns(customer, document, result.getColumns());
			}
		}

		return result;
	}

	private void processColumns(Customer customer, Document document, List<QueryColumn> columns)
	throws MetaDataException {
		boolean firstColumn = true;
		Extends inherits = document.getExtends();
		if (inherits != null) {
			Document baseDocument = getDocument(customer, inherits.getDocumentName());
			processColumns(customer, baseDocument, columns);
			firstColumn = false;
		}

		for (Attribute attribute : document.getAttributes()) {
			if (attribute.isPersistent()) {
				// Note - collections not included in generated queries
				if (attribute instanceof Field) {
					org.skyve.wildcat.metadata.module.query.QueryColumn column = new org.skyve.wildcat.metadata.module.query.QueryColumn();
					column.setEditable(false);
					column.setDisplayName(attribute.getDisplayName());
					column.setBinding(attribute.getName());
					if (firstColumn) {
						column.setSortOrder(SortDirection.ascending);
						firstColumn = false;
					}
					columns.add(column);
				}
/*
Commented this out as it inadvertently creates dependencies on first-level associations on the referenced document.
ie Link from an external module to admin.User and domain generation will moan about that module requiring admin.Contact document too.
				else if (attribute instanceof Association) {
					String targetDocumentName = ((Association) attribute).getDocumentName();
					Document targetDocument = getDocument(customer, targetDocumentName);
					Persistent targetPersistent = targetDocument.getPersistent();
					if (targetPersistent.getName() != null) { // make sure this isn't a transient document (probably mapped) that can't be queried
						org.skyve.wildcat.metadata.module.query.QueryColumn column = new org.skyve.wildcat.metadata.module.query.QueryColumn();
						column.setEditable(false);
						column.setDisplayName(attribute.getDisplayName());
						String binding = new StringBuilder(64).append(attribute.getName()).append('.').append(Bean.BIZ_KEY).toString();
						column.setBinding(binding);
						if (firstColumn) {
							column.setSortOrder(SortDirection.ascending);
							firstColumn = false;
						}
						columns.add(column);
					}
				}
*/
			}
		}
	}
	/**
	 * 
	 * @param customer Can be null which means that this method returns the un-overridden document.
	 * @param documentName
	 * @return
	 * @throws MetaDataException
	 */
	@Override
	public Document getDocument(Customer customer, String documentName) 
	throws MetaDataException {
		Document result = AbstractRepository.get().getDocument(customer, this, documentName);
		if (result == null) {
			throw new IllegalStateException("Document " + documentName + " does not exist in module " + getName());
		}

		return result;
	}
	
	@Override
	public org.skyve.metadata.module.Job getJob(String jobName) {
		return (org.skyve.metadata.module.Job) getMetaData(jobName);
	}

	public void putJob(Job job) {
		putMetaData(job.getName(), job);
		jobs.add(job);
	}
	
	@Override
	public List<Job> getJobs() {
		return Collections.unmodifiableList(jobs);
	}
	
	@Override
	public Query getQuery(String queryName) {
		// NB Cannot throw as QueryCommand tests it
		return (Query) getMetaData(queryName);
	}

	public void putQuery(Query query) {
		putMetaData(query.getName(), query);
		queries.add(query);
	}

	@Override
	public List<Query> getMetadataQueries() {
		return Collections.unmodifiableList(queries);
	}

	@Override
	public Role getRole(String roleName) {
		return (Role) getMetaData(roleName);
	}

	public void putRole(Role role) {
		((RoleImpl) role).setOwningModule(this);
		putMetaData(role.getName(), role);
		roles.add(role);
	}

	@Override
	public List<Role> getRoles() {
		return Collections.unmodifiableList(roles);
	}

	@Override
	public ViewType getHomeRef() {
		return homeRef;
	}

	public void setHomeRef(ViewType homeRef) {
		this.homeRef = homeRef;
	}

	@Override
	public String getHomeDocumentName() {
		return homeDocumentName;
	}

	public void setHomeDocumentName(String homeDocumentName) {
		this.homeDocumentName = homeDocumentName;
	}

	@Override
	public Menu getMenu() {
		return menu;
	}

	public void setMenu(Menu menu) {
		this.menu = menu;
	}

	@Override
	public String getDocumentation() {
		return documentation;
	}

	public void setDocumentation(String documentation) {
		this.documentation = documentation;
	}
}
