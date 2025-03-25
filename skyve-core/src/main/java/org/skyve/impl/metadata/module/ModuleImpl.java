package org.skyve.impl.metadata.module;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.apache.commons.lang3.mutable.MutableInt;
import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.AbstractMetaDataMap;
import org.skyve.impl.metadata.model.document.field.Content;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.metadata.module.query.MetaDataQueryContentColumnImpl;
import org.skyve.impl.metadata.module.query.MetaDataQueryDefinitionImpl;
import org.skyve.impl.metadata.module.query.MetaDataQueryProjectedColumnImpl;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.repository.module.MetaDataQueryContentColumnMetaData.DisplayType;
import org.skyve.impl.metadata.user.RoleImpl;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.menu.Menu;
import org.skyve.metadata.module.query.BizQLDefinition;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.module.query.SQLDefinition;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.view.View.ViewType;

import com.google.common.base.MoreObjects;

public class ModuleImpl extends AbstractMetaDataMap implements Module {
	private static final long serialVersionUID = -5291187014833234045L;

	private String name;

	private long lastModifiedMillis = Long.MAX_VALUE;
	private long lastCheckedMillis = System.currentTimeMillis();
	
	private String title;
	
	private boolean prototype;
	
	private FormLabelLayout formLabelLayout;
	
	private Map<String, DocumentRef> documentRefs = new TreeMap<>();

	/**
	 * The list of jobs for ths module in the order they are
	 */
	private List<JobMetaData> jobs = new ArrayList<>();
	
	/**
	 * The list of queries for this module in the order they are added.
	 */
	private List<QueryDefinition> queries = new ArrayList<>();

	/**
	 * The list of roles for this module in the order they are added.
	 */
	private List<Role> roles = new ArrayList<>();

	private ViewType homeRef;

	private String homeDocumentName;

	private Menu menu;

	private String documentation; 

	private transient ProvidedRepository repository;
	
	public ModuleImpl(ProvidedRepository repository) {
		this.repository = repository;
	}

	// Required for Serialization
	// NB Module should never be serialized.
	public ModuleImpl() {
		this.repository = ProvidedRepositoryFactory.get();
	}

	@Override
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	@Override
	public long getLastModifiedMillis() {
		return lastModifiedMillis;
	}

	public void setLastModifiedMillis(long lastModifiedMillis) {
		this.lastModifiedMillis = lastModifiedMillis;
	}

	@Override
	public long getLastCheckedMillis() {
		return lastCheckedMillis;
	}

	@Override
	public void setLastCheckedMillis(long lastCheckedMillis) {
		this.lastCheckedMillis = lastCheckedMillis;
	}

	@Override
	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	@Override
	public boolean isPrototype() {
		return prototype;
	}

	public void setPrototype(boolean prototype) {
		this.prototype = prototype;
	}

	@Override
	public FormLabelLayout getFormLabelLayout() {
		return formLabelLayout;
	}

	public void setFormLabelLayout(FormLabelLayout formLabelLayout) {
		this.formLabelLayout = formLabelLayout;
	}

	@Override
	public Map<String, DocumentRef> getDocumentRefs() {
		return documentRefs;
	}

	@Override
	public MetaDataQueryDefinition getDocumentDefaultQuery(Customer customer, String documentName) {
		return getDocumentDefaultQuery(customer, documentName, isPrototype());
	}

	@Override
	public MetaDataQueryDefinition getDocumentDefaultQuery(Customer customer, String documentName, boolean includeAssociationBizKeys) {
		MetaDataQueryDefinition result = null;

		DocumentRef documentRef = documentRefs.get(documentName);
		if (documentRef != null) {
			String queryName = documentRef.getDefaultQueryName();
			if (queryName != null) {
				result = getMetaDataQuery(queryName);
				if (result == null) {
					throw new MetaDataException("The default query of " + queryName + 
													" does not exist for document " + documentName);
				}
			}
			else {
				Document document = getDocument(customer, documentName);
				if (! document.isPersistable()) {
					throw new MetaDataException("Cannot create a query for transient Document " + document.getOwningModuleName() + "." + document.getName());
				}
				MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();

				String queryTitle = "All " + document.getLocalisedPluralAlias();
				query.setDescription(queryTitle);
				query.setName(documentName);
				query.setDocumentName(documentName);
				query.setOwningModule(this);
				
				result = query;

				processColumns(customer,
								document,
								result.getColumns(),
								includeAssociationBizKeys,
								new MutableBoolean(true),
								new MutableInt(0));
			}
		}

		return result;
	}

	private void processColumns(Customer customer,
									Document document,
									List<MetaDataQueryColumn> columns,
									boolean includeAssociationBizKeys,
									MutableBoolean orderNotApplied,
									MutableInt columnIndex) {
		// NB We have to manually traverse the document inheritance hierarchy with the given customer
		// as we cannot use document.getAllAttributes() as this method is called from 
		// the domain generator and there is no Persistence set in there.
		Extends inherits = document.getExtends();
		if (inherits != null) {
			Document baseDocument = getDocument(customer, inherits.getDocumentName());
			processColumns(customer, baseDocument, columns, includeAssociationBizKeys, orderNotApplied, columnIndex);
		}

		for (Attribute attribute : document.getAttributes()) {
			if (attribute.isPersistent() && (! attribute.isDeprecated())) {
				// Note - collections not included in generated queries
				if (attribute instanceof Content) {
					MetaDataQueryContentColumnImpl column = new MetaDataQueryContentColumnImpl();
					column.setBinding(attribute.getName());
					column.setDisplay(DisplayType.thumbnail);
					column.setPixelWidth(Integer.valueOf(64));
					column.setPixelHeight(Integer.valueOf(64));
					if (columnIndex.intValue() > 7) {
						column.setHidden(true);
					}
					columns.add(column);
					columnIndex.increment();
				}
				else if (attribute instanceof Field) {
					MetaDataQueryProjectedColumnImpl column = new MetaDataQueryProjectedColumnImpl();
					column.setEditable(false);
					column.setBinding(attribute.getName());
					if (orderNotApplied.booleanValue()) {
						column.setSortOrder(SortDirection.ascending);
						orderNotApplied.setValue(false);
					}
					if (columnIndex.intValue() > 7) {
						column.setHidden(true);
					}
					columns.add(column);
					columnIndex.increment();
				}
				else if (includeAssociationBizKeys && (attribute instanceof Association)) {
					final Association association = (Association) attribute;
					// Don't include embedded associations since there is no bizKey
					if (AssociationType.embedded != association.getType()) {
						final MetaDataQueryProjectedColumnImpl column = new MetaDataQueryProjectedColumnImpl();
						column.setEditable(false);
						column.setBinding(BindUtil.createCompoundBinding(association.getName(), Bean.BIZ_KEY));
						if (orderNotApplied.booleanValue()) {
							column.setSortOrder(SortDirection.ascending);
							orderNotApplied.setValue(false);
						}
						if (columnIndex.intValue() > 7) {
							column.setHidden(true);
						}
						columns.add(column);
						columnIndex.increment();
					}
				}
/*
Commented this out as it inadvertently creates dependencies on first-level associations on the referenced document.
ie Link from an external module to admin.User and domain generation will moan about that module requiring admin.Contact document too.
				else if (attribute instanceof Association) {
					String targetDocumentName = ((Association) attribute).getDocumentName();
					Document targetDocument = getDocument(customer, targetDocumentName);
					Persistent targetPersistent = targetDocument.getPersistent();
					if (targetPersistent.getName() != null) { // make sure this isn't a transient document (probably mapped) that can't be queried
						org.skyve.impl.metadata.module.query.QueryColumn column = new org.skyve.impl.metadata.module.query.QueryColumn();
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
	 */
	@Override
	public Document getDocument(Customer customer, String documentName) {
		Document result = repository.getDocument(customer, this, documentName);
		if (result == null) {
			throw new IllegalStateException("Document " + documentName + " does not exist in module " + getName());
		}

		return result;
	}
	
	@Override
	public JobMetaData getJob(String jobName) {
		return (JobMetaData) getMetaData(jobName);
	}

	public void putJob(JobMetaData job) {
		putMetaData(job.getName(), job);
		jobs.add(job);
	}
	
	@Override
	public List<JobMetaData> getJobs() {
		return Collections.unmodifiableList(jobs);
	}
	
	@Override
	public MetaDataQueryDefinition getMetaDataQuery(String queryName) {
		// NB Cannot throw if null as QueryCommand tests it
		return (MetaDataQueryDefinition) getMetaData(queryName);
	}

	@Override
	public SQLDefinition getSQL(String queryName) {
		// NB Cannot throw if null as QueryCommand tests it
		return (SQLDefinition) getMetaData(queryName);
	}

	@Override
	public BizQLDefinition getBizQL(String queryName) {
		// NB Cannot throw if null as QueryCommand tests it
		return (BizQLDefinition) getMetaData(queryName);
	}

	public void putQuery(QueryDefinition query) {
		putMetaData(query.getName(), query);
		queries.add(query);
	}

	@Override
	public List<QueryDefinition> getMetadataQueries() {
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

    @Override
    public String toString() {

        return MoreObjects.toStringHelper(this)
                          .add("name", name)
                          .toString();
    }
}
