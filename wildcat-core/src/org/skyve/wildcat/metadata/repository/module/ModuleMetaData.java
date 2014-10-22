package org.skyve.wildcat.metadata.repository.module;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.user.DocumentPermission;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.wildcat.metadata.module.Job;
import org.skyve.wildcat.metadata.module.ModuleImpl;
import org.skyve.wildcat.metadata.module.menu.AbstractMenuItem;
import org.skyve.wildcat.metadata.module.menu.MenuGroup;
import org.skyve.wildcat.metadata.module.query.QueryColumn;
import org.skyve.wildcat.metadata.module.query.QueryImpl;
import org.skyve.wildcat.metadata.repository.NamedMetaData;
import org.skyve.wildcat.metadata.repository.PersistentMetaData;
import org.skyve.wildcat.metadata.user.User;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlRootElement(namespace = XMLUtil.MODULE_NAMESPACE, name = "module")
@XmlType(namespace = XMLUtil.MODULE_NAMESPACE, 
			name = "module",
			propOrder = {"documentation", 
							"title",
							"homeRef",
							"homeDocument",
							"jobs",
							"documents",
							"queries",
							"roles",
							"menu"})
public class ModuleMetaData extends NamedMetaData implements PersistentMetaData<org.skyve.metadata.module.Module> {
	private String title;
	private ViewType homeRef;
	private String homeDocument;
	private List<Job> jobs = new ArrayList<>();
	private List<ModuleDocument> documents = new ArrayList<>();
	private List<QueryMetaData> queries = new ArrayList<>();
	private List<Role> roles = new ArrayList<>();
	private Menu menu;
	private String documentation;

	public String getTitle() {
		return title;
	}

	@XmlAttribute(required = true)
	public void setTitle(String title) {
		this.title = UtilImpl.processStringValue(title);
	}

	public String getHomeDocument() {
		return homeDocument;
	}

	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE, required = true)
	public void setHomeDocument(String homeDocument) {
		this.homeDocument = UtilImpl.processStringValue(homeDocument);
	}

	public ViewType getHomeRef() {
		return homeRef;
	}

	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE)
	public void setHomeRef(ViewType homeRef) {
		this.homeRef = homeRef;
	}

	@XmlElementWrapper(namespace = XMLUtil.MODULE_NAMESPACE, name = "jobs")
	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE, name = "job", required = true)
	public List<Job> getJobs() {
		return jobs;
	}

	@XmlElementWrapper(namespace = XMLUtil.MODULE_NAMESPACE, name = "documents")
	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE, name = "document", required = true)
	public List<ModuleDocument> getDocuments() {
		return documents;
	}

	@XmlElementWrapper(namespace = XMLUtil.MODULE_NAMESPACE, name = "queries")
	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE, name = "query", required = true)
	public List<QueryMetaData> getQueries() {
		return queries;
	}

	@XmlElementWrapper(namespace = XMLUtil.MODULE_NAMESPACE, name = "roles")
	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE, name = "role", required = true)
	public List<Role> getRoles() {
		return roles;
	}

	public Menu getMenu() {
		return menu;
	}

	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE)
	public void setMenu(Menu menu) {
		this.menu = menu;
	}

	public String getDocumentation() {
		return documentation;
	}

	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE)
	public void setDocumentation(String documentation) {
		this.documentation = UtilImpl.processStringValue(documentation);
	}

	@Override
	public Module convert(String metaDataName) throws MetaDataException {
		ModuleImpl result = new ModuleImpl();

		String value = getName();
		if (value == null) {
			throw new MetaDataException(metaDataName + " : The module [name] is required");
		}
		result.setName(value);

		value = getTitle();
		if (value == null) {
			throw new MetaDataException(metaDataName + " : The module [title] is required");
		}
		result.setTitle(value);

		value = getHomeDocument();
		if (value == null) {
			throw new MetaDataException(metaDataName + " : The module [homeDocument] is required");
		}
		result.setHomeDocumentName(value);
		if (getHomeRef() != null) {
			result.setHomeRef(getHomeRef());
		}
		else {
			result.setHomeRef(ViewType.list);
		}

		// Populate document refs

		List<ModuleDocument> repositoryDocuments = getDocuments();
		Set<String> documentNames = new TreeSet<>();
		if (repositoryDocuments != null) {
			for (ModuleDocument document : repositoryDocuments) {
				DocumentRef documentRef = new DocumentRef();
				documentRef.setDefaultQueryName(document.getDefaultQueryName());
				value = document.getRef();
				if (value == null) {
					throw new MetaDataException(metaDataName + " : The module document [ref] is required");
				}
				if (! documentNames.add(value)) {
					throw new MetaDataException(metaDataName + " : Duplicate document reference named " + value);
				}

				String referencedModuleName = document.getModuleRef();
				if (referencedModuleName != null) {
					documentRef.setReferencedModuleName(referencedModuleName);
					documentRef.setOwningModuleName(referencedModuleName);
				}
				else {
					documentRef.setOwningModuleName(getName());
				}

				// TODO expand on document ref when add further modules documentRef.setRelatedTo();
				result.getDocumentRefs().put(document.getRef(), documentRef);
			}
		}
		if (! documentNames.contains(result.getHomeDocumentName())) {
			throw new MetaDataException(metaDataName + " : The module [homeDocument] " + 
											result.getHomeDocumentName() + " is not a module document");
		}

		// Populate Jobs
		List<Job> repositoryJobs = getJobs();
		if (repositoryJobs != null) {
			Set<String> jobNames = new TreeSet<>();
			for (Job job : repositoryJobs) {
				value = job.getName();
				if (value == null) {
					throw new MetaDataException(metaDataName + " : The [name] for a job is required");
				}
				if (! jobNames.add(value)) {
					throw new MetaDataException(metaDataName + " : Duplicate job named " + value);
				}
				if (documentNames.contains(value)) {
					throw new MetaDataException(metaDataName + " : The job named " + value + " is a module document name.");
				}

				value = job.getDisplayName();
				if (value == null) {
					throw new MetaDataException(metaDataName + " : The [displayName] for job " + job.getName() + " is required");
				}
				job.setOwningModuleName(result.getName());
				result.putJob(job);
			}
		}
		
		// Populate queries

		List<QueryMetaData> repositoryQueries = getQueries();
		if (repositoryQueries != null) {
			Set<String> queryNames = new TreeSet<>();
			for (QueryMetaData queryMetaData : repositoryQueries) {
				QueryImpl query = new QueryImpl();
				value = queryMetaData.getName();
				if (value == null) {
					throw new MetaDataException(metaDataName + " : The [name] for a query is required");
				}
				if (! queryNames.add(value)) {
					throw new MetaDataException(metaDataName + " : Duplicate query named " + value);
				}
				if (documentNames.contains(value)) {
					throw new MetaDataException(metaDataName + " : The query named " + value + " is a module document name.");
				}
				query.setName(value);

				value = queryMetaData.getDisplayName();
				if (value == null) {
					throw new MetaDataException(metaDataName + " : The [displayName] for query " + query.getName() + " is required");
				}
				query.setDisplayName(value);

				value = queryMetaData.getDescription();
				if (value == null) {
					throw new MetaDataException(metaDataName + " : The [description] for query " + query.getName() + " is required");
				}
				query.setDescription(value);

				value = queryMetaData.getDocumentName();
				if (value == null) {
					throw new MetaDataException(metaDataName + " : The [documentName] for query " + query.getName() + " is required");
				}
				if (! documentNames.contains(value)) {
					throw new MetaDataException(metaDataName + " : The [documentName] of " + value + " for query " +
													query.getName() + " is not a module document");
				}
				query.setDocumentName(value);
				query.setFromClause(queryMetaData.getFrom());
				query.setFilterClause(queryMetaData.getFilter());
				query.setOwningModule(result);

				List<Column> repositoryQueryColumns = queryMetaData.getColumns();
				if (repositoryQueryColumns != null) {
					for (Column column : repositoryQueryColumns) {
						QueryColumn queryColumn = new QueryColumn();
						queryColumn.setName(column.getName());
						String binding = column.getBinding();
						String expression = column.getExpression();
						if ((binding == null) && (expression == null)) {
							throw new MetaDataException(metaDataName + " : The [binding] and [expression] for a query column is missing in query " + query.getName());
						}
						if ((binding != null) && (expression != null)) {
							throw new MetaDataException(metaDataName + " : Both the [binding] and [expression] for a query column are entered in query " + query.getName());
						}
						if ((expression != null) && (column.getName() == null)) {
							throw new MetaDataException(metaDataName + " : An [expression] query column requires the [name] to be entered in query " + query.getName());
						}
						queryColumn.setBinding(binding);
						queryColumn.setExpression(expression);
						queryColumn.setDisplayName(column.getDisplayName());
						FilterOperator filterOperator = column.getFilterOperator();
						String filterExpression = column.getFilterExpression();
						if ((filterOperator != null) && 
								(! filterOperator.equals(FilterOperator.isNull)) &&
								(! filterOperator.equals(FilterOperator.notNull)) && 
								(filterExpression == null)) {
							throw new MetaDataException(metaDataName + " : Operator " + filterOperator + 
															" in column " + column.getBinding() + 
															" in query " + query.getName() + " requires an [expression].");
						}
						if (((filterOperator == null) || 
								filterOperator.equals(FilterOperator.isNull) || 
								filterOperator.equals(FilterOperator.notNull)) &&
								(filterExpression != null)) {
							throw new MetaDataException(metaDataName + " : Operator " + filterOperator + 
															" in column " + column.getBinding() + 
															" in query " + query.getName() +
															" does not require an [expression].");
						}
						queryColumn.setFilterOperator(filterOperator);
						queryColumn.setFilterExpression(filterExpression);
						queryColumn.setSortOrder(column.getSortOrder());
						Boolean projected = column.getProjected();
						if (projected != null) {
							queryColumn.setSelected(projected.booleanValue());
						}
						Boolean hidden = column.getHidden();
						if (hidden != null) {
							queryColumn.setHidden(hidden.booleanValue());
						}
						Boolean sortable = column.getSortable();
						if (sortable != null) {
							queryColumn.setSortable(sortable.booleanValue());
						}
						Boolean filterable = column.getFilterable();
						if (filterable != null) {
							queryColumn.setFilterable(filterable.booleanValue());
						}
						Boolean editable = column.getEditable();
						if (editable != null) {
							queryColumn.setEditable(editable.booleanValue());
						}

						query.getColumns().add(queryColumn);
					}
				}

				query.setDocumentation(queryMetaData.getDocumentation());
				
				// TODO querylet processing query.setQuerylet();
				result.putQuery(query);
			}
		}

		// Populate Roles
		Set<String> roleNames = new TreeSet<>();
		List<Role> repositoryRoles = getRoles();
		if (repositoryRoles != null) {
			for (Role roleMetaData : repositoryRoles) {
				org.skyve.wildcat.metadata.user.Role role = new org.skyve.wildcat.metadata.user.Role();
				value = roleMetaData.getName();
				if (value == null) {
					throw new MetaDataException(metaDataName + " : The [name] for a role is required");
				}
				if (! roleNames.add(value)) {
					throw new MetaDataException(metaDataName + " : Duplicate role named " + value);
				}
				if (documentNames.contains(value)) {
					throw new MetaDataException(metaDataName + " : The role named " + value + " is a module document name.");
				}
				role.setName(value);
				role.setDescription(roleMetaData.getDescription());
				role.setDocumentation(roleMetaData.getDocumentation());
				
				Set<String> docPrivNames = new TreeSet<>();
				List<DocumentPrivilege> repositoryDocPrivileges = roleMetaData.getPrivileges();
				if (repositoryDocPrivileges != null) {
					for (DocumentPrivilege documentPrivilegeMetaData : repositoryDocPrivileges) {
						org.skyve.wildcat.metadata.user.DocumentPrivilege documentPrivilege = new org.skyve.wildcat.metadata.user.DocumentPrivilege();
						value = documentPrivilegeMetaData.getDocumentName();
						if (value == null) {
							throw new MetaDataException(metaDataName + " : The [documentName] for a privilege is required for role " + 
															role.getName());
						}
						if ( !docPrivNames.add(value)) {
							throw new MetaDataException(metaDataName + " : Duplicate document privilege for document " + value +
															" in role " + role.getName());
						}
						if ( !documentNames.contains(value)) {
							throw new MetaDataException(metaDataName + " : The privilege [documentName] value of " + value +
															" in role " + role.getName() + " is not a module document");
						}
						if (result.getDocumentRefs().get(value).getReferencedModuleName() != null) {
							throw new MetaDataException(metaDataName + " : The privilege [documentName] value of " + value +
															" in role " + role.getName() +
															" cannot be for a document referenced from another module.  Document Privileges are to be made on the owning module only.");
						}

						documentPrivilege.setName(value);
						DocumentPermission docPermission = documentPrivilegeMetaData.getPermission();
						if (docPermission == null) {
							throw new MetaDataException(metaDataName + " : Document permission is required for document " +
															documentPrivilege.getName() + " in role " + role.getName());
						}
						documentPrivilege.setPermission(docPermission);

						role.getPrivileges().add(documentPrivilege);

						// Add the action privileges also
						List<ActionPrivilege> repositoryActionPrivileges = documentPrivilegeMetaData.getActions();
						if (repositoryActionPrivileges != null) {
							for (ActionPrivilege actionPrivilegeMetaData : repositoryActionPrivileges) {
								org.skyve.wildcat.metadata.user.ActionPrivilege actionPrivilege = new org.skyve.wildcat.metadata.user.ActionPrivilege();
								value = actionPrivilegeMetaData.getActionName();
								if (value == null) {
									throw new MetaDataException(metaDataName + " : The [actionName] for a privilege is required for document " +
																	documentPrivilege.getName() + " in role " + role.getName());
								}
								actionPrivilege.setName(value);
								actionPrivilege.setDocumentName(documentPrivilege.getName());

								role.getPrivileges().add(actionPrivilege);
							}
						}

						// Add the content restrictions
						List<ContentRestriction> contentRestrictions = documentPrivilegeMetaData.getContentRestrictions();
						if (contentRestrictions != null) {
							for (ContentRestriction contentRestriction : contentRestrictions) {
								value = contentRestriction.getAttributeName();
								if (value == null) {
									throw new MetaDataException(metaDataName + " : The [attribute] for a content restriction is required for document " +
																	documentPrivilege.getName() + " in role " + role.getName());
								}
								contentRestriction.setDocumentName(documentPrivilege.getName());

								role.getContentRestrictions().add(contentRestriction);
							}
						}

						// Add the content permissions
						List<ContentPermission> contentPermissions = documentPrivilegeMetaData.getContentPermissions();
						if (contentPermissions != null) {
							for (ContentPermission contentPermission : contentPermissions) {
								value = contentPermission.getAttributeName();
								if (value == null) {
									throw new MetaDataException(metaDataName + " : The [attribute] for a content permission is required for document " +
																	documentPrivilege.getName() + " in role " + role.getName());
								}
								contentPermission.setDocumentName(documentPrivilege.getName());

								role.getContentPermissions().add(contentPermission);
							}
						}
					}
				}

				result.putRole(role);
			}
		}

		// Populate the menu

		org.skyve.wildcat.metadata.module.menu.Menu resultMenu = new org.skyve.wildcat.metadata.module.menu.Menu();
		List<MenuItem> items = resultMenu.getItems();

		populateModuleMenu(metaDataName, items, getMenu().getActions(), roleNames);
		result.setMenu(resultMenu);

		result.setDocumentation(documentation);
		
		return result;
	}

	private void populateModuleMenu(String metaDataName, 
										List<MenuItem> items, 
										List<Action> actions, 
										Set<String> validRoleNames)
	throws MetaDataException {
		for (Action action : actions) {
			if (action instanceof Group) {
				String value = action.getName();
				if (value == null) {
					throw new MetaDataException(metaDataName + " : The [name] for a menu group is required");
				}
				Group group = (Group) action;
				MenuGroup menuGroup = new MenuGroup();
				menuGroup.setName(value);
				populateUxuis(metaDataName, value, group.getUxuis(), menuGroup.getUxUis());
				populateModuleMenu(metaDataName, menuGroup.getItems(), group.getActions(), validRoleNames);
				
				items.add(menuGroup);
			}
			else if (action instanceof CalendarItem) {
				CalendarItem item = (CalendarItem) action;
				org.skyve.wildcat.metadata.module.menu.CalendarItem result = new org.skyve.wildcat.metadata.module.menu.CalendarItem();
				populateItem(metaDataName, validRoleNames, result, item);

				String documentName = item.getDocumentName();
				String queryName = item.getQueryName();
				String modelName = item.getModelName();
				String startBinding = item.getStartBinding();
				String endBinding = item.getEndBinding();
				
				if (documentName != null) {
					if ((queryName != null) ||
							(modelName != null) ||
							(startBinding == null) || 
							(endBinding == null)) {
						throw new MetaDataException(metaDataName + " : If [document] is present, then [model] and [query] should be absent " + 
														"and [startBinding] and [endBinding] are required for menu item " + item.getName());
					}
				}
				else if (queryName != null) {
					if ((modelName != null) ||
							(startBinding == null) || 
							(endBinding == null)) {
						throw new MetaDataException(metaDataName + " : If [query] is present, then [model] and [document] should be absent " + 
														"and [startBinding] and [endBinding] are required for menu item " + item.getName());
					}
				}
				else if (modelName != null) {
					if ((startBinding != null) || (endBinding != null)) {
						throw new MetaDataException(metaDataName + " : If [model] is present, then [document], [query], " + 
														"[startBinding] and [endBinding] should be absent for menu item " + item.getName());
					}
				}
				else {
					throw new MetaDataException(metaDataName + " : One of [document], [query] or [model] " + 
													"is required for menu item " + item.getName());
				}
				
				result.setDocumentName(documentName);
				result.setModelName(modelName);
				result.setQueryName(queryName);
				result.setStartBinding(startBinding);
				result.setEndBinding(endBinding);
				
				items.add(result);
			}
			else if (action instanceof LinkItem) {
				LinkItem item = (LinkItem) action;
				org.skyve.wildcat.metadata.module.menu.LinkItem result = new org.skyve.wildcat.metadata.module.menu.LinkItem();
				populateItem(metaDataName, validRoleNames, result, item);
				
				String href = item.getHref();
				if (href == null) {
					throw new MetaDataException(metaDataName + " : [href] is required for menu item " + item.getName());
				}
				result.setHref(href);
				
				items.add(result);
			}
			else if (action instanceof EditItem) {
				EditItem item = (EditItem) action;
				org.skyve.wildcat.metadata.module.menu.EditItem result = new org.skyve.wildcat.metadata.module.menu.EditItem();
				populateItem(metaDataName, validRoleNames, result, item);
				
				String documentName = item.getDocumentName();
				if (documentName == null) {
					throw new MetaDataException(metaDataName + " : [document] is required for menu item " + item.getName());
				}
				result.setDocumentName(documentName);
				
				items.add(result);
			}
			else if (action instanceof GridItem) {
				GridItem item = (GridItem) action;
				org.skyve.wildcat.metadata.module.menu.GridItem result = new org.skyve.wildcat.metadata.module.menu.GridItem();
				populateItem(metaDataName, validRoleNames, result, item);

				String documentName = item.getDocumentName();
				String queryName = item.getQueryName();
				String modelName = item.getModelName();

				if (documentName != null) {
					if ((queryName != null) ||
							(modelName != null)) {
						throw new MetaDataException(metaDataName + 
														" : If [document] is present, then [model] and [query] should be absent " + 
														"for menu item " + item.getName());
					}
				}
				else if (queryName != null) {
					if (modelName != null) {
						throw new MetaDataException(metaDataName + " : If [query] is present, then [model] and [document] should be absent " + 
														"for menu item " + item.getName());
					}
				}
				else if (modelName == null) {
					throw new MetaDataException(metaDataName + " : One of [document], [query] or [model] " + 
													"is required for menu item " + item.getName());
				}

				result.setDocumentName(documentName);
				result.setQueryName(queryName);
				result.setModelName(modelName);
				
				items.add(result);
			}
			else if (action instanceof MapItem) {
				MapItem item = (MapItem) action;
				org.skyve.wildcat.metadata.module.menu.MapItem result = new org.skyve.wildcat.metadata.module.menu.MapItem();
				populateItem(metaDataName, validRoleNames, result, item);

				String documentName = item.getDocumentName();
				String queryName = item.getQueryName();
				String modelName = item.getModelName();
				String geometryBinding = item.getGeometryBinding();
				Integer refreshTimeInSeconds = item.getRefreshTimeInSeconds();
				
				if (documentName != null) {
					if ((queryName != null) ||
							(modelName != null) ||
							(geometryBinding == null)) {
						throw new MetaDataException(metaDataName + " : If [document] is present, then [model] and [query] should be absent " + 
														"and [geometryBinding] is required for menu item " + item.getName());
					}
				}
				else if (queryName != null) {
					if ((modelName != null) ||
							(geometryBinding == null)) {
						throw new MetaDataException(metaDataName + " : If [query] is present, then [model] and [document] should be absent " + 
														"and [geometryBinding] is required for menu item " + item.getName());
					}
				}
				else if (modelName != null) {
					if (geometryBinding != null) {
						throw new MetaDataException(metaDataName + " : If [model] is present, then [document], [query] " + 
														"[geometryBinding] should be absent for menu item " + item.getName());
					}
				}
				else {
					throw new MetaDataException(metaDataName + " : One of [document], [query] or [model] " + 
													"is required for menu item " + item.getName());
				}
				if ((refreshTimeInSeconds != null) && refreshTimeInSeconds.intValue() < 5) {
					throw new MetaDataException(metaDataName + " : [refreshTimeInSeconds] must be at least 5");
				}
				
				result.setDocumentName(documentName);
				result.setModelName(modelName);
				result.setQueryName(queryName);
				result.setGeometryBinding(geometryBinding);
				result.setShowRefreshControls(item.getShowRefreshControls());
				result.setRefreshTimeInSeconds(refreshTimeInSeconds);
				
				items.add(result);
			}
			else if (action instanceof TreeItem) {
				TreeItem item = (TreeItem) action;
				org.skyve.wildcat.metadata.module.menu.TreeItem result = new org.skyve.wildcat.metadata.module.menu.TreeItem();
				populateItem(metaDataName, validRoleNames, result, item);

				String documentName = item.getDocumentName();
				String queryName = item.getQueryName();
				String modelName = item.getModelName();
				String parentBinding = item.getParentBinding();
				
				if (documentName != null) {
					if ((queryName != null) ||
							(modelName != null) ||
							(parentBinding == null)) {
						throw new MetaDataException(metaDataName + " : If [document] is present, then [model] and [query] should be absent " + 
														"and [parentBinding] is required for menu item " + item.getName());
					}
				}
				else if (queryName != null) {
					if ((modelName != null) ||
							(parentBinding == null)) {
						throw new MetaDataException(metaDataName + " : If [query] is present, then [model] and [document] should be absent " + 
														"and [parentBinding] is required for menu item " + item.getName());
					}
				}
				else if (modelName != null) {
					if (parentBinding != null) {
						throw new MetaDataException(metaDataName + " : If [model] is present, then [document], [query] and " + 
														"[parentBinding] should be absent for menu item " + item.getName());
					}
				}
				else {
					throw new MetaDataException(metaDataName + " : One of [document], [query] or [model] " + 
													"is required for menu item " + item.getName());
				}
				
				result.setDocumentName(documentName);
				result.setModelName(modelName);
				result.setQueryName(queryName);
				result.setParentBinding(parentBinding);
				
				items.add(result);
			}
			else {
				throw new MetaDataException("Item " + action.getClass() + " is not supported");
			}
		}
	}
	
	private void populateItem(String metaDataName,
								Set<String> validRoleNames,
								AbstractMenuItem result,
								Item metadata) 
	throws MetaDataException {
		String value = metadata.getName();
		if (value == null) {
			throw new MetaDataException(metaDataName + " : The [name] for a menu item is required");
		}
		result.setName(value);
		
		Set<String> grantNames = new TreeSet<>();
		List<GrantedTo> grants = metadata.getRoles();
		if (grants.isEmpty()) {
			throw new MetaDataException(metaDataName + " : At least one role is required for menu item " + result.getName());
		}
		for (GrantedTo grant : grants) {
			value = grant.getRoleName();
			if (value == null) {
				throw new MetaDataException(metaDataName + " : The [roleName] is required for a grant for menu item " +
												result.getName());
			}
			if (! grantNames.add(value)) {
				throw new MetaDataException(metaDataName + " : Duplicate grant for role " + 
												value + " in menu item " + result.getName());
			}
			if (! validRoleNames.contains(value)) {
				// allow implicit data administrator role through
				if (! User.DATA_ADMINISTRATOR_ROLE.equals(getName() + '.' + value)) {
					throw new MetaDataException(metaDataName + " : The role " + value + " granted in menu item " +
													result.getName() + " is not defined in this module");
				}
			}
			result.getRoleNames().add(value);
		}
		populateUxuis(metaDataName, result.getName(), metadata.getUxuis(), result.getUxUis());
	}
	
	private static void populateUxuis(String metaDataName, 
										String itemName, 
										List<ApplicableTo> uxuis, 
										Set<String> uxuisToAddTo)
	throws MetaDataException {
		Set<String> applicableUxuis = new TreeSet<>();
		for (ApplicableTo uxui : uxuis) {
			String value = uxui.getUxUi();
			if (value == null) {
				throw new MetaDataException(metaDataName + " : The [name] is required for a uxui for menu item " +
												itemName);
			}
			if (! applicableUxuis.add(value)) {
				throw new MetaDataException(metaDataName + " : Duplicate uxui " + 
												value + " in menu item " + itemName);
			}
			uxuisToAddTo.add(value);
		}
	}
}
