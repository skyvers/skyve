package org.skyve.impl.metadata.repository.module;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.metadata.module.JobMetaDataImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.module.menu.AbstractMenuItem;
import org.skyve.impl.metadata.module.menu.MenuGroupImpl;
import org.skyve.impl.metadata.module.menu.MenuImpl;
import org.skyve.impl.metadata.module.query.AbstractMetaDataQueryColumn;
import org.skyve.impl.metadata.module.query.BizQLDefinitionImpl;
import org.skyve.impl.metadata.module.query.MetaDataQueryContentColumnImpl;
import org.skyve.impl.metadata.module.query.MetaDataQueryDefinitionImpl;
import org.skyve.impl.metadata.module.query.MetaDataQueryProjectedColumnImpl;
import org.skyve.impl.metadata.module.query.QueryDefinitionImpl;
import org.skyve.impl.metadata.module.query.SQLDefinitionImpl;
import org.skyve.impl.metadata.repository.ConvertableMetaData;
import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.metadata.repository.module.MetaDataQueryContentColumnMetaData.DisplayType;
import org.skyve.impl.metadata.user.RoleImpl;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.DocumentPermission;
import org.skyve.metadata.user.UserAccess;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.metadata.view.View.ViewType;

@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "module")
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE, 
			name = "module",
			propOrder = {"title",
							"prototype",
							"formLabelLayout",
							"documentation", 
							"homeRef",
							"homeDocument",
							"jobs",
							"documents",
							"roles",
							"menu",
							"queries"})
public class ModuleMetaData extends NamedMetaData implements ConvertableMetaData<Module> {
	private static final long serialVersionUID = -6257431975403255783L;

	private String title;
	private Boolean prototype;
	private FormLabelLayout formLabelLayout;
	private ViewType homeRef;
	private String homeDocument;
	private List<JobMetaDataImpl> jobs = new ArrayList<>();
	private List<ModuleDocumentMetaData> documents = new ArrayList<>();
	private List<QueryMetaData> queries = new ArrayList<>();
	private List<ModuleRoleMetaData> roles = new ArrayList<>();
	private MenuMetaData menu;
	private String documentation;
	private long lastModifiedMillis = Long.MAX_VALUE;

	public String getTitle() {
		return title;
	}

	@XmlAttribute(required = true)
	public void setTitle(String title) {
		this.title = UtilImpl.processStringValue(title);
	}

	public Boolean getPrototype() {
		return prototype;
	}

	@XmlAttribute
	public void setPrototype(Boolean prototype) {
		this.prototype = prototype;
	}

	public String getHomeDocument() {
		return homeDocument;
	}

	@XmlAttribute
	public void setFormLabelLayout(FormLabelLayout formLabelLayout) {
		this.formLabelLayout = formLabelLayout;
	}

	public FormLabelLayout getFormLabelLayout() {
		return formLabelLayout;
	}

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE, required = true)
	public void setHomeDocument(String homeDocument) {
		this.homeDocument = UtilImpl.processStringValue(homeDocument);
	}

	public ViewType getHomeRef() {
		return homeRef;
	}

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	public void setHomeRef(ViewType homeRef) {
		this.homeRef = homeRef;
	}

	@XmlElementWrapper(namespace = XMLMetaData.MODULE_NAMESPACE, name = "jobs")
	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "job", required = true)
	public List<JobMetaDataImpl> getJobs() {
		return jobs;
	}

	@XmlElementWrapper(namespace = XMLMetaData.MODULE_NAMESPACE, name = "documents")
	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "document", required = true)
	public List<ModuleDocumentMetaData> getDocuments() {
		return documents;
	}

	@XmlElementWrapper(namespace = XMLMetaData.MODULE_NAMESPACE, name = "queries")
	@XmlElementRefs({@XmlElementRef(type = MetaDataQueryMetaData.class),
						@XmlElementRef(type = BizQLMetaData.class),
						@XmlElementRef(type = SQLMetaData.class)})
	public List<QueryMetaData> getQueries() {
		return queries;
	}

	@XmlElementWrapper(namespace = XMLMetaData.MODULE_NAMESPACE, name = "roles")
	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "role", required = true)
	public List<ModuleRoleMetaData> getRoles() {
		return roles;
	}

	public MenuMetaData getMenu() {
		return menu;
	}

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE, required = true)
	public void setMenu(MenuMetaData menu) {
		this.menu = menu;
	}

	public String getDocumentation() {
		return documentation;
	}

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setDocumentation(String documentation) {
		this.documentation = UtilImpl.processStringValue(documentation);
	}

	@Override
	public long getLastModifiedMillis() {
		return lastModifiedMillis;
	}

	@XmlTransient
	public void setLastModifiedMillis(long lastModifiedMillis) {
		this.lastModifiedMillis = lastModifiedMillis;
	}

	@Override
	public Module convert(String metaDataName, ProvidedRepository repository) {
		ModuleImpl result = new ModuleImpl(repository);
		result.setLastModifiedMillis(getLastModifiedMillis());

		String moduleName = getName();
		if (moduleName == null) {
			throw new MetaDataException(metaDataName + " : The module [name] is required");
		}
		result.setName(moduleName);

		String value = getTitle();
		if (value == null) {
			throw new MetaDataException(metaDataName + " : The module [title] is required");
		}
		result.setTitle(value);

		result.setPrototype(Boolean.TRUE.equals(prototype));
		
		result.setFormLabelLayout(getFormLabelLayout());
		
		result.setDocumentation(documentation);
		
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

		List<ModuleDocumentMetaData> repositoryDocuments = getDocuments();
		Set<String> documentNames = new TreeSet<>();
		if (repositoryDocuments != null) {
			for (ModuleDocumentMetaData document : repositoryDocuments) {
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
		
		List<JobMetaDataImpl> repositoryJobs = getJobs();
		if (repositoryJobs != null) {
			Set<String> jobNames = new TreeSet<>();
			for (JobMetaDataImpl job : repositoryJobs) {
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
				if (queryMetaData instanceof SQLMetaData) {
					SQLMetaData sqlMetaData = (SQLMetaData) queryMetaData;
					SQLDefinitionImpl sqlImpl = new SQLDefinitionImpl();
					populateQueryProperties(queryMetaData, 
												sqlImpl,
												metaDataName,
												result,
												queryNames,
												documentNames);
					sqlImpl.setQuery(sqlMetaData.getQuery());
				}
				else if (queryMetaData instanceof BizQLMetaData) {
					BizQLMetaData bizQLMetaData = (BizQLMetaData) queryMetaData;
					BizQLDefinitionImpl bizQLImpl = new BizQLDefinitionImpl();
					populateQueryProperties(queryMetaData,
												bizQLImpl,
												metaDataName,
												result,
												queryNames,
												documentNames);
					bizQLImpl.setQuery(bizQLMetaData.getQuery());
				}
				else if (queryMetaData instanceof MetaDataQueryMetaData) {
					MetaDataQueryMetaData documentQueryMetaData = (MetaDataQueryMetaData) queryMetaData;
					MetaDataQueryDefinitionImpl documentQueryImpl = new MetaDataQueryDefinitionImpl();
					populateQueryProperties(queryMetaData,
												documentQueryImpl,
												metaDataName,
												result,
												queryNames,
												documentNames);

					value = documentQueryMetaData.getDocumentName();
					if (value == null) {
						throw new MetaDataException(metaDataName + " : The [documentName] for query " + 
														documentQueryImpl.getName() + " is required");
					}
					if (! documentNames.contains(value)) {
						throw new MetaDataException(metaDataName + " : The [documentName] of " + value + " for query " +
														documentQueryImpl.getName() + " is not a module document");
					}
					documentQueryImpl.setDocumentName(value);
					documentQueryImpl.setPolymorphic(documentQueryMetaData.getPolymorphic());
					documentQueryImpl.setAggregate(Boolean.TRUE.equals(documentQueryMetaData.getAggregate()));
					documentQueryImpl.setFromClause(documentQueryMetaData.getFrom());
					documentQueryImpl.setFilterClause(documentQueryMetaData.getFilter());

					List<MetaDataQueryColumnMetaData> repositoryQueryColumns = documentQueryMetaData.getColumns();
					if (repositoryQueryColumns != null) {
						for (MetaDataQueryColumnMetaData repositoryColumn : repositoryQueryColumns) {
							MetaDataQueryProjectedColumnMetaData projectedRepositoryColumn = null;
							MetaDataQueryContentColumnMetaData contentRepositoryColumn = null;
							AbstractMetaDataQueryColumn column = null;
							MetaDataQueryProjectedColumnImpl projectedColumn = null;
							MetaDataQueryContentColumnImpl contentColumn = null;
							if (repositoryColumn instanceof MetaDataQueryProjectedColumnMetaData) {
								projectedRepositoryColumn = (MetaDataQueryProjectedColumnMetaData) repositoryColumn;
								projectedColumn = new MetaDataQueryProjectedColumnImpl();
								column = projectedColumn;
							}
							else {
								contentRepositoryColumn = (MetaDataQueryContentColumnMetaData) repositoryColumn;
								contentColumn = new MetaDataQueryContentColumnImpl();
								column = contentColumn;
							}
							
							column.setName(repositoryColumn.getName());
							String binding = repositoryColumn.getBinding();
							if ((projectedColumn != null) && (projectedRepositoryColumn != null)) {
								String expression = projectedRepositoryColumn.getExpression();
								if ((binding == null) && (expression == null)) {
									throw new MetaDataException(metaDataName + 
																	" : The [binding] and [expression] for a query column is missing in query " + 
																	documentQueryImpl.getName());
								}
								if ((binding != null) && (expression != null)) {
									throw new MetaDataException(metaDataName + 
																	" : Both the [binding] and [expression] for a query column are entered in query " + 
																	documentQueryImpl.getName());
								}
								if ((expression != null) && (column.getName() == null)) {
									throw new MetaDataException(metaDataName + 
																	" : An [expression] query column requires the [name] to be entered in query " + 
																	documentQueryImpl.getName());
								}
								projectedColumn.setExpression(expression);
							}
							else if (contentColumn != null) {
								if (binding == null) {
									throw new MetaDataException(metaDataName + 
																	" : The [binding] for a content query column is missing in query " + 
																	documentQueryImpl.getName());
								}
							}
							column.setBinding(binding);
							column.setDisplayName(repositoryColumn.getDisplayName());
							FilterOperator filterOperator = repositoryColumn.getFilterOperator();
							String filterExpression = repositoryColumn.getFilterExpression();
							if ((filterOperator != null) && 
									(! filterOperator.equals(FilterOperator.isNull)) &&
									(! filterOperator.equals(FilterOperator.notNull)) && 
									(filterExpression == null)) {
								throw new MetaDataException(metaDataName + " : Operator " + filterOperator + 
																" in column " + column.getBinding() + 
																" in query " + documentQueryImpl.getName() + 
																" requires an [expression].");
							}
							if (((filterOperator == null) || 
									filterOperator.equals(FilterOperator.isNull) || 
									filterOperator.equals(FilterOperator.notNull)) &&
									(filterExpression != null)) {
								throw new MetaDataException(metaDataName + " : Operator " + filterOperator + 
																" in column " + column.getBinding() + 
																" in query " + documentQueryImpl.getName() +
																" does not require an [expression].");
							}
							column.setFilterOperator(filterOperator);
							column.setFilterExpression(filterExpression);
							column.setSortOrder(repositoryColumn.getSortOrder());
							Boolean hidden = repositoryColumn.getHidden();
							if (hidden != null) {
								column.setHidden(hidden.booleanValue());
							}
							column.setPixelWidth(repositoryColumn.getPixelWidth());
							column.setAlignment(repositoryColumn.getAlignment());

							if ((projectedColumn != null) && (projectedRepositoryColumn != null)) {
								Boolean projected = projectedRepositoryColumn.getProjected();
								if (projected != null) {
									projectedColumn.setSelected(projected.booleanValue());
								}
								Boolean sortable = projectedRepositoryColumn.getSortable();
								if (sortable != null) {
									projectedColumn.setSortable(sortable.booleanValue());
								}
								Boolean filterable = projectedRepositoryColumn.getFilterable();
								if (filterable != null) {
									projectedColumn.setFilterable(filterable.booleanValue());
								}
								Boolean editable = projectedRepositoryColumn.getEditable();
								if (editable != null) {
									projectedColumn.setEditable(editable.booleanValue());
								}
								Boolean escape = projectedRepositoryColumn.getEscape();
								if (escape != null) {
									projectedColumn.setEscape(escape.booleanValue());
								}
								Sanitisation sanitise = projectedRepositoryColumn.getSanitise();
								if (sanitise != null) {
									projectedColumn.setSanitise(sanitise);
								}
							}
							else if ((contentColumn != null) && (contentRepositoryColumn != null)) {
								DisplayType display = contentRepositoryColumn.getDisplay();
								contentColumn.setDisplay(display);
								contentColumn.setPixelHeight(contentRepositoryColumn.getPixelHeight());
								String emptyThumbnailRelativeFile = contentRepositoryColumn.getEmptyThumbnailRelativeFile();
								if ((emptyThumbnailRelativeFile != null) && 
										(! DisplayType.thumbnail.equals(display))) {
									throw new MetaDataException(metaDataName + " : An [emptyThumbnailRelativeFile] should not be defined on content column " +
																	column.getBinding() + " in query " + documentQueryImpl.getName() + 
																	" as it is not a thumbnail content column");
								}
								contentColumn.setEmptyThumbnailRelativeFile(emptyThumbnailRelativeFile);
							}
							
							documentQueryImpl.getColumns().add(column);
						}
					}
				}
			}
		}

		// Populate Roles
		
		Set<String> roleNames = new TreeSet<>();
		List<ModuleRoleMetaData> repositoryRoles = getRoles();
		if (repositoryRoles != null) {
			for (ModuleRoleMetaData roleMetaData : repositoryRoles) {
				RoleImpl role = new RoleImpl();
				String roleName = roleMetaData.getName();
				if (roleName == null) {
					throw new MetaDataException(metaDataName + " : The [name] for a role is required");
				}
				if (! roleNames.add(roleName)) {
					throw new MetaDataException(metaDataName + " : Duplicate role named " + roleName);
				}
				if (documentNames.contains(roleName)) {
					throw new MetaDataException(metaDataName + " : The role named " + roleName + " is a module document name.");
				}
				role.setName(roleName);
				role.setDescription(roleMetaData.getDescription());
				role.setDocumentation(roleMetaData.getDocumentation());
				
				// Populate privileges
				Set<String> docPrivNames = new TreeSet<>();
				List<DocumentPrivilegeMetaData> repositoryDocPrivileges = roleMetaData.getPrivileges();
				if (repositoryDocPrivileges != null) {
					for (DocumentPrivilegeMetaData documentPrivilegeMetaData : repositoryDocPrivileges) {
						org.skyve.impl.metadata.user.DocumentPrivilege documentPrivilege = new org.skyve.impl.metadata.user.DocumentPrivilege();
						value = documentPrivilegeMetaData.getDocumentName();
						if (value == null) {
							throw new MetaDataException(metaDataName + " : The [documentName] for a privilege is required for role " + roleName);
						}
						if (! docPrivNames.add(value)) {
							throw new MetaDataException(metaDataName + " : Duplicate document privilege for document " + value + " in role " + roleName);
						}
						if (! documentNames.contains(value)) {
							String message = String.format("%1$s : The privilege [documentName] value of %2$s in role %3$s is not a module document. " +
																"Expected %2$s to be defined in the <documents> section of %4$s.xml",
															metaDataName, value, roleName, result.getName());
							throw new MetaDataException(message);
						}
						if (result.getDocumentRefs().get(value).getReferencedModuleName() != null) {
							throw new MetaDataException(metaDataName + " : The privilege [documentName] value of " + value +
															" in role " + roleName +
															" cannot be for a document referenced from another module.  Document Privileges are to be made on the owning module only.");
						}

						documentPrivilege.setName(value);
						DocumentPermission docPermission = documentPrivilegeMetaData.getPermission();
						if (docPermission == null) {
							throw new MetaDataException(metaDataName + " : Document permission is required for document " +
															documentPrivilege.getName() + " in role " + roleName);
						}
						documentPrivilege.setPermission(docPermission);

						role.getPrivileges().add(documentPrivilege);

						// Add the action privileges also
						List<ActionPrivilegeMetaData> repositoryActionPrivileges = documentPrivilegeMetaData.getActions();
						if (repositoryActionPrivileges != null) {
							for (ActionPrivilegeMetaData actionPrivilegeMetaData : repositoryActionPrivileges) {
								org.skyve.impl.metadata.user.ActionPrivilege actionPrivilege = new org.skyve.impl.metadata.user.ActionPrivilege();
								value = actionPrivilegeMetaData.getActionName();
								if (value == null) {
									throw new MetaDataException(metaDataName + " : The [actionName] for a privilege is required for document " +
																	documentPrivilege.getName() + " in role " + roleName);
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
																	documentPrivilege.getName() + " in role " + roleName);
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
																	documentPrivilege.getName() + " in role " + roleName);
								}
								contentPermission.setDocumentName(documentPrivilege.getName());

								role.getContentPermissions().add(contentPermission);
							}
						}
					}
				}
				
				// Populate User Accesses
				List<ModuleRoleUserAccessMetaData> repositoryAccesses = roleMetaData.getAccesses();
				if (repositoryAccesses != null) {
					Map<UserAccess, Set<String>> accesses = role.getAccesses();
					for (ModuleRoleUserAccessMetaData accessMetaData : repositoryAccesses) {
						// Validate access metadata
						accessMetaData.validate(metaDataName, roleName, result);

						// Validate and add ux/uis
						Set<String> uxuis = null;
						List<ModuleRoleUserAccessUxUiMetadata> uxuisMetaData = accessMetaData.getUxuis();
						if (uxuisMetaData.isEmpty()) {
							uxuis = UserAccess.ALL_UX_UIS;
						}
						else {
							uxuis = new TreeSet<>();
							for (ModuleRoleUserAccessUxUiMetadata uxuiMetaData : uxuisMetaData) {
								String uxuiName = uxuiMetaData.getName();
								if (uxuiName == null) {
									throw new MetaDataException(metaDataName + " : [name] is required for UX/UI in user access " + accessMetaData.toUserAccess(moduleName).toString() + " in role " + roleName);
								}
								if (! uxuis.add(uxuiMetaData.getName())) {
									throw new MetaDataException(metaDataName + " : Duplicate UX/UI of " + uxuiMetaData.getName() + " in user access " + accessMetaData.toUserAccess(moduleName).toString() + " in role " + roleName);
								}
							}
						}

						// Put into accesses
						if (accesses.put(accessMetaData.toUserAccess(moduleName), uxuis) != null) {
							throw new MetaDataException(metaDataName + " : Duplicate user access " + accessMetaData.toUserAccess(moduleName).toString() + " in role " + roleName);
						}
					}
				}

				result.putRole(role);
			}
		}

		// Populate the menu

		MenuImpl resultMenu = new MenuImpl();
		List<MenuItem> items = resultMenu.getItems();

		populateModuleMenu(metaDataName, items, getMenu().getActions(), roleNames);
		result.setMenu(resultMenu);

		return result;
	}

	private void populateModuleMenu(String metaDataName, 
										List<MenuItem> items, 
										List<ActionMetaData> actions, 
										Set<String> validRoleNames) {
		for (ActionMetaData action : actions) {
			if (action instanceof GroupMetaData) {
				String value = action.getName();
				if (value == null) {
					throw new MetaDataException(metaDataName + " : The [name] for a menu group is required");
				}
				GroupMetaData group = (GroupMetaData) action;
				MenuGroupImpl menuGroup = new MenuGroupImpl();
				menuGroup.setName(value);
				populateUxuis(metaDataName, value, group.getUxuis(), menuGroup.getUxUis());
				populateModuleMenu(metaDataName, menuGroup.getItems(), group.getActions(), validRoleNames);
				
				items.add(menuGroup);
			}
			else if (action instanceof CalendarItemMetaData) {
				CalendarItemMetaData item = (CalendarItemMetaData) action;
				org.skyve.impl.metadata.module.menu.CalendarItem result = new org.skyve.impl.metadata.module.menu.CalendarItem();
				populateItem(metaDataName, validRoleNames, result, item);

				String documentName = item.getDocumentName();
				String queryName = item.getQueryName();
				String modelName = item.getModelName();
				String startBinding = item.getStartBinding();
				String endBinding = item.getEndBinding();
				
				if (documentName != null) {
					if ((queryName != null) ||
							(startBinding == null) || 
							(endBinding == null)) {
						throw new MetaDataException(metaDataName + " : If [document] is present, then [query] should be absent " + 
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
						throw new MetaDataException(metaDataName + " : If [model] is present, then [document] is required and [query], " + 
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
			else if (action instanceof LinkItemMetaData) {
				LinkItemMetaData item = (LinkItemMetaData) action;
				org.skyve.impl.metadata.module.menu.LinkItem result = new org.skyve.impl.metadata.module.menu.LinkItem();
				populateItem(metaDataName, validRoleNames, result, item);
				
				String href = item.getHref();
				if (href == null) {
					throw new MetaDataException(metaDataName + " : [href] is required for menu item " + item.getName());
				}
				result.setHref(href);
				
				items.add(result);
			}
			else if (action instanceof EditItemMetaData) {
				EditItemMetaData item = (EditItemMetaData) action;
				org.skyve.impl.metadata.module.menu.EditItem result = new org.skyve.impl.metadata.module.menu.EditItem();
				populateItem(metaDataName, validRoleNames, result, item);
				
				String documentName = item.getDocumentName();
				if (documentName == null) {
					throw new MetaDataException(metaDataName + " : [document] is required for menu item " + item.getName());
				}
				result.setDocumentName(documentName);
				
				items.add(result);
			}
			else if (action instanceof ListItemMetaData) {
				ListItemMetaData item = (ListItemMetaData) action;
				org.skyve.impl.metadata.module.menu.ListItem result = new org.skyve.impl.metadata.module.menu.ListItem();
				populateItem(metaDataName, validRoleNames, result, item);

				String documentName = item.getDocumentName();
				String queryName = item.getQueryName();
				String modelName = item.getModelName();

				if (documentName != null) {
					if (queryName != null) {
						throw new MetaDataException(metaDataName + 
														" : If [document] is present, then [query] should be absent " + 
														"for menu item " + item.getName());
					}
				}
				else if (queryName != null) {
					if (modelName != null) {
						throw new MetaDataException(metaDataName + " : If [query] is present, then [model] and [document] should be absent " + 
														"for menu item " + item.getName());
					}
				}
				else if (modelName != null) {
					throw new MetaDataException(metaDataName + " : If [model] is present, then [document] is required " + 
													"for menu item " + item.getName());
				}
				else {
					throw new MetaDataException(metaDataName + " : One of [document], [query] or [model] " + 
													"is required for menu item " + item.getName());
				}

				result.setDocumentName(documentName);
				result.setQueryName(queryName);
				result.setModelName(modelName);
				result.setAutoPopulate(! Boolean.FALSE.equals(item.getAutoPopulate()));
				
				items.add(result);
			}
			else if (action instanceof MapItemMetaData) {
				MapItemMetaData item = (MapItemMetaData) action;
				org.skyve.impl.metadata.module.menu.MapItem result = new org.skyve.impl.metadata.module.menu.MapItem();
				populateItem(metaDataName, validRoleNames, result, item);

				String documentName = item.getDocumentName();
				String queryName = item.getQueryName();
				String modelName = item.getModelName();
				String geometryBinding = item.getGeometryBinding();
				Integer refreshTimeInSeconds = item.getRefreshTimeInSeconds();
				
				if (documentName != null) {
					if ((queryName != null) ||
							(geometryBinding == null)) {
						throw new MetaDataException(metaDataName + " : If [document] is present, then [query] should be absent " + 
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
					throw new MetaDataException(metaDataName + " : If [model] is present, then [document] is required " + 
													"for menu item " + item.getName());
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
			else if (action instanceof TreeItemMetaData) {
				TreeItemMetaData item = (TreeItemMetaData) action;
				org.skyve.impl.metadata.module.menu.TreeItem result = new org.skyve.impl.metadata.module.menu.TreeItem();
				populateItem(metaDataName, validRoleNames, result, item);

				String documentName = item.getDocumentName();
				String queryName = item.getQueryName();
				String modelName = item.getModelName();
				
				if (documentName != null) {
					if (queryName != null) {
						throw new MetaDataException(metaDataName + " : If [document] is present, " +
														"then [query] should be absent for menu item " + 
														item.getName());
					}
				}
				else if (queryName != null) {
					if (modelName != null) {
						throw new MetaDataException(metaDataName + " : If [query] is present, then [model] and [document] should be absent " + 
														"for menu item " + item.getName());
					}
				}
				else if (modelName != null) {
					throw new MetaDataException(metaDataName + " : If [model] is present, then [document] is required " + 
													"for menu item " + item.getName());
				}
				else {
					throw new MetaDataException(metaDataName + " : One of [document], [query] or [model] " + 
													"is required for menu item " + item.getName());
				}
				
				result.setDocumentName(documentName);
				result.setModelName(modelName);
				result.setQueryName(queryName);
				result.setAutoPopulate(! Boolean.FALSE.equals(item.getAutoPopulate()));
				
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
								ItemMetaData metadata) {
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
				if (! UserImpl.DATA_ADMINISTRATOR_ROLE.equals(getName() + '.' + value)) {
					throw new MetaDataException(metaDataName + " : The role " + value + " granted in menu item " +
													result.getName() + " is not defined in this module");
				}
			}
			result.getRoleNames().add(value);
		}
		populateUxuis(metaDataName, result.getName(), metadata.getUxuis(), result.getUxUis());
	}
	
	private static void populateQueryProperties(QueryMetaData queryMetaData, 
													QueryDefinitionImpl query, 
													String metaDataName,
													ModuleImpl owningModule,
													Set<String> queryNames,
													Set<String> documentNames) {
		String value = queryMetaData.getName();
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

		value = queryMetaData.getDescription();
		if (value == null) {
			throw new MetaDataException(metaDataName + " : The [description] for query " + query.getName() + " is required");
		}
		query.setDescription(value);
		query.setDocumentation(queryMetaData.getDocumentation());

		Integer timeoutInSeconds = queryMetaData.getTimeoutInSeconds();
		if (timeoutInSeconds != null) {
			query.setTimeoutInSeconds(timeoutInSeconds.intValue());
		}

		query.setOwningModule(owningModule);
		owningModule.putQuery(query);
	}

	private static void populateUxuis(String metaDataName, 
										String itemName, 
										List<ApplicableTo> uxuis, 
										Set<String> uxuisToAddTo) {
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
