package org.skyve.impl.metadata.repository;

import java.util.Iterator;
import java.util.List;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.AbstractInverse;
import org.skyve.impl.metadata.model.document.AbstractInverse.InverseRelationship;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.metadata.module.menu.AbstractDocumentMenuItem;
import org.skyve.impl.metadata.module.menu.AbstractDocumentOrQueryOrModelMenuItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.impl.metadata.repository.customer.CustomerModuleRoleMetaData;
import org.skyve.impl.metadata.repository.customer.CustomerRoleMetaData;
import org.skyve.impl.metadata.user.ActionPrivilege;
import org.skyve.impl.metadata.user.Privilege;
import org.skyve.impl.metadata.user.RoleImpl;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.customer.CustomerRole;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Inverse;
import org.skyve.metadata.model.document.Inverse.InverseCardinality;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.UniqueConstraint;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.menu.Menu;
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.ExpressionEvaluator;

/**
 * Do not instantiate directly, use CORE.getRepository().
 * 
 * @author Mike
 */
public class LocalDesignRepository extends FileSystemRepository {
	public LocalDesignRepository() {
		super();
	}

	public LocalDesignRepository(String absolutePath) {
		super(absolutePath);
	}

	public LocalDesignRepository(String absolutePath, boolean loadClasses) {
		super(absolutePath, loadClasses);
	}

	@Override
	public boolean getUseScaffoldedViews() {
		return true;
	}
	
	@Override
	public UserImpl retrieveUser(String userPrincipal) {
		throw new UnsupportedOperationException();
	}
	
	@Override
	public void populatePermissions(User user) {
		throw new UnsupportedOperationException();
		
	}

	@Override
	public void resetUserPermissions(User user) {
		throw new UnsupportedOperationException();
	}

	@Override
	public final void resetMenus(User user) {
		UserImpl internalUser = (UserImpl) user;
		for (Module module : user.getCustomer().getModules()) {
			Menu menu = UtilImpl.cloneBySerialization(module.getMenu());
			removeInaccessibleItems(module.getName(), menu, user);
			internalUser.putModuleMenu(module.getName(), menu);
		}
	}

	private static void removeInaccessibleItems(String moduleName, Menu menu, User user) {
		// Check all the child items to see if we have access
		Iterator<MenuItem> i = menu.getItems().iterator();
		while (i.hasNext()) {
			MenuItem menuItem = i.next();

			// If we are dealing with a menu group, recurse the check to its menu items
			if (menuItem instanceof Menu) {
				Menu menuGroup = (Menu) menuItem;

				removeInaccessibleItems(moduleName, menuGroup, user);

				// If there are no menu items left, remove the menu group as well.
				if (menuGroup.getItems().isEmpty()) {
					i.remove();
				}
			}
			else { // we are dealing with menu items (not a group)
				// item is secured by at least 1 role
				boolean secureMenuItem = ( !menuItem.getRoleNames().isEmpty());

				// if not a secured item then it is automatically accessible
				boolean accessibleMenuItem = ( !secureMenuItem);

				if (! accessibleMenuItem) {
					// check for a role name in the menu item that the user has permissions to.
					for (String roleName : menuItem.getRoleNames()) {
						if (user.isInRole(moduleName, roleName)) {
							accessibleMenuItem = true;
							break;
						}
					}
				}

				// Remove the menu item if it is not accessible,
				if (! accessibleMenuItem) {
					i.remove();
				}
			}
		}
	}

	@Override
	public void validateCustomerForGenerateDomain(Customer customer) {
		try {
			Module homeModule = customer.getHomeModule();
			if (homeModule == null) {
				throw new MetaDataException("Repository returned null for [homeModule] in customer " + customer.getName());
			}
		}
		catch (MetaDataException e) {
			throw new MetaDataException("Home Module reference does not reference a module in customer " + customer.getName(), e);
		}

		for (String moduleName : ((CustomerImpl) customer).getModuleNames()) {
			try {
				if (getModule(customer, moduleName) == null) {
					throw new MetaDataException("Repository returned null for " + moduleName + 
													" for customer " + customer.getName());
				}
			}
			catch (MetaDataException e) {
				throw new MetaDataException("Module reference " + moduleName + 
												" does not reference a module in customer " + customer.getName(), e);
			}
		}
		
		// Validate the role metadata module roles point to valid module roles
		// NB We don't need to check the module name of the role as this is checked when the metadata
		// is converted and we know all module names are correct from the validation performed above.
		for (CustomerRole role : customer.getRoles()) {
			for (CustomerModuleRoleMetaData moduleRole : ((CustomerRoleMetaData) role).getRoles()) {
				String moduleName = moduleRole.getModuleName();
				Module module = getModule(customer, moduleName);
				if (module.getRole(moduleRole.getName()) == null) {
					throw new MetaDataException("Module role " + moduleRole.getName() + 
													" for module " + moduleName +
													" for customer role " + role.getName() +
													" in customer " + customer.getName() +
													" does not reference a valid module role");
				}
			}
		}
		
		// TODO check the converter type corresponds to the type required.
	}

	@Override
	public void validateModuleForGenerateDomain(Customer customer, Module module) {
		// if home document is transient then home ref had better be edit
		String homeDocumentName = module.getHomeDocumentName();
		if (homeDocumentName != null) {
			Document homeDocument = module.getDocument(customer, homeDocumentName);
			if ((homeDocument.getPersistent() == null) && (! ViewType.edit.equals(module.getHomeRef()))) { // is transient but not edit
				throw new MetaDataException("Home document " + homeDocumentName + 
												" for customer " + customer.getName() + 
												" in module " + module.getName() +
												" is transient and therefore the module requires a homeRef of 'edit'.");
			}
		}

		// check query columns
		for (QueryDefinition query : module.getMetadataQueries()) {
			if (query instanceof MetaDataQueryDefinition) {
				MetaDataQueryDefinition documentQuery = (MetaDataQueryDefinition) query;
				Module queryDocumentModule = documentQuery.getDocumentModule(customer);
				Document queryDocument = queryDocumentModule.getDocument(customer, documentQuery.getDocumentName());
				for (MetaDataQueryColumn column : documentQuery.getColumns()) {
					String binding = column.getBinding();
					if (binding != null) {
						TargetMetaData target = null;
						try {
							target = BindUtil.getMetaDataForBinding(customer, 
																		queryDocumentModule,
																		queryDocument,
																		binding);
						}
						catch (MetaDataException e) {
							throw new MetaDataException("Query " + query.getName() + 
															" in module " + query.getOwningModule().getName() +
															" with column binding " + binding +
															" is not a valid binding.", e);
						}
	
						Document targetDocument = target.getDocument();
						Attribute targetAttribute = target.getAttribute();
						if ((! targetDocument.isPersistable()) || // non-persistent document
								((targetAttribute != null) && 
									(! BindUtil.isImplicit(targetAttribute.getName())) &&
									(! targetAttribute.isPersistent()))) { // transient non-implicit attribute
							if (column instanceof MetaDataQueryProjectedColumn) {
								MetaDataQueryProjectedColumn projectedColumn = (MetaDataQueryProjectedColumn) column;
								if (projectedColumn.isSortable() || projectedColumn.isFilterable() || projectedColumn.isEditable()) {
									throw new MetaDataException("Query " + query.getName() + 
																" in module " + query.getOwningModule().getName() +
																" with column binding " + binding +
																" references a transient (or mapped) attribute and should not be sortable, filterable or editable.");
								}
							}
						}
						
						// Customer overridden documents that are used in metadata queries cause an error unless 
						// <association>.bizId is used as the binding.
						if ((targetAttribute != null) && AttributeType.association.equals(targetAttribute.getAttributeType()) &&
								(column.getFilterOperator() != null)) {
							throw new MetaDataException("Query " + query.getName() + 
															" in module " + query.getOwningModule().getName() +
															" with column binding " + binding +
															" references an association which has a column filter defined.  Use [" + 
															binding + ".bizId] as the binding for the column.");
						}
					}
				}
			}
		}
		
		// check menu items
		checkMenu(module.getMenu().getItems(), customer, module);

		// check action privilege references an action in the given document view
		for (Role role : module.getRoles()) {
			for (Privilege privilege : ((RoleImpl) role).getPrivileges()) {
				if (privilege instanceof ActionPrivilege) {
					ActionPrivilege actionPrivilege = (ActionPrivilege) privilege;
					String actionPrivilegeName = actionPrivilege.getName();
					Document actionDocument = module.getDocument(customer, actionPrivilege.getDocumentName());
					if (getAction(customer, actionDocument, actionPrivilegeName, false, false) == null) {
						throw new MetaDataException("Action privilege " + actionPrivilege.getName() + 
														" for customer " + customer.getName() + 
														" in module " + module.getName() +
														" for document " + actionDocument.getName() + 
														" for role " + role.getName() +
														" does not reference a valid action");
					}
				}
			}
		}
	}

	private void checkMenu(List<MenuItem> items, Customer customer, Module module) {
		for (MenuItem item : items) {
			if (item instanceof MenuGroup) {
				checkMenu(((MenuGroup) item).getItems(), customer, module);
			}
			else {
				if (item instanceof AbstractDocumentMenuItem) {
					String documentName = ((AbstractDocumentMenuItem) item).getDocumentName();
					Document document = null;
					if (documentName != null) {
						try {
							document = module.getDocument(customer, documentName);
						}
						catch (Exception e) {
							throw new MetaDataException("Menu [" + item.getName() + 
															"] in module " + module.getName() +
															" is for document " + documentName +
															" which does not exist.", e);
						}
						// NB EditItem can be to a transient document
						if ((! (item instanceof EditItem)) && (document.getPersistent() == null)) {
							throw new MetaDataException("Menu [" + item.getName() + 
															"] in module " + module.getName() +
															" is for document " + documentName +
															" which is not persistent.");
						}
					}

					if (item instanceof AbstractDocumentOrQueryOrModelMenuItem) {
						AbstractDocumentOrQueryOrModelMenuItem dataItem = (AbstractDocumentOrQueryOrModelMenuItem) item;
						String queryName = dataItem.getQueryName();
						MetaDataQueryDefinition query = null;
						if (queryName != null) {
							query = module.getMetaDataQuery(queryName);
							if (query == null) {
								throw new MetaDataException("Menu [" + item.getName() + 
																"] in module " + module.getName() +
																" is for query " + queryName +
																" which does not exist.");
							}
							documentName = query.getDocumentName();
							document = module.getDocument(customer, documentName);
						}
						
						// TODO check list/tree/calendar model names
						String modelName = ((AbstractDocumentOrQueryOrModelMenuItem) item).getModelName();
						if (modelName != null) {
							if (item instanceof MapItem) {
								try {
									getMapModel(customer, document, modelName, false);
								}
								catch (Exception e) {
									throw new MetaDataException("Menu [" + item.getName() + 
																	"] in module " + module.getName() +
																	" is for model " + modelName +
																	" which does not exist.",
																	e);
								}
							}
						}
						
						if (item instanceof TreeItem) {
							// Not a model, then its a query or document so check the document is hierarchical
							if ((modelName == null) && (documentName != null) && (document != null)) {
								if (! documentName.equals(document.getParentDocumentName())) {
									throw new MetaDataException("Tree Menu [" + item.getName() + 
																	"] in module " + module.getName() + 
																	" is for document " + document.getName() + 
																	" which is not hierarchical.");
								}
							}
						}
						else if (item instanceof MapItem) {
							if (document != null) {
								// Check binding is valid
								String binding = ((MapItem) item).getGeometryBinding();
								try {
									TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
									Attribute attribute = target.getAttribute();
									if ((attribute == null) || 
											(! AttributeType.geometry.equals(attribute.getAttributeType()))) {
										throw new MetaDataException("Map Menu [" + item.getName() + 
																		"] in module " + module.getName() + 
																		" has a geometryBinding of " + binding + 
																		" which is not a geometry.");
									}
								}
								catch (Exception e) {
									throw new MetaDataException("Map Menu [" + item.getName() + 
																	"] in module " + module.getName() + 
																	" has a geometryBinding of " + binding + 
																	" which does not exist.",
																	e);
								}
								
								// If the query is defined and not polymorphic, check that the binding is in the query.
								if ((queryName == null) && (documentName != null)) { // default document query
									query = module.getDocumentDefaultQuery(customer, documentName);
								}
								if (query != null) {
									if (! Boolean.TRUE.equals(query.getPolymorphic())) {
										if (query.getColumns().stream().noneMatch(C -> ((C instanceof MetaDataQueryProjectedColumn) && binding.equals(((MetaDataQueryProjectedColumn) C).getBinding())))) {
											throw new MetaDataException("Map Menu [" + item.getName() + 
																			"] in module " + module.getName() + 
																			" has a geometryBinding of " + binding + 
																			" which is not a column in the " + 
																			((queryName == null) ? "default query" : "query " + queryName) + 
																			". Either add the column (preferable) to the query or set the query to be polymorphic.");
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
	
	@Override
	public void validateDocumentForGenerateDomain(Customer customer, Document document) {
		String documentIdentifier = document.getOwningModuleName() + '.' + document.getName();
		Module module = getModule(customer, document.getOwningModuleName());

		// Check conditions
		for (String conditionName : document.getConditionNames()) {
			// Check that conditions do not start with is or not
			if (conditionName.startsWith("is")) {
				throw new MetaDataException("Condition " + conditionName + " in document " + documentIdentifier + " cannot start with 'is' - the 'is' prefix is generated in the bean method.");
			}
			else if (conditionName.startsWith("not")) {
				throw new MetaDataException("Condition " + conditionName + " in document " + documentIdentifier + " cannot start with 'not' - not conditions are automatically generated.  Switch the sense of the condition.");
			}
			
			// Check expression conditions
			Condition condition = document.getCondition(conditionName);
			String expression = condition.getExpression();
			if (BindUtil.isSkyveExpression(expression)) {
				String error = ExpressionEvaluator.validate(expression,
																Boolean.class,
																customer,
																module,
																document);
				if (error != null) {
					throw new MetaDataException("Condition " + conditionName + " in document " + documentIdentifier + " with expression " + expression + " has an error : " + error);
				}
			}

		}
		
		// Check the bizKey expression, if defined
		String bizKeyExpression = document.getBizKeyExpression();
		if (bizKeyExpression != null) {
			String error = BindUtil.validateMessageExpressions(customer, module, document, bizKeyExpression);
			if (error != null) {
				throw new MetaDataException("The biz key [expression] defined contains malformed binding expressions in document " + documentIdentifier + ": " + error);
			}
		}
		
		// If document has a parentDocument defined, ensure that it exists in the document's module.
		try {
			document.getParentDocument(customer);
		}
		catch (@SuppressWarnings("unused") MetaDataException e) {
			throw new MetaDataException("The document " + documentIdentifier + 
											" has a parent document of " +
											document.getParentDocumentName() + " that does not exist in this module.");
		}
		
		// NOTE - Persistent etc is checked when generating documents as it is dependent on the hierarchy and persistence strategy etc

		// Check attributes
		for (Attribute attribute : document.getAttributes()) {
			// TODO for all fields that hasDomain is true, ensure that a bizlet exists and it returns domain values (collection length not zero)
			// TODO for all composition collections (ie reference a document that has a parentDocument = to this one) - no queryName is defined on the collection.
			// TODO for all aggregation collections (ie reference a document that has does not have a parentDocument = to this one {or parentDocument is not defined}) - a queryName must be defined on the collection.

			if (attribute instanceof Field) {
				// Check the default value expressions, if defined
				String defaultValue = ((Field) attribute).getDefaultValue();
				if (defaultValue != null) {
					Class<?> implementingType = attribute.getAttributeType().getImplementingType();
					if (String.class.equals(implementingType)) {
						if (BindUtil.containsSkyveExpressions(defaultValue)) {
							String error = BindUtil.validateMessageExpressions(customer, module, document, defaultValue);
							if (error != null) {
								throw new MetaDataException("The default value " + defaultValue + " is not a valid expression for attribute " + 
																module.getName() + '.' + document.getName() + '.' + attribute.getName() + ": " + error);
							}
						}
						// NB nothing to do here as its a string already
					}
					else {
						if (BindUtil.isSkyveExpression(defaultValue)) {
							String error = ExpressionEvaluator.validate(defaultValue, implementingType, customer, module, document);
							if (error != null) {
								throw new MetaDataException("The default value " + defaultValue + " is not a valid expression for attribute " + 
																module.getName() + '.' + document.getName() + '.' + attribute.getName() + ": " + error);
							}
						}
						else {
							try {
								BindUtil.fromSerialised(implementingType, defaultValue);
							} 
							catch (@SuppressWarnings("unused") Exception e) {
								throw new MetaDataException("The default value " + defaultValue + " for attribute " + 
																module.getName() + '.' + document.getName() + '.' + attribute.getName() + " is not coercible to type " + implementingType + 
																".  Date based types should be expressed as a standard XML date format - YYYY-MM-DD or YYYY-MM-DDTHH24:MM:SS");
							}
						}
					}
				}
			}
			else if (attribute instanceof Reference) {
				Reference reference = (Reference) attribute;
				// Check the document points to a document that exists within this module
				String targetDocumentName = reference.getDocumentName();
				DocumentRef targetDocumentRef = module.getDocumentRefs().get(targetDocumentName);
				if (targetDocumentRef == null) {
					throw new MetaDataException("The target [documentName] of " + 
													targetDocumentName + " in Reference " +
													reference.getName() + " in document " + 
													documentIdentifier + " is not a valid document reference in this module.");
				}
				Document targetDocument = module.getDocument(customer, targetDocumentName);
				if (targetDocument == null) {
					throw new MetaDataException("The target [documentName] of " + 
													targetDocumentName + " in Reference " +
													reference.getName() + " in document " + 
													documentIdentifier + " cannot be found.");
				}
				
				// Check the query (if defined) points to a query of the required document type
				String queryName = reference.getQueryName();
				if (queryName != null) {
					MetaDataQueryDefinition query = module.getMetaDataQuery(queryName);
					if (query == null) {
						throw new MetaDataException("The target [queryName] of " + 
														queryName + " in Reference " +
														reference.getName() + " in document " + 
														documentIdentifier + " is not a valid document query in this module.");
					}
					
					String queryDocumentName = query.getDocumentName();
					if (! targetDocumentName.equals(queryDocumentName)) {
						throw new MetaDataException("The target [queryName] of " + 
														queryName + " in Reference " +
														reference.getName() + " in document " + 
														documentIdentifier + " references a document query for document " + 
														queryDocumentName + ", not document " + targetDocumentName);
					}
				}
				
				// Disallow a dynamic embedded association to a static document (can't save it in hibernate without a static owner)
				if (document.isDynamic() && (! targetDocument.isDynamic()) && (reference.getType() == AssociationType.embedded)) {
					throw new MetaDataException("The dynamic embedded association " + reference.getName() + 
													" in document " + documentIdentifier + " references document " +
													targetDocumentName + " which is not a dynamic document. Dynamic embedded associations to static documents are not permitted.");
				}

				// Disallow a dynamic child collection to a static document (can't save it in hibernate without a static owner)
				if (document.isDynamic() && (! targetDocument.isDynamic()) && (reference.getType() == CollectionType.child)) {
					throw new MetaDataException("The dynamic child collection " + reference.getName() + 
													" in document " + documentIdentifier + " references document " +
													targetDocumentName + " which is not a dynamic document. Dynamic child collections to static documents are not permitted.");
				}
			}
			else if (attribute instanceof Inverse) {
				// Check that the document name and reference name point to a reference
				AbstractInverse inverse = (AbstractInverse) attribute;
				String targetDocumentName = inverse.getDocumentName();
				DocumentRef inverseDocumentRef = module.getDocumentRefs().get(targetDocumentName);
				if (inverseDocumentRef == null) {
					throw new MetaDataException("The target [documentName] of " + 
													targetDocumentName + " in Inverse " +
													inverse.getName() + " in document " + 
													documentIdentifier + " is not a valid document reference in this module.");
				}
				Module targetModule = module;
				String targetModuleName = inverseDocumentRef.getReferencedModuleName();
				if (targetModuleName != null) {
					targetModule = getModule(customer, targetModuleName);
				}
				Document targetDocument = getDocument(customer, targetModule, targetDocumentName);

				String targetReferenceName = inverse.getReferenceName();
				Reference targetReference = targetDocument.getReferenceByName(targetReferenceName);
				if (targetReference == null) {
					throw new MetaDataException("The target [referenceName] of " + 
													targetReferenceName + " in Inverse " +
													inverse.getName() + " in document " + 
													documentIdentifier + " is not a valid reference within the document " + 
													targetModule.getName() + '.' + targetDocumentName);
				}
				boolean one = InverseCardinality.one.equals(inverse.getCardinality());
				if (targetReference instanceof Collection) {
					if (one) {
						throw new MetaDataException("The target [referenceName] of " + 
														targetReferenceName + " in Inverse " +
														inverse.getName() + " in document " + 
														documentIdentifier + " points to a valid collection within the document " + 
														targetModule.getName() + '.' + targetDocumentName + 
														" but the cardinality of the inverse is set to one.");
					}
					if (CollectionType.child.equals(((Collection) targetReference).getType())) {
						throw new MetaDataException("The target [referenceName] of " + 
														targetReferenceName + " in Inverse " +
														inverse.getName() + " in document " + 
														documentIdentifier + " points to a valid collection within the document " + 
														targetModule.getName() + '.' + targetDocumentName + 
														" but the collection is a child collection.  The [parent] attribute should be used instead of an inverse reference.");
					}
				}
				inverse.setRelationship((targetReference instanceof Collection) ?
											InverseRelationship.manyToMany :
											(one ? InverseRelationship.oneToOne : InverseRelationship.oneToMany));
			}
		}
		
		// Check the message binding expressions, if present
		List<UniqueConstraint> constraints = document.getUniqueConstraints();
		if (constraints != null) {
			Module owningModule = getModule(customer, document.getOwningModuleName());
			for (UniqueConstraint constraint : constraints) {
				String message = constraint.getMessage();
				String error = BindUtil.validateMessageExpressions(customer, owningModule, document, message);
				if (error != null) {
					throw new MetaDataException("The unique constraint [message] contains malformed binding expressions in constraint " +
													constraint.getName() + " in document " + documentIdentifier + ": " + error);
				}
			}
		}
		// TODO check binding in uniqueConstraint.fieldReference.ref as above
	}

	@Override
	@SuppressWarnings("unused")
	public void validateViewForGenerateDomain(Customer customer, Document document, View view, String uxui) {
		new ViewValidator((ViewImpl) view,
							this,
							(CustomerImpl) customer,
							(DocumentImpl) document,
							uxui);
	}
}
