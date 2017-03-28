package org.skyve.impl.metadata.module.query;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.model.document.field.ConvertableField;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.query.DocumentQueryDefinition;
import org.skyve.metadata.module.query.QueryColumn;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.util.Binder.TargetMetaData;

public class DocumentQueryDefinitionImpl extends QueryDefinitionImpl implements DocumentQueryDefinition {
	private static final long serialVersionUID = 1867738351262041832L;

	private static final String USER_EXPRESSION = "{USER}";
	private static final String USERID_EXPRESSION = "{USERID}";
	private static final String USERNAME_EXPRESSION = "{USERNAME}";
	private static final String DATAGROUPID_EXPRESSION = "{DATAGROUPID}";
	private static final String CONTACTID_EXPRESSION = "{CONTACTID}";
	private static final String CUSTOMER_EXPRESSION = "{CUSTOMER}";
	private static final String DATE_EXPRESSION = "{DATE}";
	private static final String DATETIME_EXPRESSION = "{DATETIME}";

	private String documentName;

	private String fromClause;

	private String filterClause;

	private List<QueryColumn> columns = new ArrayList<>();

	@Override
	public Module getDocumentModule(Customer customer) {
		Module result = getOwningModule();
		DocumentRef ref = result.getDocumentRefs().get(getDocumentName());
		String referencedModuleName = ref.getReferencedModuleName();
		if (referencedModuleName != null) {
			result = customer.getModule(referencedModuleName);
		}
		
		return result;
	}

	@Override
	public String getDocumentName() {
		return documentName;
	}

	public void setDocumentName(String documentName) {
		this.documentName = documentName;
	}

	@Override
	public String getFromClause() {
		return fromClause;
	}

	public void setFromClause(String fromClause) {
		this.fromClause = fromClause;
	}

	@Override
	public String getFilterClause() {
		return filterClause;
	}

	public void setFilterClause(String filterClause) {
		this.filterClause = filterClause;
	}

	@Override
	public List<QueryColumn> getColumns() {
		return columns;
	}

	@Override
	@SuppressWarnings("incomplete-switch")
	public DocumentQuery constructDocumentQuery(AggregateFunction summaryType,
													String tagId) {
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Module owningModule = getOwningModule();
		Document document = owningModule.getDocument(customer, getDocumentName());

		Map<String, Object> implicitParameters = new TreeMap<>();
		String replacedFromClause = replaceImplicitExpressions(getFromClause(), implicitParameters, user, customer);
		String replacedFilterClause = replaceImplicitExpressions(getFilterClause(), implicitParameters, user, customer);
		DocumentQuery result = persistence.newDocumentQuery(document, replacedFromClause, replacedFilterClause);
		if (! implicitParameters.isEmpty()) {
			for (String implicitParameterName : implicitParameters.keySet()) {
				result.putParameter(implicitParameterName, implicitParameters.get(implicitParameterName));
			}
		}
		
		if (summaryType == null) {
			result.addBoundProjection(Bean.DOCUMENT_ID);
			result.addBoundProjection(PersistentBean.LOCK_NAME);
			result.addBoundProjection(Bean.BIZ_KEY);
			result.addBoundProjection(Bean.CUSTOMER_NAME);
			if (tagId != null) {
				StringBuilder tagSubSelect = new StringBuilder(64);
				tagSubSelect.append("(select distinct 'true' from adminTagged as tagged where tagged.tag.bizId = '");
				tagSubSelect.append(tagId);
				tagSubSelect.append("' and tagged.bizUserId = '");
				tagSubSelect.append(user.getId());
				tagSubSelect.append("' and tagged.taggedBizId = bean.bizId)");
				result.addExpressionProjection(tagSubSelect.toString(), PersistentBean.TAGGED_NAME);
			}
			result.addBoundProjection(PersistentBean.FLAG_COMMENT_NAME);
		}

		// This is used to determine if we need to add the "this" projection to the query or not
		// If we have any transient binding, then we need to load the bean too to resolve the value.
		boolean anyTransientBindingInQuery = false;
		
		for (QueryColumn column : getColumns()) {
			Attribute attribute = null;
			String binding = column.getBinding();
			String expression = column.getExpression();
			String alias = column.getName();
			if (binding != null) {
				if (alias == null) {
					alias = binding;
				}
	
				// Find the attribute
				TargetMetaData target = null;
				int lastDotIndex = binding.lastIndexOf('.');
				if (lastDotIndex > -1) { // binding is across documents
					// Find the reference for the association binding
					String associationBinding = binding.substring(0, lastDotIndex);

					// determine if we need to left join this reference (as it is not required)
					// NB check each token of the binding and if ANY is optional, we need to left join the lot
					boolean leftJoin = false;
					// determine if the projected value is transient
					// NB check each token of the binding and if ANY is transient, the lot is transient
					boolean transientBinding = false;
					int dotIndex = associationBinding.indexOf('.');
					if (dotIndex < 0) {
						dotIndex = associationBinding.length();
					}
					while (dotIndex >= 0) {
						String associationBindingPart = associationBinding.substring(0, dotIndex);
						target = BindUtil.getMetaDataForBinding(customer, owningModule, document, associationBindingPart);
						attribute = target.getAttribute();
						// Association could be null if 'parent' used in query
						if (attribute != null) {
							if (! attribute.isPersistent()) {
								transientBinding = true;
								anyTransientBindingInQuery = true;
							}
							Relation relation = (Relation) attribute;
							if (! relation.isRequired()) {
								leftJoin = true;
							}
						}
						// We've checked the entire association binding - bug out
						if (dotIndex == associationBinding.length()) {
							dotIndex = -1;
						}
						else {
							// any more '.'; if so, process them
							dotIndex = associationBinding.indexOf('.', dotIndex + 1);
							// no more '.' - so check the last part of the expression
							if (dotIndex < 0) {
								dotIndex = associationBinding.length();
							}
						}
					}

					// Find attribute of the full binding now
					target = BindUtil.getMetaDataForBinding(customer, owningModule, document, binding);
					attribute = target.getAttribute();
					if (attribute != null) {
						if (transientBinding || (! attribute.isPersistent())) {
							anyTransientBindingInQuery = true;
							continue;
						}
						// If we have a reference directly to a mapped document, don't process it coz it can't be joined.
						if (attribute instanceof Association) {
							Association association = (Association) attribute;
							// If we have a reference directly to a non-required document, add a left outer join
							if (! association.isRequired()) {
								result.addLeftOuterJoin(binding);
							}
							Document associatedDocument = owningModule.getDocument(customer, association.getDocumentName());
							if (associatedDocument != null) {
								Persistent associatedPersistent = associatedDocument.getPersistent();
								// Not a persistent document
								if (associatedPersistent == null) {
									anyTransientBindingInQuery = true;
									continue;
								}
								// Not a proper database relation, its just mapped so it can't be resolved in a query
								if (ExtensionStrategy.mapped.equals(associatedPersistent.getStrategy())) {
									anyTransientBindingInQuery = true;
									continue;
								}
							}
						}
					}

					// If we have a reference to a field in a mapped document, don't process it coz it can't be joined
					Document targetDocument = target.getDocument();
					if (targetDocument != null) {
						Persistent targetPersistent = targetDocument.getPersistent();
						// Not a persistent document
						if (targetPersistent == null) {
							anyTransientBindingInQuery = true;
							continue;
						}
						// Not a proper relation, its just mapped so it can't be resolved
						if (ExtensionStrategy.mapped.equals(targetPersistent.getStrategy())) {
							anyTransientBindingInQuery = true;
							continue;
						}
					}
					
					// left join this reference if required 
					if (leftJoin) {
						result.addLeftOuterJoin(associationBinding);
					}
				}
				else { // simple binding
					// NB Cater for document hierarchies here
					target = BindUtil.getMetaDataForBinding(customer, owningModule, document, binding);
					attribute = target.getAttribute();
					if (attribute != null) {
						if (! attribute.isPersistent()) {
							anyTransientBindingInQuery = true;
							continue;
						}

						if (attribute instanceof Association) {
							// If we have a reference to a mapped document, don't process it coz it can't be joined
							Association association = (Association) attribute;
							Document associatedDocument = owningModule.getDocument(customer, association.getDocumentName());
							if (associatedDocument != null) {
								Persistent associatedPersistent = associatedDocument.getPersistent();
								// Not a persistent document
								if (associatedPersistent == null) {
									anyTransientBindingInQuery = true;
									continue;
								}
								// Not a proper database relation, its just mapped so it can't be resolved in a query
								if (ExtensionStrategy.mapped.equals(associatedPersistent.getStrategy())) {
									anyTransientBindingInQuery = true;
									continue;
								}
							}
							
							// Outer join if this attribute is not required
							if (! association.isRequired()) {
								result.addLeftOuterJoin(binding);
							}
						}
					}
				}
			}
			else if (expression == null) {
				continue;
			}

			if (column.isProjected()) {
				if (summaryType == null) {
					if (binding != null) {
						result.addBoundProjection(binding, alias);
					}
					else {
						result.addExpressionProjection(expression, alias);
					}
				}
				else {
					if (attribute != null) {
						AttributeType type = attribute.getAttributeType();
						if (type != null) {
							switch (summaryType) {
							case Sum:
							case Avg:
								switch (type) {
								case integer:
								case longInteger:
								case decimal2:
								case decimal5:
								case decimal10:
									result.addAggregateProjection(summaryType, binding, alias);
									break;
								}
								break;
							case Count:
								result.addAggregateProjection(summaryType, binding, alias);
								break;
							case Max:
							case Min:
								switch (type) {
								case integer:
								case longInteger:
								case decimal2:
								case decimal5:
								case decimal10:
								case date:
								case dateTime:
								case time:
								case timestamp:
								case text:
								case memo:
								case markup:
								case colour:
									result.addAggregateProjection(summaryType, binding, alias);
									break;
								}
								break;
							}
						}
					}
				}
			}

			FilterOperator filterOperator = column.getFilterOperator();
			if (filterOperator != null) {
				Object operand = null;

				String filterExpression = column.getFilterExpression();
				if ((filterExpression != null) && (! filterExpression.equals("?"))) {
					if (filterExpression.equals(USER_EXPRESSION)) {
						operand = user.getName();
					}
					else if (filterExpression.equals(USERID_EXPRESSION)) {
						operand = user.getId();
					}
					else if (filterExpression.equals(USERNAME_EXPRESSION)) {
						operand = user.getContactName();
					}
					else if (filterExpression.equals(DATAGROUPID_EXPRESSION)) {
						operand = user.getDataGroupId();
					}
					else if (filterExpression.equals(CONTACTID_EXPRESSION)) {
						operand = user.getContactId();
					}
					else if (filterExpression.equals(CUSTOMER_EXPRESSION)) {
						operand = customer.getName();
					}
					else if (filterExpression.equals(DATE_EXPRESSION)) {
						operand = new DateOnly();
					}
					else if (filterExpression.equals(DATETIME_EXPRESSION)) {
						operand = new DateTime();
					}
					else {
						try {
							Converter<?> converter = ((attribute instanceof ConvertableField) ? 
														((ConvertableField) attribute).getConverterForCustomer(customer) : 
														null);
							Class<?> type = String.class;
							if (attribute != null) {
								if (attribute instanceof Enumeration) {
									type = AbstractRepository.get().getEnum((Enumeration) attribute);
								}
								else if (attribute.getAttributeType() != null) {
									type = attribute.getAttributeType().getImplementingType();
								}
							}
							operand = BindUtil.fromString(customer, converter, type, filterExpression, false);
						}
						catch (Exception e) {
							throw new MetaDataException("Could not convert " + filterExpression + " from a string", e);
						}
					}
				}

				switch (filterOperator) {
				case equal:
					result.getFilter().addEquals(binding, operand);
					break;
				case notEqual:
					result.getFilter().addNotEquals(binding, operand);
					break;
				case greater:
					result.getFilter().addGreaterThan(binding, operand);
					break;
				case less:
					result.getFilter().addLessThan(binding, operand);
					break;
				case greaterEqual:
					result.getFilter().addGreaterThanOrEqualTo(binding, operand);
					break;
				case lessEqual:
					result.getFilter().addLessThanOrEqualTo(binding, operand);
					break;
				case isNull:
					result.getFilter().addNull(binding);
					break;
				case notNull:
					result.getFilter().addNotNull(binding);
					break;
				case like:
					result.getFilter().addLike(binding, filterExpression);
					break;
				case notLike:
					result.getFilter().addNotLike(binding, filterExpression);
					break;
				case nullOrEqual:
					result.getFilter().addNullOrEquals(binding, operand);
					break;
				case nullOrNotEqual:
					result.getFilter().addNullOrNotEquals(binding, operand);
					break;
				case nullOrGreater:
					result.getFilter().addNullOrGreaterThan(binding, operand);
					break;
				case nullOrLess:
					result.getFilter().addNullOrLessThan(binding, operand);
					break;
				case nullOrGreaterEqual:
					result.getFilter().addNullOrGreaterThanOrEqualTo(binding, operand);
					break;
				case nullOrLessEqual:
					result.getFilter().addNullOrLessThanOrEqualTo(binding, operand);
					break;
				case nullOrLike:
					result.getFilter().addNullOrLike(binding, filterExpression);
					break;
				case nullOrNotLike:
					result.getFilter().addNullOrNotLike(binding, filterExpression);
					break;
				default:
					throw new IllegalStateException("Unknown operator " + filterOperator +
														" encountered whilst constructing Query.");
				}
			}

			if (summaryType == null) {
				SortDirection sortDirection = column.getSortOrder();
				if (sortDirection != null) {
					if (attribute instanceof Association) {
						StringBuilder sb = new StringBuilder(64);
						sb.append(binding).append('.').append(Bean.BIZ_KEY);
						result.addOrdering(sb.toString(), sortDirection);
					}
					else {
						result.addOrdering(binding, sortDirection);
					}
				}
			}
		}
		
		// only add the "this" projection if we have transient column bindings to load
		// and this is not a summary query
		if (anyTransientBindingInQuery && (summaryType == null)) {
			result.addThisProjection();
		}

		return result;
	}
	
	private static String replaceImplicitExpressions(String clause, 
														Map<String, Object> parametersToAddTo, 
														User user, 
														Customer customer) {
		if (clause == null) {
			return null;
		}
		int paramIndex = parametersToAddTo.size();
		StringBuilder result = new StringBuilder(clause);
		int indexFound = -1;
		while ((indexFound = result.indexOf(USER_EXPRESSION)) >= 0) {
			String paramName = "implicitUserParam" + paramIndex++;
			result = result.replace(indexFound, indexFound + USER_EXPRESSION.length(), ":" + paramName);			
			parametersToAddTo.put(paramName, user.getName());
		}
		while ((indexFound = result.indexOf(USERID_EXPRESSION)) >= 0) {
			String paramName = "implicitUserIdParam" + paramIndex++;
			result = result.replace(indexFound, indexFound + USERID_EXPRESSION.length(), ":" + paramName);
			parametersToAddTo.put(paramName, user.getId());
		}
		while ((indexFound = result.indexOf(USERNAME_EXPRESSION)) >= 0) {
			String paramName = "implicitUserNameParam" + paramIndex++;
			result = result.replace(indexFound, indexFound + USERNAME_EXPRESSION.length(), ":" + paramName);
			parametersToAddTo.put(paramName, user.getContactName());
		}
		while ((indexFound = result.indexOf(DATAGROUPID_EXPRESSION)) >= 0) {
			String paramName = "implicitDataGroupIdParam" + paramIndex++;
			result = result.replace(indexFound, indexFound + DATAGROUPID_EXPRESSION.length(), ":" + paramName);
			parametersToAddTo.put(paramName, user.getDataGroupId());
		}
		while ((indexFound = result.indexOf(CONTACTID_EXPRESSION)) >= 0) {
			String paramName = "implicitContactIdParam" + paramIndex++;
			result = result.replace(indexFound, indexFound + CONTACTID_EXPRESSION.length(), ":" + paramName);
			parametersToAddTo.put(paramName, user.getContactId());
		}
		while ((indexFound = result.indexOf(CUSTOMER_EXPRESSION)) >= 0) {
			String paramName = "implicitCustomerParam" + paramIndex++;
			result = result.replace(indexFound, indexFound + CUSTOMER_EXPRESSION.length(), ":" + paramName);
			parametersToAddTo.put(paramName, customer.getName());
		}
		while ((indexFound = result.indexOf(DATE_EXPRESSION)) >= 0) {
			String paramName = "implicitDateParam" + paramIndex++;
			result = result.replace(indexFound, indexFound + DATE_EXPRESSION.length(), ":" + paramName);
			parametersToAddTo.put(paramName, new DateOnly());
		}
		while ((indexFound = result.indexOf(DATETIME_EXPRESSION)) >= 0) {
			String paramName = "implicitDateTimeParam" + paramIndex++;
			result = result.replace(indexFound, indexFound + DATETIME_EXPRESSION.length(), ":" + paramName);
			parametersToAddTo.put(paramName, new DateTime());
		}
		
		return result.toString();
	}
}
