package org.skyve.impl.bizport;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.CORE;
import org.skyve.bizport.BizPortException;
import org.skyve.bizport.BizPortException.Problem;
import org.skyve.domain.Bean;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.bizport.DataFileField.LoadAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;

public abstract class AbstractDataFileLoader implements DataFileLoader {

	protected LoaderActivityType activityType;
	protected boolean createMissingAssociations;
	protected boolean treatAllEmptyNumericAsZero;
	protected BizPortException exception;
	protected Persistence pers;
	protected User user;
	protected Customer customer;
	protected String moduleName;
	protected String documentName;
	protected Module module;
	protected Document document;

	protected Map<String, Object> valueMap;
	protected Map<String, Bean> createdBeans;

	protected int rowIndex;
	protected int colIndex;

	protected List<Bean> results;

	protected List<DataFileField> fields; // maintain order

	public AbstractDataFileLoader(LoaderActivityType activityType, BizPortException exception,
			String moduleName, String documentName) throws Exception {
		this.activityType = activityType;
		this.exception = exception;
		
		// set values
		this.pers = CORE.getPersistence();
		this.user = pers.getUser();
		this.customer = user.getCustomer();
		setDocumentContext(moduleName, documentName);
	}

	@Override
	public void setFieldIndex(int fieldIndex) {
		colIndex = fieldIndex;
	}
	
	@Override
	public int getDataIndex() {
		return rowIndex;
	}

	@Override
	public void setDataIndex(int dataIndex) {
		this.rowIndex = dataIndex;
	}

	protected void setValueMap(Map<String, Object> valueMap) {
		this.valueMap = valueMap;
	}
	
	protected Map<String, Object> getValueMap() {
		return valueMap;
	}
	
	@Override
	public void setCreateMissingAssocations(boolean create) {
		this.createMissingAssociations = create;
	}

	@Override
	public void setEmptyAsZero(boolean emptyAsZero) {
		this.treatAllEmptyNumericAsZero = emptyAsZero;
	}

	@Override
	public void setException(BizPortException exception) {
		this.setException(exception);
	}

	@Override
	public void setActivityType(LoaderActivityType activityType) {
		this.activityType = activityType;
	}

	@Override
	public BizPortException getException() {
		return exception;
	}

	@Override
	public void addField(DataFileField dff) throws Exception {
		fields.add(dff);
	}

	@Override
	public void addFields(String... bindings) {

		fields = new ArrayList<>();

		int col = 0;
		for (String binding : bindings) {
			DataFileField df = new DataFileField(binding);

			// check for compound binding - by default these will be
			// lookup_equal
			if (binding != null && binding.indexOf('.') > 0 && LoaderActivityType.CREATE_FIND.equals(activityType)) {
				df.setLoadAction(LoadAction.LOOKUP_EQUALS);
			}
			df.setIndex(col++);
			fields.add(df);
		}
	}
	
	@Override
	public void setDocumentContext(String moduleName, String documentName) throws Exception {
		this.moduleName = moduleName;
		module = customer.getModule(moduleName);
		this.documentName = documentName;
		document = module.getDocument(customer, documentName);
	}

	/**
	 * Sets references in the bean if the binding is a reference type attribute
	 * 
	 * @param contextBean
	 * @param field
	 * @param loadValue
	 * @return
	 * @throws Exception
	 */
	protected void lookupBean(Bean contextBean, DataFileField field, Object loadValue, StringBuilder what) throws Exception {

		if (loadValue != null) {

			// default action - look for equals value if attribute document is different to starting
			// document if a compound binding is supplied, we need to
			// find if there is any top-level association which matches
			// e.g. if customer.company.contact.name is supplied,
			// we need to find if there is a customer with company.contact.name that matches
			String binding = field.getBinding();

			// the rest - e.g company.contact.name
			String restBinding = binding;
			String searchBinding = binding;
			if (binding.indexOf('.') > 0) {
				restBinding = binding.substring(binding.indexOf('.') + 1);

				// the bit to search - e.g. we are searching company
				searchBinding = binding.substring(0, binding.indexOf('.'));
			}

			// if restBinding has more than 1 dot, get up to the second dot
			// e.g. - want the binding for customer.company
			String firstLevelBinding = binding;
			if (restBinding.lastIndexOf('.') > restBinding.indexOf('.')) {
				firstLevelBinding = binding.substring(0, binding.indexOf('.', searchBinding.length() + 1));
			}

			// e.g. document
			TargetMetaData drivingMD = Binder.getMetaDataForBinding(customer, module, document, firstLevelBinding);
			Document drivingDoc = drivingMD.getDocument();
			DocumentQuery lookup = pers.newDocumentQuery(drivingDoc.getOwningModuleName(), drivingDoc.getName());
			switch (field.getLoadAction()) {
			case LOOKUP_EQUALS:
			case CONFIRM_VALUE:
				lookup.getFilter().addEquals(restBinding, loadValue);
				break;
			case LOOKUP_LIKE:
				lookup.getFilter().addLike(restBinding, (String) loadValue);
				break;
			case LOOKUP_CONTAINS:
				lookup.getFilter().addLike(restBinding, "%" + (String) loadValue + "%");
				break;
			default:
				break;
			}

			Bean foundBean = lookup.beanResult();
			if (foundBean != null) {
				if (DataFileField.LoadAction.CONFIRM_VALUE.equals(field.getLoadAction()) && contextBean != null) {
					// check if the found bean matches the bean we have already found
					Object resultValue = Binder.get(contextBean, searchBinding);
					if (!foundBean.equals(resultValue)) {
						// throw an error
						what.append("The value '").append(loadValue).append("'");
						what.append(" doesn't match the existing value of '").append(resultValue).append("'.");

						throw new Exception(what.toString());
					}
				}
			} else if (LoaderActivityType.CREATE_ALL.equals(activityType) || createMissingAssociations) {
				// first check the creationCache to establish if this bean has already been created
				StringBuilder mapReference = new StringBuilder(128);
				mapReference.append(binding).append(',').append((String) loadValue);

				// check cache
				boolean foundInCache = false;
				if (createdBeans != null) {
					if (createdBeans.containsKey(mapReference.toString())) {
						Bean previouslyCreatedBean = createdBeans.get(mapReference.toString());
						if (previouslyCreatedBean != null) {

							// reuse the same bean
							Binder.set(contextBean, searchBinding, previouslyCreatedBean);
							foundInCache = true;
						}
					}
				}

				// Binder populateProperty creates intermediate beans as required
				if (!foundInCache) {
					Binder.populateProperty(user, contextBean, binding, loadValue, false);
					if (createdBeans == null) {
						createdBeans = new TreeMap<>();
					}
					createdBeans.put(mapReference.toString(), (Bean) Binder.get(contextBean, searchBinding));
				}
			} else {
				// throw an error
				what.append("The ").append(drivingDoc.getSingularAlias());
				what.append(" '").append(loadValue.toString()).append("'");
				what.append(" doesn't match any existing ").append(drivingDoc.getPluralAlias()).append(".");

				throw new Exception(what.toString());
			}
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T extends Bean> T beanResult() throws Exception {
		nextData();

		if(null == valueMap) {
			return null;
		}
		
		// assume no values loaded
		if (document == null) {
			throw new Exception("The loader has not been initialised correctly - check that you set the document context for the loader.");
		}

		// for general find
		DocumentQuery qFind = pers.newDocumentQuery(moduleName, documentName);

		Object operand = null;
		Bean result = null;
		if (LoaderActivityType.CREATE_FIND.equals(activityType)) {
			result = document.newInstance(user);
		}
		StringBuilder what = null;
		
		for (DataFileField field : fields) {

			colIndex = field.getIndex();
			what = new StringBuilder();

			// general try - if value is not of the expected type or empty,
			// throw an exception skip null attributes
			String binding = field.getBinding();
			if (binding != null) {

				boolean treatEmptyNumericAsZero = treatAllEmptyNumericAsZero || field.isTreatEmptyNumericAsZero();

				TargetMetaData tm = Binder.getMetaDataForBinding(customer, module, document, binding);
				Attribute attr = tm.getAttribute();

				// special case attribute is an association - go to bizkey
				if (AttributeType.association.equals(attr.getAttributeType())) {
					tm = Binder.getMetaDataForBinding(customer, module, document, Binder.createCompoundBinding(binding, Bean.BIZ_KEY));
					attr = tm.getAttribute();
				}

				try {
					Object loadValue = null;

					switch (attr.getAttributeType()) {
					case association:
						// Not required - handled by use of BizKey above
						break;
					case bool:
						// TODO
						break;
					case collection:
						// not supported
						break;
					case colour:
						// TODO
						break;
					case content:
						// not supported
						break;
					case date:
						operand = getDateFieldValue(colIndex);
						if (operand != null) {
							loadValue = new DateOnly((Date) operand);
						}
						break;
					case dateTime:
						operand = getDateFieldValue(colIndex);
						if (operand != null) {
							loadValue = new DateTime((Date) operand);
						}
						break;
					case decimal10:
						operand = getNumericFieldValue(colIndex, treatEmptyNumericAsZero);
						if (operand != null) {
							loadValue = new Decimal10(((Double) operand).doubleValue());
						}
						break;
					case decimal2:
						operand = getNumericFieldValue(colIndex, treatEmptyNumericAsZero);
						if (operand != null) {
							loadValue = new Decimal2(((Double) operand).doubleValue());
						}
						break;
					case decimal5:
						operand = getNumericFieldValue(colIndex, treatEmptyNumericAsZero);
						if (operand != null) {
							loadValue = new Decimal5(((Double) operand).doubleValue());
						}
						break;
					case enumeration:
						// TODO lookup enumeration
						break;
					case geometry:
						// TODO
						break;
					case id:
						operand = getStringFieldValue(colIndex, true);
						if (operand != null) {
							loadValue = operand;
						}
						break;
					case integer:
						operand = getNumericFieldValue(colIndex, treatEmptyNumericAsZero);
						if (operand != null) {
							loadValue = new Integer(((Double) operand).intValue());
						}
						break;
					case inverseOne:
					case inverseMany:
						// not supported
						break;
					case longInteger:
						operand = getNumericFieldValue(colIndex, treatEmptyNumericAsZero);
						if (operand != null) {
							loadValue = new Long(((Double) operand).longValue());
						}
						break;
					case markup:
						operand = getStringFieldValue(colIndex, true);
						if (operand != null) {
							loadValue = operand;
						}
						break;
					case memo:
						operand = getStringFieldValue(colIndex, true);
						if (operand != null) {
							loadValue = operand;
						}
						break;
					case text:
						operand = getStringFieldValue(colIndex, true);
						if (operand != null) {
							loadValue = operand;
						}
						break;
					case time:
						operand = getDateFieldValue(colIndex);
						if (operand != null) {
							loadValue = new TimeOnly((Date) operand);
						}
						break;
					case timestamp:
						operand = getDateFieldValue(colIndex);
						if (operand != null) {
							loadValue = new Timestamp((Date) operand);
						}
						break;
					default:
						break;
					}

					// handle the operand - by default - just attempt to set the value in the binding
					if (field.isRequired() && loadValue == null) {
						what.append(" A value is required for '");
						what.append(attr.getDisplayName());
						what.append("' but no value was found.");
						throw new Exception(what.toString());
					}

					// DOES NOT SUPPORT HIERARCHICHAL UPLOAD
					if (loadValue != null) {

						switch (activityType) {
						case CREATE_ALL:
							Binder.populateProperty(user, result, binding, loadValue, false);
							break;
						case FIND:
							// compile the query filter and run at the end
							switch (field.getLoadAction()) {
							case LOOKUP_EQUALS:
								qFind.getFilter().addEquals(binding, loadValue);
								break;
							case LOOKUP_LIKE:
								qFind.getFilter().addLike(binding, (String) loadValue);
								break;
							case LOOKUP_CONTAINS:
								qFind.getFilter().addLike(binding, "%" + (String) loadValue + "%");
								break;
							default:
								break;
							}
							break;
						case CREATE_FIND:
						default:
							// check for compound binding
							if (binding.indexOf('.') > 0) {
								lookupBean(result, field, loadValue, what);
								break;
							} else if (LoadAction.SET_VALUE.equals(field.getLoadAction())) {
								Binder.set(result, binding, loadValue);
							}
							break;
						}
					}

				} catch (Exception e) {

					// construct exception message using display names
					// show raw string value
					String operandRawValue = getStringFieldValue(colIndex, true);
					if (operandRawValue == null) {
						what.append(" A value was expected for ");
						if (attr != null) {
							what.append("'").append(attr.getDisplayName()).append("'");
						} else {
							what.append("the column");
						}
						what.append(" but no value was found.");
						Problem problem = new Problem(what.toString(), getWhere(colIndex).toString());
						exception.addWarning(problem);
					} else {
						// last remaining option - if no previous issue has been identified
						// the default is bad type
						if (attr == null) {
							what.append(" The value '");
							what.append(operandRawValue);
							what.append("' is invalid or the wrong type.");

						} else if (what.length() == 0) {
							what.append(" The value '");
							what.append(operandRawValue);
							what.append("' found for '");
							what.append(attr.getDisplayName());
							what.append("' is invalid or the wrong type.");
						}
						Problem problem = new Problem(what.toString(), getWhere(colIndex).toString());
						exception.addError(problem);
					}
				}
			}

			colIndex++;
		}

		// now perform the query
		if (LoaderActivityType.FIND.equals(activityType) && !qFind.getFilter().isEmpty()) {
			result = qFind.beanResult();
		}

		return (T) result;
	}

	@SuppressWarnings("unchecked")	
	@Override
	public <T extends Bean> List<T> beanResults() throws Exception {

		while (hasNextData()) {
			nextData();

			Bean bean = beanResult();
			results.add(bean);
		}
		return (List<T>) results;
	}
}
