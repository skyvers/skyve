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
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.bizport.DataFileField.LoadAction;
import org.skyve.impl.metadata.model.document.field.ConvertableField;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.Util;
import org.skyve.util.Binder.TargetMetaData;

/**
 * <pre>
 * A DataFileLoader is an interface to file-format-specific classes to allow
 * a common approach to loading of data files irrespective of format
 * for the creation or finding of Skyve beans based on the data in the file.
 * 
 * For example the POISheetLoader provides a simplified way to load POI Worksheet files
 * containing data, and use the loaded data as the basis of bean creation or 
 * finding beans.
 * 
 * Data File Loader handles referential integrity, by handing mapping of data columns or fields
 * to compound bindings in beans. If a column is mapped to a compound binding
 * the data file loader will create the beans required so that the value from the file
 * can be stored successfully.
 * 
 * To use a DataFileLoader,
 * - create a loader
 * - specify an array of bindings (including compound bindings)
 * - specify the activity type
 * - retrieve the beanResults (a list of resulting beans)
 * 
 * For example, a specific implementation of the DataFileLoader may have a usage similar to:
 * 
 *  BizPortException loadExceptions = new BizPortException();
 * 
 *  String[] bindings = new String[]{"company.contact.name", "company.abn", "invoiceNo", "invoiceDate", "invoiceAmount", "dueDate"};
 *  SpecificLoader loader = new SpecificLoader();
 *  
 *  loader.setLoaderActivityType(LoaderActivityType.CREATE_FIND);
 *  loader.setFileInputStream(file.getInputStream());
 *  loader.setException(loadExceptions);
 *  loader.setDocumentContext(Invoice.MODULE_NAME, Invoice.DOCUMENT_NAME);
 *  loader.setBindings(bindings);
 *  loader.setCreateMissingAssocations(Boolean.TRUE); //create any beans required for the load
 *  loader.setDataIndex(1); //handle header row in file
 * 
 *  List<Invoice> newInvoices = loader.beanResults();
 *  if(loadExceptions.hasProblems()){
 * 	 //handle issues
 *  }
 * 
 * For example, if a CSV file contained invoice information to be created, and the first column contained 
 * the name of a company, the column could be mapped to invoice.company.contact.name
 * This implies that for the column data to be loaded into beans, the loader must either find
 * an existing company.contact.name or create a company bean, a contact bean and set that atribute to the value.
 * 
 * (DataFileLoader assumes that population of compound bindings will be handled by Binder.population)
 * 
 * There are three loader activity types: 
 * CREATE_ALL 	- create all beans implied by the data in the file and the mapped bindings provided 
 * 				- do not attempt to locate existing  beans
 * CREATE_FIND - create beans but attempt to locate existing beans, and reuse any created beans
 * 			- for example if two rows of a CSV had the same value for invoice.company.contact.name
 * 			the CREATE_FIND activity is to attempt 
 * 				- to locate an existing company with contact.name equal to the value provided, or
 * 				- if a bean has been created during the load already for that value, reuse that bean.
 * FIND		- no creation of beans - just find existing beans with matching values
 * 
 * DataFileLoader offers two modes of operation - total file load, or iterative.
 * Iterative provides an iterator style, "line by line" approach to loading values to one bean at a time, 
 * similar to this:
 * 
 * 		int consecutiveBlankRowCount = 0;
 * 		while (loader.hasNextData() && consecutiveBlankRowCount < 10) {
 * 			loader.nextData();
 * 
 * 			//ignore totals row 
 * 			String rowTitle = loader.getStringFieldValue(0, true);
 * 			if(!"Total".equals(rowTitle)){
 * 				Invoice newInvoice = loader.beanResult();
 * 			}
 * 			
 * 			...
 * 		}
 * 
 * DataFileField is provided to allow more specific ways of handling individual columns/fields in the file.
 * 
 * By default, a SpecificLoader implementation will take the array of bindings and create DataFileFields for each binding
 * with default settings applicable to that implementation.
 * 
 * For example, a DataFileField may be created for the binding company.contact.name using the LoadAction LOOKUP_EQUALS
 * while a DataFileField for the binding invoiceNo would be SET_VALUE.
 * </pre>
 **/
public abstract class AbstractDataFileLoader {

	public static enum LoaderActivityType {
		CREATE_ALL, CREATE_FIND, FIND
	}

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
	protected boolean debugMode;

	protected Map<String, Bean> createdBeans;

	protected int dataIndex;
	protected int fieldIndex;

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

		this.fields = new ArrayList<>();
		this.results = new ArrayList<>();
	}

	/**
	 * DebugMode Logs debugData for all rows encountered by beanResults()
	 * 
	 * @return
	 */
	public boolean isDebugMode() {
		return debugMode;
	}

	/**
	 * DebugMode Logs debugData for all rows encountered by beanResults()
	 * 
	 * @return
	 */
	public void setDebugMode(boolean debugMode) {
		this.debugMode = debugMode;
	}

	/**
	 * Set the numeric index of a field
	 * 
	 * @param fieldIndex
	 */
	public void setFieldIndex(int fieldIndex) {
		this.fieldIndex = fieldIndex;
	}

	/**
	 * Returns the data index/line number/row of the file currently being processed
	 * 
	 * @return
	 */
	public int getDataIndex() {
		return dataIndex;
	}

	/**
	 * Moves the file to the specified data index/line number/row
	 * 
	 * @param dataIndex
	 */
	public void setDataIndex(int dataIndex) {
		this.dataIndex = dataIndex;
	}

	public void setCreateMissingAssocations(boolean create) {
		this.createMissingAssociations = create;
	}

	public void setEmptyAsZero(boolean emptyAsZero) {
		this.treatAllEmptyNumericAsZero = emptyAsZero;
	}

	public void setException(BizPortException exception) {
		this.setException(exception);
	}

	public void setActivityType(LoaderActivityType activityType) {
		this.activityType = activityType;
	}

	public BizPortException getException() {
		return exception;
	}

	/**
	 * Add a field to the list of expected fields, constructed from the supplied binding
	 * 
	 * @param binding
	 * @throws Exception
	 */
	public void addField(String binding) throws Exception {
		DataFileField field = new DataFileField(binding, fields.size());
		fields.add(finaliseField(field));
	}

	/**
	 * Add a field to the list of expected fields
	 * 
	 * @param dff
	 * @throws Exception
	 */
	public void addField(DataFileField field) throws Exception {
		if (field.getIndex() == null) {
			field.setIndex(fields.size());
		}

		fields.add(finaliseField(field));
	}

	/**
	 * Add a field to the list of expected fields
	 * 
	 * @param binding
	 * @param loadAction
	 * @param required
	 * @param converter
	 */
	public void addField(String binding, LoadAction loadAction, boolean required, Converter<?> converter) throws Exception {
		DataFileField field = new DataFileField(binding, loadAction, required, fields.size(), converter);
		fields.add(finaliseField(field));
	}

	private DataFileField finaliseField(DataFileField field) throws Exception {
		if (field.getBinding() != null) {
			// default inferred load action
			if (field.getBinding().indexOf('.') > 0 && LoaderActivityType.CREATE_FIND.equals(activityType)) {
				field.setLoadAction(LoadAction.LOOKUP_EQUALS);
			}

			// evaluate attribute
			TargetMetaData tm = Binder.getMetaDataForBinding(customer, module, document, field.getBinding());
			Attribute attr = tm.getAttribute();
			if (field.getConverter() == null && attr instanceof ConvertableField) {
				ConvertableField fld = (ConvertableField) attr;
				field.setConverter(fld.getConverterForCustomer(CORE.getPersistence().getUser().getCustomer()));
			}

			// special case attribute is an association - go to bizkey
			if (AttributeType.association.equals(attr.getAttributeType())) {

				tm = Binder.getMetaDataForBinding(customer, module, document,
						Binder.createCompoundBinding(field.getBinding(), Bean.BIZ_KEY));
				field.setAttribute(tm.getAttribute());
			} else {
				// default
				field.setAttribute(attr);
			}
			if (debugMode) {
				StringBuilder log = new StringBuilder();
				log.append("Field added for binding ").append(field.getBinding());
				if (field.getAttribute() != null) {
					log.append(" evaluates to ").append(moduleName).append('.').append(documentName);
					log.append('.').append(field.getAttribute().getName()).append(" (").append(field.getAttribute().getDisplayName())
							.append(')');
				}
				if (field.getConverter() != null) {
					log.append(" with Converter ").append(field.getConverter().getClass().getName());
				}
				Util.LOGGER.info(log.toString());
			}
		}
		return field;
	}

	/**
	 * Add a field for each binding provided, with default actions and in with corresponding data indexes
	 * 
	 * @param bindings
	 */
	public void addFields(String... bindings) throws Exception {

		for (String binding : bindings) {
			addField(binding);
		}
	}

	public List<DataFileField> getFields() {
		return fields;
	}

	/**
	 * Set Field Offset provides a way to handle a field or column offset depending on the field layout encountered.
	 * 
	 * Setting the field offset has the effect of increasing every field index by the value of the offset
	 * 
	 * @param offset
	 */
	public void setFieldOffset(int offset) {
		for (DataFileField fld : fields) {
			if (fld.getIndex() == null) {
				fld.setIndex(new Integer(0));
			}
			fld.setIndex(fld.getIndex().intValue() + offset);
		}
	}

	/**
	 * Sets the module and document context for the creation of beans from the values found in the file
	 * 
	 * @param moduleName
	 * @param documentName
	 * @throws Exception
	 */
	public void setDocumentContext(String moduleName, String documentName) throws Exception {
		this.moduleName = moduleName;
		module = customer.getModule(moduleName);
		this.documentName = documentName;
		document = module.getDocument(customer, documentName);
	}

	/**
	 * Whether the file has another data or row to load
	 * 
	 * @return
	 * @throws Exception
	 */
	abstract boolean hasNextData() throws Exception;

	/**
	 * Retrieves or moves to the next data or row
	 * 
	 * @throws Exception
	 */
	abstract void nextData() throws Exception;

	/**
	 * If the data or row has no values to process
	 * 
	 * @return
	 * @throws Exception
	 */
	abstract boolean isNoData() throws Exception;

	/**
	 * Get a string value from the field at index
	 * 
	 * @param index
	 * @param emptyAsNull
	 * @return
	 * @throws Exception
	 */
	abstract String getStringFieldValue(int index, boolean emptyAsNull) throws Exception;

	/**
	 * Get a numeric value from the field at index
	 * 
	 * @param index
	 * @param emptyAsZero
	 * @return
	 * @throws Exception
	 */
	abstract Double getNumericFieldValue(int index, boolean emptyAsZero) throws Exception;

	/**
	 * Get a date value from the field at index
	 * 
	 * @param index
	 * @return
	 * @throws Exception
	 */
	abstract Date getDateFieldValue(int index) throws Exception;

	/**
	 * Describes the value location
	 * 
	 * @param fieldIndex
	 * @return
	 * @throws Exception
	 */
	private String getWhere(Integer index) throws Exception {
		StringBuilder where = new StringBuilder(128);
		where.append("Row ").append((dataIndex + 1));
		if (index != null) {
			where.append(" column ").append(index.intValue() + 1);
		}
		where.append(".");
		return where.toString();
	}

	/**
	 * Describes the value location
	 * 
	 * @param index
	 * @return
	 * @throws Exception
	 */
	public String getWhere(int index) throws Exception {
		return getWhere(new Integer(index));
	}

	/**
	 * Describes the value location
	 * 
	 * @return
	 * @throws Exception
	 */
	public String getWhere() throws Exception {
		return getWhere(null);
	}

	/**
	 * Provide a specific debug of the fields parsed in the current data or row
	 * 
	 * @return
	 * @throws Exception
	 */
	public String debugData() throws Exception {
		StringBuilder sb = new StringBuilder();
		sb.append("Row ").append(dataIndex);
		if (!isNoData()) {
			for (int index = 0; index < fields.size(); index++) {
				sb.append(", (").append(dataIndex).append(",").append(index).append(") = ");
				sb.append(getStringFieldValue(index, true));
			}
		} else {
			sb.append(" has no data.");
		}
		return sb.toString();
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
	public void lookupBean(Bean contextBean, DataFileField field, Object loadValue, StringBuilder what) throws Exception {

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

	/**
	 * Returns a bean corresponding to the values in the file at the current data index
	 * 
	 * If the load activity is find, the bean returned will be a bean of the document context type where the values encountered match values
	 * of an existing bean in the database.
	 * 
	 * If the load activity is create, the bean will be a new bean constructed using the values encountered.
	 * 
	 * @return
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public <T extends Bean> T beanResult() throws Exception {

		if (debugMode) {
			Util.LOGGER.info(debugData());
		}

		// assume no values loaded
		if (document == null) {
			throw new Exception("The loader has not been initialised correctly - check that you set the document context for the loader.");
		}

		// for general find
		DocumentQuery qFind = pers.newDocumentQuery(moduleName, documentName);

		Object operand = null;
		Bean result = null;
		if (LoaderActivityType.CREATE_ALL.equals(activityType) || LoaderActivityType.CREATE_FIND.equals(activityType)) {
			result = document.newInstance(user);
		}
		StringBuilder what = new StringBuilder(64);
		StringBuilder debugFilter = new StringBuilder(64);

		if (fields.isEmpty()) {
			what.append("No fields were provided - no data will be loaded.");
			Problem prob = new Problem(what.toString(), getWhere());
			exception.addError(prob);
		}
		for (DataFileField field : fields) {

			if (field.getIndex() == null) {
				fieldIndex++;
			} else {
				fieldIndex = field.getIndex().intValue();
			}
			what = new StringBuilder(64);
			debugFilter = new StringBuilder(64);

			// general try - if value is not of the expected type or empty,
			// throw an exception skip null attributes
			String binding = field.getBinding();
			if (binding == null) {
				if (debugMode) {
					Util.LOGGER.info("No binding provided for field " + field.getIndex());
				}
			} else {

				boolean treatEmptyNumericAsZero = treatAllEmptyNumericAsZero || field.isTreatEmptyNumericAsZero();

				Object loadValue = null;

				if (field.getAttribute() == null) {
					Problem prob = new Problem("The attribute corresponding to " + field.getBinding() + " didn't check out.",
							"Column " + (fieldIndex + 1));
					exception.addError(prob);
				} else {
					if (field.getConverter() != null) {
						try {
							// use Skyve converter
							operand = getStringFieldValue(fieldIndex, true);
							String displayValue = (String) operand;
							if (displayValue != null && displayValue.trim().length() > 0) {
								loadValue = field.getConverter().fromDisplayValue(displayValue.trim());
							}
						} catch (Exception e) {
							what.append(" The value ");
							what.append("'").append(field.getAttribute().getDisplayName()).append("'");
							what.append(" is invalid");
							what.append(" (using Converter " + field.getConverter().getClass().getSimpleName()).append(").");
							if (e.getMessage() != null) {
								what.append(" ").append(e.getMessage());
							}
							Problem problem = new Problem(what.toString(), getWhere(fieldIndex).toString());
							exception.addWarning(problem);
						}
					} else {
						try {
							// simplistic conversion making assumptions based on the attribute type
							switch (field.getAttribute().getAttributeType()) {
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
								operand = getDateFieldValue(fieldIndex);
								if (operand != null) {
									loadValue = new DateOnly((Date) operand);
								}
								break;
							case dateTime:
								operand = getDateFieldValue(fieldIndex);
								if (operand != null) {
									loadValue = new DateTime((Date) operand);
								}
								break;
							case decimal10:
								operand = getNumericFieldValue(fieldIndex, treatEmptyNumericAsZero);
								if (operand != null) {
									loadValue = new Decimal10(((Double) operand).doubleValue());
								}
								break;
							case decimal2:
								operand = getNumericFieldValue(fieldIndex, treatEmptyNumericAsZero);
								if (operand != null) {
									loadValue = new Decimal2(((Double) operand).doubleValue());
								}
								break;
							case decimal5:
								operand = getNumericFieldValue(fieldIndex, treatEmptyNumericAsZero);
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
								operand = getStringFieldValue(fieldIndex, true);
								if (operand != null) {
									loadValue = operand;
								}
								break;
							case integer:
								operand = getNumericFieldValue(fieldIndex, treatEmptyNumericAsZero);
								if (operand != null) {
									loadValue = new Integer(((Double) operand).intValue());
								}
								break;
							case inverseOne:
							case inverseMany:
								// not supported
								break;
							case longInteger:
								operand = getNumericFieldValue(fieldIndex, treatEmptyNumericAsZero);
								if (operand != null) {
									loadValue = new Long(((Double) operand).longValue());
								}
								break;
							case markup:
								operand = getStringFieldValue(fieldIndex, true);
								if (operand != null) {
									loadValue = operand;
								}
								break;
							case memo:
								operand = getStringFieldValue(fieldIndex, true);
								if (operand != null) {
									loadValue = operand;
								}
								break;
							case text:
								operand = getStringFieldValue(fieldIndex, true);
								if (operand != null) {
									loadValue = operand;
								}
								break;
							case time:
								operand = getDateFieldValue(fieldIndex);
								if (operand != null) {
									loadValue = new TimeOnly((Date) operand);
								}
								break;
							case timestamp:
								operand = getDateFieldValue(fieldIndex);
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
								what.append(field.getAttribute().getDisplayName());
								what.append("' but no value was found.");
								throw new Exception(what.toString());
							}

						} catch (Exception e) {

							// construct exception message using display names
							// show raw string value
							String operandRawValue = getStringFieldValue(fieldIndex, true);
							if (operandRawValue == null) {
								what.append(" A value was expected for ");
								what.append("'").append(field.getAttribute().getDisplayName()).append("'");
								what.append(" but no value was found.");
								Problem problem = new Problem(what.toString(), getWhere(fieldIndex).toString());
								exception.addWarning(problem);
							} else {
								// last remaining option - if no previous issue has been identified
								// the default is bad type
								what.append(" The value '");
								what.append(operandRawValue);
								what.append("' found for '");
								what.append(field.getAttribute().getDisplayName());
								what.append("' is invalid or the wrong type.");
								if (e.getMessage() != null) {
									what.append(e.getMessage());
								}
								Problem problem = new Problem(what.toString(), getWhere(fieldIndex).toString());
								exception.addError(problem);
							}
						}
					}

					try {
						// DOES NOT SUPPORT HIERARCHICHAL UPLOAD
						if (loadValue == null) {
							if (debugMode) {
								Util.LOGGER.info(getWhere(fieldIndex) + " No load value found for " + field.getBinding());
							}
						} else {

							switch (activityType) {
							case CREATE_ALL:
								if (binding.indexOf('.') > 0) {
									Binder.populateProperty(user, result, binding, loadValue, false);
								} else {
									Binder.set(result, binding, loadValue);
								}
								break;
							case FIND:
								debugFilter.append(field.getAttribute().getDisplayName());
								// compile the query filter and run at the end
								switch (field.getLoadAction()) {
								case LOOKUP_EQUALS:
									qFind.getFilter().addEquals(binding, loadValue);
									debugFilter.append(" = '").append(loadValue).append("'");
									break;
								case LOOKUP_LIKE:
									qFind.getFilter().addLike(binding, (String) loadValue);
									debugFilter.append(" like '").append(loadValue).append("'");
									break;
								case LOOKUP_CONTAINS:
									qFind.getFilter().addLike(binding, "%" + (String) loadValue + "%");
									debugFilter.append(" like '%").append(loadValue).append("%'");
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
						what.append(" The value was loaded but could not be processed.");
						what.append(e.getMessage());
						Problem problem = new Problem(what.toString(), getWhere(fieldIndex).toString());
						exception.addWarning(problem);
					}

				}
			}

			fieldIndex++;
		}

		// now perform the query
		if (LoaderActivityType.FIND.equals(activityType)) {
			if (qFind.getFilter().isEmpty() && debugMode) {
				Util.LOGGER.info(getWhere() + " No filter set for Find operation.");
			} else {
				result = qFind.beanResult();
				if (result == null && debugMode) {
					Util.LOGGER.info("No result found for filter " + debugFilter.toString());
				}
			}
		}

		return (T) result;
	}

	/**
	 * Returns a list of beans corresponding to the values encountered in the file
	 * 
	 * @return
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public <T extends Bean> List<T> beanResults() throws Exception {

		while (hasNextData()) {
			nextData();
			if (isNoData()) {
				if (debugMode) {
					Util.LOGGER.info(getWhere() + " No data found");
				}
				break;
			}

			Bean result = beanResult();
			if (result == null) {
				if (debugMode) {
					Util.LOGGER.info(getWhere() + " Null bean result after load. ");
				}
			} else {
				results.add(result);
			}
		}

		// add a warning if nothing was found
		if (results.isEmpty()) {
			StringBuilder noResults = new StringBuilder();
			noResults.append("No data has been loaded");
			Problem prob = new Problem(noResults.toString(), getWhere());
			exception.addWarning(prob);
		}

		return (List<T>) results;
	}
}
