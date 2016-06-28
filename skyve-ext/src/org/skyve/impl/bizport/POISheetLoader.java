package org.skyve.impl.bizport;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.DataFormatter;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.skyve.CORE;
import org.skyve.bizport.BizPortException;
import org.skyve.bizport.BizPortException.Problem;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
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
import org.skyve.util.Util;
import org.skyve.util.Binder.TargetMetaData;

public class POISheetLoader implements DataFileLoader {

	private LoaderActivityType activityType;
	private boolean createMissingAssociations;
	private boolean treatAllEmptyNumericAsZero;
	private int rowIndex;
	private BizPortException exception;
	private StringBuilder what;
	private Map<String, Bean> createdBeans;

	private Workbook workbook;
	private Sheet sheet;

	private Persistence pers;
	private User user;
	private Customer customer;
	private String moduleName;
	private String documentName;
	private Module module;
	private Document document;

	private List<DataFileField> fields; // maintain order

	private Row row;
	private int colIndex;
	private List<Bean> results;

	private boolean rowLoaded = false;

	// generic constructor
	public POISheetLoader() {

	}

	@Override
	public int getDataIndex() {
		return rowIndex;
	}

	@Override
	public void setDataIndex(int dataIndex) {
		this.rowIndex = dataIndex;
	}

	/**
	 * For untyped (non Bean) access to data file values
	 * 
	 * DOES NOT SUPPORT HIERARCHICHAL UPLOAD
	 * 
	 * @param file
	 * @param sheetIndex
	 * @throws Exception
	 */
	public POISheetLoader(InputStream fileInputStream, int sheetIndex, String moduleName, String documentName, BizPortException exception)
			throws Exception {

		// initialise
		this.activityType = LoaderActivityType.CREATE_FIND;
		this.exception = exception;
		this.createdBeans = new TreeMap<>();

		workbook = WorkbookFactory.create(fileInputStream);
		sheet = workbook.getSheetAt(sheetIndex);

		// set values
		this.pers = CORE.getPersistence();
		this.user = pers.getUser();
		this.customer = user.getCustomer();
		setDocumentContext(moduleName, documentName);

		// defaults
		this.createMissingAssociations = false;
		this.treatAllEmptyNumericAsZero = false;
		this.rowIndex = 0;
		this.colIndex = 0;
		this.row = sheet.getRow(rowIndex);

		this.fields = new ArrayList<>();
		this.results = new ArrayList<>();
	}

	/**
	 * Simple constructor - assumes most common usage as defaults
	 * 
	 * DOES NOT SUPPORT HIERARCHICHAL UPLOAD
	 * 
	 * @param moduleName
	 *            - the module
	 * @param documentName
	 *            - the document into which values will be loaded
	 * @param bindings
	 *            - the specific bindings in corresponding order to the columns/fields being loaded
	 */
	public POISheetLoader(LoaderActivityType activityType, InputStream fileInputStream, int sheetIndex, BizPortException exception,
			String moduleName, String documentName,
			String... bindings) throws Exception {

		this(fileInputStream, sheetIndex, moduleName, documentName, exception);
		this.setActivityType(activityType);

		// construct default FieldLoaders from bindings
		int col = 0;
		for (String binding : bindings) {
			DataFileField df = new DataFileField(binding);

			// check for compound binding - by default these will be lookup_equal
			if (binding != null && binding.indexOf('.') > 0 && LoaderActivityType.CREATE_FIND.equals(activityType)) {
				df.setLoadAction(LoadAction.LOOKUP_EQUALS);
			}
			df.setIndex(col++);
			fields.add(df);
		}
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

	/**
	 * Sets references in the bean if the binding is a reference type attribute
	 * 
	 * @param contextBean
	 * @param field
	 * @param loadValue
	 * @return
	 * @throws Exception
	 */
	private void lookupBean(Bean contextBean, DataFileField field, Object loadValue) throws Exception {

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

	@Override
	public Bean beanResult() throws Exception {

		// assume no values loaded
		if (document == null) {
			throw new Exception("The loader has not been initialised correctly - check that you set the document context for the loader.");
		}

		// for general find
		DocumentQuery qFind = pers.newDocumentQuery(moduleName, documentName);

		Object operand = null;
		Bean result = null;
		if(LoaderActivityType.CREATE_FIND.equals(activityType)){
			result = document.newInstance(user);
		}

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
						operand = getDateValueFromCell(colIndex);
						if (operand != null) {
							loadValue = new DateOnly((Date) operand);
						}
						break;
					case dateTime:
						operand = getDateValueFromCell(colIndex);
						if (operand != null) {
							loadValue = new DateTime((Date) operand);
						}
						break;
					case decimal10:
						operand = getNumericValueFromCell(colIndex, treatEmptyNumericAsZero);
						if (operand != null) {
							loadValue = new Decimal10(((Double) operand).doubleValue());
						}
						break;
					case decimal2:
						operand = getNumericValueFromCell(colIndex, treatEmptyNumericAsZero);
						if (operand != null) {
							loadValue = new Decimal2(((Double) operand).doubleValue());
						}
						break;
					case decimal5:
						operand = getNumericValueFromCell(colIndex, treatEmptyNumericAsZero);
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
						operand = getStringValueFromCell(colIndex, true);
						if (operand != null) {
							loadValue = operand;
						}
						break;
					case integer:
						operand = getNumericValueFromCell(colIndex, treatEmptyNumericAsZero);
						if (operand != null) {
							loadValue = new Integer(((Double) operand).intValue());
						}
						break;
					case inverseOne:
					case inverseMany:
						// not supported
						break;
					case longInteger:
						operand = getNumericValueFromCell(colIndex, treatEmptyNumericAsZero);
						if (operand != null) {
							loadValue = new Long(((Double) operand).longValue());
						}
						break;
					case markup:
						operand = getStringValueFromCell(colIndex, true);
						if (operand != null) {
							loadValue = operand;
						}
						break;
					case memo:
						operand = getStringValueFromCell(colIndex, true);
						if (operand != null) {
							loadValue = operand;
						}
						break;
					case text:
						operand = getStringValueFromCell(colIndex, true);
						if (operand != null) {
							loadValue = operand;
						}
						break;
					case time:
						operand = getDateValueFromCell(colIndex);
						if (operand != null) {
							loadValue = new TimeOnly((Date) operand);
						}
						break;
					case timestamp:
						operand = getDateValueFromCell(colIndex);
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
								lookupBean(result, field, loadValue);
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
					String operandRawValue = getStringValueFromCell(colIndex, true);
					if (operandRawValue == null) {
						what.append(" A value was expected for '");
						what.append(attr.getDisplayName());
						what.append("' but no value was found.");
					} else {
						// last remaining option - if no previous issue has been identified
						// the default is bad type
						if(attr == null){
							what.append(" The value '");
							what.append(operandRawValue);
							what.append("' is invalid or the wrong type.");
							
						} else if (what.length() == 0 ) {
							what.append(" The value '");
							what.append(operandRawValue);
							what.append("' found for '");
							what.append(attr.getDisplayName());
							what.append("' is invalid or the wrong type.");
						}
					}
					Problem problem = new Problem(what.toString(), getWhere(colIndex).toString());
					exception.addError(problem);
				}
			}

			colIndex++;
		}

		// now perform the query
		if (LoaderActivityType.FIND.equals(activityType) && !qFind.getFilter().isEmpty()) {
			result = qFind.beanResult();
		}

		return result;
	}

	/**
	 * Returns all the beans loaded from the file
	 */
	@Override
	public List<Bean> beanResults() throws Exception {

		while (hasNextData() && !isNoData()) {
			nextData();

			Util.LOGGER.info(debugData());
			Bean result = beanResult();
			if (result != null) {
				PersistentBean pb = (PersistentBean) result;
				Util.LOGGER.info("RESULT " + pb.getBizKey());
				results.add(result);
			}
		}

		// add a warning if nothing was found
		if (results.isEmpty()) {
			what.append("No data has been loaded");
			Problem prob = new Problem(what.toString(), "Sheet " + sheet.getSheetName());
			exception.addWarning(prob);
		}

		return results;
	}

	@Override
	public BizPortException getException() {
		return exception;
	}

	@Override
	public String getStringFieldValue(int fieldIndex, boolean blankAsNull) throws Exception {
		return getStringValueFromCell(fieldIndex, blankAsNull);
	}

	@Override
	public Date getDateFieldValue(int fieldIndex) throws Exception {
		return getDateValueFromCell(fieldIndex);
	}

	@Override
	public Double getNumericFieldValue(int fieldIndex, boolean emptyAsZero) throws Exception {
		return getNumericValueFromCell(fieldIndex, emptyAsZero);
	}

	@Override
	public void nextData() throws Exception {
		// handle first call
		if (rowLoaded) {
			++rowIndex;
		}
		row = sheet.getRow(rowIndex);
		rowLoaded = true;
	}

	@Override
	public boolean hasNextData() throws Exception {
		// POI appears to always find next rows
		return true;
	}

	@Override
	public boolean isNoData() throws Exception {
		return row == null;
	}

	@Override
	public void setFieldIndex(int fieldIndex) {
		colIndex = fieldIndex;
	}

	@Override
	public String getWhere(int fieldIndex) throws Exception {
		StringBuilder where = new StringBuilder(128);
		where.append("Row ").append((rowIndex + 1));
		where.append(" column ").append(getPOIWorksheetColumnName(fieldIndex));
		where.append(".");
		return where.toString();
	}

	@Override
	public String debugData() throws Exception {
		StringBuilder sb = new StringBuilder();
		sb.append("Row ").append(rowIndex);
		if (row != null) {
			for (int fieldIndex = 0; fieldIndex < fields.size(); fieldIndex++) {
				sb.append(", (").append(rowIndex).append(",").append(fieldIndex).append(") = ");
				sb.append(getStringFieldValue(fieldIndex, true));
			}
		} else {
			sb.append(" Null");
		}
		return sb.toString();
	}

	/**
	 * Wrapper to get a numeric value from the spreadsheet cell.
	 * 
	 * @param row
	 * @param col
	 * @return the numeric value
	 */
	private Double getNumericValueFromCell(int col, boolean emptyAsZero) throws Exception {
		Double result = Double.valueOf(0);

		Cell cell = row.getCell(col, Row.RETURN_BLANK_AS_NULL);
		if (cell != null) {
			result = Double.valueOf(cell.getNumericCellValue());
		} else if (emptyAsZero) {
			result = Double.valueOf(0);
		} else {
			throw new Exception("The cell is empty or not a valid number.");
		}

		return result;
	}

	/**
	 * Wrapper to get a string value from the spreadsheet cell.
	 * 
	 * @param row
	 * @param col
	 * @return The String Value of the cell
	 */
	private String getStringValueFromCell(int col, boolean blankAsNull) throws Exception {
		String result = null;

		if (row != null) {
			Cell cell = row.getCell(col, Row.RETURN_BLANK_AS_NULL);
			DataFormatter df = new DataFormatter();

			if (cell != null) {
				// try to interpret whatever we find as a String
				switch (cell.getCellType()) {
				case Cell.CELL_TYPE_BOOLEAN:
				case Cell.CELL_TYPE_NUMERIC:
				case Cell.CELL_TYPE_BLANK:
				case Cell.CELL_TYPE_ERROR:
					result = df.formatCellValue(cell);
					break;
				case Cell.CELL_TYPE_FORMULA:
					switch (cell.getCachedFormulaResultType()) {
					case Cell.CELL_TYPE_NUMERIC:
						if (DateUtil.isCellDateFormatted(cell)) {
							result = cell.getDateCellValue().toString();
						} else {
							result = df.formatCellValue(cell);
						}
						break;
					case Cell.CELL_TYPE_STRING:
					default:
						result = cell.getRichStringCellValue().toString().trim();
						break;
					}
					break;
				case Cell.CELL_TYPE_STRING:
				default:
					result = cell.getStringCellValue().trim();
					break;
				}
			}
		}
		if (result == null) {
			if (!blankAsNull) {
				result = "";
			}
		}

		return result;
	}

	/**
	 * Wrapper to get a date value from the spreadsheet cell.
	 * 
	 * @param row
	 * @param col
	 * @return The Date Value of the cell
	 */
	private Date getDateValueFromCell(int col) throws Exception {
		Date result = null;

		Cell cell = row.getCell(col, Row.RETURN_BLANK_AS_NULL);
		if (cell != null) {
			result = cell.getDateCellValue();
		}

		return result;
	}

	/**
	 * Construct the excel column name from a column number (e.g. 27 = "AA")
	 * 
	 * @param number
	 *            - assumes first column is 0
	 * @return
	 */
	private static String getPOIWorksheetColumnName(int number) {
		final StringBuilder sb = new StringBuilder();

		int num = number;
		while (num >= 0) {
			int numChar = (num % 26) + 65;
			sb.append((char) numChar);
			num = (num / 26) - 1;
		}
		return sb.reverse().toString();
	}

	@Override
	public void addField(DataFileField dff) throws Exception {
		fields.add(dff);
	}

	@Override
	public void setDocumentContext(String moduleName, String documentName) throws Exception {
		this.moduleName = moduleName;
		module = customer.getModule(moduleName);
		this.documentName = documentName;
		document = module.getDocument(customer, documentName);
	}

	@Override
	public void setActivityType(LoaderActivityType activityType) {
		this.activityType = activityType;

	}

}
