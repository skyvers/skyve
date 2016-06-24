package org.skyve.impl.bizport;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.DataFormatter;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.usermodel.Row;
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

public class LoadBeanFromRow {

	public static enum ValueLookupType {
		LOOKUP_EXACT, LOOKUP_LIKE, LOOKUP_CONTAINS, CONFIRM_MATCH
	}

	/**
	 * Column definition and how to load it
	 */
	public static class ColumnLoadDefinition {
		private String binding;
		private boolean required = false;
		private ValueLookupType loadAction = ValueLookupType.LOOKUP_EXACT;
		
		//treat numeric as 0
		private boolean treatEmptyNumericAsZero = false;

		public boolean isTreatEmptyNumericAsZero() {
			return treatEmptyNumericAsZero;
		}

		public void setTreatEmptyNumericAsZero(boolean treatEmptyNumericAsZero) {
			this.treatEmptyNumericAsZero = treatEmptyNumericAsZero;
		}

		public String getBinding() {
			return binding;
		}

		public ValueLookupType getLoadAction() {
			return loadAction;
		}

		public boolean isRequired() {
			return required;
		}

		public ColumnLoadDefinition(String binding, ValueLookupType loadAction, boolean required) {
			this.binding = binding;
			this.loadAction = loadAction;
			this.required = required;
		}

		public ColumnLoadDefinition(String binding) {
			this.binding = binding;
		}

	}


	/**
	 * Simple call with just bindings
	 * 
	 * @param row
	 * @param pers
	 * @param module
	 * @param document
	 * @param createMissingAssociations
	 * @param createdBeans - a cache of created referenced beans (so that created references can be reused)
	 * @param rowIndex
	 * @param bindings
	 * @return
	 * @throws Exception
	 */
	public static Bean loadRowDataToBean(Row row, Persistence pers, Module module, Document document,
			boolean createMissingAssociations, Map<String, Bean> createdBeans, boolean treatEmptyNumericAsZero, int rowIndex, BizPortException exception, String... bindings)
			throws Exception {

		List<ColumnLoadDefinition> loadDefinitions = new ArrayList<>();
		for (String binding : bindings) {
			ColumnLoadDefinition loadDef = new ColumnLoadDefinition(binding);
			loadDefinitions.add(loadDef);
		}
		ColumnLoadDefinition[] cols = loadDefinitions.toArray(new ColumnLoadDefinition[loadDefinitions.size()]);

		return loadRowDataToBean(row, pers, module, document, createMissingAssociations, createdBeans, treatEmptyNumericAsZero, rowIndex, exception, cols);
	}

	/**
	 * Load a row of values from a poi worksheet to beans, by attributename
	 * 
	 * Attempts to load into a new bean, values from the poi worksheet using the bindings supplied By default, if a compound binding is
	 * provided, loadRowDataToBean will attempt to lookup the immediate reference using the rest of the compound binding, and use that bean.
	 * 
	 * For example, if loading customers with a binding of company.contact.name loadRowDataToBean will attempt to find a company with a
	 * matching value of contact.name and set customer.company to a company bean that has contact.name with the macthing value.
	 * 
	 * @param row
	 * @param pers
	 * @param user
	 * @param document
	 * @param createMissingAssociations
	 * @param createdBeans - a cache of created referenced beans (so that created references can be reused)
	 * @param treatEmptyNumericAsZero
	 * @param rowIndex
	 * @param bindings
	 * @return
	 * @throws Exception
	 */
	public static Bean loadRowDataToBean(Row row, Persistence pers, Module module, Document document,
			boolean createMissingAssociations, Map<String, Bean> createdBeans, boolean treatEmptyNumericAsZero, int rowIndex, BizPortException exception, ColumnLoadDefinition[] cols)
			throws Exception {

		// assume no values loaded
		Bean result = document.newInstance(pers.getUser());
		User user = pers.getUser();
		Customer customer = user.getCustomer();

		int colIndex = 0;
		Object operand = null;
		String internalExceptionDescription = null;

		for (ColumnLoadDefinition col : cols) {

			// general try - if value is not of the expected type or empty,
			// throw an exception skip null attributes
			String binding = col.getBinding();
			if (binding != null) {

				TargetMetaData tm = Binder.getMetaDataForBinding(customer, module, document, binding);
				Attribute attr = tm.getAttribute();

				//TODO - don't switch on attribute because bizKey etc is not an attribute - it's a property
				
				// special case attribute is an association - go to bizkey
				if (AttributeType.association.equals(attr.getAttributeType())) {
					tm = Binder.getMetaDataForBinding(customer, module, document,
							Binder.createCompoundBinding(binding, Bean.BIZ_KEY));
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
						operand = getDateValueFromCell(row, colIndex);
						if (operand != null) {
							loadValue = new DateOnly((Date) operand);
						}
						break;
					case dateTime:
						operand = getDateValueFromCell(row, colIndex);
						if (operand != null) {
							loadValue = new DateTime((Date) operand);
						}
						break;
					case decimal10:
						operand = getNumericValueFromCell(row, colIndex, treatEmptyNumericAsZero || col.treatEmptyNumericAsZero);
						if (operand != null) {
							loadValue = new Decimal10(((Double) operand).doubleValue());
						}
						break;
					case decimal2:
						operand = getNumericValueFromCell(row, colIndex, treatEmptyNumericAsZero || col.treatEmptyNumericAsZero);
						if (operand != null) {
							loadValue = new Decimal2(((Double) operand).doubleValue());
						}
						break;
					case decimal5:
						operand = getNumericValueFromCell(row, colIndex, treatEmptyNumericAsZero || col.treatEmptyNumericAsZero);
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
						operand = getStringValueFromCell(row, colIndex, true);
						if (operand != null) {
							loadValue = operand;
						}
						break;
					case integer:
						operand = getNumericValueFromCell(row, colIndex, treatEmptyNumericAsZero || col.treatEmptyNumericAsZero);
						if (operand != null) {
							loadValue = new Integer(((Double) operand).intValue());
						}
						break;
					case inverseOne:
					case inverseMany:
						// not supported
						break;
					case longInteger:
						operand = getNumericValueFromCell(row, colIndex, treatEmptyNumericAsZero || col.treatEmptyNumericAsZero);
						if (operand != null) {
							loadValue = new Long(((Double) operand).longValue());
						}
						break;
					case markup:
						operand = getStringValueFromCell(row, colIndex, true);
						if (operand != null) {
							loadValue = operand;
						}
						break;
					case memo:
						operand = getStringValueFromCell(row, colIndex, true);
						if (operand != null) {
							loadValue = operand;
						}
						break;
					case text:
						operand = getStringValueFromCell(row, colIndex, true);
						if (operand != null) {
							loadValue = operand;
						}
						break;
					case time:
						operand = getDateValueFromCell(row, colIndex);
						if (operand != null) {
							loadValue = new TimeOnly((Date) operand);
						}
						break;
					case timestamp:
						operand = getDateValueFromCell(row, colIndex);
						if (operand != null) {
							loadValue = new Timestamp((Date) operand);
						}
						break;
					default:
						break;

					}

					// handle the operand - by default - just attempt to set the value in the binding
					if (col.isRequired() && loadValue == null) {
						StringBuilder sb = new StringBuilder();
						sb.append(" A value is required for '");
						sb.append(attr.getDisplayName());
						sb.append("' but no value was found.");
						internalExceptionDescription = sb.toString();
						throw new Exception(internalExceptionDescription);
					}

					if (document.equals(tm.getDocument())) {
						Binder.set(result, binding, loadValue);
					} else {
						if (loadValue != null) {
							// default action - look for equals value if attribute document is different to starting
							// document if a compound binding is supplied, we need to
							// find if there is any top-level association which matches
							// e.g. if customer.company.contact.name is supplied,
							// we need to find if there is a customer with company.contact.name that matches

							// the rest - e.g company.contact.name
							String restBinding = binding.substring(binding.indexOf('.') + 1);

							// the bit to search - e.g. we are searching company
							String searchBinding = binding.substring(0, binding.indexOf('.'));

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
							switch (col.loadAction) {
							case LOOKUP_EXACT:
							case CONFIRM_MATCH:
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
								if (ValueLookupType.CONFIRM_MATCH.equals(col.loadAction) && result != null) {
									// check if the found bean matches the bean we have already found
									Object resultValue = Binder.get(result, searchBinding);
									if (!foundBean.equals(resultValue)) {
										// throw an error
										StringBuilder sb = new StringBuilder();
										sb.append("The value '").append(loadValue).append("'");
										sb.append(" doesn't match the existing value of '").append(resultValue).append("'.");

										internalExceptionDescription = sb.toString();
										throw new Exception(internalExceptionDescription);
									}
								}
								Binder.set(result, searchBinding, foundBean);
							} else if (createMissingAssociations) {
								//first check the creationCache to establish if this bean has already been created
								StringBuilder mapReference = new StringBuilder(128);
								mapReference.append(binding).append(',').append((String) loadValue);

								//check cache
								boolean foundInCache = false;
								if(createdBeans!=null){
									if(createdBeans.containsKey(mapReference.toString())){
										Bean previouslyCreatedBean = createdBeans.get(mapReference.toString());
										if(previouslyCreatedBean!=null){
											
											//reuse the same bean
											Binder.set(result, searchBinding, previouslyCreatedBean);
											foundInCache= true;
										}
									} 
								} 
								
								// Binder populateProperty creates intermediate beans as required
								if(!foundInCache){
									Binder.populateProperty(user, result, binding, loadValue, false);
									if(createdBeans==null){
										createdBeans = new TreeMap<>();
									}
									createdBeans.put(mapReference.toString(), (Bean) Binder.get(result,searchBinding));
								}
							} else {
								// throw an error
								StringBuilder sb = new StringBuilder();
								sb.append("The ").append(drivingDoc.getSingularAlias());
								sb.append(" '").append(loadValue.toString()).append("'");
								sb.append(" doesn't match any existing ").append(drivingDoc.getPluralAlias()).append(".");

								internalExceptionDescription = sb.toString();
								throw new Exception(internalExceptionDescription);
							}
						}
					}

				} catch (Exception e) {

					// construct exception message using display names
					StringBuilder where = new StringBuilder(128);
					where.append("Row ").append((rowIndex + 1));
					where.append(" column ").append(getExcelColumnName(colIndex + 1));
					where.append(".");
					// show raw string value
					String operandRawValue = getStringValueFromCell(row, colIndex, true);
					StringBuilder what = new StringBuilder(128);
					if (operandRawValue == null) {
						what.append(" A value was expected for '");
						what.append(attr.getDisplayName());
						what.append("' but no value was found.");
					} else {
						if (internalExceptionDescription != null) {
							what.append(internalExceptionDescription);
						} else {
							what.append(" The value '");
							what.append(operandRawValue);
							what.append("' found for '");
							what.append(attr.getDisplayName());
							what.append("' is invalid or the wrong type.");
						}
					}
					Problem problem = new Problem(what.toString(), where.toString());					
					exception.addError(problem);
				}
			}

			colIndex++;
		}

		return result;
	}

	/**
	 * Wrapper to get a numeric value from the spreadsheet cell.
	 * 
	 * @param row
	 * @param col
	 * @return the numeric value
	 */
	// TODO is there a use in returning 0 if there is no value?
	public static Double getNumericValueFromCell(Row row, int col, boolean treatEmptyNumericAsZero) throws Exception {
		Double result = Double.valueOf(0);

		Cell cell = row.getCell(col, Row.RETURN_BLANK_AS_NULL);
		if (cell != null) {
			result = Double.valueOf(cell.getNumericCellValue());
		} else if(treatEmptyNumericAsZero){
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
	// TODO is there any use in return "" if the cell is empty?
	public static String getStringValueFromCell(Row row, int col, boolean blankAsNull) throws Exception {
		String result = null;

		Cell cell = row.getCell(col, Row.RETURN_BLANK_AS_NULL);
		DataFormatter df = new DataFormatter();
		
		if (cell != null) {
			//try to interpret whatever we find as a String
			//TODO - need to review how this works in all circumstances
			switch(cell.getCellType()){
			case Cell.CELL_TYPE_BOOLEAN:
			case Cell.CELL_TYPE_NUMERIC:
			case Cell.CELL_TYPE_BLANK:
			case Cell.CELL_TYPE_ERROR:
				result = df.formatCellValue(cell);
				break;
			case Cell.CELL_TYPE_FORMULA:
				switch(cell.getCachedFormulaResultType()){
				case Cell.CELL_TYPE_STRING:	
					result = cell.getRichStringCellValue().toString().trim();
					break;
				case Cell.CELL_TYPE_NUMERIC:
					if(DateUtil.isCellDateFormatted(cell)){
						result = cell.getDateCellValue().toString();
					} else {
						result = df.formatCellValue(cell);
					}
					break;
				}
				break;
			case Cell.CELL_TYPE_STRING:
				result = cell.getStringCellValue().trim();
				break;
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
	public static Date getDateValueFromCell(Row row, int col) throws Exception {
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
	 * @return
	 */
	public static String getExcelColumnName(int number) {
		final StringBuilder sb = new StringBuilder();

		int num = number - 1;
		while (num >= 0) {
			int numChar = (num % 26) + 65;
			sb.append((char) numChar);
			num = (num / 26) - 1;
		}
		return sb.reverse().toString();
	}

}
