package org.skyve.impl.bizport;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
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

public class POISheetLoader extends AbstractDataFileLoader {

	private StringBuilder what;

	private Workbook workbook;
	private Sheet sheet;
	private Row row;

	private boolean rowLoaded = false;

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

		super(LoaderActivityType.CREATE_FIND, exception, moduleName, documentName);

		// initialise
		this.exception = exception;
		this.createdBeans = new TreeMap<>();

		workbook = WorkbookFactory.create(fileInputStream);
		sheet = workbook.getSheetAt(sheetIndex);
		what = new StringBuilder();

		// set values
		this.pers = CORE.getPersistence();
		this.user = pers.getUser();
		this.customer = user.getCustomer();
		setDocumentContext(moduleName, documentName);

		// defaults
		this.createMissingAssociations = false;
		this.treatAllEmptyNumericAsZero = false;
		this.dataIndex = 0;
		this.fieldIndex = 0;
		this.row = sheet.getRow(dataIndex);

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

		addFields(bindings);
	}

	/**
	 * Returns all the beans loaded from the file
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <T extends Bean> List<T> beanResults() throws Exception {

		while (hasNextData()) {
			nextData();
			if (isNoData()) {
				break;
			}
			Bean result = beanResult();
			if (result != null) {
				results.add(result);
			}

		}

		// add a warning if nothing was found
		if (results.isEmpty()) {
			what.append("No data has been loaded");
			Problem prob = new Problem(what.toString(), "Sheet " + sheet.getSheetName());
			exception.addWarning(prob);
		}

		return (List<T>) results;
	}

	@Override
	public String getStringFieldValue(int index, boolean blankAsNull) throws Exception {
		return getStringValueFromCell(index, blankAsNull);
	}

	@Override
	public Date getDateFieldValue(int index) throws Exception {
		return getDateValueFromCell(index);
	}

	@Override
	public Double getNumericFieldValue(int index, boolean emptyAsZero) throws Exception {
		return getNumericValueFromCell(index, emptyAsZero);
	}

	@Override
	public void nextData() throws Exception {
		// handle first call
		if (rowLoaded) {
			++dataIndex;
		}
		row = sheet.getRow(dataIndex);
		rowLoaded = true;
	}

	@Override
	public boolean hasNextData() throws Exception {
		// POI appears to always find next rows
		return true;
	}

	@Override
	public boolean isNoData() throws Exception {
		if (row == null) {
			return true;
		}
		
		boolean foundNonEmpty = false;
		for (DataFileField field : fields) {
			String val = getStringFieldValue(field.getIndex(), true);
			if (val != null && val.trim().length() > 0) {
				foundNonEmpty = true;
				break;
			}
		}

		return !foundNonEmpty;
	}

	@Override
	public String getWhere(int index) throws Exception {
		StringBuilder where = new StringBuilder(128);
		where.append("Row ").append((dataIndex + 1));
		where.append(" column ").append(getPOIWorksheetColumnName(index));
		where.append(".");
		return where.toString();
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
		try {
			if (cell != null) {
				//handle empty string more robustly
				String raw = getStringValueFromCell(col, true);
				if(raw==null || raw.trim().length()==0){
					if (emptyAsZero) {
						result = Double.valueOf(0);
					} else {
						result = null;
					}
				} else {
					result = Double.valueOf(cell.getNumericCellValue());
				}
			} else if (emptyAsZero) {
				result = Double.valueOf(0);
			} else {
				result = null;
			}
		} catch (Exception de) {
			// get a debug value
			String raw = "";
			try {
				raw = getStringFieldValue(col, true);
			} catch (Exception e) {
				// do nothing
			}
			throw new Exception("The " + getPOICellTypeDescription(cell) + " value '" + raw + "' is not numeric.");
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
		try {
			if (cell != null) {
				result = cell.getDateCellValue();
			}
		} catch (Exception de) {
			// get a debug value
			String raw = "";
			try {
				raw = getStringFieldValue(col, true);
			} catch (Exception e) {
				// do nothing
			}
			throw new Exception("The " + getPOICellTypeDescription(cell) + " value '" + raw + "' is not a valid date.");
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
	
	/**
	 * Gets a description of the type of value for the cell
	 * 
	 * @param cell
	 * @return
	 */
	private static String getPOICellTypeDescription(Cell cell){
		String result = "unknown";
		switch (cell.getCellType()) {
		case Cell.CELL_TYPE_BOOLEAN:
			result = "Boolean";
			break;
		case Cell.CELL_TYPE_NUMERIC:
			result = "Numeric";
			break;
		case Cell.CELL_TYPE_BLANK:
			result = "Blank";
			break;
		case Cell.CELL_TYPE_ERROR:
			result = "Error";
			break;
		case Cell.CELL_TYPE_FORMULA:
			result = "Formula";
			switch (cell.getCachedFormulaResultType()) {
			case Cell.CELL_TYPE_NUMERIC:
				result = "Numeric "+ result;
				break;
			case Cell.CELL_TYPE_STRING:
				result= "String " + result;
				break;
			default:
				result = "Rich Text " + result;
				break;
			}
			break;
		case Cell.CELL_TYPE_STRING:
			result = "String";
			break;
		default:
			result = "unknown";
			break;
		}
		return result;
	}
}
