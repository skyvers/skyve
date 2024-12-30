package org.skyve.impl.bizport;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.TreeMap;

import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.DataFormatter;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.converters.Converter;

public class POISheetLoader extends AbstractDataFileLoader {

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
	public POISheetLoader(InputStream fileInputStream, int sheetIndex, String moduleName, String documentName, UploadException exception)
			throws Exception {

		super(LoaderActivityType.CREATE_FIND, exception, moduleName, documentName);

		// initialise
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
	public POISheetLoader(LoaderActivityType activityType, InputStream fileInputStream, int sheetIndex, UploadException exception,
			String moduleName, String documentName) throws Exception {

		this(fileInputStream, sheetIndex, moduleName, documentName, exception);
		this.setActivityType(activityType);
	}


	@Override
	public String getStringFieldValue(int index, boolean blankAsNull) {
		return getStringValueFromCell(index, blankAsNull);
	}

	@Override
	public Date getDateFieldValue(int index) {
		return getDateValueFromCell(index);
	}

	@Override
	public Double getNumericFieldValue(int index, boolean emptyAsZero) {
		return getNumericValueFromCell(index, emptyAsZero);
	}

	@Override
	public void nextData() {
		// handle first call
		if (rowLoaded) {
			++dataIndex;
		}
		row = sheet.getRow(dataIndex);
		rowLoaded = true;
	}

	@Override
	public boolean hasNextData() {
		// POI appears to always find next rows
		return true;
	}

	@Override
	public boolean isNoData() {
		if (row == null) {
			return true;
		}

		boolean foundNonEmpty = false;
		for (DataFileField field : fields) {
			if(field.getIndex()==null){
				field.setIndex(Integer.valueOf(0));
			}
			String val = getStringFieldValue(field.getIndex().intValue(), true);
			if (val != null && val.trim().length() > 0) {
				foundNonEmpty = true;
				break;
			} else if(debugMode){
				LOGGER.info(getWhere(field.getIndex().intValue()) + " No value was found at this location.");
			}
		}

		return !foundNonEmpty;
	}

	@Override
	public String getWhere(int index) {
		StringBuilder where = new StringBuilder(128);
		where.append("Sheet ").append(sheet.getSheetName());
		where.append(" Row ").append((dataIndex + 1));
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
	private Double getNumericValueFromCell(int col, boolean emptyAsZero) {
		Double result = Double.valueOf(0);

		Cell cell = row.getCell(col, Row.MissingCellPolicy.RETURN_BLANK_AS_NULL);
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
		} catch (@SuppressWarnings("unused") Exception de) {
			// get a debug value
			String raw = "";
			try {
				raw = getStringFieldValue(col, true);
			} catch (@SuppressWarnings("unused") Exception e) {
				// do nothing
			}
			throw new DomainException("The " + getPOICellTypeDescription(cell) + " value '" + raw + "' is not numeric.");
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
	private String getStringValueFromCell(int col, boolean blankAsNull) {
		String result = null;

		if (row != null) {
			Cell cell = row.getCell(col, Row.MissingCellPolicy.RETURN_BLANK_AS_NULL);
			DataFormatter df = new DataFormatter();

			if (cell != null) {
				// try to interpret whatever we find as a String
				switch (cell.getCellType()) {
				case BOOLEAN:
				case NUMERIC:
				case BLANK:
				case ERROR:
					result = df.formatCellValue(cell);
					break;
				case FORMULA:
					switch (cell.getCachedFormulaResultType()) {
					case NUMERIC:
						if (DateUtil.isCellDateFormatted(cell)) {
							result = cell.getDateCellValue().toString();
						} else {
							result = df.formatCellValue(cell);
						}
						break;
					case STRING:
					default:
						result = cell.getRichStringCellValue().toString().trim();
						break;
					}
					break;
				case STRING:
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
	private Date getDateValueFromCell(int col) {
		Date result = null;
		Cell cell = row.getCell(col, Row.MissingCellPolicy.RETURN_BLANK_AS_NULL);
		try {
			if (cell != null) {
				result = cell.getDateCellValue();
			}
		} catch (@SuppressWarnings("unused") Exception de) {
			StringBuilder problem = new StringBuilder(128);
			Converter<DateTime> converter = CORE.getPersistence().getUser().getCustomer().getDefaultDateTimeConverter();
			// get a debug value
			String raw = "";
			try {
				//try again using the default converter
				raw = getStringFieldValue(col, true);
				result = converter.fromDisplayValue(raw);
			} catch (@SuppressWarnings("unused") Exception e) {
				// do nothing
				problem.append("The value '").append(raw).append("' is not a valid date");
				if(converter!=null){
					problem.append(" (using customer default Converter " + converter.getClass().getSimpleName() + ").");
				}
				throw new DomainException(problem.toString());
			}
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
	public static String getPOIWorksheetColumnName(int number) {
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
	private static String getPOICellTypeDescription(Cell cell) {
		String result = "unknown";
		if (cell != null) {
			switch (cell.getCellType()) {
			case BOOLEAN:
				result = "Boolean";
				break;
			case NUMERIC:
				result = "Numeric";
				break;
			case BLANK:
				result = "Blank";
				break;
			case ERROR:
				result = "Error";
				break;
			case FORMULA:
				result = "Formula";
				switch (cell.getCachedFormulaResultType()) {
				case NUMERIC:
					result = "Numeric "+ result;
					break;
				case STRING:
					result= "String " + result;
					break;
				default:
					result = "Rich Text " + result;
					break;
				}
				break;
			case STRING:
				result = "String";
				break;
			default:
				result = "unknown";
				break;
			}
		}
		return result;
	}
}
