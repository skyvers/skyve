package org.skyve.impl.bizport;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.CreationHelper;
import org.apache.poi.ss.usermodel.DataFormat;
import org.apache.poi.ss.usermodel.FillPatternType;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFFont;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.skyve.bizport.BizPortColumn;
import org.skyve.bizport.BizPortSheet;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.bizport.SheetKey;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.types.DateOnly;
import org.skyve.metadata.customer.Customer;

/**
 * Adapts an Excel workbook.
 * This class collects SheetData (adaption of Excel sheets).
 * The materialize() is used to populate data in the spreadsheet, for a newly created WorkbookData.
 * The write() will put the Excel format (xls or xlsx) onto the output stream given.
 *
 * @author mike
 */
public final class POIWorkbook implements BizPortWorkbook {
	// The adapted Excel workbook
	Workbook workbook;

	// whether we are creating or reading an xls or an xlsx
	boolean ooxmlFormat;

	// Factory for creating things at the workbook level
	CreationHelper creationHelper;

	// Factory for formats
	private DataFormat format;

	// different styles in use in the sheets
	CellStyle headingStyle;
	CellStyle foreignKeyHeadingStyle;
	CellStyle foreignKeyDescriptionStyle;
	CellStyle dateStyle;
	CellStyle timeStyle;
	CellStyle dateTimeStyle;
	CellStyle timestampStyle;

	// Appended to keep sheet titles unique, if required
	private short sheetNumber = 0;

	// Document Name, or collection binding -> sheet data
	private Map<SheetKey, POISheet> sheets = new LinkedHashMap<>();

	/**
	 * New file constructor.
	 */
	public POIWorkbook(boolean ooxmlFormat) {
		this.ooxmlFormat = ooxmlFormat;
	}

	/**
	 * Existing file constructor.
	 *
	 * @param customer	The current customer (for the logged in user).
	 * @param workbook	The workbook.
	 */
	public POIWorkbook(Customer customer, Workbook workbook, UploadException e) {
		this.workbook = workbook;
		ooxmlFormat = (workbook instanceof XSSFWorkbook);

		setupWorkbookInfrastructure();

		for (int i = 0, l = workbook.getNumberOfSheets(); i < l; i++) {
			Sheet sheet = workbook.getSheetAt(i);
			if (workbook.isSheetHidden(i) || workbook.isSheetVeryHidden(i)) {
				continue;
			}
			POISheet sheetData = new POISheet(customer, this, sheet, e);

			Row row = sheet.getRow(POISheet.NAME_ROW);
			Cell moduleCell = row.getCell(POISheet.MODULE_COLUMN, Row.MissingCellPolicy.RETURN_BLANK_AS_NULL);
			String moduleName = (moduleCell == null) ? null : moduleCell.getStringCellValue();
			Cell documentCell = row.getCell(POISheet.DOCUMENT_COLUMN, Row.MissingCellPolicy.RETURN_BLANK_AS_NULL);
			String documentName = documentCell == null ? null : documentCell.getStringCellValue();
			Cell collectionCell = row.getCell(POISheet.COLLECTION_COLUMN, Row.MissingCellPolicy.RETURN_BLANK_AS_NULL);
			String binding = collectionCell == null ? null : collectionCell.getStringCellValue();

			SheetKey key = new SheetKey(moduleName, documentName, binding);

			sheets.put(key, sheetData);
		}
	}

	@Override
	public POISheet getSheet(SheetKey key) {
		return sheets.get(key);
	}

	@Override
	public void addSheet(SheetKey key, BizPortSheet sheet) {
		if (workbook != null) {
			throw new IllegalStateException("Workbook has already been materialized");
		}

		if (sheets.put(key, (POISheet) sheet) != null) {
			throw new IllegalArgumentException("Cannot add a second sheet called " + key);
		}
	}

	@Override
	public POISheet removeSheet(SheetKey key) {
		if (workbook != null) {
			throw new IllegalStateException("Workbook has already been materialized");
		}

		return sheets.remove(key);
	}

	@Override
	public Set<SheetKey> getSheetKeys() {
		return sheets.keySet();
	}

	@Override
	public void materialise() {
		Workbook newWorkbook = ooxmlFormat ? new XSSFWorkbook() : new HSSFWorkbook();
		workbook = newWorkbook;

		setupWorkbookInfrastructure();

		// create the sheets in the workbook in 1 iteration
		for (SheetKey key : sheets.keySet()) {
			BizPortSheet sheetData = sheets.get(key);
			String title = generateValidUniqueSheetTitle(sheetData.getTitle());
			sheetData.setTitle(title);
			workbook.createSheet(title);
		}

		// materialise the SheetData in another iteration
		for (SheetKey key : sheets.keySet()) {
			POISheet sheetData = sheets.get(key);
			sheetData.materialise(key, this, workbook.getSheet(sheetData.getTitle()));
		}
	}

	private void setupWorkbookInfrastructure() {
		creationHelper = workbook.getCreationHelper();
		format = workbook.createDataFormat();

		// Create heading style
	    Font headingFont = workbook.createFont();
	    headingFont.setFontHeightInPoints((short) 14);
	    headingFont.setFontName("Arial");
	    headingFont.setBold(true);
		headingStyle = workbook.createCellStyle();
		headingStyle.setFont(headingFont);

		// Create foreign key heading style
		Font foreignKeyHeadingFont = workbook.createFont();
	    foreignKeyHeadingFont.setFontHeightInPoints((short) 14);
	    foreignKeyHeadingFont.setFontName("Arial");
		headingFont.setBold(true);
	    foreignKeyHeadingFont.setUnderline(Font.U_SINGLE);
	    foreignKeyHeadingFont.setColor(IndexedColors.BLUE.getIndex());
		foreignKeyHeadingStyle = workbook.createCellStyle();
		foreignKeyHeadingStyle.setFont(foreignKeyHeadingFont);
		foreignKeyDescriptionStyle = workbook.createCellStyle();
		foreignKeyDescriptionStyle.setLocked(true);
//		foreignKeyDescriptionStyle.setFillBackgroundColor(IndexedColors.YELLOW.getIndex());
		foreignKeyDescriptionStyle.setFillForegroundColor(IndexedColors.LIGHT_YELLOW.getIndex());
		foreignKeyDescriptionStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);

		dateStyle = workbook.createCellStyle();
		dateStyle.setDataFormat(format.getFormat("m/d/yy"));
		timeStyle = workbook.createCellStyle();
		timeStyle.setDataFormat(format.getFormat("h:mm"));
		dateTimeStyle = workbook.createCellStyle();
		dateTimeStyle.setDataFormat(format.getFormat("m/d/yy h:mm"));
		timestampStyle = workbook.createCellStyle();
		timestampStyle.setDataFormat(format.getFormat("m/d/yy h:mm"));
	}

	@Override
	public void write(OutputStream out) throws IOException {
		if (workbook == null) {
			throw new IllegalStateException("Workbook has not been materialized");
		}

		for (POISheet sheet : sheets.values()) {
			// Set the column widths automatically for each sheet
			int i = 0;
			for (String columnBinding : sheet.getColumnBindings()) {
				BizPortColumn column = sheet.getColumn(columnBinding);
				if (column.getReferencedSheet() != null) {
					sheet.sheet.autoSizeColumn(i);
					i++;
				}
				sheet.sheet.autoSizeColumn(i);
				i++;
			}
		}

		workbook.write(out);
	}

	@Override
	public BizPortFormat getFormat() {
		return ooxmlFormat ? BizPortFormat.xlsx : BizPortFormat.xls;
	}

	private String generateValidUniqueSheetTitle(String proposedSheetTitle) {
		String result = proposedSheetTitle;
		if (proposedSheetTitle.length() > 29) {
			result = proposedSheetTitle.substring(0, 29);
		}
		if (workbook.getSheet(result) != null) { // already in use
			result += sheetNumber;
			sheetNumber++;
		}

		return result.replace('/', '|');
	}

	/**
	 * Put value into POI Cell
	 *
	 * @param sheet
	 * @param rowNum
	 * @param colNum
	 * @param cellType
	 * @param value
	 * @throws Exception
	 */
	public static void putPOICellValue(XSSFSheet sheet, int rowNum, int colNum, CellType cellType, Object value, boolean forceNumericNullToZero, boolean bold) throws Exception {
		@SuppressWarnings("resource")
		XSSFWorkbook workbook = sheet.getWorkbook();

		//microsoft counts from 1 not 0 - so offset rows and columns by 1 to be consistent when inspecting the resulting sheet
		XSSFCellStyle style = workbook.createCellStyle();
		style.setBorderTop(BorderStyle.DOUBLE); // double lines border
		style.setBorderBottom(BorderStyle.THIN); // single line border

		XSSFFont font = workbook.createFont();
		font.setFontHeightInPoints((short) 15);
		font.setBold(true);
		style.setFont(font);

		XSSFRow row = sheet.getRow(rowNum-1);
		if(row==null){
			row = sheet.createRow(rowNum-1);
		}
		XSSFCell cell = row.getCell(colNum-1);
		if (cell == null){
			cell = row.createCell(colNum-1);
		}

//		Util.LOGGER.info("VALUE for " + rowNum + ", " + colNum + "=" + value);

		if (value != null) {
//			Util.LOGGER.info("VALUE for " + rowNum + ", " + colNum + " IS NOT NULL");
			cell.setCellType(cellType);
			if(bold){
				cell.setCellStyle(style);
			}

			switch (cellType) {
			case STRING:
				cell.setCellValue((String) value);
				break;
			case BOOLEAN:
				break;
			case BLANK:
				break;
			case NUMERIC:
			default:
				if (value instanceof Date) {
					CellStyle cellStyle = workbook.createCellStyle();
					CreationHelper createHelper = workbook.getCreationHelper();
					if(value instanceof DateOnly) {
						cellStyle.setDataFormat(createHelper.createDataFormat().getFormat("m/d/yyyy"));
					} else {
						cellStyle.setDataFormat(createHelper.createDataFormat().getFormat("m/d/yyyy h:mm"));
					}
					cell.setCellValue((Date) value);
					cell.setCellStyle(cellStyle);

				} else if (value instanceof Number) {
					cell.setCellValue(((Number) value).doubleValue());
				}
				break;
			}
		} else {
			// empty value
//			Util.LOGGER.info("VALUE for " + rowNum + ", " + colNum + " IS NULL with cellType " + (Cell.CELL_TYPE_NUMERIC == cellType ));
			if(CellType.NUMERIC == cellType && forceNumericNullToZero){
				cell.setCellValue(0.0);
			}
			else {
				cell.setCellType(CellType.BLANK);
			}
		}
	}
	public static void putPOICellValue(XSSFSheet sheet, int rowNum, int colNum, CellType cellType, Object value) throws Exception {
		putPOICellValue(sheet, rowNum, colNum, cellType, value, false, false);
	}

	public static void putPOICellValue(XSSFSheet sheet, int rowNum, int colNum, CellType cellType, String value, boolean bold) throws Exception {
		putPOICellValue(sheet, rowNum, colNum, cellType, value, false, bold);
	}

}
