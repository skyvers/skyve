package org.skyve.impl.bizport;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

import org.apache.poi.hssf.usermodel.DVConstraint;
import org.apache.poi.hssf.usermodel.HSSFClientAnchor;
import org.apache.poi.hssf.usermodel.HSSFDataValidation;
import org.apache.poi.hssf.usermodel.HSSFPatriarch;
import org.apache.poi.hssf.usermodel.HSSFSheet;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Comment;
import org.apache.poi.ss.usermodel.DataValidation;
import org.apache.poi.ss.usermodel.DataValidationConstraint;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.usermodel.Drawing;
import org.apache.poi.ss.usermodel.Hyperlink;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.ss.util.CellRangeAddressList;
import org.apache.poi.ss.util.CellReference;
import org.apache.poi.ss.util.NumberToTextConverter;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.skyve.bizport.BizPortColumn;
import org.skyve.bizport.BizPortException;
import org.skyve.bizport.BizPortException.Problem;
import org.skyve.bizport.BizPortSheet;
import org.skyve.bizport.SheetKey;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.Enumeration;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.bind.BindUtil;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.module.Module;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.impl.bizport.POISheet;
import org.skyve.impl.bizport.POIWorkbook;
import org.skyve.impl.bizport.StandardGenerator;

/**
 * Sets up a sheet with a number of columns in it.
 * Rows can be added to the sheet and rows are added and moved using a composite key structure.
 * Usually (unless overridden) the key is of the form "module.document.bizId".
 * The sheet has a cursor which represents the current row.
 * 
 * @author msands
 */
public final class POISheet implements BizPortSheet {
	// Rows 0, 1 and 2 hold sheet metadata and the title Row.
	private static final int START_ROW = 3;

	// The name row holds the module, document name or relative sheet binding and (optionally) the collection attribute name of the sheet.
	static final int NAME_ROW = 0;
	// The binding row holds the binding of the sheet columns relative to the sheet document.
	private static final int BINDING_ROW = 1;
	// The title row holds visible column title names.
	private static final int TITLE_ROW = 2;

	// Row 0, Col 0 holds the module name.
	static final int MODULE_COLUMN = 0;
	// Row 0, Col 1 holds the document name.
	static final int DOCUMENT_COLUMN = 1;
	// Row 0, Col 2 optionally holds the collection attribute name, if this is a collection sheet.
	static final int COLLECTION_COLUMN = 2;
	
	// Keeps track of the next validation column to use for range values
	// this number is decremented as domain values are added
	private static int validationColumn = 255;

	// My WorkbookData parent
	private POIWorkbook parent;
	
	// the sheet backing this sheet data
	Sheet sheet;

	// The title of the sheet (as per the tab)
	private String title;
	
	// the drawing patriarch to draw to this sheet.
	// this is used to create tooltips
	private Drawing drawing;
	
	// the index to the next row number to append
	// The next index to use if a column definition is added.
	private int nextRow = START_ROW;
	
	// the current row
	// The row that the cursor is currently on
	private Row currentRow;
	
	// the column information, keyed by binding
	// allows easy lookup of column metadata.
	private LinkedHashMap<String, BizPortColumn> columns = new LinkedHashMap<>();

	// the data index - row key to row number
	// allows easy lookup of data by the sheet key 
	// v(default is bizId for document sheets, ownerId + '#|' + elementId for collection sheets)
	private SortedMap<String, Integer> indices = new TreeMap<>();
		
	/**
	 * New File constructor
	 * This constructor is used when creating a new Excel sheet.
	 */
	public POISheet(String title) {
		this.title = title;
	}

	/**
	 * Existing file constructor.
	 * This constructor is used when creating a sheet in-memory representation 
	 * from an existing Excel file.
	 * 
	 * @param sheet	The excel worksheet.
	 */
	POISheet(Customer customer, POIWorkbook parent, Sheet sheet, BizPortException problems) {
		title = sheet.getSheetName();
		
		// get the columns in the sheet (row 2 is binding, row 3 is titles)
		Row nameRow = sheet.getRow(NAME_ROW);
		Cell moduleCell = nameRow.getCell(MODULE_COLUMN, Row.RETURN_BLANK_AS_NULL);
		Cell documentCell = nameRow.getCell(DOCUMENT_COLUMN, Row.RETURN_BLANK_AS_NULL);
		Cell collectionCell = nameRow.getCell(COLLECTION_COLUMN, Row.RETURN_BLANK_AS_NULL);
		String moduleName = (moduleCell == null) ? null : moduleCell.getStringCellValue();
		String documentName = (documentCell == null) ? null : documentCell.getStringCellValue();
		String collectionBinding = (collectionCell == null) ? null : collectionCell.getStringCellValue();
		Module module = customer.getModule(moduleName);
		Document document = module.getDocument(customer, documentName);
		
		Row bindingRow = sheet.getRow(BINDING_ROW);
		Row titleRow = sheet.getRow(TITLE_ROW);
		for (int i = 0, l = bindingRow.getLastCellNum(); i < l; i++) {
			// don't process rows that aren't bound.
			Cell bindingCell = bindingRow.getCell(i, Row.RETURN_BLANK_AS_NULL);
			if (bindingCell == null) {
				continue;
			}
			String binding = bindingCell.getStringCellValue();
			if (binding == null) {
				continue;
			}
			
			// create column metadata for the current column
			Cell titleCell = titleRow.getCell(i, Row.RETURN_BLANK_AS_NULL);
			Comment cellComment = (titleCell == null) ? null : titleCell.getCellComment();
			BizPortColumn column = new BizPortColumn((titleCell == null) ? binding : titleCell.getStringCellValue(),
														(cellComment == null) ? null : cellComment.getString().getString());
			column.setIndex(i);

			// if we are working with a document sheet, determine if we need to setup the 
			// column's "referencedSheet" property
			if (collectionBinding == null) { // have a document sheet
				if (ChildBean.PARENT_NAME.equals(binding)) { // cater for the parent binding
					Document parentDocument = document.getParentDocument(customer);
					column.setReferencedSheet(new SheetKey(parentDocument.getOwningModuleName(), 
															parentDocument.getName()));
				}
				else { // check if we are dealing with a reference
					TargetMetaData target = Binder.getMetaDataForBinding(customer, module, document, binding);
					Attribute attribute = target.getAttribute();
					if (attribute instanceof Reference) {
						Reference reference = (Reference) attribute;
						Document referenceDocument = module.getDocument(customer, reference.getDocumentName());
						column.setReferencedSheet(new SheetKey(referenceDocument.getOwningModuleName(), 
																referenceDocument.getName()));
					}
				}
			}
			else { // have a collection sheet
				if (StandardGenerator.OWNER_ID.equals(binding)) {
					int lastDotIndex = collectionBinding.lastIndexOf('.');
					if (lastDotIndex < 0) { // simple binding from driving document
						column.setReferencedSheet(new SheetKey(moduleName, documentName));
					}
					else { // compound binding
						Module drivingModule = customer.getModule(moduleName);
						Document drivingDocument = module.getDocument(customer, documentName);
						TargetMetaData target = BindUtil.getMetaDataForBinding(customer, 
																				drivingModule,
																				drivingDocument,
																				collectionBinding);
						Document owningDocument = target.getDocument();
						column.setReferencedSheet(new SheetKey(owningDocument.getOwningModuleName(),
																owningDocument.getName()));
					}
				}
				else { // element ID
					Module drivingModule = customer.getModule(moduleName);
					Document drivingDocument = module.getDocument(customer, documentName);
					TargetMetaData target = BindUtil.getMetaDataForBinding(customer, 
																			drivingModule,
																			drivingDocument,
																			collectionBinding);
					Reference reference = (Reference) target.getAttribute();
					Document referenceDocument = drivingModule.getDocument(customer, reference.getDocumentName());
					column.setReferencedSheet(new SheetKey(referenceDocument.getOwningModuleName(), 
															referenceDocument.getName()));
				}
			}
			addColumn(binding, column);
		}
		
		this.parent = parent;
		this.sheet = sheet;
		
		// index the data in the sheet, using column 0 for document, column 0 and 1 for collections
		currentRow = sheet.getRow(nextRow);
		if (currentRow != null) { // we have data to index
			if (collectionBinding == null) { // have a document sheet
				String id = getValue(Bean.DOCUMENT_ID, AttributeType.text, problems);
				if (id != null) {
					indices.put(id, new Integer(nextRow));
				}
				while (nextRow()) {
					id = getValue(Bean.DOCUMENT_ID, AttributeType.text, problems);
					if (id != null) {
						indices.put(id, new Integer(nextRow));
					}
				}
			}
			else { // have a collection sheet
// TODO key columns should be stored in the spread sheet?
				String ownerId = getValue(StandardGenerator.OWNER_ID, AttributeType.text, problems);
				String elementId = getValue(StandardGenerator.ELEMENT_ID, AttributeType.text, problems);
				if ((ownerId != null) && (elementId != null)) {
					indices.put(ownerId + elementId, new Integer(nextRow));
				}
				while (nextRow()) {
					ownerId = getValue(StandardGenerator.OWNER_ID, AttributeType.text, problems);
					elementId = getValue(StandardGenerator.ELEMENT_ID, AttributeType.text, problems);
					if ((ownerId != null) && (elementId != null)) {
						indices.put(ownerId + elementId, new Integer(nextRow));
					}
				}
			}
		}
		
		// reset
		nextRow = START_ROW - 1;
	}
	
	/**
	 * This method is called by WorkbookData to materialize each sheet within the workbook.
	 * Once a sheet has been materialized, no structural changes to the sheet are allowed.
	 * 
	 * @param key	The key of the sheet (module.document/relative binding.collection)
	 * @param parent	The pure Excel workbook that owns this sheet.
	 * @param sheet	The pure Excel worksheet that backs this sheet in-memoty representation.
	 */
	@SuppressWarnings({"incomplete-switch", "static-access"})
	void materialise(SheetKey key,
						@SuppressWarnings("hiding") POIWorkbook parent,
						@SuppressWarnings("hiding") Sheet sheet) {
		this.parent = parent;
		this.sheet = sheet;
		this.sheet.createFreezePane(0, 3);
		drawing = sheet.createDrawingPatriarch();
		
		// Row 0 - module name in cell 0, document name or binding in cell 1
		// Row 1 - column bindings
		// Row 2 - Column Headings

		Row nameRow = sheet.createRow(NAME_ROW);
		nameRow.setZeroHeight(true);
		Cell cell = nameRow.createCell(MODULE_COLUMN, Cell.CELL_TYPE_STRING);
		cell.setCellValue(key.getModuleName());
		cell = nameRow.createCell(DOCUMENT_COLUMN, Cell.CELL_TYPE_STRING);
		cell.setCellValue(key.getDocumentName());
		cell = nameRow.createCell(COLLECTION_COLUMN, Cell.CELL_TYPE_STRING);
		cell.setCellValue(key.getCollectionBinding());
		
		Row bindingRow = sheet.createRow(BINDING_ROW);
		bindingRow.setZeroHeight(true);
		Row titleRow = sheet.createRow(TITLE_ROW);
		
		// for each column
		int i = 0;
		for (String columnBinding : columns.keySet()) {
			BizPortColumn column = columns.get(columnBinding);
			
			// set the column indices now we are materialising
			column.setIndex(i);
			
			// Put in the column binding
			cell = bindingRow.createCell(i, Cell.CELL_TYPE_STRING);
			cell.setCellValue(columnBinding);
			
			// Put in column title
			cell = titleRow.createCell(i, Cell.CELL_TYPE_STRING);
			cell.setCellValue(column.getTitle());
			
			SheetKey referencedSheetKey = column.getReferencedSheet();
			if (referencedSheetKey != null) {
				POISheet referencedSheet = parent.getSheet(referencedSheetKey);
				if (referencedSheet != null) { // not struck out by exclusions
					Hyperlink link = parent.creationHelper.createHyperlink(Hyperlink.LINK_DOCUMENT);
					link.setAddress("'" + referencedSheet.getTitle() + "'!A3");
					cell.setHyperlink(link);
					cell.setCellStyle(parent.foreignKeyHeadingStyle);

					// TODO Make the bizkey column
					i++;
					Cell titleCell = titleRow.createCell(i, Cell.CELL_TYPE_STRING);
					titleCell.setCellValue(column.getTitle() + " Description");
					createCellComment(titleCell, 
										i,
										"The business description of the related record.  " +
											"This value is NOT uploaded but allows a referential description within the spreadsheet.");
					titleCell.setCellStyle(parent.headingStyle);

					// Apply a template formula to the first data row so the user can 
					// still copy it down if there is no data.
					Row firstDataRow = sheet.getRow(TITLE_ROW + 1);
					if (firstDataRow == null) {
						firstDataRow = sheet.createRow(TITLE_ROW + 1);
					}
					applyLookupFormula(firstDataRow, i - 1, referencedSheet.getTitle());
				}
				else { // struck out by exclusions
					cell.setCellStyle(parent.headingStyle);
				}
			}
			else {
				cell.setCellStyle(parent.headingStyle);
			}
			
			// Set the column comment if applicable
			createCellComment(cell, i, column.getComment());
			
			// Set the range values if necessary
			String[] rangeValues = column.getRangeValues();
			Object minValue = column.getMinValue(); // TODO use the min and max values
			Object maxValue = column.getMaxValue();
			
			if (! parent.ooxmlFormat) { // old school
				if (rangeValues != null) {
					setRangeValues(rangeValues, i);
				}
				else {
					CellRangeAddressList addresslist = new CellRangeAddressList(START_ROW, Integer.MAX_VALUE, i, i);
					switch (column.getType()) {
					case date:
					case dateTime:
					case time:
					case timestamp:
						DVConstraint dateConstraint = DVConstraint.createDateConstraint(DataValidationConstraint.OperatorType.IGNORED, 
																							"01/01/1900", 
																							"31/12/2999", 
																							"d/M/yy");
						HSSFDataValidation dateValidation = new HSSFDataValidation(addresslist, dateConstraint);
						dateValidation.setSuppressDropDownArrow(true);
						dateValidation.setShowErrorBox(true);
						dateValidation.setErrorStyle(DataValidation.ErrorStyle.STOP);
						dateValidation.createErrorBox("Not a valid value", "Please enter a valid data value between .");
						dateValidation.setEmptyCellAllowed(true);
						((HSSFSheet) sheet).addValidationData(dateValidation);
						break;
					case integer:
					case longInteger:
// TODO look at string length and date ValidationTypes
						DVConstraint longConstraint = DVConstraint.createNumericConstraint(DataValidationConstraint.ValidationType.INTEGER,
																							DataValidationConstraint.OperatorType.BETWEEN, 
																							Integer.toString(Integer.MIN_VALUE), 
																							Integer.toString(Integer.MAX_VALUE));
						HSSFDataValidation longValidation = new HSSFDataValidation(addresslist, longConstraint);
						longValidation.setSuppressDropDownArrow(true);
						longValidation.setShowErrorBox(true);
						longValidation.setErrorStyle(DataValidation.ErrorStyle.STOP);
						longValidation.createErrorBox("Not a valid value", "Please enter a valid value.");
						longValidation.setEmptyCellAllowed(true);
						((HSSFSheet) sheet).addValidationData(longValidation);
						break;
					case decimal2:
					case decimal5:
					case decimal10:
						DVConstraint floatConstraint = DVConstraint.createNumericConstraint(DataValidationConstraint.ValidationType.DECIMAL,
																								DataValidationConstraint.OperatorType.BETWEEN, 
																								Integer.toString(Integer.MIN_VALUE), 
																								Integer.toString(Integer.MAX_VALUE));
						HSSFDataValidation floatValidation = new HSSFDataValidation(addresslist, floatConstraint);
						floatValidation.setSuppressDropDownArrow(true);
						floatValidation.setShowErrorBox(true);
						floatValidation.setErrorStyle(DataValidation.ErrorStyle.STOP);
						floatValidation.createErrorBox("Not a valid value", "Please enter a valid value.");
						floatValidation.setEmptyCellAllowed(true);
						((HSSFSheet) sheet).addValidationData(floatValidation);
						break;
					}
				}
			}
			i++;
		}
	}

	private void createCellComment(Cell cell, int cellIndex, String comment) {
		if (comment != null) {
			Comment commentBox = null;
			if (parent.ooxmlFormat) {
				commentBox = ((XSSFSheet) sheet).createComment();
			}
			else {
				HSSFPatriarch patriarch = (HSSFPatriarch) drawing;
				commentBox = patriarch.createComment(new HSSFClientAnchor(0, 0, 0, 0, (short) (cellIndex + 1), 2, (short) (cellIndex + 4), 6));
			}
			commentBox.setAuthor("BizHub");
			commentBox.setString(parent.creationHelper.createRichTextString(comment));
			cell.setCellComment(commentBox);
		}
	}
	
	/**
	 * Set the range values for an entire column within the current sheet.
	 * 
	 * @param rangeValues	The range values to set.
	 * @param columnIndex	The index of the column (starts with 0 index).
	 */
	private void setRangeValues(String[] rangeValues, int columnIndex) {

	    // populate range values
		for (int i = 0, l = rangeValues.length; i < l; i++) {
			Row row = sheet.getRow(i + START_ROW);
			if (row == null) {
				row = sheet.createRow(i + START_ROW);
			}
			row.createCell(validationColumn).setCellValue(rangeValues[i]);
		}

		// Create lookup cell range address expression
		CellRangeAddressList addresslist = new CellRangeAddressList(START_ROW, Integer.MAX_VALUE, columnIndex, columnIndex);
		CellRangeAddress validationListVals = new CellRangeAddress(START_ROW, rangeValues.length + START_ROW, validationColumn, validationColumn);
		StringBuilder sb = new StringBuilder(validationListVals.formatAsString());
		String validationListValsExpression = sb.insert(0, '$').insert(3, '$').insert(6, '$').insert(9, '$').toString();
		
		// Create the constraint
		DVConstraint constraint = DVConstraint.createFormulaListConstraint(validationListValsExpression);

		HSSFDataValidation validation = new HSSFDataValidation(addresslist, constraint);
		validation.setSuppressDropDownArrow(false);
		validation.setShowErrorBox(true);
		validation.setErrorStyle(DataValidation.ErrorStyle.STOP);
		validation.createErrorBox("Pick a valid value", "Please pick a valid value from the drop down");
		validation.setEmptyCellAllowed(true);
		((HSSFSheet) sheet).addValidationData(validation);

		validationColumn--;
	}
	
	@Override
	public BizPortColumn addColumn(String columnBinding, BizPortColumn column) {
		if (sheet != null) {
			throw new IllegalStateException("This workbook data has been materialized");
		}
		return columns.put(columnBinding, column);
	}

	@Override
	public BizPortColumn removeColumn(String columnBinding) {
		if (sheet != null) {
			throw new IllegalStateException("This workbook data has been materialized");
		}
		return columns.remove(columnBinding);
	}
	
	@Override
	public BizPortColumn getColumn(String columnBinding) {
		return columns.get(columnBinding);
	}
	
	@Override
	public Set<String> getColumnBindings() {
		return columns.keySet();
	}
	
	@Override
	public boolean moveToRow(Object... rowKey) {
		if (sheet == null) {
			throw new IllegalStateException("This workbook data has not been materialized");
		}
		
		Integer index = indices.get(buildRowKey(rowKey));
		if (index != null) {
			currentRow = sheet.getRow(index.intValue());
		}
		
		return (index != null);
	}
	
	@Override
	public void addRow(Object... rowKey) {
        if (sheet == null) {
            throw new IllegalStateException("This workbook data has not been materialized");
        }

        // A row may already exist if it has been created to make static domain values
        currentRow = sheet.getRow(nextRow);
        if (currentRow == null) {
            currentRow = sheet.createRow(nextRow);
        }
        nextRow++;
		indices.put(buildRowKey(rowKey), new Integer(nextRow));
	}
	
	/**
	 * Append the key components together to make a string key.
	 * 
	 * @param rowKey	The key components.
	 * @return	The key string.
	 */
	private static String buildRowKey(Object... rowKey) {
		// Build the composite row key
		String result = null;
		if (rowKey.length == 1) {
			result = rowKey[0].toString();
		}
		else {
			StringBuilder rowKeyStringBuilder = new StringBuilder(64);
			for (Object rowKeyComponent : rowKey) {
				rowKeyStringBuilder.append(rowKeyComponent).append("#|");
			}
			result = rowKeyStringBuilder.toString();
		}

		return result;
	}
	
	@Override
	public boolean nextRow() {
		int lastRow = sheet.getLastRowNum();
		currentRow = sheet.getRow(++nextRow);
		while ((currentRow == null) && (nextRow <= lastRow)) {
			currentRow = sheet.getRow(++nextRow);
		}
		
		return (nextRow <= lastRow);
	}
	
	private String getCurrentRowExcelRef(BizPortColumn column) {
		StringBuilder ref = new StringBuilder(64);
		ref.append(getTitle()).append('!');
		if ((currentRow != null) && (column != null)) {
			ref.append(new CellReference(currentRow.getRowNum(), 
											column.getIndex()).formatAsString());
		}
		else {
			ref.append("#REF");
		}

		return ref.toString();
	}
	
	@Override
	public void addErrorAtCurrentRow(BizPortException problems, 
										BizPortColumn column, 
										String message) {
		problems.addError(new Problem(message, getCurrentRowExcelRef(column)));
	}
	
	@Override
	public void addWarningAtCurrentRow(BizPortException problems, 
										BizPortColumn column, 
										String message) {
		problems.addWarning(new Problem(message, getCurrentRowExcelRef(column)));
	}

	@Override
	public void resetRow() {
		nextRow = START_ROW - 1;
		currentRow = null;
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public <T> T getValue(String columnBinding, 
							AttributeType attributeType,
							BizPortException problems) {
		T result = null;
		
		if (sheet == null) {
			throw new IllegalStateException("This workbook data has not been materialized");
		}
		BizPortColumn column = getColumn(columnBinding);
		if (column == null) {
			throw new IllegalArgumentException("Column " + columnBinding + " does not exist.");
		}
		if (currentRow == null) {
			throw new IllegalStateException("No current row.  Use moveToRow() or addRow() or next() first.");
		}
		
		Cell cell = currentRow.getCell(column.getIndex(), Row.RETURN_BLANK_AS_NULL);
		if (cell != null) {
			switch (cell.getCellType()) {
			case Cell.CELL_TYPE_STRING:
				if (AttributeType.text.equals(attributeType) ||
						AttributeType.enumeration.equals(attributeType) ||
						AttributeType.association.equals(attributeType) ||
						AttributeType.colour.equals(attributeType) ||
						AttributeType.content.equals(attributeType) ||
						AttributeType.memo.equals(attributeType) ||
						AttributeType.markup.equals(attributeType)) {
					result = (T) cell.getStringCellValue();
				}
				else {
					addErrorAtCurrentRow(problems,
											column, 
											"Type should be " + attributeType + " but the corresponding Excel cell is a String");
				}
				break;
			case Cell.CELL_TYPE_BOOLEAN:
				if (AttributeType.bool.equals(attributeType)) {
					result = (T) (cell.getBooleanCellValue() ? Boolean.TRUE : Boolean.FALSE);
				}
				else {
					addErrorAtCurrentRow(problems, 
											column, 
											"Type should be " + attributeType + " but the corresponding Excel cell is a Boolean");
				}
				break;
			case Cell.CELL_TYPE_NUMERIC:
				if (DateUtil.isCellDateFormatted(cell)) {
					Date date = cell.getDateCellValue();
					if (date != null) {
						if (AttributeType.date.equals(attributeType)) {
							result = (T) new DateOnly(date.getTime());
						}
						else if (AttributeType.dateTime.equals(attributeType)) {
							result = (T) new DateTime(date.getTime());
						}
						else if (AttributeType.time.equals(attributeType)) {
							result = (T) new TimeOnly(date.getTime());
						}
						else if (AttributeType.timestamp.equals(attributeType)) {
							result = (T) new Timestamp(date.getTime());
						}
						else {
							addErrorAtCurrentRow(problems, 
													column, 
													"Type should be " + attributeType + " but the corresponding Excel cell is a Date.");
						}
					}
				}
				else {
					if (AttributeType.integer.equals(attributeType)) {
						result = (T) new Integer((int) cell.getNumericCellValue());
					}
					else if (AttributeType.longInteger.equals(attributeType)) {
						result = (T) new Long((long) cell.getNumericCellValue());
					}
					else if (AttributeType.decimal2.equals(attributeType)) {
						result = (T) new Decimal2(cell.getNumericCellValue());
					}
					else if (AttributeType.decimal5.equals(attributeType)) {
						result = (T) new Decimal5(cell.getNumericCellValue());
					}
					else if (AttributeType.decimal10.equals(attributeType)) {
						result = (T) new Decimal10(cell.getNumericCellValue());
					}
					else if (AttributeType.text.equals(attributeType) ||
								AttributeType.association.equals(attributeType) ||
								AttributeType.colour.equals(attributeType) ||
								AttributeType.content.equals(attributeType) ||
								AttributeType.memo.equals(attributeType) ||
								AttributeType.markup.equals(attributeType)) {
						result = (T) NumberToTextConverter.toText(cell.getNumericCellValue());
					}
					else {
						addErrorAtCurrentRow(problems, 
												column, 
												"Type should be " + attributeType + " but the corresponding Excel cell is Numeric.");
					}
				}
				break;
			default:
			}
		}
		
		return result; 
	}
	
	@Override
	public void setValue(String columnBinding, Object value) {
		if (sheet == null) {
			throw new IllegalStateException("This workbook data has not been materialized");
		}
		BizPortColumn column = getColumn(columnBinding);
		if (column == null) {
			throw new IllegalArgumentException("Column " + columnBinding + " does not exist.");
		}
		if (currentRow == null) {
			throw new IllegalStateException("No current row.  Use moveToOrAppendRow() first.");
		}
		
		Cell cell = currentRow.getCell(column.getIndex(), Row.RETURN_BLANK_AS_NULL);
		if (cell == null) { // not created yet
			cell = currentRow.createCell(column.getIndex());
		}

		if (value instanceof String) {
			cell.setCellValue((String) value);
		}
		else if (value instanceof Enumeration) {
			cell.setCellValue(((Enumeration) value).toCode());
		}
		else if (value instanceof DateOnly) {
			cell.setCellValue((Date) value);
			cell.setCellStyle(parent.dateStyle);
		}
		else if (value instanceof DateTime) {
			cell.setCellValue((Date) value);
			cell.setCellStyle(parent.dateTimeStyle);
		}
		else if (value instanceof TimeOnly) {
			cell.setCellValue((Date) value);
			cell.setCellStyle(parent.timeStyle);
		}
		else if (value instanceof Timestamp) {
			cell.setCellValue((Date) value);
			cell.setCellStyle(parent.timestampStyle);
		}
		else if (value instanceof Number) {
			cell.setCellValue(((Number) value).doubleValue());
		}
		else if (value instanceof Boolean) {
			cell.setCellValue(((Boolean) value).booleanValue());
		}
		
		// if this column is a foreign key or parent key, setup the description for it
		SheetKey referencedSheetKey = column.getReferencedSheet();
		if (referencedSheetKey != null) {
			POISheet referencedSheet = parent.getSheet(referencedSheetKey);
			if (referencedSheet != null) {
			    applyLookupFormula(currentRow, column.getIndex(), referencedSheet.getTitle());
			}
		}
	}
	
	// Place a formula like =VLOOKUP(Q4, Model!$A$4:$B$65535, 2, FALSE)
	// to enable the bizkey of the reference sheets to be displayed
	private void applyLookupFormula(Row row, int keyColumn, String referencedSheetTitle) {
	    int descriptionColumn = keyColumn + 1;
	    Cell descriptionCell = row.getCell(descriptionColumn, Row.RETURN_BLANK_AS_NULL);
        if (descriptionCell == null) { // not created yet
            descriptionCell = row.createCell(descriptionColumn);
        }

        descriptionCell.setCellType(Cell.CELL_TYPE_FORMULA);
        StringBuilder formula = new StringBuilder(50);
        formula.append("VLOOKUP(");
        formula.append(new CellReference(row.getRowNum(), keyColumn).formatAsString());
        formula.append(", '").append(referencedSheetTitle).append("'!$A$4:$B$65535, 2, FALSE)");
        descriptionCell.setCellFormula(formula.toString());
        descriptionCell.setCellStyle(parent.foreignKeyDescriptionStyle);
	}

	@Override
	public String getTitle() {
		return title;
	}

	@Override
	public void setTitle(String title) {
		if (sheet != null) { // materialised
			int sheetIndex = parent.workbook.getSheetIndex(sheet);
			parent.workbook.setSheetName(sheetIndex, title);
		}
		this.title = title;
	}

	@Override
	public String toString() {
		return title;
	}
}
