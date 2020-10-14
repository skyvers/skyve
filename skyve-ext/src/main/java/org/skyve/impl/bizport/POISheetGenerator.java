package org.skyve.impl.bizport;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.List;

import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.skyve.CORE;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.metadata.controller.DownloadAction.Download;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;

public class POISheetGenerator {

	private static final String XLSX = ".xlsx";

	private String moduleName;
	private String documentName;
	private Boolean columnTitles;
	private String downloadName;
	private String absoluteFilePath;
	private Boolean columnTitlesOnly;

	public Boolean getColumnTitlesOnly() {
		return columnTitlesOnly;
	}
	public void setColumnTitlesOnly(Boolean columnTitlesOnly) {
		this.columnTitlesOnly = columnTitlesOnly;
	}

	protected List<DataFileExportField> fields; // maintain order

	public String getAbsoluteFilePath() {
		return absoluteFilePath;
	}

	public String getDownloadName() {
		return downloadName;
	}

	public void setDownloadName(String downloadName) {
		// ensure download is correctly named for POI sheet export
		if (downloadName.endsWith(XLSX)) {
			this.downloadName = downloadName;
		} else {
			this.downloadName = downloadName + XLSX;
		}
	}

	public String getModuleName() {
		return moduleName;
	}

	public void setModuleName(String moduleName) {
		this.moduleName = moduleName;
	}

	public String getDocumentName() {
		return documentName;
	}

	public void setDocumentName(String documentName) {
		this.documentName = documentName;
	}

	public Boolean isColumnTitles() {
		return columnTitles;
	}

	public void setColumnTitles(Boolean columnTitles) {
		this.columnTitles = columnTitles;
	}

	public List<DataFileExportField> getFields() {
		return fields;
	}

	public void setFields(List<DataFileExportField> fields) {
		this.fields = fields;
	}

	public POISheetGenerator(String moduleName, String documentName) {
		this.moduleName = moduleName;
		this.documentName = documentName;
		this.fields = new ArrayList<>();
	}

	/**
	 * Add a field to the list of expected fields
	 * 
	 * @param dff
	 * @throws Exception
	 */
	public void addField(DataFileExportField field) throws Exception {
		fields.add(field);
	}

	/**
	 * Add a field to the list of expected fields
	 * 
	 * @param fieldTitle
	 * @param binding
	 */
	public void addField(String fieldTitle, String binding) throws Exception {
		fields.add(new DataFileExportField(fieldTitle, binding));
	}

	public Download getDownload() throws Exception {
		
		Download result= null;

		if (moduleName != null && documentName != null) {
			// find the workbook template
			XSSFWorkbook wb = new XSSFWorkbook();

			// common values
			XSSFSheet sheet = wb.createSheet();
			Persistence pers = CORE.getPersistence();
			Customer customer = pers.getUser().getCustomer();
			Module module = customer.getModule(moduleName);
			Document document = module.getDocument(customer, documentName);

			int rowNum = 1;
			int colNum = 1;

			if (!Boolean.FALSE.equals(columnTitles)) {
				// export column titles
				for (DataFileExportField f : fields) {
					POIWorkbook.putPOICellValue(sheet, rowNum, colNum++, Cell.CELL_TYPE_STRING, f.getFieldTitle(), true);
				}
			}

			if (!Boolean.TRUE.equals(columnTitlesOnly)) {
				
				// export values
				DocumentQuery q = pers.newDocumentQuery(moduleName, documentName);
				List<Bean> beans = q.beanResults();

				for (Bean b : beans) {
					rowNum++;
					colNum = 1;
					
					for (DataFileExportField f : fields) {

						// attempt to find attribute with the binding
						// if not assume it is a compound expression and use binder.formatMessage
						String resolvedBinding = f.getBindingExpression();
						if (resolvedBinding != null && resolvedBinding.startsWith("{") && resolvedBinding.endsWith("}")) {
							resolvedBinding = f.getBindingExpression().substring(1, f.getBindingExpression().length() - 1);
						}
						
						if (resolvedBinding != null) {
							try {
								TargetMetaData tm = Binder.getMetaDataForBinding(customer, module, document, resolvedBinding);
								Attribute attr = tm.getAttribute();
								Object value = null;
								switch (attr.getAttributeType()) {
								case association:
								case bool:
								case colour:
								case date:
								case dateTime:
								case enumeration:
								case geometry:
								case id:
								case markup:
								case memo:
								case text:
								case time:
								case timestamp:
									value = Binder.formatMessage(String.format("{%s}", resolvedBinding), b);
									POIWorkbook.putPOICellValue(sheet, rowNum, colNum, Cell.CELL_TYPE_STRING, value);
									break;
								case decimal10:
								case decimal2:
								case decimal5:
								case integer:
								case longInteger:
									value = Binder.get(b, resolvedBinding); //allow excel to interpret from type
									POIWorkbook.putPOICellValue(sheet, rowNum, colNum, Cell.CELL_TYPE_NUMERIC, value);
									break;
								default:
									break;
								}
								
							} catch (@SuppressWarnings("unused") Exception e) {
//								Util.LOGGER.info("Putting compound expression " + f.getBindingExpression() + " with value " + Binder.formatMessage(customer, f.getBindingExpression(), b));
								POIWorkbook.putPOICellValue(sheet, rowNum, colNum, Cell.CELL_TYPE_STRING, Binder.formatMessage(f.getBindingExpression(), b));
							}
						}

						colNum++;
					}
				}
			}
			
			
			//autosize columns
			for(int i=0;i<colNum;i++) {
				sheet.autoSizeColumn(i);
			}

			//construct the Download
			try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
				wb.write(baos); // write changes
				
				ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
				result = new Download(downloadName, bais , MimeType.xlsx);
			}
		}
		
		return result;
	}
}
