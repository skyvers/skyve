package org.skyve.impl.bizport;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;

import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.skyve.CORE;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateTime;
import org.skyve.metadata.controller.DownloadAction.Download;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.Util;

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

	public void generateFile() throws Exception {

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
					POIWorkbook.putPOICellValue(sheet, rowNum, colNum++, Cell.CELL_TYPE_STRING, f.getFieldTitle());
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
//						Util.LOGGER.info("Preparing export of " + f.getBindingExpression());

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

//								Util.LOGGER.info("Putting resolved Binding " + resolvedBinding + " with type " + attr.getAttributeType().toString() + " and value " + value);
								switch (attr.getAttributeType()) {
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
									value = Binder.formatMessage(customer, "{" + resolvedBinding + "}", b);
									POIWorkbook.putPOICellValue(sheet, rowNum, colNum, Cell.CELL_TYPE_STRING, value);
									break;
								case decimal10:
								case decimal2:
								case decimal5:
								case integer:
								case longInteger:
								case time:
								case timestamp:
									value = Binder.get(b, resolvedBinding); //allow excel to interpret from type
									POIWorkbook.putPOICellValue(sheet, rowNum, colNum, Cell.CELL_TYPE_NUMERIC, value);
									break;
								default:
									break;
								}
								
							} catch (Exception e) {
//								Util.LOGGER.info("Putting compound expression " + f.getBindingExpression() + " with value " + Binder.formatMessage(customer, f.getBindingExpression(), b));
								POIWorkbook.putPOICellValue(sheet, rowNum, colNum, Cell.CELL_TYPE_STRING, Binder.formatMessage(customer, f.getBindingExpression(), b));
							}
						}

						colNum++;
					}
				}
			}

			// save temp file to content folder
			DateFormat df = new SimpleDateFormat("yyyyMMddhhmm");
			absoluteFilePath = String.format("%sdownload_%s", Util.getContentDirectory(), df.format(new DateTime()) + XLSX);

			try (FileOutputStream output_file = new FileOutputStream(absoluteFilePath)) {// Open FileOutputStream to write updates
				wb.write(output_file); // write changes
				output_file.close(); // close the stream
			}
		}
	}

	@SuppressWarnings("resource")
	public Download getDownload() throws Exception {

		generateFile();

		File download = new File(absoluteFilePath);
		if (!download.exists()) {
			Util.LOGGER.warning("Download " + absoluteFilePath + " DNE");
			throw new ValidationException(new Message("Download creation failed. File no longer exists"));
		}

		Download result = new Download(downloadName, new FileInputStream(download), MimeType.xlsx);
		return result;
	}
}
