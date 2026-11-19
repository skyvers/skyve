package org.skyve.impl.bizport;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.List;

import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.skyve.CORE;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Generates styled Excel ({@code .xlsx}) worksheets from a Skyve document's
 * query results using Apache POI.
 */
public class POISheetGenerator {
	private static final String XLSX = ".xlsx";

	private String moduleName;
	private String documentName;
	private Boolean columnTitles;
	private String downloadName;
	private Boolean columnTitlesOnly;

	public @Nullable Boolean getColumnTitlesOnly() {
		return columnTitlesOnly;
	}
	public void setColumnTitlesOnly(@Nullable Boolean columnTitlesOnly) {
		this.columnTitlesOnly = columnTitlesOnly;
	}

	protected List<DataFileExportField> fields; // maintain order

	public @Nullable String getDownloadName() {
		return downloadName;
	}

	public void setDownloadName(@Nonnull String downloadName) {
		// ensure download is correctly named for POI sheet export
		if (downloadName.endsWith(XLSX)) {
			this.downloadName = downloadName;
		} else {
			this.downloadName = downloadName + XLSX;
		}
	}

	public @Nullable String getModuleName() {
		return moduleName;
	}

	public void setModuleName(@Nullable String moduleName) {
		this.moduleName = moduleName;
	}

	public @Nullable String getDocumentName() {
		return documentName;
	}

	public void setDocumentName(@Nullable String documentName) {
		this.documentName = documentName;
	}

	public @Nullable Boolean isColumnTitles() {
		return columnTitles;
	}

	public void setColumnTitles(@Nullable Boolean columnTitles) {
		this.columnTitles = columnTitles;
	}

	public @Nonnull List<DataFileExportField> getFields() {
		return fields;
	}

	public void setFields(@Nonnull List<DataFileExportField> fields) {
		this.fields = fields;
	}

	public POISheetGenerator(@Nullable String moduleName, @Nullable String documentName) {
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
	public void addField(@Nonnull DataFileExportField field) {
		fields.add(field);
	}

	/**
	 * Add a field to the list of expected fields
	 *
	 * @param fieldTitle
	 * @param binding
	 */
	public void addField(@Nullable String fieldTitle, @Nullable String binding) {
		fields.add(new DataFileExportField(fieldTitle, binding));
	}

	@SuppressWarnings("java:S3776") // Complexity OK
	public @Nullable Download getDownload() throws Exception {

		Download result= null;

		if (moduleName != null && documentName != null) {
			// find the workbook template
			try (XSSFWorkbook wb = new XSSFWorkbook()) {

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
						POIWorkbook.putPOICellValue(sheet, rowNum, colNum++, CellType.STRING, f.getFieldTitle(), true);
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
							String bindingExpression = f.getBindingExpression();
							String resolvedBinding = bindingExpression;
							if (resolvedBinding != null && resolvedBinding.startsWith("{") && resolvedBinding.endsWith("}")) {
								resolvedBinding = resolvedBinding.substring(1, resolvedBinding.length() - 1);
							}
	
							if ((bindingExpression != null) && (resolvedBinding != null)) {
								try {
									TargetMetaData tm = Binder.getMetaDataForBinding(customer, module, document, resolvedBinding);
									Object value = null;
									Attribute attr = tm.getAttribute();
									if (attr != null) {
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
											POIWorkbook.putPOICellValue(sheet, rowNum, colNum, CellType.STRING, value);
											break;
										case decimal10:
										case decimal2:
										case decimal5:
										case integer:
										case longInteger:
											value = Binder.get(b, resolvedBinding); //allow excel to interpret from type
											POIWorkbook.putPOICellValue(sheet, rowNum, colNum, CellType.NUMERIC, value);
											break;
										default:
											break;
										}
									}
								} catch (@SuppressWarnings("unused") Exception e) {
//									Util.LOGGER.info("Putting compound expression {} with value {}", f.getBindingExpression(), Binder.formatMessage(customer, f.getBindingExpression(), b));
									POIWorkbook.putPOICellValue(sheet, rowNum, colNum, CellType.STRING, Binder.formatMessage(bindingExpression, b));
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
	
					result = new Download(downloadName, baos.toByteArray() , MimeType.xlsx);
				}
			}
		}

		return result;
	}
}
