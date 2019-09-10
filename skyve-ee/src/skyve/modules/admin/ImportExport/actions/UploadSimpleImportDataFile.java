package modules.admin.ImportExport.actions;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.skyve.CORE;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.UploadException;
import org.skyve.impl.bizport.POISheetLoader;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import modules.admin.ImportExport.ImportExportExtension;
import modules.admin.domain.ImportExport;
import modules.admin.domain.ImportExportColumn;

public class UploadSimpleImportDataFile extends UploadAction<ImportExport> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -8154709480999519405L;

	@Override
	public ImportExport upload(ImportExport importExport,
			UploadedFile file,
			UploadException exception,
			WebContext webContext)
			throws Exception {

		ImportExport bean = importExport;
		
		if (bean.getModuleName() != null && bean.getDocumentName() != null) {

			if(!bean.isPersisted()) {
				bean =CORE.getPersistence().save(bean);
			}
			
			// create the import upload file
			File importFile = new File(String.format("%simportExport_%s%s%s",
					Util.getContentDirectory(),
					CORE.getUser().getCustomerName(),
					File.separator,
					file.getFileName()));

			// and save temporary copy of file
			File import_directory = new File(String.format("%simportExport_%s",
					Util.getContentDirectory(),
					CORE.getUser().getCustomerName()));
						
			if (!import_directory.exists()) {
				import_directory.mkdir();
			}

			//replace file if it exists
			if (importFile.exists()) {
				if (!importFile.delete()) {
					throw new Exception("The previous upload of this file can't be removed.");
				}
			} 
			Files.copy(file.getInputStream(), Paths.get(importFile.getAbsolutePath()));
			bean.setImportFileName(file.getFileName());
			bean.setImportFileAbsolutePath(importFile.getAbsolutePath());				
			
			int i = loadColumnsFromFile(bean, exception);
			
			//construct a result message
			StringBuilder sb = new StringBuilder();
			if(i>0) {
				sb.append("Successfully loaded definitions for ").append(i).append(" column")
						.append(i != 1 ? "s" : "")
						.append(". Configure each column title you wish to import with an appropriate "
								+ "binding then click the Import Data button.");
			} else {
				sb.append("No rows uploaded, try again.");
			}
			bean.setResults(sb.toString());
			webContext.growl(MessageSeverity.info, bean.getResults());

		}
		
		return bean;
	}
	
	public static int loadColumnsFromFile(ImportExport bean, UploadException exception) throws Exception{

		File importFile = new File(bean.getImportFileAbsolutePath());
		User user = CORE.getPersistence().getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(bean.getModuleName());
		Document document = module.getDocument(customer, bean.getDocumentName());
				
		// load the columns from the file
		int i = 0;
		bean.getImportExportColumns().clear();			
		
		try (InputStream poiStream = new FileInputStream(importFile)) {
			POISheetLoader loader = new POISheetLoader(poiStream, 0, bean.getModuleName(), bean.getDocumentName(), exception);
			boolean moreCells = true;
			while (moreCells) {
				
				//load until an empty column is found
				String columnName = loader.getStringFieldValue(i, true);
				if (columnName == null || "".equals(columnName.trim())) {
					moreCells = false;
					break;
				}

				// create a new import export column config row for each column in the spreadsheet
				ImportExportColumn newCol = ImportExportColumn.newInstance();
				newCol.setParent((ImportExportExtension) bean);
				newCol.setBizOrdinal(new Integer(i)); //preserve load order
				bean.getImportExportColumns().add(newCol);
				if (Boolean.TRUE.equals(bean.getFileContainsHeaders())) {
					newCol.setColumnName(columnName);
					
					// and guess a binding
					for (Attribute a : document.getAttributes()) {
						if (a.getDisplayName().toLowerCase().equals(columnName.toLowerCase())) {
							newCol.setBindingName(a.getName());
							break;
						}
					}
				} else {
					newCol.setColumnName(POISheetLoader.getPOIWorksheetColumnName(i));
				}

				i++;
			}
		}
		
		return i;
	}
}
