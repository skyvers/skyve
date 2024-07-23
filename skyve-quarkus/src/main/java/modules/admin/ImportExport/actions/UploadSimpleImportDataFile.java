package modules.admin.ImportExport.actions;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.UploadException;
import org.skyve.impl.bizport.POISheetLoader;
import org.skyve.metadata.controller.Upload;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.web.WebContext;

import modules.admin.ImportExport.ImportExportExtension;
import modules.admin.domain.ImportExportColumn;

public class UploadSimpleImportDataFile extends UploadAction<ImportExportExtension> {
	@Override
	public ImportExportExtension upload(ImportExportExtension importExport,
											Upload upload,
											UploadException exception,
											WebContext webContext)
	throws Exception {

		ImportExportExtension bean = importExport;

		if (bean.getModuleName() != null && bean.getDocumentName() != null) {

			// remove any previous import file
			bean.cleanupImportFile();

			try {
				// create the import upload file
				File importFile = new File(String.format("%s%s%s",
						bean.baseFolder(),
						File.separator,
						upload.getFileName()));

				// and save temporary copy of file
				File import_directory = new File(bean.baseFolder());

				if (!import_directory.exists()) {
					import_directory.mkdirs();
				}

				// replace file if it exists
				if (importFile.exists()) {
					if (!importFile.delete()) {
						throw new Exception("The previous upload of this file can't be removed.");
					}
				}
				try (InputStream is = upload.getInputStream()) {
					Files.copy(is, Paths.get(importFile.getAbsolutePath()));
				}
				bean.setImportFileName(upload.getFileName());
				bean.setImportFileAbsolutePath(importFile.getAbsolutePath());

				bean = loadColumnsFromFile(bean, exception);
				int i = bean.getImportExportColumns().size();

				// construct a result message
				StringBuilder sb = new StringBuilder();
				if (i > 0) {
					sb.append("Successfully loaded definitions for ").append(i).append(" column")
							.append(i != 1 ? "s" : "")
							.append(". Configure each column title you wish to import with an appropriate "
									+ "binding then click the `Import data from file` button.");
				} else {
					sb.append("No rows uploaded, try again.");
				}
				bean.setResults(sb.toString());
				webContext.growl(MessageSeverity.info, bean.getResults());
			} catch (Exception e) {
				// clean up any uploaded file
				e.printStackTrace();
				bean.cleanupImportFile();
			}
		}

		// bean must be saved to enable import and export actions
		bean = CORE.getPersistence().save(bean);

		return bean;
	}

	public static ImportExportExtension loadColumnsFromFile(ImportExportExtension bean, UploadException exception) throws Exception {

		// clear previous columns
		bean.getImportExportColumns().clear();

		Persistence pers = CORE.getPersistence();
		File importFile = new File(bean.getImportFileAbsolutePath());
		User user = pers.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(bean.getModuleName());
		Document document = module.getDocument(customer, bean.getDocumentName());

		// load the columns from the file
		int i = 0;
		try (InputStream poiStream = new FileInputStream(importFile)) {
			POISheetLoader loader = new POISheetLoader(poiStream, 0, bean.getModuleName(), bean.getDocumentName(), exception);
			boolean moreCells = true;
			while (moreCells) {

				// load until an empty column is found
				String columnName = loader.getStringFieldValue(i, true);
				if (columnName == null || "".equals(columnName.trim())) {
					moreCells = false;
					break;
				}

				// create a new import export column config row for each column in the spreadsheet
				ImportExportColumn newCol = ImportExportColumn.newInstance();
				newCol.setParent(bean);
				newCol.setBizOrdinal(Integer.valueOf(i)); // preserve load order
				bean.getImportExportColumns().add(newCol);
				if (Boolean.TRUE.equals(bean.getFileContainsHeaders())) {
					newCol.setColumnName(columnName);

					// and guess a binding
					// prefer a like match on Display Name
					boolean bindingFound = false;
					List<? extends Attribute> attributes = document.getAllAttributes(customer);
					for (Attribute a : attributes) {
						if (a.getLocalisedDisplayName().equalsIgnoreCase(columnName)) {
							newCol.setBindingName(a.getName());
							bindingFound = true;
							break;
						}
					}
					if (!bindingFound) {
						// attempt a close match on binding name
						for (Attribute a : attributes) {
							String cleanColName = Binder.toJavaInstanceIdentifier(columnName);
							if (a.getName().equalsIgnoreCase(cleanColName)) {
								newCol.setBindingName(a.getName());
								bindingFound = true;
								break;
							}
						}
					}
					if (!bindingFound) {
						// attempt a close match on description
						for (Attribute a : attributes) {
							String localisedDescription = a.getLocalisedDescription();
							if ((localisedDescription) != null && localisedDescription.equalsIgnoreCase(columnName)) {
								newCol.setBindingName(a.getName());
								bindingFound = true;
								break;
							}
						}
					}
				} else {
					newCol.setColumnName(POISheetLoader.getPOIWorksheetColumnName(i));
				}

				i++;
			}
		}
		return bean;
	}
}
