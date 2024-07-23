package modules.admin.ImportExport.actions;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;

import org.skyve.CORE;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.OptimisticLockException;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.bizport.AbstractDataFileLoader.LoaderActivityType;
import org.skyve.impl.bizport.DataFileField;
import org.skyve.impl.bizport.DataFileField.LoadAction;
import org.skyve.impl.bizport.POISheetLoader;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import modules.admin.ImportExportColumn.ImportExportColumnBizlet;
import modules.admin.domain.ImportExport;
import modules.admin.domain.ImportExport.LoadType;
import modules.admin.domain.ImportExport.RollbackErrors;
import modules.admin.domain.ImportExportColumn;

public class RunImport implements ServerSideAction<ImportExport> {

	@Override
	public ServerSideActionResult<ImportExport> execute(ImportExport bean, WebContext webContext)
			throws Exception {

		if (bean.getImportFileAbsolutePath() != null) {
			
			File importFile = new File(bean.getImportFileAbsolutePath());
			UploadException exception = new UploadException();

			int loadedRows = 0;
			int created = 0;

			Persistence persistence = CORE.getPersistence();
			Customer customer = CORE.getCustomer();
			Module module = customer.getModule(bean.getModuleName());
			Document document = module.getDocument(customer, bean.getDocumentName());

			try (InputStream poiStream = new FileInputStream(importFile)) {

				POISheetLoader loader = new POISheetLoader(poiStream, 0, bean.getModuleName(), bean.getDocumentName(), exception);
				loader.setDebugMode(Boolean.TRUE.equals(bean.getDetailedLogging()));
				if (LoadType.createAll.equals(bean.getLoadType())) {
					loader.setActivityType(LoaderActivityType.CREATE_ALL);
				} else {
					loader.setActivityType(LoaderActivityType.CREATE_FIND);
				}

				// include headers
				if (Boolean.TRUE.equals(bean.getFileContainsHeaders())) {
					loader.setDataIndex(1);

					// verify columns match
					int i = 0;
					boolean moreCells = true;
					while (moreCells) {
						// load until an empty column is found
						String columnName = loader.getStringFieldValue(i, true);
						if (columnName == null || "".equals(columnName.trim()) || i > bean.getImportExportColumns().size()) {
							moreCells = false;
							break;
						}

						// strip any line breaks or tabs in the column name
						columnName = columnName.replace("\n", "").replace("\t", "");

						if (!columnName.equals(bean.getImportExportColumns().get(i).getColumnName())) {
							StringBuilder sb = new StringBuilder();
							sb.append("The column title ").append(bean.getImportExportColumns().get(i).getColumnName());
							sb.append(" doesn't match the title of the column in the file (").append(columnName).append(")");
							throw new ValidationException(new Message(Binder.createIndexedBinding(ImportExport.importExportColumnsPropertyName, i), sb.toString()));
						}
						i++;
					}

				}

				// and field bindings to loader
				for (ImportExportColumn col : bean.getImportExportColumns()) {

					String resolvedBinding = col.getBindingName();
					if (ImportExportColumnBizlet.EXPRESSION.equals(col.getBindingName())) {
						if (col.getBindingExpression() != null) {
							if (col.getBindingExpression().indexOf("{") > -1) {
								resolvedBinding = col.getBindingExpression().substring(col.getBindingExpression().indexOf("{") + 1, col.getBindingExpression().lastIndexOf("}"));
							} else {
								resolvedBinding = col.getBindingExpression();
							}
						} else {
							StringBuilder msg = new StringBuilder();
							msg.append("You selected '").append(ImportExportColumnBizlet.EXPRESSION).append("' for column ").append(col.getColumnName());
							msg.append(" but have not provided a binding expression.");
							throw new ValidationException(new Message(msg.toString()));
						}
					}
					StringBuilder sb = new StringBuilder();
					sb.append("Adding field with binding ").append(resolvedBinding);

					// add field to loader configuration
					DataFileField f = new DataFileField(resolvedBinding);
					f.setLoadAction(null); // default behaviour
					if (col.getLoadAction() != null) {
						switch (col.getLoadAction()) {
						case confirmValue:
							f.setLoadAction(LoadAction.CONFIRM_VALUE);
							break;
						case lookupContains:
							f.setLoadAction(LoadAction.LOOKUP_CONTAINS);
							break;
						case lookupEquals:
							f.setLoadAction(LoadAction.LOOKUP_EQUALS);
							break;
						case lookupLike:
							f.setLoadAction(LoadAction.LOOKUP_LIKE);
							break;
						case setValue:
							f.setLoadAction(LoadAction.SET_VALUE);
							break;
						default:
							break;
						}
						sb.append(" using load action ").append(col.getLoadAction().toLocalisedDescription());
					}

					if (loader.isDebugMode()) {
						Util.LOGGER.info(sb.toString());
					}
					loader.addField(f);
					if (loader.isDebugMode()) {
						Util.LOGGER.info("Field added at position " + f.getIndex().toString());
					}
				}

				// save uploaded rows
				while (loader.hasNextData()) {
					loader.nextData();

					// stop at empty row
					if (loader.isNoData()) {
						Util.LOGGER.info("End of import found at " + loader.getWhere());
						break;
					}

					PersistentBean b = loader.beanResult();

					if (loader.isDebugMode()) {
						if (b == null) {
							Util.LOGGER.info("Loaded failed at " + loader.getWhere());
						} else {
							Util.LOGGER.info(b.getBizKey() + " - Loaded successfully");
						}
					}
					try {
						if (b != null && (b.getBizKey() == null || b.getBizKey().trim().length() == 0)) {
							String msg = "The new record has no value for bizKey at row " + created + ".";
							ValidationException ve = new ValidationException(new Message(msg));
							throw ve;
						}

						b = persistence.save(b);
						if (loader.isDebugMode()) {
							Util.LOGGER.info(b.getBizKey() + " - Saved successfully");
						}
						persistence.evictCached(b);

						// commit and start a new transaction if selected
						if (RollbackErrors.noRollbackErrors.equals(bean.getRollbackErrors())) {
							persistence.commit(false);
							persistence.begin();
						}
						created++;

					} catch (ValidationException ve) {
						ve.printStackTrace();
						StringBuilder msg = new StringBuilder();
						msg.append("The import succeeded but the imported record could not be saved because imported values were not valid:");
						msg.append("\nCheck upload values and try again.");
						msg.append("\n");
						for (Message m : ve.getMessages()) {
							msg.append("\n").append(m.getText());
						}

						throw new ValidationException(new Message(msg.toString()));
					} catch (OptimisticLockException ole) {
						ole.printStackTrace();
						StringBuilder msg = new StringBuilder();
						msg.append("The import succeeded but the save failed.");
						msg.append(
								"\nCheck that you don't have duplicates in your file, or multiple rows in your file are finding the same related record, or that other users are not changing related data.");
						throw new ValidationException(new Message(msg.toString()));
					} catch (Exception e) {
						e.printStackTrace();
						StringBuilder msg = new StringBuilder();
						msg.append("The import succeeded but saving the records failed.");
						msg.append("\nCheck that you are uploading to the correct binding and that you have supplied enough information for the results to be saved.");
						throw new ValidationException(new Message(msg.toString()));
					}
					loadedRows++;
				}
			}

			// construct result message
			StringBuilder sb = new StringBuilder();
			if (loadedRows > 0) {
				sb.append("Successfully loaded ").append(loadedRows).append(" rows. ");
				sb.append(created).append(' ').append(document.getLocalisedPluralAlias()).append(" created.");
			} else {
				sb.append("Import unsuccessful. Try again.");
			}
			bean.setResults(sb.toString());
			webContext.growl(MessageSeverity.info, sb.toString());
		}

		return new ServerSideActionResult<>(bean);
	}
}
