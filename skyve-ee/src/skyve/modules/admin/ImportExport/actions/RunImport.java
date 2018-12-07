package modules.admin.ImportExport.actions;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.UploadException;
import org.skyve.impl.bizport.POISheetLoader;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.Binder;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import modules.admin.ImportExportColumn.ImportExportColumnBizlet;
import modules.admin.domain.ImportExport;
import modules.admin.domain.ImportExportColumn;

public class RunImport implements ServerSideAction<ImportExport> {

	private static final long serialVersionUID = 7301976416286938546L;

	@Override
	public ServerSideActionResult<ImportExport> execute(ImportExport bean, WebContext webContext)
			throws Exception {

		if (bean.getImportFileAbsolutePath() != null) {

			File importFile = new File(bean.getImportFileAbsolutePath());
			UploadException exception = new UploadException();

			int i = 0;
			try (InputStream poiStream = new FileInputStream(importFile)) {

				POISheetLoader loader = new POISheetLoader(poiStream, 0, bean.getModuleName(), bean.getDocumentName(), exception);
				loader.setDebugMode(true);

				if (Boolean.TRUE.equals(bean.getFileContainsHeaders())) {
					loader.setDataIndex(1);
				}

				// prepare debug message
				StringBuilder sb = new StringBuilder(64);
				sb.append("Bean ");

				// and field bindings to loader
				for (ImportExportColumn col : bean.getImportExportColumns()) {
					sb.append(" ");
					if (Boolean.TRUE.equals(bean.getAdvancedMode()) || ImportExportColumnBizlet.ADVANCED.equals(col.getBindingName())) {
						if (col.getBindingExpression() != null) {
//							Util.LOGGER.info("adding " + col.getBindingExpression());
							loader.addField(col.getBindingExpression());

							// prepare debug
							sb.append(col.getBindingExpression()).append("=`").append(col.getBindingExpression()).append("`");
						} else {
							loader.addField((String) null);
						}
					} else {
						if (col.getBindingName() != null) {
//							Util.LOGGER.info("adding " + col.getBindingName());
							loader.addField(col.getBindingName());

							// prepare debug
							sb.append(col.getBindingName()).append("=`{").append(col.getBindingName()).append("}`");
						} else {
							loader.addField((String) null);
						}
					}

				}

				// get results from sheet
				List<PersistentBean> beans = loader.beanResults();

				// save them
				for (PersistentBean b : beans) {
					if (loader.isDebugMode()) {
						Util.LOGGER.info(Binder.formatMessage(CORE.getCustomer(), sb.toString(), b));
					}
					b = CORE.getPersistence().save(b);
					i++;
				}
			}

			// construct result message
			StringBuilder sb = new StringBuilder();
			if (i > 0) {
				sb.append("Successfully created ").append(i).append(" records");
			} else {
				sb.append("Import unsuccessful. Try again.");
			}
			bean.setResults(sb.toString());
			webContext.growl(MessageSeverity.info, sb.toString());
		}

		return new ServerSideActionResult<>(bean);
	}
}
