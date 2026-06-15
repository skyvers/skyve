package modules.admin.ImportExport.actions;

import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.ImportExport.ImportExportExtension;
import modules.admin.ImportExport.ImportExportService;

/**
 * Produces an import-template spreadsheet download for a selected document.
 */
public class GetImportTemplate extends DownloadAction<ImportExportExtension> {
	@Inject
	@SuppressWarnings("java:S6813") // allow member injection
	private transient ImportExportService importExportService;

	/**
	 * Performs no pre-download validation for template generation.
	 *
	 * @param bean
	 *        the import/export bean
	 * @param webContext
	 *        the current web context
	 * @throws Exception
	 *         if preparation fails
	 */
	@Override
	public void prepare(ImportExportExtension bean, WebContext webContext) throws Exception {
		// Nothing to see here
	}

	/**
	 * Generates a download containing only template headers/columns.
	 *
	 * @param bean
	 *        the import/export bean
	 * @param webContext
	 *        the current web context
	 * @return the generated import-template download
	 * @throws Exception
	 *         if generation fails
	 */
	@Override
	public Download download(ImportExportExtension bean, WebContext webContext)
			throws Exception {
		return RunExport.generateDownload(bean, importExportService.generateColumns(bean), Boolean.TRUE, Boolean.TRUE);
	}
}
