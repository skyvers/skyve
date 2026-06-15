package modules.admin.ImportExport.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.ImportExport.ImportExportExtension;
import modules.admin.ImportExport.ImportExportService;
import modules.admin.domain.ImportExport;

/**
 * Regenerates import/export column definitions from the selected document.
 */
public class ResetColumns implements ServerSideAction<ImportExportExtension> {
	@Inject
	@SuppressWarnings("java:S6813") // allow member injection
	private transient ImportExportService importExportService;

	/**
	 * Rebuilds columns using document-name change semantics.
	 *
	 * @param bean
	 *        the import/export bean
	 * @param webContext
	 *        the current web context
	 * @return a result wrapping {@code bean}
	 * @throws Exception
	 *         if column regeneration fails
	 */
	@Override
	public ServerSideActionResult<ImportExportExtension> execute(ImportExportExtension bean, WebContext webContext)
			throws Exception {
		importExportService.updateColumns(ImportExport.documentNamePropertyName, bean);

		return new ServerSideActionResult<>(bean);
	}
}
