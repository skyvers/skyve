package modules.admin.ImportExport.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.ImportExport.ImportExportExtension;

/**
 * Clears previously uploaded import files from an import/export record.
 */
public class ClearImport implements ServerSideAction<ImportExportExtension> {
	/**
	 * Removes uploaded import file state and returns the updated bean.
	 *
	 * @param bean
	 *        the import/export bean
	 * @param webContext
	 *        the current web context
	 * @return a result wrapping {@code bean}
	 * @throws Exception
	 *         if cleanup fails
	 */
	@Override
	public ServerSideActionResult<ImportExportExtension> execute(ImportExportExtension bean, WebContext webContext)
			throws Exception {
		bean.cleanupImportFile();

		return new ServerSideActionResult<>(bean);
	}
}
