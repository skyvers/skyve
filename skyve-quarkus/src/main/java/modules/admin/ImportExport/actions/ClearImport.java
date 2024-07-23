package modules.admin.ImportExport.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.ImportExport.ImportExportExtension;

public class ClearImport implements ServerSideAction<ImportExportExtension> {

	@Override
	public ServerSideActionResult<ImportExportExtension> execute(ImportExportExtension bean, WebContext webContext)
			throws Exception {

		bean.cleanupImportFile();

		return new ServerSideActionResult<>(bean);
	}
}
