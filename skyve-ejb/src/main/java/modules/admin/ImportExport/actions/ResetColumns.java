package modules.admin.ImportExport.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.ImportExport.ImportExportBizlet;
import modules.admin.ImportExport.ImportExportExtension;
import modules.admin.domain.ImportExport;

public class ResetColumns implements ServerSideAction<ImportExportExtension> {

	@Override
	public ServerSideActionResult<ImportExportExtension> execute(ImportExportExtension bean, WebContext webContext)
			throws Exception {

		ImportExportBizlet.updateColumns(ImportExport.documentNamePropertyName, bean);

		return new ServerSideActionResult<>(bean);
	}
}
