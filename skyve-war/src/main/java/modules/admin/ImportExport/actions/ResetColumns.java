package modules.admin.ImportExport.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.ImportExport.ImportExportExtension;
import modules.admin.ImportExport.ImportExportService;
import modules.admin.domain.ImportExport;

public class ResetColumns implements ServerSideAction<ImportExportExtension> {
	@Inject
	private transient ImportExportService importExportService;

	@Override
	public ServerSideActionResult<ImportExportExtension> execute(ImportExportExtension bean, WebContext webContext)
			throws Exception {

		importExportService.updateColumns(ImportExport.documentNamePropertyName, bean);

		return new ServerSideActionResult<>(bean);
	}
}
