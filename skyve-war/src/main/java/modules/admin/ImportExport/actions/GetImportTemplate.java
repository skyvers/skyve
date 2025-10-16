package modules.admin.ImportExport.actions;

import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.ImportExport.ImportExportExtension;
import modules.admin.ImportExport.ImportExportService;

public class GetImportTemplate extends DownloadAction<ImportExportExtension> {
	@Inject
	private transient ImportExportService importExportService;
	
	@Override
	public void prepare(ImportExportExtension bean, WebContext webContext) throws Exception {
		// Nothing to see here
	}

	@Override
	public Download download(ImportExportExtension bean, WebContext webContext)
			throws Exception {
		return RunExport.generateDownload(bean, importExportService.generateColumns(bean), Boolean.TRUE, Boolean.TRUE);
	}
}
