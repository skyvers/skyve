package modules.admin.ImportExport.actions;

import org.skyve.impl.bizport.POISheetGenerator;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.web.WebContext;

import modules.admin.ImportExportColumn.ImportExportColumnBizlet;
import modules.admin.domain.ImportExport;
import modules.admin.domain.ImportExportColumn;

public class RunExport extends DownloadAction<ImportExport> {
	
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2886341074753936987L;

	@Override
	public Download download(ImportExport bean, WebContext webContext)
			throws Exception {

		POISheetGenerator generator = new POISheetGenerator(bean.getModuleName(), bean.getDocumentName());
		generator.setColumnTitles(bean.getFileContainsHeaders());
		generator.setColumnTitlesOnly(bean.getColumnTitlesOnly());	
		
		//add fields to generator
		for(ImportExportColumn c: bean.getImportExportColumns()) {
			String binding = c.getBindingName();
			if (Boolean.TRUE.equals(bean.getAdvancedMode()) || ImportExportColumnBizlet.ADVANCED.equals(c.getBindingName()) 
					&& c.getBindingExpression()!=null) {
				binding=c.getBindingExpression();				
			}
			
			generator.addField(c.getColumnName(), binding);
		}
		

		//Generate download file name
		StringBuilder sb = new StringBuilder();
		sb.append(bean.getModuleName()).append("_").append(bean.getDocumentName());
		generator.setDownloadName(sb.toString());

		return generator.getDownload();
	}
}
