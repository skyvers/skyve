package modules.admin.ImportExport.actions;

import java.util.List;

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

		return generateDownload(bean, bean.getImportExportColumns() , bean.getFileContainsHeaders(), Boolean.FALSE);
	}
	
	public static Download generateDownload(ImportExport bean, List<ImportExportColumn> columns, Boolean containsHeaders, Boolean empty) throws Exception {
		POISheetGenerator generator = new POISheetGenerator(bean.getModuleName(), bean.getDocumentName());
		generator.setColumnTitles(containsHeaders);
		generator.setColumnTitlesOnly(empty);	
		
		//add fields to generator
		for(ImportExportColumn c: columns) {
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
