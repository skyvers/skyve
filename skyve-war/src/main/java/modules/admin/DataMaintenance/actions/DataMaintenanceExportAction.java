package modules.admin.DataMaintenance.actions;

import org.skyve.EXT;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.metadata.controller.BizExportAction;
import org.skyve.web.WebContext;

import modules.admin.domain.DataMaintenance;

/**
 * Exports Data Maintenance results to a BizPort workbook.
 */
public class DataMaintenanceExportAction extends BizExportAction {
	/**
	 * Performs the bizExport operation.
	 * @param webContext the webContext value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	@Override
	public BizPortWorkbook bizExport(WebContext webContext) throws Exception {
		DataMaintenance d = (DataMaintenance) webContext.getCurrentBean();
		String modocName = d.getModDocName();

		// If nothing selected, just return a blank spreadsheet
		if (modocName == null) {
			BizPortWorkbook result = EXT.newBizPortWorkbook(false);
			result.materialise();
			return result;
		}

		String[] refs = modocName.split("\\.");
		return EXT.standardBizExport(refs[0], refs[1]);
	}
}
