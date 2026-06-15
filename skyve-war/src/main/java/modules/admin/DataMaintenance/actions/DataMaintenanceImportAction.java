package modules.admin.DataMaintenance.actions;

import org.skyve.EXT;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.domain.messages.UploadException;
import org.skyve.metadata.controller.BizImportAction;

/**
 * Imports Data Maintenance data from a BizPort workbook.
 */
public class DataMaintenanceImportAction extends BizImportAction {
	/**
	 * Performs the bizImport operation.
	 * @param workbook the workbook value
	 * @param problems the problems value
	 * @throws Exception if the operation fails
	 */
	@Override
	public void bizImport(BizPortWorkbook workbook, UploadException problems) throws Exception {
		EXT.standardBizImport(workbook, problems);
	}
}
