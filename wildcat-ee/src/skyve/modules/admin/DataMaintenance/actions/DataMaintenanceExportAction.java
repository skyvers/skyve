package modules.admin.DataMaintenance.actions;

import org.skyve.bizport.BizPortWorkbook;
import org.skyve.metadata.controller.BizExportAction;
import org.skyve.web.WebContext;

import modules.ModulesUtil;
import modules.admin.domain.DataMaintenance;

public class DataMaintenanceExportAction extends BizExportAction {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -4559826499532491074L;

	@Override
	public BizPortWorkbook bizExport(WebContext webContext) throws Exception {
		DataMaintenance d  = (DataMaintenance) webContext.getCurrentBean();
		String[] refs = d.getModDocName().split("\\.");
		
		return ModulesUtil.standardBeanBizExport(refs[0], refs[1], null);
	}
}
