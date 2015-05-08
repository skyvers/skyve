package modules.admin.DataMaintenance.actions;

import modules.admin.domain.DataMaintenance;

import org.skyve.app.ModulesUtil;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.metadata.controller.BizExportAction;
import org.skyve.web.WebContext;

public class DataMaintenanceExportAction extends BizExportAction {
	private static final long serialVersionUID = -4559826499532491074L;

	@Override
	public BizPortWorkbook bizExport(WebContext webContext) throws Exception {
		DataMaintenance d  = (DataMaintenance) webContext.getCurrentBean();
		String modocName = d.getModDocName();
		if (modocName != null) {
			String[] refs = modocName.split("\\.");
			return ModulesUtil.standardBeanBizExport(refs[0], refs[1], null);
		}
		
		return null;
	}
}
