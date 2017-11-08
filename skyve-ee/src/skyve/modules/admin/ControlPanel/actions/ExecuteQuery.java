package modules.admin.ControlPanel.actions;

import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;

public class ExecuteQuery implements ServerSideAction<ControlPanelExtension> {
	private static final long serialVersionUID = 5990074876826469688L;

	@Override
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext)
	throws Exception {
		Persistence persistence = CORE.getPersistence();

		StringBuilder queryResults = new StringBuilder(5120);
		try {
			List<Bean> results = persistence.newBizQL(bean.getQuery()).beanResults();
			for (Bean result : results) {
				queryResults.append(result).append('\n');
			}
			bean.setResults(queryResults.toString());
		}
		catch (Exception e) {
			bean.trapException(e);
		}
		
		return new ServerSideActionResult<>(bean);
	}
}
