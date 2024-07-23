package modules.admin.ControlPanel.actions;

import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.ControlPanel;

public class ExecuteQuery implements ServerSideAction<ControlPanelExtension> {
	@Override
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext)
	throws Exception {
		bean.setResults(null);
		bean.setTabIndex(null);

		String query = bean.getQuery();
		if (query == null) {
			throw new ValidationException(new Message(ControlPanel.queryPropertyName, "Enter a query"));
		}
		
		Persistence persistence = CORE.getPersistence();

		StringBuilder queryResults = new StringBuilder(5120);
		try {
			List<Bean> results = persistence.newBizQL(query).beanResults();
			for (Bean result : results) {
				queryResults.append(result).append('\n');
			}
			bean.setResults(queryResults.toString());
		}
		catch (Exception e) {
			bean.trapException(e);
		}
		bean.setTabIndex(Integer.valueOf(2));
		return new ServerSideActionResult<>(bean);
	}
}
