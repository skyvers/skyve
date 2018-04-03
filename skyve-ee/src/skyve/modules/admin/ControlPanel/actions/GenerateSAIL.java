package modules.admin.ControlPanel.actions;

import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.web.UserAgentType;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.ControlPanel;
import modules.admin.domain.ControlPanel.SailUserAgentType;

public abstract class GenerateSAIL implements ServerSideAction<ControlPanelExtension> {
	private static final long serialVersionUID = 7370653121212184868L;

	@Override
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext)
	throws Exception {
		bean.setResults(null);
		bean.setTabIndex(null);

		modules.admin.domain.User user = bean.getSailUser();
		String moduleName = bean.getSailModuleName();
		String uxui = bean.getSailUxUi();
		SailUserAgentType userAgentType = bean.getSailUserAgentType();

		boolean error = false;
		Message message = new Message("Select a value");
		if (user == null) {
			error = true;
			message.addBinding(ControlPanel.sailUserPropertyName);
		}
		if (uxui == null) {
			error = true;
			message.addBinding(ControlPanel.sailUxUiPropertyName);
		}
		if (userAgentType == null) {
			error = true;
			message.addBinding(ControlPanel.sailUserAgentTypePropertyName);
		}
		if (error) {
			throw new ValidationException(message);
		}
		
		try {
			Repository r = CORE.getRepository();
			@SuppressWarnings("null")
			User u = r.retrieveUser(String.format("%s/%s", user.getBizCustomer(), user.getUserName()));
			if (moduleName != null) {
				@SuppressWarnings("null")
				Automation result = singular(u, moduleName, uxui, UserAgentType.valueOf(userAgentType.toCode()));
				bean.setResults(XMLMetaData.marshalSAIL(result));
			}
			else {
				@SuppressWarnings("null")
				List<Automation> result = plural(u, uxui, UserAgentType.valueOf(userAgentType.toCode()));
				StringBuilder sb = new StringBuilder(4096);
				for (Automation automation : result) {
					sb.append(XMLMetaData.marshalSAIL(automation));
					sb.append('\n');
				}
				bean.setResults(sb.toString());
			}
		}
		catch (Exception e) {
			bean.trapException(e);
		}
		bean.setTabIndex(Integer.valueOf(2));

		return new ServerSideActionResult<>(bean);
	}
	
	protected abstract Automation singular(User user, String moduleName, String uxui, UserAgentType userAgentType)
	throws Exception;
	protected abstract List<Automation> plural(User user, String uxui, UserAgentType userAgentType)
	throws Exception;
}
