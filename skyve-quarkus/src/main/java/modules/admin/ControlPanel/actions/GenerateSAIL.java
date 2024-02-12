package modules.admin.ControlPanel.actions;

import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.user.User;
import org.skyve.web.UserAgentType;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.ControlPanel;
import modules.admin.domain.ControlPanel.SailTestStrategy;
import modules.admin.domain.ControlPanel.SailUserAgentType;
import modules.admin.domain.UserProxy;

public abstract class GenerateSAIL implements ServerSideAction<ControlPanelExtension> {
	@Override
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext)
	throws Exception {
		bean.setResults(null);
		bean.setTabIndex(null);

		UserProxy user = bean.getSailUser();
		String moduleName = bean.getSailModuleName();
		String uxui = bean.getSailUxUi();
		SailUserAgentType sailUserAgentType = bean.getSailUserAgentType();
		SailTestStrategy sailTestStrategy = bean.getSailTestStrategy();

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
		if (sailUserAgentType == null) {
			error = true;
			message.addBinding(ControlPanel.sailUserAgentTypePropertyName);
		}
		if (sailTestStrategy == null) {
			error = true;
			message.addBinding(ControlPanel.sailTestStrategyPropertyName);
		}
		if (error) {
			throw new ValidationException(message);
		}
		
		AbstractPersistence p = (AbstractPersistence) CORE.getPersistence();
		User currentUser = CORE.getUser();
		try {
			Repository r = CORE.getRepository();
			@SuppressWarnings("null")
			User u = r.retrieveUser(String.format("%s/%s", user.getBizCustomer(), user.getUserName()));
			p.setUser(u);

			@SuppressWarnings("null")
			UserAgentType userAgentType = UserAgentType.valueOf(sailUserAgentType.toCode());
			@SuppressWarnings("null")
			TestStrategy testStrategy = TestStrategy.valueOf(sailTestStrategy.toCode());
			if (moduleName != null) {
				Automation result = single(u,
											bean.getSailLoginCustomer(),
											bean.getSailLoginPassword(),
											moduleName,
											uxui,
											userAgentType,
											testStrategy);
				if (result.getInteractions().isEmpty()) {
					bean.setResults("NOTHING WAS GENERATED");
				}
				else {
					bean.setResults(XMLMetaData.marshalSAIL(result));
				}
			}
			else {
				List<Automation> result = multiple(u,
													bean.getSailLoginCustomer(),
													bean.getSailLoginPassword(),
													uxui,
													userAgentType,
													testStrategy);
				if (result.isEmpty()) {
					bean.setResults("NOTHING WAS GENERATED");
				}
				else {
					StringBuilder sb = new StringBuilder(4096);
					for (Automation automation : result) {
						sb.append(XMLMetaData.marshalSAIL(automation));
						sb.append('\n');
					}
					bean.setResults(sb.toString());
				}
			}
		}
		catch (Exception e) {
			bean.trapException(e);
		}
		finally {
			p.setUser(currentUser);
		}
		bean.setTabIndex(Integer.valueOf(2));

		return new ServerSideActionResult<>(bean);
	}
	
	protected abstract Automation single(User user,
											String loginCustomer,
											String loginPassword,
											String moduleName,
											String uxui,
											UserAgentType userAgentType,
											TestStrategy testStrategy)
	throws Exception;
	protected abstract List<Automation> multiple(User user,
													String loginCustomer,
													String loginPassword,
													String uxui,
													UserAgentType userAgentType,
													TestStrategy testStrategy)
	throws Exception;
}
