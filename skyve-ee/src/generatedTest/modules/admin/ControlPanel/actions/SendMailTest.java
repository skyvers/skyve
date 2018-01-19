package modules.admin.ControlPanel.actions;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.util.ControlPanelFactory;
import modules.admin.util.ControlPanelFactoryExtension;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class SendMailTest extends AbstractActionTest<ControlPanelExtension, SendMail> {

	private ControlPanelFactory factory;

	@Override
	protected SendMail getAction() {
		return new SendMail();
	}

	@Override
	protected ControlPanelExtension getBean() throws Exception {
		if (factory == null) {
			factory = new ControlPanelFactoryExtension();
		}

		return factory.getInstance();
	}
}