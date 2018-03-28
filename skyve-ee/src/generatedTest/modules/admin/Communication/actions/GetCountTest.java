package modules.admin.Communication.actions;

import modules.admin.Communication.CommunicationFactoryExtension;
import modules.admin.domain.Communication;
import modules.admin.util.CommunicationFactory;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class GetCountTest extends AbstractActionTest<Communication, GetCount> {

	private CommunicationFactory factory;

	@Override
	protected GetCount getAction() {
		return new GetCount();
	}

	@Override
	protected Communication getBean() throws Exception {
		if (factory == null) {
			factory = new CommunicationFactoryExtension();
		}

		return factory.getInstance();
	}
}