package modules.admin.Communication.actions;

import modules.admin.domain.Communication;
import modules.admin.util.CommunicationFactory;
import modules.admin.util.CommunicationFactoryExtension;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class DeleteBatchTest extends AbstractActionTest<Communication, DeleteBatch> {

	private CommunicationFactory factory;

	@Override
	protected DeleteBatch getAction() {
		return new DeleteBatch();
	}

	@Override
	protected Communication getBean() throws Exception {
		if (factory == null) {
			factory = new CommunicationFactoryExtension();
		}

		return factory.getInstance();
	}
}