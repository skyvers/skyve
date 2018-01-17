package modules.admin.DocumentCreator.actions;

import modules.admin.domain.DocumentCreator;
import modules.admin.util.DocumentCreatorFactory;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class SubmitTest extends AbstractActionTest<DocumentCreator, Submit> {

	private DocumentCreatorFactory factory;

	@Override
	protected Submit getAction() {
		return new Submit();
	}

	@Override
	protected DocumentCreator getBean() throws Exception {
		if (factory == null) {
			factory = new DocumentCreatorFactory();
		}

		return factory.getInstance();
	}
}