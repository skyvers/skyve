package modules.admin.DocumentCreator.actions;

import modules.admin.domain.DocumentCreator;
import modules.admin.util.DocumentCreatorFactory;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class UpdatePreviewTest extends AbstractActionTest<DocumentCreator, UpdatePreview> {

	private DocumentCreatorFactory factory;

	@Override
	protected UpdatePreview getAction() {
		return new UpdatePreview();
	}

	@Override
	protected DocumentCreator getBean() throws Exception {
		if (factory == null) {
			factory = new DocumentCreatorFactory();
		}

		return factory.getInstance();
	}
}