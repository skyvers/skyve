package modules.admin.Tag.actions;

import modules.admin.domain.Tag;
import modules.admin.util.TagFactory;
import modules.admin.util.TagFactoryExtension;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class UpdateUploadModuleTest extends AbstractActionTest<Tag, UpdateUploadModule> {

	private TagFactory factory;

	@Override
	protected UpdateUploadModule getAction() {
		return new UpdateUploadModule();
	}

	@Override
	protected Tag getBean() throws Exception {
		if (factory == null) {
			factory = new TagFactoryExtension();
		}

		return factory.getInstance();
	}
}