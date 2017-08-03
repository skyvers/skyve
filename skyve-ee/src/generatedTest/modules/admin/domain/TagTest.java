package modules.admin.domain;

import modules.admin.util.TagFactory;
import modules.admin.util.TagFactoryExtension;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class TagTest extends AbstractDomainTest<Tag> {

	private TagFactory factory;

	@Override
	protected Tag getBean() throws Exception {
		if (factory == null) {
			factory = new TagFactoryExtension();
		}

		return factory.getInstance();
	}
}