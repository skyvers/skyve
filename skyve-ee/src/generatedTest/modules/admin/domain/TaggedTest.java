package modules.admin.domain;

import modules.admin.util.TaggedFactory;
import util.AbstractDomainTest;
import util.AbstractH2Test;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractH2Test} to create your own tests for this document.
 */
public class TaggedTest extends AbstractDomainTest<Tagged> {

	private TaggedFactory factory;

	@Override
	public void setUp() throws Exception {
		factory = new TaggedFactory();
	}

	@Override
	protected Tagged getBean() throws Exception {
		return factory.getInstance();
	}
}