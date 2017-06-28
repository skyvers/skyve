package modules.admin.domain;

import modules.admin.util.TagFactory;
import util.AbstractDomainTest;
import util.AbstractH2Test;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractH2Test} to create your own tests for this document.
 */
public class TagTest extends AbstractDomainTest<Tag> {

	private TagFactory factory;

	@Override
	public void setUp() throws Exception {
		factory = new TagFactory();
	}

	@Override
	protected Tag getBean() throws Exception {
		return factory.getInstance();
	}
}