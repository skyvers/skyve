package modules.admin.domain;

import modules.admin.util.GroupFactory;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class GroupTest extends AbstractDomainTest<Group> {

	private GroupFactory factory;

	@Override
	public void setUp() throws Exception {
		factory = new GroupFactory();
	}

	@Override
	protected Group getBean() throws Exception {
		return factory.getInstance();
	}
}