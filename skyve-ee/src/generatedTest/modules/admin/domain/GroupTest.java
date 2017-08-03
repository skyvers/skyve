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
	protected Group getBean() throws Exception {
		if (factory == null) {
			factory = new GroupFactory();
		}

		return factory.getInstance();
	}
}