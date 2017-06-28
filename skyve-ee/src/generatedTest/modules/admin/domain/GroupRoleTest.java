package modules.admin.domain;

import modules.admin.util.GroupRoleFactory;
import util.AbstractDomainTest;
import util.AbstractH2Test;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractH2Test} to create your own tests for this document.
 */
public class GroupRoleTest extends AbstractDomainTest<GroupRole> {

	private GroupRoleFactory factory;

	@Override
	public void setUp() throws Exception {
		factory = new GroupRoleFactory();
	}

	@Override
	protected GroupRole getBean() throws Exception {
		return factory.getInstance();
	}
}