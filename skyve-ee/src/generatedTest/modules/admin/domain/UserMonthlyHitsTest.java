package modules.admin.domain;

import modules.admin.util.UserMonthlyHitsFactory;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class UserMonthlyHitsTest extends AbstractDomainTest<UserMonthlyHits> {

	private UserMonthlyHitsFactory factory;

	@Override
	public void setUp() throws Exception {
		factory = new UserMonthlyHitsFactory();
	}

	@Override
	protected UserMonthlyHits getBean() throws Exception {
		return factory.getInstance();
	}
}