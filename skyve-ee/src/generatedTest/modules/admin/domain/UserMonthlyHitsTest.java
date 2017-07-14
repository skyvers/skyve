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
	protected UserMonthlyHits getBean() throws Exception {
		if (factory == null) {
			factory = new UserMonthlyHitsFactory();
		}

		return factory.getInstance();
	}
}