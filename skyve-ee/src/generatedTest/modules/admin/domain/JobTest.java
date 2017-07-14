package modules.admin.domain;

import modules.admin.util.JobFactory;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class JobTest extends AbstractDomainTest<Job> {

	private JobFactory factory;

	@Override
	protected Job getBean() throws Exception {
		if (factory == null) {
			factory = new JobFactory();
		}

		return factory.getInstance();
	}
}