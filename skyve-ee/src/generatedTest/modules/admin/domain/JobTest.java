package modules.admin.domain;

import modules.admin.util.JobFactory;
import util.AbstractDomainTest;
import util.AbstractH2Test;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractH2Test} to create your own tests for this document.
 */
public class JobTest extends AbstractDomainTest<Job> {

	private JobFactory factory;

	@Override
	public void setUp() throws Exception {
		factory = new JobFactory();
	}

	@Override
	protected Job getBean() throws Exception {
		return factory.getInstance();
	}
}