package modules.admin.domain;

import modules.admin.util.JobScheduleFactory;
import util.AbstractDomainTest;
import util.AbstractH2Test;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractH2Test} to create your own tests for this document.
 */
public class JobScheduleTest extends AbstractDomainTest<JobSchedule> {

	private JobScheduleFactory factory;

	@Override
	public void setUp() throws Exception {
		factory = new JobScheduleFactory();
	}

	@Override
	protected JobSchedule getBean() throws Exception {
		return factory.getInstance();
	}
}