package modules.admin.ReportDesign.actions;

import modules.admin.domain.ReportDesign;
import modules.admin.util.ReportDesignFactory;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class GenerateDefaultTest extends AbstractActionTest<ReportDesign, GenerateDefault> {

	private ReportDesignFactory factory;

	@Override
	protected GenerateDefault getAction() {
		return new GenerateDefault();
	}

	@Override
	protected ReportDesign getBean() throws Exception {
		if (factory == null) {
			factory = new ReportDesignFactory();
		}

		return factory.getInstance();
	}
}