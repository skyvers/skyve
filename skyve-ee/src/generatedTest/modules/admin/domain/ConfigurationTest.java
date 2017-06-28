package modules.admin.domain;

import modules.admin.util.ConfigurationFactory;
import util.AbstractDomainTest;
import util.AbstractH2Test;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractH2Test} to create your own tests for this document.
 */
public class ConfigurationTest extends AbstractDomainTest<Configuration> {

	private ConfigurationFactory factory;

	@Override
	public void setUp() throws Exception {
		factory = new ConfigurationFactory();
	}

	@Override
	protected Configuration getBean() throws Exception {
		return factory.getInstance();
	}
}