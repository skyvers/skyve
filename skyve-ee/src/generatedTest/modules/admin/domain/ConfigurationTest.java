package modules.admin.domain;

import modules.admin.util.ConfigurationFactory;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class ConfigurationTest extends AbstractDomainTest<Configuration> {

	private ConfigurationFactory factory;

	@Override
	protected Configuration getBean() throws Exception {
		if (factory == null) {
			factory = new ConfigurationFactory();
		}

		return factory.getInstance();
	}
}