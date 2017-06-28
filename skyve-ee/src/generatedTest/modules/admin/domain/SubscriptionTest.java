package modules.admin.domain;

import modules.admin.util.SubscriptionFactory;
import modules.admin.util.SubscriptionFactoryExtension;
import util.AbstractDomainTest;
import util.AbstractH2Test;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractH2Test} to create your own tests for this document.
 */
public class SubscriptionTest extends AbstractDomainTest<Subscription> {

	private SubscriptionFactory factory;

	@Override
	public void setUp() throws Exception {
		factory = new SubscriptionFactoryExtension();
	}

	@Override
	protected Subscription getBean() throws Exception {
		return factory.getInstance();
	}
}