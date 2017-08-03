package modules.admin.domain;

import modules.admin.util.SubscriptionFactory;
import modules.admin.util.SubscriptionFactoryExtension;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class SubscriptionTest extends AbstractDomainTest<Subscription> {

	private SubscriptionFactory factory;

	@Override
	protected Subscription getBean() throws Exception {
		if (factory == null) {
			factory = new SubscriptionFactoryExtension();
		}

		return factory.getInstance();
	}
}