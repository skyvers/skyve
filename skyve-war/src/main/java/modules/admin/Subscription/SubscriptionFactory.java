package modules.admin.Subscription;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFactory;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.Subscription;

@SkyveFactory(testDomain = false)
public class SubscriptionFactory {
	@SkyveFixture(types = FixtureType.crud)
	@SuppressWarnings("static-method")
	public Subscription crudInstance() throws Exception {
		Subscription sub = new DataBuilder()
				.optional(true, false)
				.depth(1)
				.build(Subscription.MODULE_NAME, Subscription.DOCUMENT_NAME);

		return sub;
	}
}
