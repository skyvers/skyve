package modules.admin.Subscription;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFactory;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.Subscription;

/**
 * Provides fixture construction helpers for Subscription document tests.
 */
@SkyveFactory(testDomain = false)
public class SubscriptionFactory {
	/**
	 * Builds a CRUD-oriented subscription fixture.
	 *
	 * @return a subscription fixture instance
	 * @throws Exception if fixture generation fails
	 */
	@SkyveFixture(types = FixtureType.crud)
	@SuppressWarnings("static-method")
	public Subscription crudInstance() throws Exception {
		Subscription sub = new DataBuilder()
				.optional(true, false)
				.depth(1)
				.factoryBuild(Subscription.MODULE_NAME, Subscription.DOCUMENT_NAME);

		return sub;
	}
}
