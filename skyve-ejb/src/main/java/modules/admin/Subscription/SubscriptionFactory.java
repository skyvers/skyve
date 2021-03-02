package modules.admin.Subscription;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.Communication.FormatType;
import modules.admin.domain.Subscription;

public class SubscriptionFactory {

	@SkyveFixture(types = FixtureType.crud)
	public static Subscription crudInstance() throws Exception {
		Subscription sub = new DataBuilder()
				.optional(true, false)
				.depth(1)
				.build(Subscription.MODULE_NAME, Subscription.DOCUMENT_NAME);
		sub.setFormatType(FormatType.email);

		return sub;
	}
}
