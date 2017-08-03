package modules.admin.util;

import modules.admin.domain.Communication.FormatType;
import modules.admin.domain.Subscription;

public class SubscriptionFactoryExtension extends SubscriptionFactory {

	@Override
	public Subscription getInstance() throws Exception {
		Subscription sub = super.getInstance();
		sub.setFormatType(FormatType.email);

		return sub;
	}
}
