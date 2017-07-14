package modules.admin.util;

import org.skyve.util.test.SkyveFactory;

import modules.admin.domain.UserCandidateContact;

@SkyveFactory(testAction = false)
public class UserCandidateContactFactoryExtension extends UserCandidateContactFactory {

	@Override
	public UserCandidateContact getInstance() throws Exception {
		return super.getInstance();
	}
}
