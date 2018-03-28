package modules.admin.UserCandidateContact;

import org.skyve.util.test.SkyveFactory;

import modules.admin.domain.UserCandidateContact;
import modules.admin.util.UserCandidateContactFactory;

@SkyveFactory(testAction = false)
public class UserCandidateContactFactoryExtension extends UserCandidateContactFactory {

	@Override
	public UserCandidateContact getInstance() throws Exception {
		return super.getInstance();
	}
}
