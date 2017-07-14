package modules.admin.util;

import org.skyve.util.test.SkyveFactory;

import modules.admin.domain.ChangePassword;

@SkyveFactory(testAction = false)
public class ChangePasswordFactoryExtension extends ChangePasswordFactory {

	@Override
	public ChangePassword getInstance() throws Exception {
		return super.getInstance();
	}
}
