package modules.admin.ChangePassword;

import org.skyve.util.test.SkyveFactory;

import modules.admin.domain.ChangePassword;
import modules.admin.util.ChangePasswordFactory;

@SkyveFactory(testAction = false)
public class ChangePasswordFactoryExtension extends ChangePasswordFactory {

	@Override
	public ChangePassword getInstance() throws Exception {
		return super.getInstance();
	}
}
