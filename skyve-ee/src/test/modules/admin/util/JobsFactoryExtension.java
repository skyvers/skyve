package modules.admin.util;

import org.skyve.util.test.SkyveFactory;

import modules.admin.domain.Jobs;

@SkyveFactory(testAction = false)
public class JobsFactoryExtension extends JobsFactory {

	@Override
	public Jobs getInstance() throws Exception {
		return super.getInstance();
	}
}
