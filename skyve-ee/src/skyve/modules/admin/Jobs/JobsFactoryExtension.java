package modules.admin.Jobs;

import org.skyve.util.test.SkyveFactory;

import modules.admin.domain.Jobs;
import modules.admin.util.JobsFactory;

@SkyveFactory(testAction = false)
public class JobsFactoryExtension extends JobsFactory {

	@Override
	public Jobs getInstance() throws Exception {
		return super.getInstance();
	}
}
