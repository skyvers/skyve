package modules.admin.JobSchedule;

import org.skyve.util.test.SkyveFactory;

import modules.admin.domain.JobSchedule;
import modules.admin.util.JobScheduleFactory;

@SkyveFactory(testDomain = false, testAction = false)
public class JobScheduleFactoryExtension extends JobScheduleFactory {

	@Override
	public JobSchedule getInstance() throws Exception {
		return super.getInstance();
	}
}
