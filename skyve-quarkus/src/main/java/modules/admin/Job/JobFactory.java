package modules.admin.Job;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFactory;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.Job.actions.CancelJob;
import modules.admin.domain.Job;

@SkyveFactory(excludedActions = CancelJob.class)
public class JobFactory {
	@SkyveFixture(types = FixtureType.crud)
	public static Job crudInstance() {
		return new DataBuilder().build(Job.MODULE_NAME, Job.DOCUMENT_NAME);
	}
}
