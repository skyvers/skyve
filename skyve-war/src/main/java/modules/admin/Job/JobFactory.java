package modules.admin.Job;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFactory;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.Job.actions.CancelJob;
import modules.admin.domain.Job;

/**
 * Creates fixture instances for {@link Job}.
 */
@SkyveFactory(excludedActions = CancelJob.class)
public class JobFactory {
	/**
	 * Builds a CRUD fixture for the admin job document.
	 *
	 * @return a fixture-ready job bean
	 */
	@SkyveFixture(types = FixtureType.crud)
	public static Job crudInstance() {
		return new DataBuilder().factoryBuild(Job.MODULE_NAME, Job.DOCUMENT_NAME);
	}
}
