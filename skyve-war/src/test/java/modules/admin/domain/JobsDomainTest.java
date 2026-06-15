package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class JobsDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() {
		Jobs bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(Jobs.MODULE_NAME, Jobs.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() {
		Jobs bean = Jobs.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("Jobs", bean.getBizDocument());
	}

	@Test
	void runningJobsListInitialized() {
		Jobs bean = Jobs.newInstance();
		assertNotNull(bean.getRunningJobs());
	}
}
