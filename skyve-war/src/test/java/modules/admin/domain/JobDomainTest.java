package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Timestamp;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class JobDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() {
		Job bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(Job.MODULE_NAME, Job.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() {
		Job bean = Job.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("Job", bean.getBizDocument());
	}

	@Test
	void displayNameSetAndGet() {
		Job bean = Job.newInstance();
		bean.setDisplayName("My Background Job");
		assertEquals("My Background Job", bean.getDisplayName());
	}

	@Test
	void percentCompleteSetAndGet() {
		Job bean = Job.newInstance();
		bean.setPercentComplete(Integer.valueOf(75));
		assertEquals(Integer.valueOf(75), bean.getPercentComplete());
	}

	@Test
	void statusSetAndGet() {
		Job bean = Job.newInstance();
		bean.setStatus("running");
		assertEquals("running", bean.getStatus());
	}

	@Test
	void logSetAndGet() {
		Job bean = Job.newInstance();
		bean.setLog("Step 1 complete");
		assertEquals("Step 1 complete", bean.getLog());
	}

	@Test
	void startTimeSetAndGet() {
		Job bean = Job.newInstance();
		Timestamp now = new Timestamp();
		bean.setStartTime(now);
		assertEquals(now, bean.getStartTime());
	}

	@Test
	void beanModuleAndDocumentNamesSetAndGet() {
		Job bean = Job.newInstance();
		bean.setBeanModuleName("admin");
		bean.setBeanDocumentName("User");
		assertEquals("admin", bean.getBeanModuleName());
		assertEquals("User", bean.getBeanDocumentName());
	}
}
