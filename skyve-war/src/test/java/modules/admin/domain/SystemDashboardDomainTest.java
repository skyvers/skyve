package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class SystemDashboardDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() throws Exception {
		SystemDashboard bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(SystemDashboard.MODULE_NAME, SystemDashboard.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() throws Exception {
		SystemDashboard bean = SystemDashboard.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("SystemDashboard", bean.getBizDocument());
	}

	@Test
	void statusListInitialized() throws Exception {
		SystemDashboard bean = SystemDashboard.newInstance();
		assertNotNull(bean.getStatus());
	}
}
