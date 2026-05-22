package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class SnapshotsDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() throws Exception {
		Snapshots bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(Snapshots.MODULE_NAME, Snapshots.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() throws Exception {
		Snapshots bean = Snapshots.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("Snapshots", bean.getBizDocument());
	}

	@Test
	void moduleNameSetAndGet() throws Exception {
		Snapshots bean = Snapshots.newInstance();
		bean.setModuleName("admin");
		assertEquals("admin", bean.getModuleName());
	}

	@Test
	void queryNameSetAndGet() throws Exception {
		Snapshots bean = Snapshots.newInstance();
		bean.setQueryName("qUsers");
		assertEquals("qUsers", bean.getQueryName());
	}

	@Test
	void snapshotsToReorderListInitialized() throws Exception {
		Snapshots bean = Snapshots.newInstance();
		assertNotNull(bean.getSnapshotsToReorder());
	}
}
