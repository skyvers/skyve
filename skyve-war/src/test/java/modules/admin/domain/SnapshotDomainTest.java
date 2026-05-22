package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class SnapshotDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() throws Exception {
		Snapshot bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(Snapshot.MODULE_NAME, Snapshot.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() throws Exception {
		Snapshot bean = Snapshot.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("Snapshot", bean.getBizDocument());
	}

	@Test
	void moduleNameSetAndGet() throws Exception {
		Snapshot bean = Snapshot.newInstance();
		bean.setModuleName("admin");
		assertEquals("admin", bean.getModuleName());
	}

	@Test
	void queryNameSetAndGet() throws Exception {
		Snapshot bean = Snapshot.newInstance();
		bean.setQueryName("qUsers");
		assertEquals("qUsers", bean.getQueryName());
	}

	@Test
	void nameSetAndGet() throws Exception {
		Snapshot bean = Snapshot.newInstance();
		bean.setName("My snapshot");
		assertEquals("My snapshot", bean.getName());
	}

	@Test
	void snapshotDataSetAndGet() throws Exception {
		Snapshot bean = Snapshot.newInstance();
		bean.setSnapshot("{\"filter\":{}}");
		assertEquals("{\"filter\":{}}", bean.getSnapshot());
	}

	@Test
	void ordinalSetAndGet() throws Exception {
		Snapshot bean = Snapshot.newInstance();
		bean.setOrdinal(Integer.valueOf(3));
		assertEquals(Integer.valueOf(3), bean.getOrdinal());
	}
}
