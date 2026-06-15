package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class SnapshotDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() {
		Snapshot bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(Snapshot.MODULE_NAME, Snapshot.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() {
		Snapshot bean = Snapshot.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("Snapshot", bean.getBizDocument());
	}

	@Test
	void moduleNameSetAndGet() {
		Snapshot bean = Snapshot.newInstance();
		bean.setModuleName("admin");
		assertEquals("admin", bean.getModuleName());
	}

	@Test
	void queryNameSetAndGet() {
		Snapshot bean = Snapshot.newInstance();
		bean.setQueryName("qUsers");
		assertEquals("qUsers", bean.getQueryName());
	}

	@Test
	void nameSetAndGet() {
		Snapshot bean = Snapshot.newInstance();
		bean.setName("My snapshot");
		assertEquals("My snapshot", bean.getName());
	}

	@Test
	void snapshotDataSetAndGet() {
		Snapshot bean = Snapshot.newInstance();
		bean.setSnapshot("{\"filter\":{}}");
		assertEquals("{\"filter\":{}}", bean.getSnapshot());
	}

	@Test
	void ordinalSetAndGet() {
		Snapshot bean = Snapshot.newInstance();
		bean.setOrdinal(Integer.valueOf(3));
		assertEquals(Integer.valueOf(3), bean.getOrdinal());
	}
}
