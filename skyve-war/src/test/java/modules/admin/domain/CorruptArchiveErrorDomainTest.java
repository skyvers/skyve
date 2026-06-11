package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.archive.support.CorruptArchiveError.Resolution;
import org.skyve.domain.types.Timestamp;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class CorruptArchiveErrorDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() {
		CorruptArchiveError bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(CorruptArchiveError.MODULE_NAME, CorruptArchiveError.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() {
		CorruptArchiveError bean = CorruptArchiveError.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("CorruptArchiveError", bean.getBizDocument());
	}

	@Test
	void filenameSetAndGet() {
		CorruptArchiveError bean = CorruptArchiveError.newInstance();
		bean.setFilename("archive.zip");
		assertEquals("archive.zip", bean.getFilename());
	}

	@Test
	void archiveTypeModuleSetAndGet() {
		CorruptArchiveError bean = CorruptArchiveError.newInstance();
		bean.setArchiveTypeModule("admin");
		assertEquals("admin", bean.getArchiveTypeModule());
	}

	@Test
	void archiveTypeDocumentSetAndGet() {
		CorruptArchiveError bean = CorruptArchiveError.newInstance();
		bean.setArchiveTypeDocument("User");
		assertEquals("User", bean.getArchiveTypeDocument());
	}

	@Test
	void timestampSetAndGet() {
		CorruptArchiveError bean = CorruptArchiveError.newInstance();
		Timestamp ts = new Timestamp(System.currentTimeMillis());
		bean.setTimestamp(ts);
		assertEquals(ts, bean.getTimestamp());
	}

	@Test
	void resolutionSetAndGet() {
		CorruptArchiveError bean = CorruptArchiveError.newInstance();
		bean.setResolution(Resolution.unresolved);
		assertEquals(Resolution.unresolved, bean.getResolution());
	}

	@Test
	void getBizKeyWithNoFieldsNotNull() {
		CorruptArchiveError bean = CorruptArchiveError.newInstance();
		assertNotNull(bean.getBizKey());
	}
}
