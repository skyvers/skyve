package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class DownloadFolderDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() {
		DownloadFolder bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(DownloadFolder.MODULE_NAME, DownloadFolder.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() {
		DownloadFolder bean = DownloadFolder.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("DownloadFolder", bean.getBizDocument());
	}

	@Test
	void nameSetAndGet() {
		DownloadFolder bean = DownloadFolder.newInstance();
		bean.setName("Downloads");
		assertEquals("Downloads", bean.getName());
	}
}
