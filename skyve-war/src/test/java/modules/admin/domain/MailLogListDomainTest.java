package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class MailLogListDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() {
		MailLogList bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(MailLogList.MODULE_NAME, MailLogList.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() {
		MailLogList bean = MailLogList.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("MailLogList", bean.getBizDocument());
	}

	@Test
	void getBizKeyNotNull() {
		MailLogList bean = MailLogList.newInstance();
		assertNotNull(bean.getBizKey());
	}

	@Test
	void isShowNonArchivedReturnsTrue() {
		MailLogList bean = MailLogList.newInstance();
		assertTrue(bean.isShowNonArchived());
	}

	@Test
	void isNotShowNonArchivedReturnsFalse() {
		MailLogList bean = MailLogList.newInstance();
		assertTrue(!bean.isNotShowNonArchived());
	}
}
