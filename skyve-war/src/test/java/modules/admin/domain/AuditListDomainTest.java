package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class AuditListDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() {
		AuditList bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(AuditList.MODULE_NAME, AuditList.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() {
		AuditList bean = AuditList.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("AuditList", bean.getBizDocument());
	}

	@Test
	void getBizKeyNotNull() {
		AuditList bean = AuditList.newInstance();
		assertNotNull(bean.getBizKey());
	}

	@Test
	void isShowNonArchivedReturnsTrue() {
		AuditList bean = AuditList.newInstance();
		assertTrue(bean.isShowNonArchived());
	}

	@Test
	void isNotShowNonArchivedReturnsFalse() {
		AuditList bean = AuditList.newInstance();
		assertTrue(!bean.isNotShowNonArchived());
	}
}
