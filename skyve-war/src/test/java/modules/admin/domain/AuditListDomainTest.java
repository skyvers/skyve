package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class AuditListDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() throws Exception {
		AuditList bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(AuditList.MODULE_NAME, AuditList.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() throws Exception {
		AuditList bean = AuditList.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("AuditList", bean.getBizDocument());
	}

	@Test
	void getBizKeyNotNull() throws Exception {
		AuditList bean = AuditList.newInstance();
		assertNotNull(bean.getBizKey());
	}

	@Test
	void isShowNonArchivedReturnsTrue() throws Exception {
		AuditList bean = AuditList.newInstance();
		assertTrue(bean.isShowNonArchived());
	}

	@Test
	void isNotShowNonArchivedReturnsFalse() throws Exception {
		AuditList bean = AuditList.newInstance();
		assertTrue(!bean.isNotShowNonArchived());
	}
}
