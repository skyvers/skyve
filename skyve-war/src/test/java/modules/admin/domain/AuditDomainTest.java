package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Timestamp;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

class AuditDomainTest extends AbstractH2Test {

	@Test
	@SuppressWarnings("static-method")
	void dataBuilderCreatesAudit() {
		Audit bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	@SuppressWarnings("static-method")
	void moduleAndDocumentNames() {
		Audit bean = new Audit();
		assertEquals("admin", bean.getBizModule());
		assertEquals("Audit", bean.getBizDocument());
	}

	@Test
	@SuppressWarnings("static-method")
	void auditModuleNameSetAndGet() {
		Audit bean = new Audit();
		bean.setAuditModuleName("admin");
		assertEquals("admin", bean.getAuditModuleName());
	}

	@Test
	@SuppressWarnings("static-method")
	void auditDocumentNameSetAndGet() {
		Audit bean = new Audit();
		bean.setAuditDocumentName("User");
		assertEquals("User", bean.getAuditDocumentName());
	}

	@Test
	@SuppressWarnings("static-method")
	void auditBizIdSetAndGet() {
		Audit bean = new Audit();
		bean.setAuditBizId("test-biz-id-123");
		assertEquals("test-biz-id-123", bean.getAuditBizId());
	}

	@Test
	@SuppressWarnings("static-method")
	void auditBizKeySetAndGet() {
		Audit bean = new Audit();
		bean.setAuditBizKey("Test Biz Key");
		assertEquals("Test Biz Key", bean.getAuditBizKey());
	}

	@Test
	@SuppressWarnings("static-method")
	void operationInsertSetAndGet() {
		Audit bean = new Audit();
		bean.setOperation(Audit.Operation.insert);
		assertEquals(Audit.Operation.insert, bean.getOperation());
	}

	@Test
	@SuppressWarnings("static-method")
	void operationUpdateSetAndGet() {
		Audit bean = new Audit();
		bean.setOperation(Audit.Operation.update);
		assertEquals(Audit.Operation.update, bean.getOperation());
	}

	@Test
	@SuppressWarnings("static-method")
	void operationDeleteSetAndGet() {
		Audit bean = new Audit();
		bean.setOperation(Audit.Operation.delete);
		assertEquals(Audit.Operation.delete, bean.getOperation());
	}

	@Test
	@SuppressWarnings("static-method")
	void operationEnumCodes() {
		assertEquals("I", Audit.Operation.insert.toCode());
		assertEquals("U", Audit.Operation.update.toCode());
		assertEquals("D", Audit.Operation.delete.toCode());
	}

	@Test
	@SuppressWarnings("static-method")
	void timestampSetAndGet() {
		Audit bean = new Audit();
		Timestamp ts = new Timestamp();
		bean.setTimestamp(ts);
		assertNotNull(bean.getTimestamp());
	}

	@Test
	@SuppressWarnings("static-method")
	void millisSetAndGet() {
		Audit bean = new Audit();
		bean.setMillis(Long.valueOf(12345L));
		assertEquals(Long.valueOf(12345L), bean.getMillis());
	}

	@Test
	@SuppressWarnings("static-method")
	void userNameSetAndGet() {
		Audit bean = new Audit();
		bean.setUserName("testuser");
		assertEquals("testuser", bean.getUserName());
	}

	@Test
	@SuppressWarnings("static-method")
	void auditDetailSetAndGet() {
		Audit bean = new Audit();
		bean.setAuditDetail("{\"field\":\"value\"}");
		assertEquals("{\"field\":\"value\"}", bean.getAuditDetail());
	}

        @Test
        @SuppressWarnings("static-method")
        void operationToLocalisedDescription() {
                assertNotNull(Audit.Operation.insert.toLocalisedDescription());
        }

        @Test
        @SuppressWarnings("static-method")
        void operationToDomainValue() {
                assertNotNull(Audit.Operation.insert.toDomainValue());
        }

        @Test
        @SuppressWarnings("static-method")
        void operationToDomainValues() {
                assertNotNull(Audit.Operation.toDomainValues());
                assertFalse(Audit.Operation.toDomainValues().isEmpty());
        }

        @Test
        @SuppressWarnings("static-method")
        void operationFromLocalisedDescription() {
                String desc = Audit.Operation.insert.toLocalisedDescription();
                assertEquals(Audit.Operation.insert, Audit.Operation.fromLocalisedDescription(desc));
        }

        @Test
        @SuppressWarnings("static-method")
        void operationFromLocalisedDescriptionUnknownReturnsNull() {
                assertNull(Audit.Operation.fromLocalisedDescription("nonexistent_xyz_operation"));
        }
}
