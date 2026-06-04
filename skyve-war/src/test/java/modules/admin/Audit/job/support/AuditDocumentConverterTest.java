package modules.admin.Audit.job.support;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.apache.lucene.document.Document;
import org.junit.jupiter.api.Test;
import org.skyve.archive.support.DocumentConverter;
import org.skyve.domain.Bean;
import org.skyve.domain.types.Timestamp;

import modules.admin.domain.Audit;
import modules.admin.domain.Audit.Operation;

@SuppressWarnings("static-method")
class AuditDocumentConverterTest {

	@Test
	void handlesWithAuditModuleAndDocumentReturnsTrue() {
		AuditDocumentConverter converter = new AuditDocumentConverter();
		assertTrue(converter.handles(Audit.MODULE_NAME, Audit.DOCUMENT_NAME));
	}

	@Test
	void handlesWithDifferentModuleReturnsFalse() {
		AuditDocumentConverter converter = new AuditDocumentConverter();
		assertFalse(converter.handles("someOtherModule", Audit.DOCUMENT_NAME));
	}

	@Test
	void handlesWithDifferentDocumentReturnsFalse() {
		AuditDocumentConverter converter = new AuditDocumentConverter();
		assertFalse(converter.handles(Audit.MODULE_NAME, "someOtherDocument"));
	}

	@Test
	void handlesWithBothDifferentReturnsFalse() {
		AuditDocumentConverter converter = new AuditDocumentConverter();
		assertFalse(converter.handles("test", "Document"));
	}

	@Test
	void convertIndexesExpectedFieldsAndSortBindings() {
		AuditDocumentConverter converter = new AuditDocumentConverter();
		Audit audit = new Audit();
		audit.setBizId("audit-1");
		audit.setBizUserId("user-1");
		audit.setTimestamp(new Timestamp());
		audit.setUserName("admin");
		audit.setOperation(Operation.update);
		audit.setAuditModuleName("crm");
		audit.setAuditDocumentName("Account");
		audit.setAuditBizId("account-1");
		audit.setAuditBizKey("Acme");

		Document doc = converter.convert(audit);

		String timestamp = DocumentConverter.dateToString(audit.getTimestamp());
		assertThat(doc.get(Audit.timestampPropertyName), is(timestamp));
		assertThat(doc.get(Audit.userNamePropertyName), is("admin"));
		assertThat(doc.get(Bean.USER_ID), is("user-1"));
		assertThat(doc.get(Bean.DOCUMENT_ID), is("audit-1"));
		assertThat(doc.get(Audit.operationPropertyName), is(Operation.update.name()));
		assertThat(doc.get(Audit.auditModuleNamePropertyName), is("crm"));
		assertThat(doc.get(Audit.auditDocumentNamePropertyName), is("Account"));
		assertThat(doc.get(Audit.auditBizIdPropertyName), is("account-1"));
		assertThat(doc.get(Audit.auditBizKeyPropertyName), is("Acme"));

		assertThat(doc.getField(DocumentConverter.toSortBinding(Audit.timestampPropertyName)), notNullValue());
		assertThat(doc.getField(DocumentConverter.toSortBinding(Audit.userNamePropertyName)), notNullValue());
		assertThat(doc.getField(DocumentConverter.toSortBinding(Audit.operationPropertyName)), notNullValue());
		assertThat(doc.getField(DocumentConverter.toSortBinding(Audit.auditModuleNamePropertyName)), notNullValue());
		assertThat(doc.getField(DocumentConverter.toSortBinding(Audit.auditDocumentNamePropertyName)), notNullValue());
		assertThat(doc.getField(DocumentConverter.toSortBinding(Audit.auditBizIdPropertyName)), notNullValue());
		assertThat(doc.getField(DocumentConverter.toSortBinding(Audit.auditBizKeyPropertyName)), notNullValue());
	}

	@Test
	void convertOmitsAuditBizKeyWhenAbsent() {
		AuditDocumentConverter converter = new AuditDocumentConverter();
		Audit audit = new Audit();
		audit.setBizId("audit-2");
		audit.setBizUserId("user-2");
		audit.setTimestamp(new Timestamp());
		audit.setUserName("admin");
		audit.setOperation(Operation.insert);
		audit.setAuditModuleName("crm");
		audit.setAuditDocumentName("Account");
		audit.setAuditBizId("account-2");

		Document doc = converter.convert(audit);

		assertThat(doc.get(Audit.auditBizKeyPropertyName), is((String) null));
		assertThat(doc.getField(DocumentConverter.toSortBinding(Audit.auditBizKeyPropertyName)), is((org.apache.lucene.index.IndexableField) null));
	}
}
