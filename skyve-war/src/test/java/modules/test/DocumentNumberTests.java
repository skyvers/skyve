package modules.test;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.domain.number.DocumentNumberAutonomousTransactionGenerator;
import org.skyve.impl.domain.number.DocumentNumberGenerator;

import modules.admin.domain.DocumentNumber;
import modules.test.domain.AllAttributesPersistent;

class DocumentNumberTests extends AbstractSkyveTestDispose {

	DocumentNumber dN;
	DocumentNumberGenerator generator = new DocumentNumberGenerator();
	DocumentNumberAutonomousTransactionGenerator autonomousGenerator = new DocumentNumberAutonomousTransactionGenerator();

	@BeforeEach
	void setup() {
		dN = DocumentNumber.newInstance();
		dN.setModuleName(AllAttributesPersistent.MODULE_NAME);
		dN.setDocumentName(AllAttributesPersistent.DOCUMENT_NAME);
		dN.setSequenceName(AllAttributesPersistent.textPropertyName);
		dN.setDocumentNumber("1");
		dN = p.save(dN);
		p.commit(false);
		p.begin();
	}

	@Test
	void testDefaultGetNumberReturnsNextNumber() {
		generator.next(null, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.textPropertyName, 1);
		p.evictAllCached();
		dN = p.retrieve(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME, dN.getBizId());
		Assertions.assertEquals("2", dN.getDocumentNumber());

		generator.next(null, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.textPropertyName, 1);
		p.evictAllCached();
		dN = p.retrieve(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME, dN.getBizId());
		Assertions.assertEquals("3", dN.getDocumentNumber());

		generator.next(null, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.textPropertyName, 1);
		p.evictAllCached();
		dN = p.retrieve(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME, dN.getBizId());
		Assertions.assertEquals("4", dN.getDocumentNumber());

	}

	@Test
	void testAutonomousGetNumberReturnsNextNumber() {
		autonomousGenerator.next(null, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.textPropertyName, 1);
		p.evictAllCached();
		dN = p.retrieve(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME, dN.getBizId());
		Assertions.assertEquals("2", dN.getDocumentNumber());

		autonomousGenerator.next(null, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.textPropertyName, 1);
		p.evictAllCached();
		dN = p.retrieve(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME, dN.getBizId());
		Assertions.assertEquals("3", dN.getDocumentNumber());

		autonomousGenerator.next(null, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.textPropertyName, 1);
		p.evictAllCached();
		dN = p.retrieve(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME, dN.getBizId());
		Assertions.assertEquals("4", dN.getDocumentNumber());
	}

	@Test
	void testDefaultRollbackDoesNotCommit() {
		p.evictAllCached();

		generator.next(null, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.textPropertyName, 1);
		p.rollback();
		dN = p.retrieve(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME, dN.getBizId());
		Assertions.assertEquals("1", dN.getDocumentNumber());

		p.begin();
		generator.next(null, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.textPropertyName, 1);
		p.rollback();
		dN = p.retrieve(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME, dN.getBizId());
		Assertions.assertEquals("1", dN.getDocumentNumber());

		p.begin();
		generator.next(null, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.textPropertyName, 1);
		p.rollback();
		dN = p.retrieve(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME, dN.getBizId());
		Assertions.assertEquals("1", dN.getDocumentNumber());
	}

	@Test
	void testAutonomousNextNumberRollbackIncrements() {
		p.evictAllCached();

		autonomousGenerator.next(null, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.textPropertyName, 1);
		p.rollback();
		dN = p.retrieve(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME, dN.getBizId());
		Assertions.assertEquals("2", dN.getDocumentNumber());

		p.evictAllCached();
		p.begin();
		autonomousGenerator.next(null, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.textPropertyName, 1);
		p.rollback();
		dN = p.retrieve(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME, dN.getBizId());
		Assertions.assertEquals("3", dN.getDocumentNumber());

		p.evictAllCached();
		p.begin();
		autonomousGenerator.next(null, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.textPropertyName, 1);
		p.rollback();
		dN = p.retrieve(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME, dN.getBizId());
		Assertions.assertEquals("4", dN.getDocumentNumber());
	}
}
