package modules.test;

import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.domain.number.DocumentNumberAutonomousTransactionGenerator;
import org.skyve.impl.domain.number.DocumentNumberGenerator;

import modules.admin.domain.DocumentNumber;
import modules.test.domain.AllAttributesPersistent;

public class DocumentNumberTests extends AbstractSkyveTestDispose {

	DocumentNumber dN;
	DocumentNumberGenerator generator = new DocumentNumberGenerator();
	DocumentNumberAutonomousTransactionGenerator autonomousGenerator = new DocumentNumberAutonomousTransactionGenerator();

	@BeforeEach
	public void setup() throws Exception {
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
	public void testDefaultGetNumberReturnsNextNumber() throws Exception {
		generator.next(null, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.textPropertyName, 1);
		p.evictAllCached();
		dN = p.retrieve(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME, dN.getBizId());
		Assert.assertEquals("2", dN.getDocumentNumber());

		generator.next(null, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.textPropertyName, 1);
		p.evictAllCached();
		dN = p.retrieve(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME, dN.getBizId());
		Assert.assertEquals("3", dN.getDocumentNumber());

		generator.next(null, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.textPropertyName, 1);
		p.evictAllCached();
		dN = p.retrieve(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME, dN.getBizId());
		Assert.assertEquals("4", dN.getDocumentNumber());

	}

	@Test
	public void testAutonomousGetNumberReturnsNextNumber() throws Exception {
		autonomousGenerator.next(null, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.textPropertyName, 1);
		p.evictAllCached();
		dN = p.retrieve(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME, dN.getBizId());
		Assert.assertEquals("2", dN.getDocumentNumber());

		autonomousGenerator.next(null, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.textPropertyName, 1);
		p.evictAllCached();
		dN = p.retrieve(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME, dN.getBizId());
		Assert.assertEquals("3", dN.getDocumentNumber());

		autonomousGenerator.next(null, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.textPropertyName, 1);
		p.evictAllCached();
		dN = p.retrieve(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME, dN.getBizId());
		Assert.assertEquals("4", dN.getDocumentNumber());
	}

	@Test
	public void testDefaultRollbackDoesNotCommit() throws Exception {
		p.evictAllCached();

		generator.next(null, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.textPropertyName, 1);
		p.rollback();
		dN = p.retrieve(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME, dN.getBizId());
		Assert.assertEquals("1", dN.getDocumentNumber());

		p.begin();
		generator.next(null, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.textPropertyName, 1);
		p.rollback();
		dN = p.retrieve(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME, dN.getBizId());
		Assert.assertEquals("1", dN.getDocumentNumber());

		p.begin();
		generator.next(null, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.textPropertyName, 1);
		p.rollback();
		dN = p.retrieve(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME, dN.getBizId());
		Assert.assertEquals("1", dN.getDocumentNumber());
	}

	@Test
	public void testAutonomousNextNumberRollbackIncrements() throws Exception {
		p.evictAllCached();

		autonomousGenerator.next(null, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.textPropertyName, 1);
		p.rollback();
		dN = p.retrieve(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME, dN.getBizId());
		Assert.assertEquals("2", dN.getDocumentNumber());

		p.evictAllCached();
		p.begin();
		autonomousGenerator.next(null, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.textPropertyName, 1);
		p.rollback();
		dN = p.retrieve(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME, dN.getBizId());
		Assert.assertEquals("3", dN.getDocumentNumber());

		p.evictAllCached();
		p.begin();
		autonomousGenerator.next(null, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.textPropertyName, 1);
		p.rollback();
		dN = p.retrieve(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME, dN.getBizId());
		Assert.assertEquals("4", dN.getDocumentNumber());
	}
}
