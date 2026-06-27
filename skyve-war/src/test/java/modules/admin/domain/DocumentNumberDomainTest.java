package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class DocumentNumberDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() {
		DocumentNumber bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() {
		DocumentNumber bean = DocumentNumber.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("DocumentNumber", bean.getBizDocument());
	}

	@Test
	void moduleNameSetAndGet() {
		DocumentNumber bean = DocumentNumber.newInstance();
		bean.setModuleName("myModule");
		assertEquals("myModule", bean.getModuleName());
	}

	@Test
	void documentNameSetAndGet() {
		DocumentNumber bean = DocumentNumber.newInstance();
		bean.setDocumentName("myDocument");
		assertEquals("myDocument", bean.getDocumentName());
	}

	@Test
	void sequenceNameSetAndGet() {
		DocumentNumber bean = DocumentNumber.newInstance();
		bean.setSequenceName("mySequence");
		assertEquals("mySequence", bean.getSequenceName());
	}

        @Test
        void getBizKeyNotNull() {
                DocumentNumber bean = DocumentNumber.newInstance();
                assertNotNull(bean.getBizKey());
        }

        @Test
        void documentNumberSetAndGet() {
                DocumentNumber bean = DocumentNumber.newInstance();
                bean.setDocumentNumber("00042");
                assertEquals("00042", bean.getDocumentNumber());
        }
}
