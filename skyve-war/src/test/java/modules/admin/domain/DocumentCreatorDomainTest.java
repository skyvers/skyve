package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class DocumentCreatorDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() throws Exception {
		DocumentCreator bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(DocumentCreator.MODULE_NAME, DocumentCreator.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() throws Exception {
		DocumentCreator bean = DocumentCreator.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("DocumentCreator", bean.getBizDocument());
	}

	@Test
	void outputLocationSetAndGet() throws Exception {
		DocumentCreator bean = DocumentCreator.newInstance();
		bean.setOutputLocation("/tmp/docs");
		assertEquals("/tmp/docs", bean.getOutputLocation());
	}

	@Test
	void scriptSetAndGet() throws Exception {
		DocumentCreator bean = DocumentCreator.newInstance();
		bean.setScript("def create() { }");
		assertEquals("def create() { }", bean.getScript());
	}
}
