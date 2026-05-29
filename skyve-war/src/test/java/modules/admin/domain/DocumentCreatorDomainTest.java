package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class DocumentCreatorDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() {
		DocumentCreator bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(DocumentCreator.MODULE_NAME, DocumentCreator.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() {
		DocumentCreator bean = DocumentCreator.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("DocumentCreator", bean.getBizDocument());
	}

	@Test
	void outputLocationSetAndGet() {
		DocumentCreator bean = DocumentCreator.newInstance();
		bean.setOutputLocation("/tmp/docs");
		assertEquals("/tmp/docs", bean.getOutputLocation());
	}

	@Test
	void scriptSetAndGet() {
		DocumentCreator bean = DocumentCreator.newInstance();
		bean.setScript("def create() { }");
		assertEquals("def create() { }", bean.getScript());
	}

	@Test
	void documentPreviewSetAndGet() {
		DocumentCreator bean = DocumentCreator.newInstance();
		bean.setDocumentPreview("<html/>");
		assertEquals("<html/>", bean.getDocumentPreview());
	}

	@Test
	void markdownPreviewSetAndGet() {
		DocumentCreator bean = DocumentCreator.newInstance();
		bean.setMarkdownPreview("# Title");
		assertEquals("# Title", bean.getMarkdownPreview());
	}

	@Test
	void errorsSetAndGet() {
		DocumentCreator bean = DocumentCreator.newInstance();
		bean.setErrors("some error");
		assertEquals("some error", bean.getErrors());
	}

	@Test
	void defaultModuleSetAndGet() {
		DocumentCreator bean = DocumentCreator.newInstance();
		bean.setDefaultModule("admin");
		assertEquals("admin", bean.getDefaultModule());
	}

	@Test
	void isHasErrorsWhenErrorsSet() {
		DocumentCreator bean = DocumentCreator.newInstance();
		bean.setErrors("error occurred");
		assertTrue(bean.isHasErrors());
		assertFalse(bean.isNotHasErrors());
	}

	@Test
	void isNotHasErrorsWhenNoErrors() {
		DocumentCreator bean = DocumentCreator.newInstance();
		assertFalse(bean.isHasErrors());
		assertTrue(bean.isNotHasErrors());
	}
}
