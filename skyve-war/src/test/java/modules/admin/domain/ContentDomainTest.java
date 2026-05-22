package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Timestamp;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

public class ContentDomainTest extends AbstractH2Test {

	@Test
	@SuppressWarnings("static-method")
	void dataBuilderCreatesContent() throws Exception {
		Content bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(Content.MODULE_NAME, Content.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	@SuppressWarnings("static-method")
	void moduleAndDocumentNames() throws Exception {
		Content bean = new Content();
		assertEquals("admin", bean.getBizModule());
		assertEquals("Content", bean.getBizDocument());
	}

	@Test
	@SuppressWarnings("static-method")
	void contentIdSetAndGet() throws Exception {
		Content bean = new Content();
		bean.setContentId("test-content-id-123");
		assertEquals("test-content-id-123", bean.getContentId());
	}

	@Test
	@SuppressWarnings("static-method")
	void customerNameSetAndGet() throws Exception {
		Content bean = new Content();
		bean.setCustomerName("demo");
		assertEquals("demo", bean.getCustomerName());
	}

	@Test
	@SuppressWarnings("static-method")
	void moduleNameSetAndGet() throws Exception {
		Content bean = new Content();
		bean.setModuleName("admin");
		assertEquals("admin", bean.getModuleName());
	}

	@Test
	@SuppressWarnings("static-method")
	void documentNameSetAndGet() throws Exception {
		Content bean = new Content();
		bean.setDocumentName("Contact");
		assertEquals("Contact", bean.getDocumentName());
	}

	@Test
	@SuppressWarnings("static-method")
	void contentBizIdSetAndGet() throws Exception {
		Content bean = new Content();
		bean.setContentBizId("biz-id-456");
		assertEquals("biz-id-456", bean.getContentBizId());
	}

	@Test
	@SuppressWarnings("static-method")
	void attributeNameSetAndGet() throws Exception {
		Content bean = new Content();
		bean.setAttributeName("image");
		assertEquals("image", bean.getAttributeName());
	}

	@Test
	@SuppressWarnings("static-method")
	void lastModifiedSetAndGet() throws Exception {
		Content bean = new Content();
		Timestamp ts = new Timestamp();
		bean.setLastModified(ts);
		assertEquals(ts, bean.getLastModified());
	}

	@Test
	@SuppressWarnings("static-method")
	void contentTextSetAndGet() throws Exception {
		Content bean = new Content();
		bean.setContent("This is the content text");
		assertEquals("This is the content text", bean.getContent());
	}
}
