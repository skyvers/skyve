package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class TaggedDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() throws Exception {
		Tagged bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(Tagged.MODULE_NAME, Tagged.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() throws Exception {
		Tagged bean = Tagged.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("Tagged", bean.getBizDocument());
	}

	@Test
	void taggedModuleSetAndGet() throws Exception {
		Tagged bean = Tagged.newInstance();
		bean.setTaggedModule("admin");
		assertEquals("admin", bean.getTaggedModule());
	}

	@Test
	void taggedDocumentSetAndGet() throws Exception {
		Tagged bean = Tagged.newInstance();
		bean.setTaggedDocument("User");
		assertEquals("User", bean.getTaggedDocument());
	}

	@Test
	void taggedBizIdSetAndGet() throws Exception {
		Tagged bean = Tagged.newInstance();
		bean.setTaggedBizId("abc123");
		assertEquals("abc123", bean.getTaggedBizId());
	}
}
