package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.Tag.TagExtension;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class TaggedDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() {
		Tagged bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(Tagged.MODULE_NAME, Tagged.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() {
		Tagged bean = Tagged.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("Tagged", bean.getBizDocument());
	}

	@Test
	void taggedModuleSetAndGet() {
		Tagged bean = Tagged.newInstance();
		bean.setTaggedModule("admin");
		assertEquals("admin", bean.getTaggedModule());
	}

	@Test
	void taggedDocumentSetAndGet() {
		Tagged bean = Tagged.newInstance();
		bean.setTaggedDocument("User");
		assertEquals("User", bean.getTaggedDocument());
	}

	@Test
	void taggedBizIdSetAndGet() {
		Tagged bean = Tagged.newInstance();
		bean.setTaggedBizId("abc123");
		assertEquals("abc123", bean.getTaggedBizId());
	}

	@Test
	void tagSetAndGet() {
		Tagged bean = Tagged.newInstance();
		TagExtension tag = new TagExtension();
		tag.setName("MyTag");
		bean.setTag(tag);
		assertEquals(tag, bean.getTag());
	}

	@Test
	void tagSetToNullReturnsNull() {
		Tagged bean = Tagged.newInstance();
		bean.setTag(null);
		assertNull(bean.getTag());
	}

        @Test
        void getBizKeyNotNull() {
                Tagged bean = Tagged.newInstance();
                assertNotNull(bean.getBizKey());
        }

        @Test
        void tagSetSameValueNoOp() {
                Tagged bean = new Tagged();
                modules.admin.Tag.TagExtension tag = new modules.admin.Tag.TagExtension();
                bean.setTag(tag);
                bean.setTag(tag);
                assertEquals(tag, bean.getTag());
        }
}
