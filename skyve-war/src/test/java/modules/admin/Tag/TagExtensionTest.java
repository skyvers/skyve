package modules.admin.Tag;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class TagExtensionTest {

	@Test
	void countDocumentWithNullModuleNameReturnsZero() {
		TagExtension tag = new TagExtension();
		assertEquals(0L, tag.countDocument(null, "someDocument"));
	}

	@Test
	void countDocumentWithNullDocumentNameReturnsZero() {
		TagExtension tag = new TagExtension();
		assertEquals(0L, tag.countDocument("admin", null));
	}

	@Test
	void countDocumentWithBothNullReturnsZero() {
		TagExtension tag = new TagExtension();
		assertEquals(0L, tag.countDocument(null, null));
	}
}
