package modules.admin.Tag;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
public class TagDefaultActionTest {

	@Test
	void fromCodeWithValidCodeReturnsEnum() {
		assertEquals(TagDefaultAction.tagUpsert, TagDefaultAction.fromCode("SkyveUpsert"));
	}

	@Test
	void fromCodeWithInvalidCodeReturnsNull() {
		assertNull(TagDefaultAction.fromCode("unknownCode"));
	}

	@Test
	void toCodeReturnsExpectedCode() {
		assertEquals("SkyveUpsert", TagDefaultAction.tagUpsert.toCode());
		assertEquals("SkyveResave", TagDefaultAction.tagResave.toCode());
		assertEquals("SkyveDelete", TagDefaultAction.tagDelete.toCode());
		assertEquals("SkyveValidate", TagDefaultAction.tagValidate.toCode());
	}

	@Test
	void isDefaultTagActionWithValidCodeReturnsTrue() {
		assertTrue(TagDefaultAction.isDefaultTagAction("SkyveUpsert"));
	}

	@Test
	void isDefaultTagActionWithInvalidCodeReturnsFalse() {
		assertFalse(TagDefaultAction.isDefaultTagAction("unknownAction"));
	}

	@Test
	void toDomainValuesReturnsNonEmptyList() {
		var values = TagDefaultAction.toDomainValues();
		assertNotNull(values);
		assertFalse(values.isEmpty());
	}
}
