package org.skyve.impl.metadata.repository.behaviour;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.MetaDataException;

@SuppressWarnings("static-method")
class BehaviourMetaDataTest {

	// ---- BizletMetaData ----

	@Test
	void bizletDefaultConstructorCreatesInstance() {
		BizletMetaData b = new BizletMetaData();
		assertNotNull(b);
	}

	@Test
	void bizletDocumentationRoundTrip() {
		BizletMetaData b = new BizletMetaData();
		b.setDocumentation("Some docs");
		assertEquals("Some docs", b.getDocumentation());
	}

	@Test
	void bizletDocumentationBlankBecomesNull() {
		BizletMetaData b = new BizletMetaData();
		b.setDocumentation("   ");
		assertNull(b.getDocumentation());
	}

	@Test
	void bizletLastModifiedMillisDefaultIsMaxValue() {
		BizletMetaData b = new BizletMetaData();
		assertEquals(Long.MAX_VALUE, b.getLastModifiedMillis());
	}

	@Test
	void bizletLastModifiedMillisRoundTrip() {
		BizletMetaData b = new BizletMetaData();
		b.setLastModifiedMillis(100L);
		assertEquals(100L, b.getLastModifiedMillis());
	}

	@Test
	void bizletLastCheckedMillisRoundTrip() {
		BizletMetaData b = new BizletMetaData();
		b.setLastCheckedMillis(200L);
		assertEquals(200L, b.getLastCheckedMillis());
	}

	@Test
	void bizletConvertReturnsSelf() {
		BizletMetaData b = new BizletMetaData();
		assertSame(b, b.convert("test"));
	}

	@Test
	void bizletPropertiesMapNotNull() {
		assertNotNull(new BizletMetaData().getProperties());
	}

	// ---- ActionMetaData ----

	@Test
	void actionDefaultConstructorCreatesInstance() {
		ActionMetaData a = new ActionMetaData();
		assertNotNull(a);
	}

	@Test
	void actionNameRoundTrip() {
		ActionMetaData a = new ActionMetaData();
		a.setName("myAction");
		assertEquals("myAction", a.getName());
	}

	@Test
	void actionNameBlankBecomesNull() {
		ActionMetaData a = new ActionMetaData();
		a.setName("  ");
		assertNull(a.getName());
	}

	@Test
	void actionDocumentationRoundTrip() {
		ActionMetaData a = new ActionMetaData();
		a.setDocumentation("Action docs");
		assertEquals("Action docs", a.getDocumentation());
	}

	@Test
	void actionStatementsListNotNull() {
		assertNotNull(new ActionMetaData().getStatements());
		assertTrue(new ActionMetaData().getStatements().isEmpty());
	}

	@Test
	void actionLastModifiedMillisDefaultIsMaxValue() {
		assertEquals(Long.MAX_VALUE, new ActionMetaData().getLastModifiedMillis());
	}

	@Test
	void actionLastModifiedMillisRoundTrip() {
		ActionMetaData a = new ActionMetaData();
		a.setLastModifiedMillis(99L);
		assertEquals(99L, a.getLastModifiedMillis());
	}

	@Test
	void actionLastCheckedMillisRoundTrip() {
		ActionMetaData a = new ActionMetaData();
		a.setLastCheckedMillis(88L);
		assertEquals(88L, a.getLastCheckedMillis());
	}

	@Test
	void actionConvertWithNameReturnsSelf() {
		ActionMetaData a = new ActionMetaData();
		a.setName("myAction");
		assertSame(a, a.convert("test"));
	}

	@Test
	void actionConvertWithNullNameThrows() {
		ActionMetaData a = new ActionMetaData();
		assertThrows(MetaDataException.class, () -> a.convert("test"));
	}

	@Test
	void actionPropertiesMapNotNull() {
		assertNotNull(new ActionMetaData().getProperties());
	}

	// ---- BizletMetaData lifecycle no-op methods ----

	@Test
	void bizletNewInstanceDoesNotThrow() {
		assertDoesNotThrow(() -> new BizletMetaData().newInstance(null));
	}

	@Test
	void bizletValidateDoesNotThrow() {
		assertDoesNotThrow(() -> new BizletMetaData().validate(null, null));
	}

	@Test
	void bizletGetConstantDomainValuesReturnsNull() throws Exception {
		assertNull(new BizletMetaData().getConstantDomainValues("attr"));
	}

	@Test
	void bizletGetVariantDomainValuesReturnsNull() throws Exception {
		assertNull(new BizletMetaData().getVariantDomainValues("attr"));
	}

	@Test
	void bizletGetDynamicDomainValuesReturnsNull() throws Exception {
		assertNull(new BizletMetaData().getDynamicDomainValues("attr", null));
	}

	@Test
	void bizletCompleteReturnsNull() throws Exception {
		assertNull(new BizletMetaData().complete("attr", "val", null));
	}

	@Test
	void bizletResolveReturnsNull() throws Exception {
		assertNull(new BizletMetaData().resolve("id", null));
	}

	@Test
	void bizletPreSaveDoesNotThrow() {
		assertDoesNotThrow(() -> new BizletMetaData().preSave(null));
	}

	@Test
	void bizletPostSaveDoesNotThrow() {
		assertDoesNotThrow(() -> new BizletMetaData().postSave(null));
	}

	@Test
	void bizletPreDeleteDoesNotThrow() {
		assertDoesNotThrow(() -> new BizletMetaData().preDelete(null));
	}

	@Test
	void bizletPostDeleteDoesNotThrow() {
		assertDoesNotThrow(() -> new BizletMetaData().postDelete(null));
	}

	@Test
	void bizletPostLoadDoesNotThrow() {
		assertDoesNotThrow(() -> new BizletMetaData().postLoad(null));
	}

	@Test
	void bizletPreRerenderDoesNotThrow() {
		assertDoesNotThrow(() -> new BizletMetaData().preRerender(null, null));
	}

	@Test
	void bizletPostRenderDoesNotThrow() {
		assertDoesNotThrow(() -> new BizletMetaData().postRender(null));
	}

	// ---- ActionMetaData.execute ----

	@Test
	void actionExecuteWithEmptyStatementsDoesNotThrow() {
		ActionMetaData a = new ActionMetaData();
		assertDoesNotThrow(() -> a.execute(null));
	}
}

