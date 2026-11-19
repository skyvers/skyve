package modules.admin;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import java.lang.reflect.Field;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Document;

/**
 * Unit tests for LoggingInterceptor.
 * These tests exercise every before/after callback with default (non-vetoing) state.
 */
class LoggingInterceptorTest {

	private LoggingInterceptor interceptor;

	@BeforeEach
	void setUp() {
		interceptor = new LoggingInterceptor();
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void beforeNewInstanceReturnsFalse() {
		assertFalse(interceptor.beforeNewInstance(null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void afterNewInstanceDoesNotThrow() {
		Assertions.assertDoesNotThrow(() -> interceptor.afterNewInstance(null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void beforeValidateReturnsFalse() {
		assertFalse(interceptor.beforeValidate(null, null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void afterValidateDoesNotThrow() {
		Assertions.assertDoesNotThrow(() -> interceptor.afterValidate(null, null));
	}

	@Test
	void beforeGetConstantDomainValuesReturnsFalse() {
		assertFalse(interceptor.beforeGetConstantDomainValues("attr"));
	}

	@Test
	void afterGetConstantDomainValuesDoesNotThrow() {
		Assertions.assertDoesNotThrow(() -> interceptor.afterGetConstantDomainValues("attr", null));
	}

	@Test
	void beforeGetVariantDomainValuesReturnsFalse() {
		assertFalse(interceptor.beforeGetVariantDomainValues("attr"));
	}

	@Test
	void afterGetVariantDomainValuesDoesNotThrow() {
		Assertions.assertDoesNotThrow(() -> interceptor.afterGetVariantDomainValues("attr", null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void beforeGetDynamicDomainValuesReturnsFalse() {
		assertFalse(interceptor.beforeGetDynamicDomainValues("attr", null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void afterGetDynamicDomainValuesDoesNotThrow() {
		Assertions.assertDoesNotThrow(() -> interceptor.afterGetDynamicDomainValues("attr", null, null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void beforeSaveReturnsFalse() throws Exception {
		assertFalse(interceptor.beforeSave(null, null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void afterSaveDoesNotThrow() {
		Assertions.assertDoesNotThrow(() -> interceptor.afterSave(null, null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void beforePreSaveReturnsFalse() {
		assertFalse(interceptor.beforePreSave(null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void afterPreSaveDoesNotThrow() {
		Assertions.assertDoesNotThrow(() -> interceptor.afterPreSave(null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void beforePostSaveReturnsFalse() {
		assertFalse(interceptor.beforePostSave(null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void afterPostSaveDoesNotThrow() {
		Assertions.assertDoesNotThrow(() -> interceptor.afterPostSave(null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void beforeDeleteReturnsFalse() {
		assertFalse(interceptor.beforeDelete(null, null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void afterDeleteDoesNotThrow() {
		Assertions.assertDoesNotThrow(() -> interceptor.afterDelete(null, null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void beforePreDeleteReturnsFalse() {
		assertFalse(interceptor.beforePreDelete(null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void afterPreDeleteDoesNotThrow() {
		Assertions.assertDoesNotThrow(() -> interceptor.afterPreDelete(null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void beforePostLoadReturnsFalse() {
		assertFalse(interceptor.beforePostLoad(null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void afterPostLoadDoesNotThrow() {
		Assertions.assertDoesNotThrow(() -> interceptor.afterPostLoad(null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void beforePreExecuteReturnsFalse() {
		assertFalse(interceptor.beforePreExecute(ImplicitActionName.Save, null, null, null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void afterPreExecuteDoesNotThrow() {
		Assertions.assertDoesNotThrow(() -> interceptor.afterPreExecute(ImplicitActionName.Save, null, null, null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void beforeServerSideActionReturnsFalse() {
		Document doc = mock(Document.class);
		assertFalse(interceptor.beforeServerSideAction(doc, "TestAction", null, null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void afterServerSideActionDoesNotThrow() {
		Document doc = mock(Document.class);
		Assertions.assertDoesNotThrow(() -> interceptor.afterServerSideAction(doc, "TestAction", null, null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void beforeUploadActionReturnsFalse() {
		Document doc = mock(Document.class);
		assertFalse(interceptor.beforeUploadAction(doc, "TestAction", null, null, null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void afterUploadActionDoesNotThrow() {
		Document doc = mock(Document.class);
		Assertions.assertDoesNotThrow(() -> interceptor.afterUploadAction(doc, "TestAction", null, null, null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void beforeBizImportActionReturnsFalse() {
		Document doc = mock(Document.class);
		assertFalse(interceptor.beforeBizImportAction(doc, "TestAction", null, null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void afterBizImportActionDoesNotThrow() {
		Document doc = mock(Document.class);
		Assertions.assertDoesNotThrow(() -> interceptor.afterBizImportAction(doc, "TestAction", null, null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void beforeBizExportActionReturnsFalse() {
		Document doc = mock(Document.class);
		assertFalse(interceptor.beforeBizExportAction(doc, "TestAction", null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void afterBizExportActionDoesNotThrow() {
		Document doc = mock(Document.class);
		Assertions.assertDoesNotThrow(() -> interceptor.afterBizExportAction(doc, "TestAction", null, null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void vetoFieldMakesBeforeMethodsReturnTrue() throws Exception {
		Field vetoField = Assertions.assertDoesNotThrow(() -> LoggingInterceptor.class.getDeclaredField("veto"));
		vetoField.setAccessible(true);
		vetoField.set(interceptor, Boolean.TRUE);
		assertTrue(interceptor.beforeNewInstance(null), "veto=true should return true");
		assertTrue(interceptor.beforePreSave(null), "veto=true should return true");
	}
}
