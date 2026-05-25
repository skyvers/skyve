package modules.admin;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.mock;

import java.lang.reflect.Field;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Document;

/**
 * Unit tests for LoggingInterceptor.
 * These tests exercise every before/after callback with default (non-vetoing) state.
 */
@SuppressWarnings("static-method")
class LoggingInterceptorTest {

	private LoggingInterceptor interceptor;

	@BeforeEach
	void setUp() {
		interceptor = new LoggingInterceptor();
	}

	@Test
	void beforeNewInstanceReturnsFalse() {
		assertFalse(interceptor.beforeNewInstance(null));
	}

	@Test
	void afterNewInstanceDoesNotThrow() {
		interceptor.afterNewInstance(null);
	}

	@Test
	void beforeValidateReturnsFalse() {
		assertFalse(interceptor.beforeValidate(null, null));
	}

	@Test
	void afterValidateDoesNotThrow() {
		interceptor.afterValidate(null, null);
	}

	@Test
	void beforeGetConstantDomainValuesReturnsFalse() {
		assertFalse(interceptor.beforeGetConstantDomainValues("attr"));
	}

	@Test
	void afterGetConstantDomainValuesDoesNotThrow() {
		interceptor.afterGetConstantDomainValues("attr", null);
	}

	@Test
	void beforeGetVariantDomainValuesReturnsFalse() {
		assertFalse(interceptor.beforeGetVariantDomainValues("attr"));
	}

	@Test
	void afterGetVariantDomainValuesDoesNotThrow() {
		interceptor.afterGetVariantDomainValues("attr", null);
	}

	@Test
	void beforeGetDynamicDomainValuesReturnsFalse() {
		assertFalse(interceptor.beforeGetDynamicDomainValues("attr", null));
	}

	@Test
	void afterGetDynamicDomainValuesDoesNotThrow() {
		interceptor.afterGetDynamicDomainValues("attr", null, null);
	}

	@Test
	void beforeSaveReturnsFalse() throws Exception {
		assertFalse(interceptor.beforeSave(null, null));
	}

	@Test
	void afterSaveDoesNotThrow() throws Exception {
		interceptor.afterSave(null, null);
	}

	@Test
	void beforePreSaveReturnsFalse() {
		assertFalse(interceptor.beforePreSave(null));
	}

	@Test
	void afterPreSaveDoesNotThrow() {
		interceptor.afterPreSave(null);
	}

	@Test
	void beforePostSaveReturnsFalse() {
		assertFalse(interceptor.beforePostSave(null));
	}

	@Test
	void afterPostSaveDoesNotThrow() {
		interceptor.afterPostSave(null);
	}

	@Test
	void beforeDeleteReturnsFalse() {
		assertFalse(interceptor.beforeDelete(null, null));
	}

	@Test
	void afterDeleteDoesNotThrow() {
		interceptor.afterDelete(null, null);
	}

	@Test
	void beforePreDeleteReturnsFalse() {
		assertFalse(interceptor.beforePreDelete(null));
	}

	@Test
	void afterPreDeleteDoesNotThrow() {
		interceptor.afterPreDelete(null);
	}

	@Test
	void beforePostLoadReturnsFalse() {
		assertFalse(interceptor.beforePostLoad(null));
	}

	@Test
	void afterPostLoadDoesNotThrow() {
		interceptor.afterPostLoad(null);
	}

	@Test
	void beforePreExecuteReturnsFalse() {
		assertFalse(interceptor.beforePreExecute(ImplicitActionName.Save, null, null, null));
	}

	@Test
	void afterPreExecuteDoesNotThrow() {
		interceptor.afterPreExecute(ImplicitActionName.Save, null, null, null);
	}

	@Test
	void beforeServerSideActionReturnsFalse() {
		Document doc = mock(Document.class);
		assertFalse(interceptor.beforeServerSideAction(doc, "TestAction", null, null));
	}

	@Test
	void afterServerSideActionDoesNotThrow() {
		Document doc = mock(Document.class);
		interceptor.afterServerSideAction(doc, "TestAction", null, null);
	}

	@Test
	void beforeUploadActionReturnsFalse() {
		Document doc = mock(Document.class);
		assertFalse(interceptor.beforeUploadAction(doc, "TestAction", null, null, null));
	}

	@Test
	void afterUploadActionDoesNotThrow() {
		Document doc = mock(Document.class);
		interceptor.afterUploadAction(doc, "TestAction", null, null, null);
	}

	@Test
	void beforeBizImportActionReturnsFalse() {
		Document doc = mock(Document.class);
		assertFalse(interceptor.beforeBizImportAction(doc, "TestAction", null, null));
	}

	@Test
	void afterBizImportActionDoesNotThrow() {
		Document doc = mock(Document.class);
		interceptor.afterBizImportAction(doc, "TestAction", null, null);
	}

	@Test
	void beforeBizExportActionReturnsFalse() {
		Document doc = mock(Document.class);
		assertFalse(interceptor.beforeBizExportAction(doc, "TestAction", null));
	}

	@Test
	void afterBizExportActionDoesNotThrow() {
		Document doc = mock(Document.class);
		interceptor.afterBizExportAction(doc, "TestAction", null, null);
	}

	@Test
	void vetoFieldMakesBeforeMethodsReturnTrue() throws Exception {
		Field vetoField = LoggingInterceptor.class.getDeclaredField("veto");
		vetoField.setAccessible(true);
		vetoField.set(interceptor, Boolean.TRUE);
		assert interceptor.beforeNewInstance(null) : "veto=true should return true";
		assert interceptor.beforePreSave(null) : "veto=true should return true";
	}
}
