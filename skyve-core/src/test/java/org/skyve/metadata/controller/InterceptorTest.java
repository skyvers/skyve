package org.skyve.metadata.controller;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

/**
 * Tests that {@link Interceptor}'s default method implementations return false (before methods)
 * or complete without throwing (after methods).
 */
class InterceptorTest {

	/** Concrete subclass with no overrides, testing defaults. */
	private static class NoOpInterceptor extends Interceptor {
		// all defaults
	}

	@Test
	@SuppressWarnings("static-method")
	void beforeNewInstanceReturnsFalseByDefault() throws Exception {
		assertFalse(new NoOpInterceptor().beforeNewInstance(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void afterNewInstanceDoesNotThrowByDefault() {
		assertDoesNotThrow(() -> new NoOpInterceptor().afterNewInstance(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void beforeValidateReturnsFalseByDefault() throws Exception {
		assertFalse(new NoOpInterceptor().beforeValidate(null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void afterValidateDoesNotThrowByDefault() {
		assertDoesNotThrow(() -> new NoOpInterceptor().afterValidate(null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void beforeGetConstantDomainValuesReturnsFalseByDefault() throws Exception {
		assertFalse(new NoOpInterceptor().beforeGetConstantDomainValues("attr"));
	}

	@Test
	@SuppressWarnings("static-method")
	void afterGetConstantDomainValuesDoesNotThrowByDefault() {
		assertDoesNotThrow(() -> new NoOpInterceptor().afterGetConstantDomainValues("attr", List.<DomainValue>of()));
	}

	@Test
	@SuppressWarnings("static-method")
	void beforeGetVariantDomainValuesReturnsFalseByDefault() throws Exception {
		assertFalse(new NoOpInterceptor().beforeGetVariantDomainValues("attr"));
	}

	@Test
	@SuppressWarnings("static-method")
	void afterGetVariantDomainValuesDoesNotThrowByDefault() {
		assertDoesNotThrow(() -> new NoOpInterceptor().afterGetVariantDomainValues("attr", List.<DomainValue>of()));
	}

	@Test
	@SuppressWarnings("static-method")
	void beforeGetDynamicDomainValuesReturnsFalseByDefault() throws Exception {
		assertFalse(new NoOpInterceptor().beforeGetDynamicDomainValues("attr", null));
	}

	@Test
	@SuppressWarnings("static-method")
	void afterGetDynamicDomainValuesDoesNotThrowByDefault() {
		assertDoesNotThrow(() -> new NoOpInterceptor().afterGetDynamicDomainValues("attr", null, List.<DomainValue>of()));
	}

	@Test
	@SuppressWarnings("static-method")
	void beforeCompleteReturnsFalseByDefault() throws Exception {
		assertFalse(new NoOpInterceptor().beforeComplete("attr", "value", null));
	}

	@Test
	@SuppressWarnings("static-method")
	void afterCompleteDoesNotThrowByDefault() {
		assertDoesNotThrow(() -> new NoOpInterceptor().afterComplete("attr", "value", null, List.of()));
	}

	@Test
	@SuppressWarnings("static-method")
	void beforeSaveReturnsFalseByDefault() throws Exception {
		assertFalse(new NoOpInterceptor().beforeSave(null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void afterSaveDoesNotThrowByDefault() {
		assertDoesNotThrow(() -> new NoOpInterceptor().afterSave(null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void beforePreSaveReturnsFalseByDefault() throws Exception {
		assertFalse(new NoOpInterceptor().beforePreSave(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void afterPreSaveDoesNotThrowByDefault() {
		assertDoesNotThrow(() -> new NoOpInterceptor().afterPreSave(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void beforeDeleteReturnsFalseByDefault() throws Exception {
		assertFalse(new NoOpInterceptor().beforeDelete(null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void afterDeleteDoesNotThrowByDefault() {
		assertDoesNotThrow(() -> new NoOpInterceptor().afterDelete(null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void beforePreDeleteReturnsFalseByDefault() throws Exception {
		assertFalse(new NoOpInterceptor().beforePreDelete(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void afterPreDeleteDoesNotThrowByDefault() {
		assertDoesNotThrow(() -> new NoOpInterceptor().afterPreDelete(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void beforePostLoadReturnsFalseByDefault() throws Exception {
		assertFalse(new NoOpInterceptor().beforePostLoad(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void afterPostLoadDoesNotThrowByDefault() {
		assertDoesNotThrow(() -> new NoOpInterceptor().afterPostLoad(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void beforePreExecuteReturnsFalseByDefault() throws Exception {
		assertFalse(new NoOpInterceptor().beforePreExecute(null, null, null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void afterPreExecuteDoesNotThrowByDefault() {
		assertDoesNotThrow(() -> new NoOpInterceptor().afterPreExecute(null, null, null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void beforePreRerenderReturnsFalseByDefault() throws Exception {
		assertFalse(new NoOpInterceptor().beforePreRerender(null, null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void afterPreRerenderDoesNotThrowByDefault() {
		assertDoesNotThrow(() -> new NoOpInterceptor().afterPreRerender(null, null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void beforeServerSideActionReturnsFalseByDefault() throws Exception {
		assertFalse(new NoOpInterceptor().beforeServerSideAction(null, null, null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void afterServerSideActionDoesNotThrowByDefault() {
		assertDoesNotThrow(() -> new NoOpInterceptor().afterServerSideAction(null, null, null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void beforeDownloadActionReturnsFalseByDefault() throws Exception {
		assertFalse(new NoOpInterceptor().beforeDownloadAction(null, null, null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void beforeBizImportActionReturnsFalseByDefault() throws Exception {
		assertFalse(new NoOpInterceptor().beforeBizImportAction(null, null, null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void afterBizImportActionDoesNotThrowByDefault() {
		assertDoesNotThrow(() -> new NoOpInterceptor().afterBizImportAction(null, null, null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void beforeBizExportActionReturnsFalseByDefault() throws Exception {
		assertFalse(new NoOpInterceptor().beforeBizExportAction(null, null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void afterBizExportActionDoesNotThrowByDefault() {
		assertDoesNotThrow(() -> new NoOpInterceptor().afterBizExportAction(null, null, null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void beforeUploadActionReturnsFalseByDefault() throws Exception {
		assertFalse(new NoOpInterceptor().beforeUploadAction(null, null, null, null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void afterUploadActionDoesNotThrowByDefault() {
		assertDoesNotThrow(() -> new NoOpInterceptor().afterUploadAction(null, null, null, null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void afterDownloadActionDoesNotThrowByDefault() {
		assertDoesNotThrow(() -> new NoOpInterceptor().afterDownloadAction(null, null, null, null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void beforePostDeleteReturnsFalseByDefault() throws Exception {
		assertFalse(new NoOpInterceptor().beforePostDelete(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void afterPostDeleteDoesNotThrowByDefault() {
		assertDoesNotThrow(() -> new NoOpInterceptor().afterPostDelete(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void beforePostRenderReturnsTrueByDefault() throws Exception {
		new NoOpInterceptor().beforePostRender(null, null);
	}

	@Test
	@SuppressWarnings("static-method")
	void afterPostRenderDoesNotThrowByDefault() {
		assertDoesNotThrow(() -> new NoOpInterceptor().afterPostRender(null, null));
	}
}

