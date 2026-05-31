package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.primefaces.event.FileUploadEvent;
import org.primefaces.model.file.UploadedFile;
import org.skyve.impl.util.UtilImpl;

import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;

@SuppressWarnings("static-method")
class UploadViewsStateTest {
	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext context) {
			setCurrentInstance(context);
		}
	}

	@AfterEach
	void clearFacesContext() {
		FacesContextBridge.setCurrent(null);
	}

	@Test
	void contentUploadPostConstructSetsSanitisedStateWithoutSession() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		ContentUploadView view = new ContentUploadView();
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(AbstractUploadView.class, view, "binding", "beanBinding");
		setField(ContentUploadView.class, view, "contentBinding", "contentId");

		view.postConstruct();

		assertEquals("ctx", view.getContext());
		assertEquals("beanBinding", view.getBinding());
		assertEquals("contentId", view.getContentBinding());
		assertFalse(view.isCanAccess());
		assertTrue(view.getBaseHref() != null);
	}

	@Test
	void fileUploadDefaultsAndNoAccessBranchAreStable() {
		FileUploadView view = new FileUploadView();
		FileUploadEvent event = Mockito.mock(FileUploadEvent.class);

		assertNull(view.getAction());
		assertTrue(view.getProblems().isEmpty());
		assertEquals(UtilImpl.UPLOADS_FILE_WHITELIST_REGEX, view.getWhitelistRegex());
		assertEquals(UtilImpl.UPLOADS_FILE_MAXIMUM_SIZE_IN_MB * AbstractUploadView.MB_IN_BYTES,
					 view.getMaximumSizeInBytes());

		assertDoesNotThrow(() -> view.handleFileUpload(event));
		verifyNoInteractions(event);
	}

	@Test
	void bizportImportDefaultsRemainStable() {
		BizportImportView view = new BizportImportView();

		assertNull(view.getAction());
		assertTrue(view.getProblems().isEmpty());
		assertEquals(UtilImpl.UPLOADS_BIZPORT_WHITELIST_REGEX, view.getWhitelistRegex());
		assertEquals(UtilImpl.UPLOADS_BIZPORT_MAXIMUM_SIZE_IN_MB * AbstractUploadView.MB_IN_BYTES,
					 view.getMaximumSizeInBytes());
	}

	@Test
	void fileUploadHandleFileUploadShowsMalformedUrlMessageWhenContextMissing() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		FacesContextBridge.setCurrent(context);
		FileUploadView view = new FileUploadView();
		FileUploadEvent event = Mockito.mock(FileUploadEvent.class);
		UploadedFile file = Mockito.mock(UploadedFile.class);

		setField(AbstractUploadView.class, view, "canAccess", Boolean.TRUE);
		when(event.getFile()).thenReturn(file);
		doReturn(Long.valueOf(1024L)).when(file).getSize();
		when(file.getFileName()).thenReturn("ok.txt");

		assertDoesNotThrow(() -> view.handleFileUpload(event));

		verify(context).addMessage(isNull(), any());
	}

	@Test
	void bizportImportHandleFileUploadShowsMalformedUrlMessageWhenContextMissing() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		FacesContextBridge.setCurrent(context);
		BizportImportView view = new BizportImportView();
		FileUploadEvent event = Mockito.mock(FileUploadEvent.class);
		UploadedFile file = Mockito.mock(UploadedFile.class);

		when(event.getFile()).thenReturn(file);
		doReturn(Long.valueOf(1024L)).when(file).getSize();
		when(file.getFileName()).thenReturn("import.xlsx");

		assertDoesNotThrow(() -> view.handleFileUpload(event));

		verify(context).addMessage(isNull(), any());
	}

	private static void setField(Class<?> owner, Object target, String name, Object value) throws Exception {
		Field field = owner.getDeclaredField(name);
		field.setAccessible(true);
		field.set(target, value);
	}
}
