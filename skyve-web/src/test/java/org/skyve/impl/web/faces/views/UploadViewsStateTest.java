package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.stream.Stream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.primefaces.event.FileUploadEvent;
import org.primefaces.model.file.UploadedFile;
import org.skyve.cache.ConversationCacheConfig;
import org.skyve.cache.SessionCacheConfig;
import org.skyve.content.AttachmentContent;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.messages.UploadException;
import org.skyve.impl.cache.DefaultCaching;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.metadata.view.widget.bound.input.ContentCapture;
import org.skyve.impl.metadata.view.widget.bound.input.ContentDisplay;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.views.UnifiedUploadState.UploadAffordance;
import org.skyve.impl.web.faces.views.UnifiedUploadState.UploadCategory;
import org.skyve.impl.web.faces.views.UnifiedUploadState.UploadKind;
import org.skyve.impl.web.faces.views.UnifiedUploadState.UploadMode;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

import jakarta.faces.application.FacesMessage;
import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

@SuppressWarnings("static-method")
class UploadViewsStateTest {
	private static final class TestUploadView extends AbstractUploadView {
		private static final long serialVersionUID = -6633082540979492136L;

		TestUploadView(String whitelistRegex, int maximumSizeMB) {
			super(whitelistRegex, maximumSizeMB);
		}

		boolean isValidFile(UploadedFile file, FacesContext context) {
			return validFile(file, context);
		}
	}

	private static final class TestContentUploadView extends ContentUploadView {
		private static final long serialVersionUID = 742298348576624144L;

		TestContentUploadView() {
			super();
		}

		TestContentUploadView(String whitelistRegex, int maximumSizeMB) {
			super(whitelistRegex, maximumSizeMB);
		}

		boolean isValidFile(UploadedFile file, FacesContext context) {
			return validFile(file, context);
		}
	}

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
		setField(ContentUploadView.class, view, "companionBinding", "_contentId");

		view.postConstruct();

		assertEquals("ctx", view.getContext());
		assertEquals("beanBinding", view.getBinding());
		assertEquals("contentId", view.getContentBinding());
		assertFalse(view.isCanAccess());
		assertNotNull(view.getBaseHref());
	}

	@Test
	void contentUploadPostConstructDetectsSessionUserAccess() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		HttpSession session = Mockito.mock(HttpSession.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(session);
		when(session.getAttribute(org.skyve.web.WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(new Object());
		FacesContextBridge.setCurrent(context);

		ContentUploadView view = new ContentUploadView();
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(ContentUploadView.class, view, "contentBinding", "contentId");

		view.postConstruct();

		assertTrue(view.isCanAccess());
	}

	@Test
	void contentUploadSuccessScriptUpdatesSmartClientCompanionBeforeContentId() throws Exception {
		ContentUploadView view = new ContentUploadView();
		setField(ContentUploadView.class, view, "companionBinding", "_attachment_media");
		Bean bean = Mockito.mock(Bean.class);
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("Contact");
		AttachmentContent content = new AttachmentContent("demo", "admin", "Contact", null, "user", "biz", "attachment")
				.attachment("clip.mp4", "video/mp4", new byte[] {1});

		String script = view.createUploadSuccessScript(bean, content, "content-123", "attachment");

		int companionIndex = script.indexOf("setValue('_attachment_media','video')");
		int contentIndex = script.indexOf("setValue('attachment','content-123')");
		assertTrue(script.contains("var skyveUploadWindow=SKYVE.Util.findSkyveWindow();"), script);
		assertTrue(script.contains("if(skyveUploadWindow){"), script);
		assertTrue(script.contains("skyveUploadWindow.isc.BizUtil.afterContentUpload('attachment','content-123','admin.Contact','clip.mp4','video','_attachment_media')"), script);
		assertTrue(script.contains("isc.BizUtil.afterContentUpload('attachment','content-123','admin.Contact','clip.mp4','video','_attachment_media')"), script);
		assertTrue(companionIndex >= 0, script);
		assertTrue(contentIndex > companionIndex, script);
		assertTrue(script.contains("WindowStack.popoff(false)"), script);
		assertTrue(script.contains("skyveUploadWindow.SKYVE.PF.afterContentUpload('attachment','content-123','admin.Contact','clip.mp4','video','_attachment_media')"), script);
		assertFalse(script.contains("skyveUploadWindow=skyveUploadWindow.parent"), script);
		assertFalse(script.contains("window.parent.isc"), script);
		assertFalse(script.contains("top.SKYVE"), script);
	}

	@Test
	void contentUploadSuccessScriptOmitsCompanionAndEscapesFileNameWhenNoCompanionBinding() {
		ContentUploadView view = new ContentUploadView();
		Bean bean = Mockito.mock(Bean.class);
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("Contact");
		AttachmentContent content = new AttachmentContent("demo", "admin", "Contact", null, "user", "biz", "attachment")
				.attachment("quote's.png", "image/png", new byte[] {1});

		String script = view.createUploadSuccessScript(bean, content, "content-456", "attachment");

		assertFalse(script.contains("_attachment_media"), script);
		assertTrue(script.contains("setValue('attachment','content-456')"), script);
		assertTrue(script.contains("isc.BizUtil.afterContentUpload('attachment','content-456','admin.Contact','quote\\'s.png','image')"), script);
		assertTrue(script.contains("skyveUploadWindow.SKYVE.PF.afterContentUpload('attachment','content-456','admin.Contact','quote\\'s.png','image')"), script);
		assertFalse(script.contains("window.parent.isc"), script);
		assertFalse(script.contains("top.SKYVE"), script);
	}

	@Test
	void unifiedImageUploadPostConstructSetsSanitisedStateWithoutSession() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		ContentUploadView view = new ContentUploadView();
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(AbstractUploadView.class, view, "binding", "beanBinding");
		setField(ContentUploadView.class, view, "contentBinding", "contentId");
		setField(ContentUploadView.class, view, "display", "image");

		view.postConstruct();

		assertEquals("ctx", view.getContext());
		assertEquals("beanBinding", view.getBinding());
		assertEquals("contentId", view.getContentBinding());
		assertFalse(view.isCanAccess());
		assertEquals(UtilImpl.UPLOADS_IMAGE_WHITELIST_REGEX, view.getWhitelistRegex());
		assertEquals(UtilImpl.UPLOADS_IMAGE_MAXIMUM_SIZE_IN_MB * AbstractUploadView.MB_IN_BYTES,
					 view.getMaximumSizeInBytes());
	}

	@Test
	void unifiedUploadPostConstructSelectsImageLimitsFromRouteState() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		ContentUploadView view = new ContentUploadView();
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(ContentUploadView.class, view, "contentBinding", "photo");
		setField(ContentUploadView.class, view, "uploadKind", "boundContent");
		setField(ContentUploadView.class, view, "display", "image");

		view.postConstruct();

		assertSame(ContentDisplay.image, view.getDisplay());
		assertTrue(view.isImageUpload());
		assertFalse(view.isVideoUpload());
		assertTrue(view.isGenericUploadAllowed());
		assertFalse(view.isCameraUploadAllowed());
		assertFalse(view.isVideoUploadAllowed());
		assertEquals("image/*", view.getAccept());
		assertNull(view.getCaptureHint());
		assertEquals(UtilImpl.UPLOADS_IMAGE_WHITELIST_REGEX, view.getWhitelistRegex());
		assertEquals(UtilImpl.UPLOADS_IMAGE_MAXIMUM_SIZE_IN_MB * AbstractUploadView.MB_IN_BYTES,
					 view.getMaximumSizeInBytes());
	}

	@Test
	void unifiedUploadPostConstructSelectsImageLimitsForCameraCaptureFallback() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		ContentUploadView view = new ContentUploadView();
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(ContentUploadView.class, view, "contentBinding", "photo");
		setField(ContentUploadView.class, view, "uploadKind", "boundContent");
		setField(ContentUploadView.class, view, "display", "auto");
		setField(ContentUploadView.class, view, "capture", "camera");

		view.postConstruct();

		assertSame(ContentCapture.camera, view.getCapture());
		assertEquals("camera", view.getUploadAffordance());
		assertTrue(view.isImageUpload());
		assertFalse(view.isGenericUploadAllowed());
		assertTrue(view.isCameraUploadAllowed());
		assertFalse(view.isVideoUploadAllowed());
		assertEquals("image/*", view.getAccept());
		assertEquals("environment", view.getCaptureHint());
		assertEquals(UtilImpl.UPLOADS_IMAGE_WHITELIST_REGEX, view.getWhitelistRegex());
		assertEquals(UtilImpl.UPLOADS_IMAGE_MAXIMUM_SIZE_IN_MB * AbstractUploadView.MB_IN_BYTES,
					 view.getMaximumSizeInBytes());
	}

	@Test
	void unifiedUploadPostConstructSelectsVideoLimitsForVideoCaptureFallback() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		ContentUploadView view = new ContentUploadView();
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(ContentUploadView.class, view, "contentBinding", "clip");
		setField(ContentUploadView.class, view, "uploadKind", "boundContent");
		setField(ContentUploadView.class, view, "display", "auto");
		setField(ContentUploadView.class, view, "capture", "video");

		view.postConstruct();

		assertSame(ContentCapture.video, view.getCapture());
		assertEquals("video", view.getUploadAffordance());
		assertFalse(view.isImageUpload());
		assertTrue(view.isVideoUpload());
		assertFalse(view.isGenericUploadAllowed());
		assertFalse(view.isCameraUploadAllowed());
		assertTrue(view.isVideoUploadAllowed());
		assertEquals("video/*", view.getAccept());
		assertEquals("camcorder", view.getCaptureHint());
		assertEquals(UtilImpl.UPLOADS_VIDEO_WHITELIST_REGEX, view.getWhitelistRegex());
		assertEquals(UtilImpl.UPLOADS_VIDEO_MAXIMUM_SIZE_IN_MB * AbstractUploadView.MB_IN_BYTES,
					 view.getMaximumSizeInBytes());
	}

	@Test
	void unifiedUploadPostConstructSelectsGenericContentLimitsForLinkRoute() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		ContentUploadView view = new ContentUploadView();
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(ContentUploadView.class, view, "contentBinding", "attachment");
		setField(ContentUploadView.class, view, "uploadKind", "boundContent");
		setField(ContentUploadView.class, view, "display", "link");

		view.postConstruct();

		assertSame(ContentDisplay.link, view.getDisplay());
		assertFalse(view.isImageUpload());
		assertFalse(view.isVideoUpload());
		assertTrue(view.isGenericUploadAllowed());
		assertFalse(view.isCameraUploadAllowed());
		assertFalse(view.isVideoUploadAllowed());
		assertNull(view.getAccept());
		assertNull(view.getCaptureHint());
		assertEquals(UtilImpl.UPLOADS_CONTENT_WHITELIST_REGEX, view.getWhitelistRegex());
		assertEquals(UtilImpl.UPLOADS_CONTENT_MAXIMUM_SIZE_IN_MB * AbstractUploadView.MB_IN_BYTES,
					 view.getMaximumSizeInBytes());
	}

	@Test
	void captureAllAutoRouteExposesGenericCameraAndVideoAffordances() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		ContentUploadView view = new ContentUploadView();
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(ContentUploadView.class, view, "contentBinding", "attachment");
		setField(ContentUploadView.class, view, "uploadKind", "boundContent");
		setField(ContentUploadView.class, view, "display", "auto");
		setField(ContentUploadView.class, view, "capture", "all");

		view.postConstruct();

		assertSame(ContentCapture.all, view.getCapture());
		assertEquals("generic", view.getUploadAffordance());
		assertTrue(view.isGenericUploadAllowed());
		assertTrue(view.isCameraUploadAllowed());
		assertTrue(view.isVideoUploadAllowed());
		assertFalse(view.isImageUpload());
		assertFalse(view.isVideoUpload());
		assertNull(view.getAccept());
		assertNull(view.getCaptureHint());
		assertEquals(UtilImpl.UPLOADS_CONTENT_WHITELIST_REGEX, view.getWhitelistRegex());
		assertEquals(UtilImpl.UPLOADS_CONTENT_MAXIMUM_SIZE_IN_MB * AbstractUploadView.MB_IN_BYTES,
					 view.getMaximumSizeInBytes());
	}

	@Test
	void captureAllLinkRouteExposesGenericCameraAndVideoAffordances() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		ContentUploadView view = new ContentUploadView();
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(ContentUploadView.class, view, "contentBinding", "attachment");
		setField(ContentUploadView.class, view, "uploadKind", "boundContent");
		setField(ContentUploadView.class, view, "display", "link");
		setField(ContentUploadView.class, view, "capture", "all");

		view.postConstruct();

		assertSame(ContentDisplay.link, view.getDisplay());
		assertSame(ContentCapture.all, view.getCapture());
		assertEquals("generic", view.getUploadAffordance());
		assertTrue(view.isGenericUploadAllowed());
		assertTrue(view.isCameraUploadAllowed());
		assertTrue(view.isVideoUploadAllowed());
		assertFalse(view.isImageUpload());
		assertFalse(view.isVideoUpload());
		assertNull(view.getAccept());
		assertNull(view.getCaptureHint());
		assertEquals(UtilImpl.UPLOADS_CONTENT_WHITELIST_REGEX, view.getWhitelistRegex());
		assertEquals(UtilImpl.UPLOADS_CONTENT_MAXIMUM_SIZE_IN_MB * AbstractUploadView.MB_IN_BYTES,
					 view.getMaximumSizeInBytes());
		assertEquals(UtilImpl.UPLOADS_IMAGE_MAXIMUM_SIZE_IN_MB * AbstractUploadView.MB_IN_BYTES,
					 view.getCameraMaximumSizeInBytes());
	}

	@Test
	void captureAllAutoRouteSelectedCameraAffordanceUsesImageLimits() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		ContentUploadView view = new ContentUploadView();
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(ContentUploadView.class, view, "contentBinding", "attachment");
		setField(ContentUploadView.class, view, "uploadKind", "boundContent");
		setField(ContentUploadView.class, view, "display", "auto");
		setField(ContentUploadView.class, view, "capture", "all");
		setField(ContentUploadView.class, view, "uploadAffordance", "camera");

		view.postConstruct();

		assertEquals("camera", view.getUploadAffordance());
		assertTrue(view.isImageUpload());
		assertFalse(view.isVideoUpload());
		assertEquals("image/*", view.getAccept());
		assertEquals("environment", view.getCaptureHint());
		assertEquals(UtilImpl.UPLOADS_IMAGE_WHITELIST_REGEX, view.getWhitelistRegex());
		assertEquals(UtilImpl.UPLOADS_IMAGE_MAXIMUM_SIZE_IN_MB * AbstractUploadView.MB_IN_BYTES,
					 view.getMaximumSizeInBytes());
	}

	@Test
	void captureAllAutoRouteSelectedVideoAffordanceUsesVideoLimits() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		ContentUploadView view = new ContentUploadView();
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(ContentUploadView.class, view, "contentBinding", "attachment");
		setField(ContentUploadView.class, view, "uploadKind", "boundContent");
		setField(ContentUploadView.class, view, "display", "auto");
		setField(ContentUploadView.class, view, "capture", "all");
		setField(ContentUploadView.class, view, "uploadAffordance", "video");

		view.postConstruct();

		assertEquals("video", view.getUploadAffordance());
		assertFalse(view.isImageUpload());
		assertTrue(view.isVideoUpload());
		assertEquals("video/*", view.getAccept());
		assertEquals("camcorder", view.getCaptureHint());
		assertEquals(UtilImpl.UPLOADS_VIDEO_WHITELIST_REGEX, view.getWhitelistRegex());
		assertEquals(UtilImpl.UPLOADS_VIDEO_MAXIMUM_SIZE_IN_MB * AbstractUploadView.MB_IN_BYTES,
					 view.getMaximumSizeInBytes());
	}

	@Test
	void captureAllImageRouteExposesOnlyGenericAndCameraAffordances() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		ContentUploadView view = new ContentUploadView();
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(ContentUploadView.class, view, "contentBinding", "photo");
		setField(ContentUploadView.class, view, "uploadKind", "boundContent");
		setField(ContentUploadView.class, view, "display", "image");
		setField(ContentUploadView.class, view, "capture", "all");

		view.postConstruct();

		assertTrue(view.isGenericUploadAllowed());
		assertTrue(view.isCameraUploadAllowed());
		assertFalse(view.isVideoUploadAllowed());
		assertTrue(view.isImageUpload());
		assertFalse(view.isVideoUpload());
		assertEquals("image/*", view.getAccept());
		assertNull(view.getCaptureHint());
	}

	@Test
	void captureAllVideoRouteExposesOnlyGenericAndVideoAffordances() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		ContentUploadView view = new ContentUploadView();
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(ContentUploadView.class, view, "contentBinding", "clip");
		setField(ContentUploadView.class, view, "uploadKind", "boundContent");
		setField(ContentUploadView.class, view, "display", "video");
		setField(ContentUploadView.class, view, "capture", "all");

		view.postConstruct();

		assertTrue(view.isGenericUploadAllowed());
		assertFalse(view.isCameraUploadAllowed());
		assertTrue(view.isVideoUploadAllowed());
		assertFalse(view.isImageUpload());
		assertTrue(view.isVideoUpload());
		assertEquals("video/*", view.getAccept());
		assertNull(view.getCaptureHint());
	}

	@Test
	void selectedAffordanceCannotBypassDisplayCaptureContract() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		ContentUploadView view = new ContentUploadView();
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(ContentUploadView.class, view, "contentBinding", "photo");
		setField(ContentUploadView.class, view, "uploadKind", "boundContent");
		setField(ContentUploadView.class, view, "display", "image");
		setField(ContentUploadView.class, view, "capture", "all");
		setField(ContentUploadView.class, view, "uploadAffordance", "video");

		assertThrows(IllegalArgumentException.class, view::postConstruct);
	}

	@Test
	void selectedAffordanceRejectsUnknownPostedValue() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		ContentUploadView view = new ContentUploadView();
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(ContentUploadView.class, view, "contentBinding", "attachment");
		setField(ContentUploadView.class, view, "uploadKind", "boundContent");
		setField(ContentUploadView.class, view, "capture", "all");
		setField(ContentUploadView.class, view, "uploadAffordance", "audio");

		assertThrows(IllegalArgumentException.class, view::postConstruct);
	}

	@Test
	void contentUploadStateIsNullBeforePostConstruct() {
		ContentUploadView view = new ContentUploadView();

		assertNull(view.getUploadState());
		assertSame(ContentDisplay.auto, view.getDisplay());
		assertSame(ContentCapture.none, view.getCapture());
		assertFalse(view.isImageUpload());
		assertFalse(view.isVideoUpload());
		assertFalse(view.isGenericUploadAllowed());
		assertFalse(view.isCameraUploadAllowed());
		assertFalse(view.isVideoUploadAllowed());
		assertNull(view.getAccept());
		assertNull(view.getCaptureHint());
	}

	@Test
	void imageModeRejectsNonImageFileNamesThroughImageWhitelist() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);
		TestContentUploadView view = new TestContentUploadView();
		UploadedFile file = Mockito.mock(UploadedFile.class);

		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(ContentUploadView.class, view, "contentBinding", "photo");
		setField(ContentUploadView.class, view, "uploadKind", "boundContent");
		setField(ContentUploadView.class, view, "display", "image");
		doReturn(Long.valueOf(1024L)).when(file).getSize();
		when(file.getFileName()).thenReturn("document.pdf");

		view.postConstruct();

		assertFalse(view.isValidFile(file, context));
		verify(context).addMessage(isNull(), any());
	}

	@Test
	void imageModeStopsValidationWhenBaseUploadValidationFails() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);
		TestContentUploadView view = new TestContentUploadView();
		UploadedFile file = Mockito.mock(UploadedFile.class);

		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(ContentUploadView.class, view, "contentBinding", "photo");
		setField(ContentUploadView.class, view, "uploadKind", "boundContent");
		setField(ContentUploadView.class, view, "display", "image");
		doReturn(Long.valueOf((UtilImpl.UPLOADS_IMAGE_MAXIMUM_SIZE_IN_MB * AbstractUploadView.MB_IN_BYTES) + 1L)).when(file).getSize();
		when(file.getFileName()).thenReturn("photo.png");

		view.postConstruct();

		assertFalse(view.isValidFile(file, context));
		verify(context).addMessage(isNull(), any());
	}

	@Test
	void videoModeRejectsNonVideoUploadsAfterBaseValidationPasses() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);
		TestContentUploadView view = new TestContentUploadView();
		UploadedFile file = Mockito.mock(UploadedFile.class);
		String originalVideoRegex = UtilImpl.UPLOADS_VIDEO_WHITELIST_REGEX;

		try {
			UtilImpl.UPLOADS_VIDEO_WHITELIST_REGEX = "^.+\\.(MP4|PNG)$";
			setField(AbstractUploadView.class, view, "context", "ctx");
			setField(ContentUploadView.class, view, "contentBinding", "clip");
			setField(ContentUploadView.class, view, "uploadKind", "boundContent");
			setField(ContentUploadView.class, view, "display", "video");
			doReturn(Long.valueOf(1024L)).when(file).getSize();
			when(file.getFileName()).thenReturn("still.png");
			when(file.getContentType()).thenReturn("image/png");

			view.postConstruct();

			assertFalse(view.isValidFile(file, context));
			verify(context).addMessage(isNull(), any());
		}
		finally {
			UtilImpl.UPLOADS_VIDEO_WHITELIST_REGEX = originalVideoRegex;
		}
	}

	@Test
	void videoModeAcceptsVideoUploadsAndBypassesImageCropperValidation() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);
		TestContentUploadView view = new TestContentUploadView();
		UploadedFile file = Mockito.mock(UploadedFile.class);

		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(ContentUploadView.class, view, "contentBinding", "clip");
		setField(ContentUploadView.class, view, "uploadKind", "boundContent");
		setField(ContentUploadView.class, view, "display", "video");
		doReturn(Long.valueOf(1024L)).when(file).getSize();
		when(file.getFileName()).thenReturn("clip.mp4");
		when(file.getContentType()).thenReturn("video/mp4");

		view.postConstruct();

		assertTrue(view.isVideoUpload());
		assertFalse(view.isImageUpload());
		assertTrue(view.isValidFile(file, context));
		verify(context, never()).addMessage(isNull(), any());
	}

	@Test
	void contentUploadHandleFileUploadStopsWhenValidationFails() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		FacesContextBridge.setCurrent(context);
		ContentUploadView view = new ContentUploadView();
		FileUploadEvent event = Mockito.mock(FileUploadEvent.class);
		UploadedFile file = Mockito.mock(UploadedFile.class);

		when(event.getFile()).thenReturn(file);
		doReturn(Long.valueOf((UtilImpl.UPLOADS_CONTENT_MAXIMUM_SIZE_IN_MB * AbstractUploadView.MB_IN_BYTES) + 1L)).when(file).getSize();
		when(file.getFileName()).thenReturn("oversized.txt");

		assertDoesNotThrow(() -> view.handleFileUpload(event));

		verify(context).addMessage(isNull(), any());
	}

	@Test
	@SuppressWarnings("null")
	void contentUploadHandleFileUploadRequiresEvent() {
		ContentUploadView view = new ContentUploadView();

		assertThrows(NullPointerException.class, () -> view.handleFileUpload(null));
	}

	@Test
	void contentUploadHandleFileUploadRequiresUploadedFile() {
		FacesContext context = Mockito.mock(FacesContext.class);
		FacesContextBridge.setCurrent(context);
		ContentUploadView view = new ContentUploadView();
		FileUploadEvent event = Mockito.mock(FileUploadEvent.class);

		when(event.getFile()).thenReturn(null);

		assertThrows(NullPointerException.class, () -> view.handleFileUpload(event));
	}

	@Test
	void unifiedUploadPostConstructSelectsVideoLimitsFromRouteState() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		ContentUploadView view = new ContentUploadView();
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(ContentUploadView.class, view, "contentBinding", "clip");
		setField(ContentUploadView.class, view, "uploadKind", "boundContent");
		setField(ContentUploadView.class, view, "display", "video");

		view.postConstruct();

		assertSame(ContentDisplay.video, view.getDisplay());
		assertFalse(view.isImageUpload());
		assertTrue(view.isVideoUpload());
		assertTrue(view.isGenericUploadAllowed());
		assertFalse(view.isCameraUploadAllowed());
		assertFalse(view.isVideoUploadAllowed());
		assertEquals("video/*", view.getAccept());
		assertNull(view.getCaptureHint());
		assertEquals(UtilImpl.UPLOADS_VIDEO_WHITELIST_REGEX, view.getWhitelistRegex());
		assertEquals(UtilImpl.UPLOADS_VIDEO_MAXIMUM_SIZE_IN_MB * AbstractUploadView.MB_IN_BYTES,
					 view.getMaximumSizeInBytes());
	}

	@Test
	void actionUploadDefaultsAndNoAccessBranchAreStable() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		ContentUploadView view = new ContentUploadView();
		FileUploadEvent event = Mockito.mock(FileUploadEvent.class);
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(ContentUploadView.class, view, "uploadKind", "action");
		setField(ContentUploadView.class, view, "action", "uploadAction");
		view.postConstruct();

		assertEquals("uploadAction", view.getAction());
		assertTrue(view.getProblems().isEmpty());
		assertEquals(UtilImpl.UPLOADS_FILE_WHITELIST_REGEX, view.getWhitelistRegex());
		assertEquals(UtilImpl.UPLOADS_FILE_MAXIMUM_SIZE_IN_MB * AbstractUploadView.MB_IN_BYTES,
					 view.getMaximumSizeInBytes());

		assertDoesNotThrow(() -> view.handleFileUpload(event));
		verifyNoInteractions(event);
	}

	@Test
	void explicitContentUploadConstructorAppliesCustomLimits() {
		TestContentUploadView view = new TestContentUploadView(".*\\.csv", 2);

		assertEquals(".*\\.csv", view.getWhitelistRegex());
		assertEquals(2L * AbstractUploadView.MB_IN_BYTES, view.getMaximumSizeInBytes());
	}

	@Test
	void actionUploadValidationFailureStopsBeforeRequestLookup() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		ContentUploadView view = new ContentUploadView();
		FileUploadEvent event = Mockito.mock(FileUploadEvent.class);
		UploadedFile file = Mockito.mock(UploadedFile.class);
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(ContentUploadView.class, view, "uploadKind", "action");
		setField(ContentUploadView.class, view, "action", "uploadAction");
		view.postConstruct();
		setField(AbstractUploadView.class, view, "canAccess", Boolean.TRUE);

		when(event.getFile()).thenReturn(file);
		doReturn(Long.valueOf((UtilImpl.UPLOADS_FILE_MAXIMUM_SIZE_IN_MB * AbstractUploadView.MB_IN_BYTES) + 1L)).when(file).getSize();
		when(file.getFileName()).thenReturn("oversized.csv");

		assertDoesNotThrow(() -> view.handleFileUpload(event));

		verify(context).addMessage(isNull(), any());
		verify(externalContext, never()).getRequest();
	}

	@Test
	void actionUploadShowsMalformedUrlWhenRouteStateIsMissing() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		ContentUploadView view = new ContentUploadView();
		FileUploadEvent event = Mockito.mock(FileUploadEvent.class);
		UploadedFile file = Mockito.mock(UploadedFile.class);
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(ContentUploadView.class, view, "uploadKind", "action");
		setField(ContentUploadView.class, view, "action", "uploadAction");
		view.postConstruct();
		setField(AbstractUploadView.class, view, "context", null);
		setField(AbstractUploadView.class, view, "canAccess", Boolean.TRUE);

		when(event.getFile()).thenReturn(file);
		doReturn(Long.valueOf(1024L)).when(file).getSize();
		when(file.getFileName()).thenReturn("import.csv");

		assertDoesNotThrow(() -> view.handleFileUpload(event));

		verify(context).addMessage(isNull(), any());
		verify(externalContext, never()).getRequest();
	}

	@Test
	void actionUploadShowsMalformedUrlWhenConversationMissing() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		HttpServletRequest request = Mockito.mock(HttpServletRequest.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		when(externalContext.getRequest()).thenReturn(request);
		FacesContextBridge.setCurrent(context);

		ContentUploadView view = new ContentUploadView();
		FileUploadEvent event = Mockito.mock(FileUploadEvent.class);
		UploadedFile file = Mockito.mock(UploadedFile.class);
		setField(AbstractUploadView.class, view, "context", "missing-context");
		setField(ContentUploadView.class, view, "uploadKind", "action");
		setField(ContentUploadView.class, view, "action", "uploadAction");
		view.postConstruct();
		setField(AbstractUploadView.class, view, "canAccess", Boolean.TRUE);

		when(event.getFile()).thenReturn(file);
		doReturn(Long.valueOf(1024L)).when(file).getSize();
		when(file.getFileName()).thenReturn("import.csv");

		assertDoesNotThrow(() -> view.handleFileUpload(event));

		verify(context).addMessage(isNull(), any());
	}

	@Test
	void addActionUploadProblemsCopiesErrorsAndWarnings() throws Exception {
		ContentUploadView view = new ContentUploadView();
		FacesContext context = Mockito.mock(FacesContext.class);
		UploadException exception = new UploadException();
		UploadException.Problem error = new UploadException.Problem("bad value", "row 1");
		UploadException.Problem warning = new UploadException.Problem("odd value", "row 2");
		exception.addError(error);
		exception.addWarning(warning);

		invokeActionUploadProblems(view, exception, context);

		assertEquals(2, view.getProblems().size());
		assertSame(error, view.getProblems().get(0));
		assertSame(warning, view.getProblems().get(1));
		verify(context).addMessage(isNull(), any(FacesMessage.class));
	}

	@Test
	void addActionUploadProblemsUsesWarningSummaryWhenOnlyWarningsExist() throws Exception {
		ContentUploadView view = new ContentUploadView();
		FacesContext context = Mockito.mock(FacesContext.class);
		UploadException exception = new UploadException();
		UploadException.Problem warning = new UploadException.Problem("odd value", "row 2");
		exception.addWarning(warning);

		invokeActionUploadProblems(view, exception, context);

		assertEquals(1, view.getProblems().size());
		assertSame(warning, view.getProblems().get(0));
		verify(context).addMessage(isNull(), any(FacesMessage.class));
	}

	@Test
	void addActionUploadSuccessFormatsKilobytesAndMegabytes() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		UploadedFile small = Mockito.mock(UploadedFile.class);
		UploadedFile large = Mockito.mock(UploadedFile.class);
		when(small.getFileName()).thenReturn("small.csv");
		doReturn(Long.valueOf(2048L)).when(small).getSize();
		when(large.getFileName()).thenReturn("large.csv");
		doReturn(Long.valueOf(2L * 1024L * 1024L)).when(large).getSize();

		invokeActionUploadSuccess(small, context);
		invokeActionUploadSuccess(large, context);

		verify(context, Mockito.times(2)).addMessage(isNull(), any(FacesMessage.class));
	}

	@Test
	void baseUploadValidationAcceptsAllowedFile() {
		FacesContext context = Mockito.mock(FacesContext.class);
		TestUploadView view = new TestUploadView(".*\\.txt", 1);
		UploadedFile file = Mockito.mock(UploadedFile.class);
		doReturn(Long.valueOf(1024L)).when(file).getSize();
		when(file.getFileName()).thenReturn("notes.txt");

		assertTrue(view.isValidFile(file, context));
		verifyNoInteractions(context);
	}

	@Test
	void baseUploadValidationRejectsOversizedFile() {
		FacesContext context = Mockito.mock(FacesContext.class);
		TestUploadView view = new TestUploadView(".*\\.txt", 1);
		UploadedFile file = Mockito.mock(UploadedFile.class);
		doReturn(Long.valueOf(AbstractUploadView.MB_IN_BYTES + 1L)).when(file).getSize();
		when(file.getFileName()).thenReturn("notes.txt");

		assertFalse(view.isValidFile(file, context));
		verify(context).addMessage(isNull(), any());
	}

	@Test
	void baseUploadValidationRejectsHiddenPathSegment() {
		FacesContext context = Mockito.mock(FacesContext.class);
		TestUploadView view = new TestUploadView(".*\\.txt", 1);
		UploadedFile file = Mockito.mock(UploadedFile.class);
		doReturn(Long.valueOf(1024L)).when(file).getSize();
		when(file.getFileName()).thenReturn("uploads/.secret.txt");

		assertFalse(view.isValidFile(file, context));
		verify(context).addMessage(isNull(), any());
	}

	@Test
	void baseUploadValidationRejectsLeadingHiddenFileName() {
		FacesContext context = Mockito.mock(FacesContext.class);
		TestUploadView view = new TestUploadView(".*\\.txt", 1);
		UploadedFile file = Mockito.mock(UploadedFile.class);
		doReturn(Long.valueOf(1024L)).when(file).getSize();
		when(file.getFileName()).thenReturn(".secret.txt");

		assertFalse(view.isValidFile(file, context));
		verify(context).addMessage(isNull(), any());
	}

	@Test
	void baseUploadValidationRejectsHiddenBackslashPathSegment() {
		FacesContext context = Mockito.mock(FacesContext.class);
		TestUploadView view = new TestUploadView(".*\\.txt", 1);
		UploadedFile file = Mockito.mock(UploadedFile.class);
		doReturn(Long.valueOf(1024L)).when(file).getSize();
		when(file.getFileName()).thenReturn("uploads\\.secret.txt");

		assertFalse(view.isValidFile(file, context));
		verify(context).addMessage(isNull(), any());
	}

	@Test
	void baseUploadValidationAllowsNullFileNameWhenSizeIsValid() {
		FacesContext context = Mockito.mock(FacesContext.class);
		TestUploadView view = new TestUploadView(".*\\.txt", 1);
		UploadedFile file = Mockito.mock(UploadedFile.class);
		doReturn(Long.valueOf(1024L)).when(file).getSize();
		when(file.getFileName()).thenReturn(null);

		assertTrue(view.isValidFile(file, context));
		verifyNoInteractions(context);
	}

	@Test
	void baseUploadValidationRejectsWhitelistMismatch() {
		FacesContext context = Mockito.mock(FacesContext.class);
		TestUploadView view = new TestUploadView(".*\\.txt", 1);
		UploadedFile file = Mockito.mock(UploadedFile.class);
		doReturn(Long.valueOf(1024L)).when(file).getSize();
		when(file.getFileName()).thenReturn("document.pdf");

		assertFalse(view.isValidFile(file, context));
		verify(context).addMessage(isNull(), any());
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
	void unifiedBoundContentStateSanitisesAndDerivesMixedMode() {
		UnifiedUploadState state = UnifiedUploadState.fromRoute("boundContent",
																	" ctx ",
																	" bean ",
																	" attachment ",
																	"ignoredAction",
																	"auto",
																	"all",
																	null,
																	" _attachment ",
																	" item ");

		assertSame(UploadKind.boundContent, state.getUploadKind());
		assertEquals("ctx", state.getContext());
		assertEquals("bean", state.getBinding());
		assertEquals("attachment", state.getContentBinding());
		assertNull(state.getAction());
		assertSame(ContentDisplay.auto, state.getDisplay());
		assertSame(ContentCapture.all, state.getCapture());
		assertSame(UploadMode.mixed, state.getMode());
		assertEquals("_attachment", state.getCompanion());
		assertEquals("item", state.getCallbackTarget());
		assertSame(UploadCategory.content, state.categoryFor(UploadAffordance.generic));
		assertSame(UploadCategory.image, state.categoryFor(UploadAffordance.camera));
		assertSame(UploadCategory.video, state.categoryFor(UploadAffordance.video));
	}

	@Test
	void unifiedBoundContentStateSelectsImageAndVideoCategoriesByDisplay() {
		UnifiedUploadState image = UnifiedUploadState.fromRoute("boundContent",
																	"ctx",
																	null,
																	"photo",
																	null,
																	"image",
																	null,
																	null,
																	null,
																	null);
		UnifiedUploadState video = UnifiedUploadState.fromRoute("boundContent",
																	"ctx",
																	null,
																	"clip",
																	null,
																	"video",
																	null,
																	null,
																	null,
																	null);

		assertSame(UploadMode.image, image.getMode());
		assertSame(UploadCategory.image, image.categoryFor(UploadAffordance.generic));
		assertSame(UploadMode.video, video.getMode());
		assertSame(UploadCategory.video, video.categoryFor(UploadAffordance.generic));
	}

	@Test
	void unifiedActionStateUsesFileCategoryUnlessCaptureAffordanceOptsIn() {
		UnifiedUploadState state = UnifiedUploadState.fromRoute("action",
																	"ctx",
																	"bean",
																	"ignoredContent",
																	"uploadAction",
																	null,
																	"all",
																	null,
																	null,
																	"button");

		assertSame(UploadKind.action, state.getUploadKind());
		assertNull(state.getContentBinding());
		assertEquals("uploadAction", state.getAction());
		assertSame(UploadMode.mixed, state.getMode());
		assertSame(UploadCategory.file, state.categoryFor(UploadAffordance.generic));
		assertSame(UploadCategory.image, state.categoryFor(UploadAffordance.camera));
		assertSame(UploadCategory.video, state.categoryFor(UploadAffordance.video));
	}

	@Test
	void unifiedUploadViewActionRouteUsesActionStateAndFileLimits() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		ContentUploadView view = new ContentUploadView();
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(AbstractUploadView.class, view, "binding", "beanBinding");
		setField(ContentUploadView.class, view, "uploadKind", "action");
		setField(ContentUploadView.class, view, "action", "uploadAction");

		view.postConstruct();

		assertEquals("uploadAction", view.getAction());
		UnifiedUploadState state = view.getUploadState();
		assertNotNull(state);
		assertSame(UploadKind.action, state.getUploadKind());
		assertSame(UploadMode.file, state.getMode());
		assertEquals(UtilImpl.UPLOADS_FILE_WHITELIST_REGEX, view.getWhitelistRegex());
		assertEquals(UtilImpl.UPLOADS_FILE_MAXIMUM_SIZE_IN_MB * AbstractUploadView.MB_IN_BYTES,
					 view.getMaximumSizeInBytes());
	}

	@Test
	void unifiedActionUploadReturnsBeforeReadingEventWhenAccessIsMissing() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		ContentUploadView view = new ContentUploadView();
		FileUploadEvent event = Mockito.mock(FileUploadEvent.class);
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(ContentUploadView.class, view, "uploadKind", "action");
		setField(ContentUploadView.class, view, "action", "uploadAction");
		view.postConstruct();

		assertDoesNotThrow(() -> view.handleFileUpload(event));

		verifyNoInteractions(event);
	}

	@Test
	void unifiedActionUploadStopsWhenValidationFails() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		FacesContextBridge.setCurrent(context);
		ContentUploadView view = new ContentUploadView();
		FileUploadEvent event = Mockito.mock(FileUploadEvent.class);
		UploadedFile file = Mockito.mock(UploadedFile.class);

		setField(AbstractUploadView.class, view, "canAccess", Boolean.TRUE);
		setField(ContentUploadView.class, view, "uploadState", UnifiedUploadState.fromRoute("action",
																							"ctx",
																							null,
																							null,
																							"uploadAction",
																							null,
																							null,
																							null,
																							null,
																							null));
		when(event.getFile()).thenReturn(file);
		doReturn(Long.valueOf((UtilImpl.UPLOADS_CONTENT_MAXIMUM_SIZE_IN_MB * AbstractUploadView.MB_IN_BYTES) + 1L)).when(file).getSize();
		when(file.getFileName()).thenReturn("oversized.txt");

		assertDoesNotThrow(() -> view.handleFileUpload(event));

		verify(context).addMessage(isNull(), any());
	}

	@Test
	void unifiedActionUploadShowsMalformedUrlMessageWhenActionIsCleared() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		FacesContextBridge.setCurrent(context);
		ContentUploadView view = new ContentUploadView();
		FileUploadEvent event = Mockito.mock(FileUploadEvent.class);
		UploadedFile file = Mockito.mock(UploadedFile.class);

		setField(AbstractUploadView.class, view, "canAccess", Boolean.TRUE);
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(ContentUploadView.class, view, "action", null);
		setField(ContentUploadView.class, view, "uploadState", UnifiedUploadState.fromRoute("action",
																							"ctx",
																							null,
																							null,
																							"uploadAction",
																							null,
																							null,
																							null,
																							null,
																							null));
		when(event.getFile()).thenReturn(file);
		doReturn(Long.valueOf(1024L)).when(file).getSize();
		when(file.getFileName()).thenReturn("ok.txt");

		assertDoesNotThrow(() -> view.handleFileUpload(event));

		verify(context).addMessage(isNull(), any());
	}

	@Test
	void unifiedActionUploadShowsMalformedUrlMessageWhenConversationMissing() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		HttpServletRequest request = Mockito.mock(HttpServletRequest.class);
		FacesContextBridge.setCurrent(context);
		ContentUploadView view = new ContentUploadView();
		FileUploadEvent event = Mockito.mock(FileUploadEvent.class);
		UploadedFile file = Mockito.mock(UploadedFile.class);

		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getRequest()).thenReturn(request);
		setField(AbstractUploadView.class, view, "canAccess", Boolean.TRUE);
		setField(AbstractUploadView.class, view, "context", "missing-context");
		setField(ContentUploadView.class, view, "action", "uploadAction");
		setField(ContentUploadView.class, view, "uploadState", UnifiedUploadState.fromRoute("action",
																							"missing-context",
																							null,
																							null,
																							"uploadAction",
																							null,
																							null,
																							null,
																							null,
																							null));
		when(event.getFile()).thenReturn(file);
		doReturn(Long.valueOf(1024L)).when(file).getSize();
		when(file.getFileName()).thenReturn("ok.txt");

		assertDoesNotThrow(() -> view.handleFileUpload(event));

		verify(context).addMessage(isNull(), any());
	}

	@Test
	void unifiedUploadViewActionCameraRouteUsesImageLimits() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		ContentUploadView view = new ContentUploadView();
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(ContentUploadView.class, view, "uploadKind", "action");
		setField(ContentUploadView.class, view, "action", "uploadAction");
		setField(ContentUploadView.class, view, "capture", "camera");

		view.postConstruct();

		assertSame(ContentCapture.camera, view.getCapture());
		assertTrue(view.isImageUpload());
		assertFalse(view.isVideoUpload());
		assertFalse(view.isGenericUploadAllowed());
		assertTrue(view.isCameraUploadAllowed());
		assertFalse(view.isVideoUploadAllowed());
		assertEquals("image/*", view.getAccept());
		assertEquals("environment", view.getCaptureHint());
		assertEquals(UtilImpl.UPLOADS_IMAGE_WHITELIST_REGEX, view.getWhitelistRegex());
		assertEquals(UtilImpl.UPLOADS_IMAGE_MAXIMUM_SIZE_IN_MB * AbstractUploadView.MB_IN_BYTES,
					 view.getMaximumSizeInBytes());
	}

	@Test
	void unifiedUploadViewActionVideoRouteUsesVideoLimits() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		ContentUploadView view = new ContentUploadView();
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(ContentUploadView.class, view, "uploadKind", "action");
		setField(ContentUploadView.class, view, "action", "uploadAction");
		setField(ContentUploadView.class, view, "capture", "video");

		view.postConstruct();

		assertSame(ContentCapture.video, view.getCapture());
		assertFalse(view.isImageUpload());
		assertTrue(view.isVideoUpload());
		assertFalse(view.isGenericUploadAllowed());
		assertFalse(view.isCameraUploadAllowed());
		assertTrue(view.isVideoUploadAllowed());
		assertEquals("video/*", view.getAccept());
		assertEquals("camcorder", view.getCaptureHint());
		assertEquals(UtilImpl.UPLOADS_VIDEO_WHITELIST_REGEX, view.getWhitelistRegex());
		assertEquals(UtilImpl.UPLOADS_VIDEO_MAXIMUM_SIZE_IN_MB * AbstractUploadView.MB_IN_BYTES,
					 view.getMaximumSizeInBytes());
	}

	@Test
	void unifiedUploadStateRejectsClientModeAndInvalidCombinations() {
		assertThrows(IllegalArgumentException.class,
						() -> UnifiedUploadState.fromRoute("boundContent", "ctx", null, "attachment", null, null, null, "video", null, null));
		assertThrows(IllegalArgumentException.class,
						() -> UnifiedUploadState.fromRoute("boundContent", "ctx", null, "attachment", null, "image", "video", null, null, null));
		assertThrows(IllegalArgumentException.class,
						() -> UnifiedUploadState.fromRoute("boundContent", "ctx", null, "attachment", null, "video", "camera", null, null, null));
		assertThrows(IllegalArgumentException.class,
						() -> UnifiedUploadState.fromRoute("boundContent", "ctx", null, "attachment", null, null, "audio", null, null, null));
		assertThrows(IllegalArgumentException.class,
						() -> UnifiedUploadState.fromRoute("boundContent", "ctx", null, "attachment", null, null, null, null, null, null)
													.categoryFor(UploadAffordance.camera));
	}

	@Test
	void unifiedUploadStateRejectsMissingRequiredRouteValues() {
		assertThrows(IllegalArgumentException.class,
						() -> UnifiedUploadState.fromRoute(null, "ctx", null, "attachment", null, null, null, null, null, null));
		assertThrows(IllegalArgumentException.class,
						() -> UnifiedUploadState.fromRoute("boundContent", null, null, "attachment", null, null, null, null, null, null));
		assertThrows(IllegalArgumentException.class,
						() -> UnifiedUploadState.fromRoute("boundContent", "ctx", null, null, null, null, null, null, null, null));
		assertThrows(IllegalArgumentException.class,
						() -> UnifiedUploadState.fromRoute("action", "ctx", null, null, null, null, null, null, null, null));
	}

	@Test
	void actionUploadPostConstructSanitisesActionParameter() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		ContentUploadView view = new ContentUploadView();
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(AbstractUploadView.class, view, "binding", "beanBinding");
		setField(ContentUploadView.class, view, "uploadKind", "action");
		setField(ContentUploadView.class, view, "action", " uploadAction ");

		view.postConstruct();

		assertEquals("uploadAction", view.getAction());
		assertEquals("ctx", view.getContext());
		assertEquals("beanBinding", view.getBinding());
	}

	@Test
	void bizportImportPostConstructSanitisesActionParameter() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);

		BizportImportView view = new BizportImportView();
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(AbstractUploadView.class, view, "binding", "beanBinding");
		setField(BizportImportView.class, view, "action", " importAction ");

		view.postConstruct();

		assertEquals("importAction", view.getAction());
		assertEquals("ctx", view.getContext());
		assertEquals("beanBinding", view.getBinding());
	}

	@Test
	void actionUploadHandleFileUploadShowsMalformedUrlMessageWhenContextMissing() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		FacesContextBridge.setCurrent(context);
		ContentUploadView view = new ContentUploadView();
		FileUploadEvent event = Mockito.mock(FileUploadEvent.class);
		UploadedFile file = Mockito.mock(UploadedFile.class);

		setField(AbstractUploadView.class, view, "context", "missingContext");
		setField(ContentUploadView.class, view, "uploadKind", "action");
		setField(ContentUploadView.class, view, "action", "uploadAction");
		view.postConstruct();
		setField(AbstractUploadView.class, view, "canAccess", Boolean.TRUE);
		when(event.getFile()).thenReturn(file);
		doReturn(Long.valueOf(1024L)).when(file).getSize();
		when(file.getFileName()).thenReturn("ok.txt");

		assertDoesNotThrow(() -> view.handleFileUpload(event));

		verify(context).addMessage(isNull(), any());
	}

	@Test
	void contentUploadHandleFileUploadShowsMalformedUrlMessageWhenContextMissing() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		FacesContextBridge.setCurrent(context);
		ContentUploadView view = new ContentUploadView();
		FileUploadEvent event = Mockito.mock(FileUploadEvent.class);
		UploadedFile file = Mockito.mock(UploadedFile.class);

		setField(AbstractUploadView.class, view, "canAccess", Boolean.TRUE);
		setField(ContentUploadView.class, view, "contentBinding", "contentId");
		when(event.getFile()).thenReturn(file);
		doReturn(Long.valueOf(1024L)).when(file).getSize();
		when(file.getFileName()).thenReturn("ok.txt");

		assertDoesNotThrow(() -> view.handleFileUpload(event));

		verify(context).addMessage(isNull(), any());
	}

	@Test
	void contentUploadHandleFileUploadShowsMalformedUrlMessageWhenConversationMissing() throws Exception {
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		HttpServletRequest request = Mockito.mock(HttpServletRequest.class);
		FacesContextBridge.setCurrent(context);
		ContentUploadView view = new ContentUploadView();
		FileUploadEvent event = Mockito.mock(FileUploadEvent.class);
		UploadedFile file = Mockito.mock(UploadedFile.class);

		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getRequest()).thenReturn(request);
		setField(AbstractUploadView.class, view, "canAccess", Boolean.TRUE);
		setField(AbstractUploadView.class, view, "context", "missing-context");
		setField(ContentUploadView.class, view, "contentBinding", "contentId");
		when(event.getFile()).thenReturn(file);
		doReturn(Long.valueOf(1024L)).when(file).getSize();
		when(file.getFileName()).thenReturn("ok.txt");

		assertDoesNotThrow(() -> view.handleFileUpload(event));

		verify(context).addMessage(isNull(), any());
	}

	@Test
	void contentUploadRollsBackAndReportsMessageWhenConversationBeanMissing() throws Exception {
		boolean originalForceNonPersistentCaching = UtilImpl.FORCE_NON_PERSISTENT_CACHING;
		String originalCacheDirectory = UtilImpl.CACHE_DIRECTORY;
		SessionCacheConfig originalSessionCache = UtilImpl.SESSION_CACHE;
		ConversationCacheConfig originalConversationCache = UtilImpl.CONVERSATION_CACHE;
		Path cacheDirectory = Files.createTempDirectory("content-upload-cache");
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		HttpServletRequest request = Mockito.mock(HttpServletRequest.class);
		HttpSession session = Mockito.mock(HttpSession.class);
		MockWebContext webContext = new MockWebContext();
		AbstractPersistence persistence = Mockito.mock(AbstractPersistence.class);
		HashMap<String, Object> values = new HashMap<>();
		values.put(Bean.DOCUMENT_ID, "bean-1");
		Bean bean = new DynamicBean("test", "Doc", values);
		ContentUploadView view = new ContentUploadView();
		FileUploadEvent event = Mockito.mock(FileUploadEvent.class);
		UploadedFile file = Mockito.mock(UploadedFile.class);

		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getRequest()).thenReturn(request);
		when(request.getSession(false)).thenReturn(session);
		when(session.getId()).thenReturn(webContext.getSessionId());
		FacesContextBridge.setCurrent(context);
		setField(AbstractUploadView.class, view, "canAccess", Boolean.TRUE);
		setField(ContentUploadView.class, view, "contentBinding", "contentId");
		when(event.getFile()).thenReturn(file);
		doReturn(Long.valueOf(1024L)).when(file).getSize();
		when(file.getFileName()).thenReturn("ok.txt");
		when(file.getContent()).thenReturn(new byte[] {1, 2, 3});
		when(file.getContentType()).thenReturn("text/plain");
		webContext.setCurrentBean(bean);

		try {
			UtilImpl.FORCE_NON_PERSISTENT_CACHING = true;
			UtilImpl.CACHE_DIRECTORY = cacheDirectory.toString() + File.separator;
			UtilImpl.SESSION_CACHE = new SessionCacheConfig(100, 10);
			UtilImpl.CONVERSATION_CACHE = new ConversationCacheConfig(100, 10);
			DefaultCaching.get().shutdown();
			DefaultCaching.get().startup();
			StateUtil.cacheConversation(webContext);
			setField(AbstractUploadView.class, view, "context", webContext.getWebId());
			bindPersistenceToThread(persistence);

			assertDoesNotThrow(() -> view.handleFileUpload(event));
		}
		finally {
			unbindPersistenceFromThread();
			DefaultCaching.get().shutdown();
			UtilImpl.FORCE_NON_PERSISTENT_CACHING = originalForceNonPersistentCaching;
			UtilImpl.CACHE_DIRECTORY = originalCacheDirectory;
			UtilImpl.SESSION_CACHE = originalSessionCache;
			UtilImpl.CONVERSATION_CACHE = originalConversationCache;
			try (Stream<Path> paths = Files.walk(cacheDirectory)) {
				paths.sorted((a, b) -> b.compareTo(a))
						.map(Path::toFile)
						.forEach(File::delete);
			}
		}

		verify(persistence).rollback();
		verify(context).addMessage(isNull(), any());
	}

	@Test
	void actionUploadRollsBackAndReportsMessageWhenActionDenied() throws Exception {
		boolean originalForceNonPersistentCaching = UtilImpl.FORCE_NON_PERSISTENT_CACHING;
		String originalCacheDirectory = UtilImpl.CACHE_DIRECTORY;
		SessionCacheConfig originalSessionCache = UtilImpl.SESSION_CACHE;
		ConversationCacheConfig originalConversationCache = UtilImpl.CONVERSATION_CACHE;
		Path cacheDirectory = Files.createTempDirectory("action-upload-cache");
		FacesContext context = Mockito.mock(FacesContext.class);
		ExternalContext externalContext = Mockito.mock(ExternalContext.class);
		HttpServletRequest request = Mockito.mock(HttpServletRequest.class);
		HttpSession session = Mockito.mock(HttpSession.class);
		MockWebContext webContext = new MockWebContext();
		AbstractPersistence persistence = Mockito.mock(AbstractPersistence.class);
		User user = Mockito.mock(User.class);
		CustomerImpl customer = Mockito.mock(CustomerImpl.class);
		Module module = Mockito.mock(Module.class);
		Document document = Mockito.mock(Document.class);
		HashMap<String, Object> values = new HashMap<>();
		values.put(Bean.DOCUMENT_ID, "bean-1");
		Bean bean = new DynamicBean("test", "Doc", values);
		ContentUploadView view = new ContentUploadView();
		FileUploadEvent event = Mockito.mock(FileUploadEvent.class);
		UploadedFile file = Mockito.mock(UploadedFile.class);

		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSession(false)).thenReturn(null);
		when(externalContext.getRequest()).thenReturn(request);
		when(request.getSession(false)).thenReturn(session);
		when(session.getId()).thenReturn(webContext.getSessionId());
		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(user.getName()).thenReturn("tester");
		when(customer.getModule("test")).thenReturn(module);
		when(module.getDocument(customer, "Doc")).thenReturn(document);
		FacesContextBridge.setCurrent(context);
		setField(AbstractUploadView.class, view, "context", "ctx");
		setField(ContentUploadView.class, view, "uploadKind", "action");
		setField(ContentUploadView.class, view, "action", "uploadAction");
		when(event.getFile()).thenReturn(file);
		doReturn(Long.valueOf(1024L)).when(file).getSize();
		when(file.getFileName()).thenReturn("ok.csv");
		webContext.setCurrentBean(bean);

		try {
			UtilImpl.FORCE_NON_PERSISTENT_CACHING = true;
			UtilImpl.CACHE_DIRECTORY = cacheDirectory.toString() + File.separator;
			UtilImpl.SESSION_CACHE = new SessionCacheConfig(100, 10);
			UtilImpl.CONVERSATION_CACHE = new ConversationCacheConfig(100, 10);
			DefaultCaching.get().shutdown();
			DefaultCaching.get().startup();
			StateUtil.cacheConversation(webContext);
			setField(AbstractUploadView.class, view, "context", webContext.getWebId());
			view.postConstruct();
			setField(AbstractUploadView.class, view, "canAccess", Boolean.TRUE);
			bindPersistenceToThread(persistence);

			assertDoesNotThrow(() -> view.handleFileUpload(event));
		}
		finally {
			unbindPersistenceFromThread();
			DefaultCaching.get().shutdown();
			UtilImpl.FORCE_NON_PERSISTENT_CACHING = originalForceNonPersistentCaching;
			UtilImpl.CACHE_DIRECTORY = originalCacheDirectory;
			UtilImpl.SESSION_CACHE = originalSessionCache;
			UtilImpl.CONVERSATION_CACHE = originalConversationCache;
			try (Stream<Path> paths = Files.walk(cacheDirectory)) {
				paths.sorted((a, b) -> b.compareTo(a))
						.map(Path::toFile)
						.forEach(File::delete);
			}
		}

		verify(persistence).rollback();
		verify(context).addMessage(isNull(), any());
	}

	@Test
	void contentUploadHandleFileUploadReturnsWhenAccessIsMissing() {
		FacesContext context = Mockito.mock(FacesContext.class);
		FacesContextBridge.setCurrent(context);
		ContentUploadView view = new ContentUploadView();
		FileUploadEvent event = Mockito.mock(FileUploadEvent.class);
		UploadedFile file = Mockito.mock(UploadedFile.class);

		when(event.getFile()).thenReturn(file);
		doReturn(Long.valueOf(1024L)).when(file).getSize();
		when(file.getFileName()).thenReturn("ok.txt");

		assertDoesNotThrow(() -> view.handleFileUpload(event));

		verifyNoInteractions(context);
	}

	@Test
	void bizportImportHandleFileUploadShowsMalformedUrlMessageWhenContextMissing() {
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

	private static void invokeActionUploadProblems(ContentUploadView view, UploadException exception, FacesContext context) throws Exception {
		Method method = ContentUploadView.class.getDeclaredMethod("addActionUploadProblems", UploadException.class, FacesContext.class);
		method.setAccessible(true);
		method.invoke(view, exception, context);
	}

	private static void invokeActionUploadSuccess(UploadedFile file, FacesContext context) throws Exception {
		Method method = ContentUploadView.class.getDeclaredMethod("addActionUploadSuccess", UploadedFile.class, FacesContext.class);
		method.setAccessible(true);
		method.invoke(null, file, context);
	}

	@SuppressWarnings("unchecked")
	private static void bindPersistenceToThread(AbstractPersistence persistence) throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).set(persistence);
	}

	@SuppressWarnings("unchecked")
	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
	}
}
