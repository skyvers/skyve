package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.FileSystemContentManager;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.router.UxUi;
import org.skyve.web.WebContext;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import jakarta.servlet.http.HttpServletResponse;
import modules.kitchensink.domain.KitchenSink;
import modules.test.AbstractSkyveTest;

class ContentServletH2Test extends AbstractSkyveTest {
	private Class<? extends AbstractContentManager> previousContentManagerClass;
	private boolean previousContentFileStorage;
	private boolean previousContentFileSuffixes;

	@BeforeEach
	void configureContentManager() {
		previousContentManagerClass = AbstractContentManager.IMPLEMENTATION_CLASS;
		previousContentFileStorage = UtilImpl.CONTENT_FILE_STORAGE;
		previousContentFileSuffixes = UtilImpl.CONTENT_FILE_SUFFIXES;

		AbstractContentManager.IMPLEMENTATION_CLASS = FileSystemContentManager.class;
		UtilImpl.CONTENT_FILE_STORAGE = true;
		UtilImpl.CONTENT_FILE_SUFFIXES = true;
	}

	@AfterEach
	void restoreContentManager() {
		AbstractContentManager.IMPLEMENTATION_CLASS = previousContentManagerClass;
		UtilImpl.CONTENT_FILE_STORAGE = previousContentFileStorage;
		UtilImpl.CONTENT_FILE_SUFFIXES = previousContentFileSuffixes;
	}

	@Test
	void createResourceLoadsAttachmentThroughConfiguredContentManager() throws Exception {
		byte[] bytes = "managed-content".getBytes(StandardCharsets.UTF_8);
		AttachmentContent attachment = putAttachment(bytes);

		AbstractResourceServlet.RequestParams params = new AbstractResourceServlet.RequestParams(
				KitchenSink.MODULE_NAME,
				KitchenSink.DOCUMENT_NAME,
				KitchenSink.contentLinkPropertyName,
				attachment.getContentId(),
				0,
				0,
				u,
				c,
				c.getName());
		AbstractResourceServlet.Resource resource = new ContentServlet().createResource(new MockHttpServletRequest(), params);

		try {
			assertEquals("video/mp4", resource.getContentType());
			assertEquals("clip.mp4", resource.getFileName());
			assertTrue(resource instanceof AbstractResourceServlet.StreamableResource);
			assertEquals(bytes.length, ((AbstractResourceServlet.StreamableResource) resource).getContentLength());
		}
		finally {
			resource.dispose();
		}
	}

	@Test
	void serviceStreamsRangeFromManagedAttachmentThroughRealContentManager() throws Exception {
		AttachmentContent attachment = putAttachment("0123456789".getBytes(StandardCharsets.UTF_8));
		MockHttpServletRequest request = contentRequest(attachment.getContentId());
		request.addHeader("Range", "bytes=3-6");
		MockHttpServletResponse response = new MockHttpServletResponse();

		new ContentServlet().service(request, response);

		assertEquals(HttpServletResponse.SC_PARTIAL_CONTENT, response.getStatus());
		assertEquals("video/mp4", response.getContentType());
		assertEquals("bytes", response.getHeader("Accept-Ranges"));
		assertEquals("bytes 3-6/10", response.getHeader("Content-Range"));
		assertEquals(4, response.getContentLength());
		assertArrayEquals("3456".getBytes(StandardCharsets.UTF_8), response.getContentAsByteArray());
	}

	@Test
	void serviceStreamsFullManagedAttachmentThroughRealContentManager() throws Exception {
		byte[] bytes = "full-managed-content".getBytes(StandardCharsets.UTF_8);
		AttachmentContent attachment = putAttachment(bytes);
		MockHttpServletResponse response = new MockHttpServletResponse();

		new ContentServlet().service(contentRequest(attachment.getContentId()), response);

		assertEquals(HttpServletResponse.SC_OK, response.getStatus());
		assertEquals("video/mp4", response.getContentType());
		assertEquals("bytes", response.getHeader("Accept-Ranges"));
		assertEquals(bytes.length, response.getContentLength());
		assertArrayEquals(bytes, response.getContentAsByteArray());
	}

	@Test
	void serviceReturns404WhenRealContentManagerMissesAttachment() throws Exception {
		MockHttpServletResponse response = new MockHttpServletResponse();

		new ContentServlet().service(contentRequest("12345678-1234-1234-1234-123456789abc"), response);

		assertEquals(HttpServletResponse.SC_NOT_FOUND, response.getStatus());
		assertEquals(0, response.getContentAsByteArray().length);
	}

	@Test
	void noMarkupRequestClearsStoredMarkupAndStreamsOriginalContent() throws Exception {
		byte[] bytes = "raw-marked-content".getBytes(StandardCharsets.UTF_8);
		AttachmentContent attachment = putAttachment(bytes, "<svg/>");
		MockHttpServletRequest request = contentRequest(attachment.getContentId());
		request.setParameter(AbstractWebContext.NO_MARKUP, "");
		MockHttpServletResponse response = new MockHttpServletResponse();

		new ContentServlet().service(request, response);

		assertEquals(HttpServletResponse.SC_OK, response.getStatus());
		assertEquals("bytes", response.getHeader("Accept-Ranges"));
		assertEquals(bytes.length, response.getContentLength());
		assertArrayEquals(bytes, response.getContentAsByteArray());
	}

	private AttachmentContent putAttachment(byte[] bytes) throws Exception {
		return putAttachment(bytes, null);
	}

	private AttachmentContent putAttachment(byte[] bytes, String markup) throws Exception {
		AttachmentContent attachment = new AttachmentContent(c.getName(),
																KitchenSink.MODULE_NAME,
																KitchenSink.DOCUMENT_NAME,
																null,
																u.getId(),
																"bizId",
																KitchenSink.contentLinkPropertyName)
				.attachment("clip.mp4", "video/mp4", bytes);
		attachment.setMarkup(markup);
		try (ContentManager contentManager = EXT.newContentManager()) {
			contentManager.put(attachment, false);
		}
		return attachment;
	}

	private MockHttpServletRequest contentRequest(String contentId) {
		MockHttpServletRequest request = new MockHttpServletRequest("GET", "/content");
		request.setParameter(AbstractWebContext.DOCUMENT_NAME, KitchenSink.MODULE_NAME + '.' + KitchenSink.DOCUMENT_NAME);
		request.setParameter(AbstractWebContext.BINDING_NAME, KitchenSink.contentLinkPropertyName);
		request.setParameter(AbstractWebContext.RESOURCE_FILE_NAME, contentId);
		request.getSession(true).setAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME, u);
		request.setAttribute(AbstractWebContext.UXUI, UxUi.newSmartClient(UxUi.DESKTOP_NAME, "Cerulean", "omega"));
		return request;
	}
}
