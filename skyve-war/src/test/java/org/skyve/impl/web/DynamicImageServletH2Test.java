package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.util.UUID;

import javax.imageio.ImageIO;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.web.service.smartclient.SmartClientWebContext;
import org.skyve.web.WebContext;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockHttpSession;

import modules.test.AbstractSkyveTest;
import modules.test.domain.AllAttributesPersistent;

/**
 * Requests a dynamic image of the bean in a cached conversation.
 */
class DynamicImageServletH2Test extends AbstractSkyveTest {
	private final MockHttpSession session = new MockHttpSession();

	@BeforeEach
	void startSession() {
		session.setAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME, u);
		StateUtil.addSession(u.getId(), session);
	}

	@Test
	void anImageFromACachedConversationWithoutPersistenceIsTheBlankImage() throws Exception {
		SmartClientWebContext webContext = new SmartClientWebContext(UUID.randomUUID().toString(), request());
		webContext.setCurrentBean(AllAttributesPersistent.newInstance()); // and no persistence
		StateUtil.commitAndCacheConversation(webContext);
		MockHttpServletRequest request = request();
		request.addParameter(AbstractWebContext.CONTEXT_NAME, webContext.getWebId());
		request.addParameter(AbstractWebContext.DOCUMENT_NAME, AllAttributesPersistent.MODULE_NAME + '.' + AllAttributesPersistent.DOCUMENT_NAME);
		request.addParameter(DynamicImageServlet.IMAGE_NAME, "Image");
		request.addParameter(DynamicImageServlet.IMAGE_WIDTH_NAME, "64");
		request.addParameter(DynamicImageServlet.IMAGE_HEIGHT_NAME, "48");
		MockHttpServletResponse response = new MockHttpServletResponse();

		new DynamicImageServlet().doGet(request, response);

		assertTrue(response.getContentType().startsWith("image/png"), response.getContentType());
		BufferedImage image = ImageIO.read(new ByteArrayInputStream(response.getContentAsByteArray()));
		assertNotNull(image);
		assertEquals(1, image.getWidth());
		assertEquals(1, image.getHeight());
	}

	private MockHttpServletRequest request() {
		MockHttpServletRequest result = new MockHttpServletRequest("GET", "/image");
		result.setSession(session);
		return result;
	}
}
