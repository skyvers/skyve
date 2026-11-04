package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.stream.Stream;

import javax.imageio.ImageIO;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.skyve.impl.util.UtilImpl;

import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.WriteListener;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@SuppressWarnings({"static-method", "resource"})
class DynamicImageServletTest {
	@ParameterizedTest
	@MethodSource("invalidRequiredParameterRequests")
	void invalidRequiredParameterRequestsReturnBlankPngFallback(String documentName, String imageName, String width, String height) throws Exception {
		DynamicImageServlet servlet = new DynamicImageServlet();
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getParameter(AbstractWebContext.DOCUMENT_NAME)).thenReturn(documentName);
		when(request.getParameter(DynamicImageServlet.IMAGE_NAME)).thenReturn(imageName);
		when(request.getParameter(DynamicImageServlet.IMAGE_WIDTH_NAME)).thenReturn(width);
		when(request.getParameter(DynamicImageServlet.IMAGE_HEIGHT_NAME)).thenReturn(height);

		try (CapturingServletOutputStream output = new CapturingServletOutputStream()) {
		HttpServletResponse response = mock(HttpServletResponse.class);
		doReturn(output).when(response).getOutputStream();

		servlet.doGet(request, response);

		verify(response).setCharacterEncoding(StandardCharsets.UTF_8.name());
		verify(response, never()).setHeader("Accept-Ranges", "bytes");
		verify(response).setContentType("image/png");
		verify(response).addHeader("Cache-Control", "private,no-cache,no-store");

		byte[] bytes = output.toByteArray();
		assertTrue(bytes.length > 0);
		BufferedImage image = ImageIO.read(new ByteArrayInputStream(bytes));
		assertNotNull(image);
		assertEquals(1, image.getWidth());
		assertEquals(1, image.getHeight());
		}
	}

	@Test
	void zoomedSizeAppliesTheZoomAndShrinksBothSidesToFitTheCap() {
		int cap = UtilImpl.THUMBNAIL_MAXIMUM_DIMENSION;
		assertAll(() -> assertEquals(new DynamicImageServlet.ImageSize(80, 36), DynamicImageServlet.zoomedSize(64, 125, 48, 75), "zoomed in and out"),
					() -> assertEquals(new DynamicImageServlet.ImageSize(cap, cap), DynamicImageServlet.zoomedSize(cap, 100, cap, 100), "exactly at the cap"),
					() -> assertEquals(new DynamicImageServlet.ImageSize(2048, 512), DynamicImageServlet.zoomedSize(4096, 100, 1024, 100), "keeps the shape"),
					() -> assertEquals(new DynamicImageServlet.ImageSize(512, 2048), DynamicImageServlet.zoomedSize(1024, 100, 4096, 100), "keeps the shape when taller"),
					() -> assertEquals(new DynamicImageServlet.ImageSize(2048, 1000), DynamicImageServlet.zoomedSize(1024, 400, 500, 400), "zoomed above the cap"),
					() -> assertEquals(new DynamicImageServlet.ImageSize(2048, 683), DynamicImageServlet.zoomedSize(3000, 100, 1000, 100), "rounds to the nearest pixel"),
					() -> assertEquals(new DynamicImageServlet.ImageSize(2048, 1), DynamicImageServlet.zoomedSize(50_000, 100, 1, 100), "a thin side stays 1 pixel"),
					() -> assertEquals(new DynamicImageServlet.ImageSize(2048, 0), DynamicImageServlet.zoomedSize(50_000, 100, 0, 100), "a zero side stays 0"),
					() -> assertEquals(new DynamicImageServlet.ImageSize(2048, 2048), DynamicImageServlet.zoomedSize(Integer.MAX_VALUE, Integer.MAX_VALUE, Integer.MAX_VALUE, Integer.MAX_VALUE), "no int overflow"));
	}

	@Test
	void missingContextReturnsBlankPngFallbackAfterRequiredParametersArePresent() throws Exception {
		DynamicImageServlet servlet = new DynamicImageServlet();
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getParameter(AbstractWebContext.DOCUMENT_NAME)).thenReturn("admin.Contact");
		when(request.getParameter(DynamicImageServlet.IMAGE_NAME)).thenReturn("avatar");
		when(request.getParameter(DynamicImageServlet.IMAGE_WIDTH_NAME)).thenReturn("64");
		when(request.getParameter(DynamicImageServlet.IMAGE_HEIGHT_NAME)).thenReturn("48");
		when(request.getParameter(DynamicImageServlet.IMAGE_WIDTH_ZOOM_NAME)).thenReturn("125");
		when(request.getParameter(DynamicImageServlet.IMAGE_HEIGHT_ZOOM_NAME)).thenReturn("75");
		when(request.getParameter(AbstractWebContext.CONTEXT_NAME)).thenReturn("missing");

		try (CapturingServletOutputStream output = new CapturingServletOutputStream()) {
		HttpServletResponse response = mock(HttpServletResponse.class);
		doReturn(output).when(response).getOutputStream();

		servlet.doGet(request, response);

		verify(response).setContentType("image/png");
		verify(response).addHeader("Cache-Control", "private,no-cache,no-store");
		assertTrue(output.toByteArray().length > 0);
		}
	}

	@Test
	void invalidZoomParameterReturnsBlankPngFallback() throws Exception {
		DynamicImageServlet servlet = new DynamicImageServlet();
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getParameter(AbstractWebContext.DOCUMENT_NAME)).thenReturn("admin.Contact");
		when(request.getParameter(DynamicImageServlet.IMAGE_NAME)).thenReturn("avatar");
		when(request.getParameter(DynamicImageServlet.IMAGE_WIDTH_NAME)).thenReturn("64");
		when(request.getParameter(DynamicImageServlet.IMAGE_HEIGHT_NAME)).thenReturn("48");
		when(request.getParameter(DynamicImageServlet.IMAGE_WIDTH_ZOOM_NAME)).thenReturn("wide");

		try (CapturingServletOutputStream output = new CapturingServletOutputStream()) {
			HttpServletResponse response = mock(HttpServletResponse.class);
			doReturn(output).when(response).getOutputStream();

			servlet.doGet(request, response);

			verify(response).setContentType("image/png");
			verify(response).addHeader("Cache-Control", "private,no-cache,no-store");
			BufferedImage image = ImageIO.read(new ByteArrayInputStream(output.toByteArray()));
			assertNotNull(image);
			assertEquals(1, image.getWidth());
			assertEquals(1, image.getHeight());
		}
	}

	private static Stream<Arguments> invalidRequiredParameterRequests() {
		return Stream.of(
				Arguments.of(null, "image", "10", "10"),
				Arguments.of("admin.Contact", null, "10", "10"),
				Arguments.of("admin.Contact", "image", null, "10"),
				Arguments.of("admin.Contact", "image", "10", null),
				Arguments.of("admin.Contact", "image", "wide", "10"),
				Arguments.of("admin.Contact", "image", "10", "high"));
	}

	private static final class CapturingServletOutputStream extends ServletOutputStream {
		private final ByteArrayOutputStream bytes = new ByteArrayOutputStream();

		@Override
		public void write(int b) {
			bytes.write(b);
		}

		@Override
		public void write(byte[] b, int off, int len) {
			bytes.write(b, off, len);
		}

		@Override
		public boolean isReady() {
			return true;
		}

		@Override
		public void setWriteListener(WriteListener writeListener) {
			// Synchronous test stream.
		}

		byte[] toByteArray() {
			return bytes.toByteArray();
		}
	}
}
