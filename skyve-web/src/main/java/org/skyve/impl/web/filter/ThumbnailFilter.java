package org.skyve.impl.web.filter;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;

import org.skyve.content.MimeType;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.DynamicImageServlet;
import org.skyve.util.Thumbnail;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.WriteListener;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpServletResponseWrapper;

/**
 * A servlet filter that passes requests through the chain and, when {@code _w} and {@code _h}
 * query parameters are present, intercepts the response and generates a thumbnail of the
 * requested dimensions using Skyve's {@link Thumbnail} system.
 *
 * <p>The filter drives off the {@code Content-Type} set by the downstream resource. Known image
 * types are thumbnailed; behaviour for non-image responses is controlled by the
 * {@code nonImageResponse} init-param.
 *
 * <p>All configuration is via {@code web.xml} – no annotations are used.
 *
 * <p>Example configuration:
 * <pre>{@code
 * <filter>
 *     <filter-name>ThumbnailFilter</filter-name>
 *     <filter-class>org.skyve.impl.web.filter.ThumbnailFilter</filter-class>
 *     <async-supported>true</async-supported>
 *     <init-param><param-name>maxWidth</param-name><param-value>800</param-value></init-param>
 *     <init-param><param-name>maxHeight</param-name><param-value>600</param-value></init-param>
 *     <init-param><param-name>nonImageResponse</param-name><param-value>svg</param-value></init-param>
 * </filter>
 * <filter-mapping>
 *     <filter-name>ThumbnailFilter</filter-name>
 *     <url-pattern>/images/*</url-pattern>
 * </filter-mapping>
 * }</pre>
 *
 * <p>Supported {@code init-param} names:
 * <ul>
 *   <li>{@code widthParam} – query parameter name for width (default {@code _w})</li>
 *   <li>{@code heightParam} – query parameter name for height (default {@code _h})</li>
 *   <li>{@code maxWidth} – maximum permitted width; 0 means unlimited (default {@code 0})</li>
 *   <li>{@code maxHeight} – maximum permitted height; 0 means unlimited (default {@code 0})</li>
 *   <li>{@code nonImageResponse} – what to do when the downstream content type is not a known
 *       image: {@code passthrough} (default) writes the original response unchanged;
 *       {@code svg} generates a file-type SVG placeholder via the Thumbnail system.</li>
 * </ul>
 */
public class ThumbnailFilter implements Filter {
	private static final Logger LOGGER = LoggerFactory.getLogger(ThumbnailFilter.class);

	private static final String PASSTHROUGH = "passthrough";
	private static final String SVG = "svg";

	private String widthParam;
	private String heightParam;
	private int maxWidth;
	private int maxHeight;
	private boolean nonImageSvg;

	/**
	 * Reads and validates init-params from the filter configuration.
	 *
	 * @param config the filter configuration provided by the container
	 * @throws ServletException if {@code nonImageResponse} is set to an unrecognised value
	 */
	@Override
	public void init(FilterConfig config) throws ServletException {
		String wp = UtilImpl.processStringValue(config.getInitParameter("widthParam"));
		widthParam = (wp != null) ? wp : DynamicImageServlet.IMAGE_WIDTH_NAME;

		String hp = UtilImpl.processStringValue(config.getInitParameter("heightParam"));
		heightParam = (hp != null) ? hp : DynamicImageServlet.IMAGE_HEIGHT_NAME;

		String mw = UtilImpl.processStringValue(config.getInitParameter("maxWidth"));
		maxWidth = (mw != null) ? Integer.parseInt(mw) : 0;

		String mh = UtilImpl.processStringValue(config.getInitParameter("maxHeight"));
		maxHeight = (mh != null) ? Integer.parseInt(mh) : 0;

		String nonImage = UtilImpl.processStringValue(config.getInitParameter("nonImageResponse"));
		if (nonImage != null && !PASSTHROUGH.equalsIgnoreCase(nonImage) && !SVG.equalsIgnoreCase(nonImage)) {
			throw new ServletException("ThumbnailFilter: nonImageResponse must be '" + PASSTHROUGH + "' or '" + SVG + "'");
		}
		nonImageSvg = SVG.equalsIgnoreCase(nonImage);
	}

	/** No resources to release. */
	@Override
	public void destroy() {
		// nothing to release
	}

	/**
	 * Intercepts the response and generates a thumbnail when {@code widthParam} and
	 * {@code heightParam} query parameters are present and positive.
	 *
	 * <p>When both parameters are absent or invalid the request is passed straight
	 * through without buffering. When the downstream content type is not a recognised
	 * image type, behaviour is governed by the {@code nonImageResponse} init-param.
	 *
	 * @param request  the incoming servlet request
	 * @param response the outgoing servlet response
	 * @param chain    the remainder of the filter chain
	 * @throws IOException      if an I/O error occurs writing the response
	 * @throws ServletException if a servlet error occurs during chain execution
	 */
	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
	throws IOException, ServletException {
		// Parse width and height; if absent or invalid pass straight through
		int width = 0;
		int height = 0;
		try {
			String wParam = UtilImpl.processStringValue(request.getParameter(widthParam));
			String hParam = UtilImpl.processStringValue(request.getParameter(heightParam));
			if (wParam == null || hParam == null) {
				chain.doFilter(request, response);
				return;
			}
			width = Integer.parseInt(wParam);
			height = Integer.parseInt(hParam);
			if (width <= 0 || height <= 0) {
				chain.doFilter(request, response);
				return;
			}
		}
		catch (@SuppressWarnings("unused") NumberFormatException e) {
			chain.doFilter(request, response);
			return;
		}

		// Apply caps
		if (maxWidth > 0 && width > maxWidth) {
			width = maxWidth;
		}
		if (maxHeight > 0 && height > maxHeight) {
			height = maxHeight;
		}

		// Capture the chain response
		CapturingResponseWrapper wrapper = new CapturingResponseWrapper((HttpServletResponse) response);
		chain.doFilter(request, wrapper);
		wrapper.flushBuffer();

		byte[] capturedBytes = wrapper.toByteArray();
		String capturedContentType = wrapper.getCapturedContentType();

		// Resolve the content type to a MimeType
		String baseType = null;
		MimeType mimeType = null;
		if (capturedContentType != null) {
			// strip charset or boundary parameters before lookup
			int semicolon = capturedContentType.indexOf(';');
			baseType = (semicolon >= 0) ? capturedContentType.substring(0, semicolon).trim() : capturedContentType.trim();
			mimeType = MimeType.fromContentType(baseType);
		}

		boolean isImage = mimeType != null && mimeType.toString().startsWith("image/");

		if ((! isImage) && (! nonImageSvg)) {
			// Pass through unchanged
			writeThrough(response, capturedBytes, capturedContentType);
			return;
		}

		// Request URI is a stable path (no query string) used as the disk cache key
		String resourceUrl = ((HttpServletRequest) request).getRequestURI();

		try {
			Thumbnail thumbnail = new Thumbnail(new ByteArrayInputStream(capturedBytes), resourceUrl, baseType != null ? baseType : "", width, height);
			byte[] thumbnailBytes = thumbnail.getBytes();
			if (thumbnailBytes == null) {
				writeThrough(response, capturedBytes, capturedContentType);
			}
			else {
				MimeType thumbnailMimeType = thumbnail.getMimeType();
				if (thumbnailMimeType != null) {
					response.setContentType(thumbnailMimeType.toString());
				}
				response.setContentLength(thumbnailBytes.length);
				
				@SuppressWarnings("resource")
				ServletOutputStream sos = response.getOutputStream();
				sos.write(thumbnailBytes);
			}
		}
		catch (Exception e) {
			LOGGER.error("ThumbnailFilter failed to generate thumbnail - falling back to original response", e);
			writeThrough(response, capturedBytes, capturedContentType);
		}
	}

	/**
	 * Writes the captured bytes directly to {@code response} without any transformation,
	 * preserving the original content type and length.
	 *
	 * @param response    the response to write to
	 * @param bytes       the raw response body captured from the filter chain
	 * @param contentType the {@code Content-Type} reported by the downstream resource,
	 *                    or {@code null} if none was set
	 * @throws IOException if an I/O error occurs writing the response
	 */
	@SuppressWarnings("resource")
	private static void writeThrough(ServletResponse response, byte[] bytes, String contentType)
	throws IOException {
		if (contentType != null) {
			response.setContentType(contentType);
		}
		response.setContentLength(bytes.length);
		response.getOutputStream().write(bytes);
	}

	/**
	 * Response wrapper that captures all output written to it into an in-memory buffer,
	 * and reliably captures the content type set by the downstream servlet or resource
	 * (working around containers that lazily populate {@code getContentType()}).
	 */
	private static final class CapturingResponseWrapper extends HttpServletResponseWrapper {
		private final ByteArrayOutputStream buffer = new ByteArrayOutputStream(4096);
		@SuppressWarnings("resource")
		private final ServletOutputStream capturingStream = new CapturingOutputStream(buffer);
		@SuppressWarnings("resource")
		private PrintWriter capturingWriter;
		private String capturedContentType;

		CapturingResponseWrapper(HttpServletResponse response) {
			super(response);
		}

		@Override
		public ServletOutputStream getOutputStream() {
			return capturingStream;
		}

		@Override
		public PrintWriter getWriter() throws IOException {
			if (capturingWriter == null) {
				String encoding = getCharacterEncoding();
				capturingWriter = (encoding != null)
						? new PrintWriter(new OutputStreamWriter(buffer, encoding))
						: new PrintWriter(buffer);
			}
			return capturingWriter;
		}

		@Override
		public void setContentType(String type) {
			capturedContentType = type;
			super.setContentType(type);
		}

		@Override
		public void setHeader(String name, String value) {
			if ("Content-Type".equalsIgnoreCase(name)) {
				capturedContentType = value;
			}
			super.setHeader(name, value);
		}

		@Override
		public void addHeader(String name, String value) {
			if ("Content-Type".equalsIgnoreCase(name)) {
				capturedContentType = value;
			}
			super.addHeader(name, value);
		}

		@Override
		public void flushBuffer() throws IOException {
			if (capturingWriter != null) {
				capturingWriter.flush();
			}
			capturingStream.flush();
		}

		/** @return the full response body written to this wrapper */
		byte[] toByteArray() {
			return buffer.toByteArray();
		}

		/** @return the {@code Content-Type} intercepted from the downstream response, or {@code null} if none was set */
		String getCapturedContentType() {
			return capturedContentType;
		}
	}

	/**
	 * A {@link ServletOutputStream} that delegates all writes to an in-memory
	 * {@link ByteArrayOutputStream}, allowing the captured bytes to be inspected
	 * after the downstream filter chain has completed.
	 */
	private static final class CapturingOutputStream extends ServletOutputStream {
		private final ByteArrayOutputStream buffer;

		/**
		 * @param buffer the buffer to which all writes will be directed
		 */
		CapturingOutputStream(ByteArrayOutputStream buffer) {
			this.buffer = buffer;
		}

		@Override
		public void write(int b) {
			buffer.write(b);
		}

		@Override
		public void write(byte[] b, int off, int len) {
			buffer.write(b, off, len);
		}

		@Override
		public boolean isReady() {
			return true;
		}

		@Override
		public void setWriteListener(WriteListener writeListener) {
			// not needed for synchronous capture
		}
	}
}
