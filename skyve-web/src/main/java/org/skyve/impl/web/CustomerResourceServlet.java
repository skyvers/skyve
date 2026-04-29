package org.skyve.impl.web;

import java.io.File;
import java.io.IOException;

import org.skyve.CORE;
import org.skyve.content.MimeType;
import org.skyve.metadata.repository.Repository;
import org.skyve.util.Thumbnail;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.servlet.http.HttpServletRequest;

/**
 * Servlet that resolves and serves static resource files scoped to a customer, module,
 * or the framework itself.  Given a resource file name (and optional customer/module
 * context carried in request parameters), it locates the physical file through the
 * {@link org.skyve.metadata.repository.Repository} and streams it back to the client,
 * optionally scaling it to a thumbnail when width/height parameters are supplied.
 * <p>
 * The servlet also implements a fallback lookup strategy: if the requested file is not
 * found it strips a trailing {@code _<suffix>} segment (before the file extension) and
 * retries the lookup, which supports locale- or theme-variant resource naming conventions
 * used elsewhere in the framework.
 * </p>
 */
public class CustomerResourceServlet extends AbstractResourceServlet {
	private static final long serialVersionUID = 1L;

	private static final Logger LOGGER = LoggerFactory.getLogger(CustomerResourceServlet.class);

	/**
	 * An {@link AbstractResource} implementation backed by a {@link File} on the local
	 * file system.  Supports both full-content delivery and on-the-fly thumbnail
	 * generation via {@link Thumbnail}.
	 */
	protected static class FileResource extends AbstractResource {
		private File file;

		/**
		 * Releases the reference to the backing {@link File} and delegates to the
		 * superclass to release any other held resources.
		 */
		@Override
		public void dispose() throws Exception {
			super.dispose();
			file = null;
		}

		/**
		 * Returns the last-modified timestamp of the backing file, or the superclass
		 * default when no file has been resolved.
		 *
		 * @return milliseconds since the epoch, as per {@link File#lastModified()}
		 */
		@Override
		public long getLastModified() {
			return (file != null) ? file.lastModified() : super.getLastModified();
		}

		/**
		 * Loads the resource content from the backing file.
		 * <p>
		 * When both {@code imageWidth} and {@code imageHeight} are positive the file is
		 * scaled to the requested dimensions; otherwise the raw file content is wrapped
		 * in a {@link Thumbnail} and returned as-is.
		 * </p>
		 *
		 * @return a {@link Thumbnail} wrapping the file content, or {@code null} if no
		 *         file has been resolved
		 * @throws IOException if the file cannot be read
		 */
		@Override
		protected Thumbnail load() throws IOException {
			Thumbnail result = null;
			if (file != null) {
				if ((imageWidth > 0) && (imageHeight > 0)) { // a thumbnail image
					result = new Thumbnail(file, imageWidth, imageHeight);
				}
				else { // full content
					result = new Thumbnail(file);
				}
			}
			return result;
		}

		/**
		 * Resolves the MIME content type for the backing file by inspecting its extension.
		 *
		 * @return the MIME type string (e.g. {@code "image/png"}), or {@code null} if the
		 *         type cannot be determined or no file has been resolved
		 */
		@Override
		protected String resolveContentType() {
			if (file != null) {
				MimeType mimeType = MimeType.fromFileName(file.getName());
				if (mimeType != null) {
					return mimeType.toString();
				}
			}
			return null;
		}

		/**
		 * Returns the simple name of the backing file, or {@code null} if no file has
		 * been resolved.
		 *
		 * @return the file name, e.g. {@code "logo.png"}
		 */
		@Override
		protected String resolveFileName() {
			return (file != null) ? file.getName() : null;
		}
	}

	/**
	 * Creates a {@link FileResource} for the resource identified by the supplied request
	 * parameters.
	 * <p>
	 * Lookup is performed in two passes:
	 * <ol>
	 *   <li>The resource file name from {@code params} is looked up directly via the
	 *       repository for the given customer and module context.</li>
	 *   <li>If the file is not found and the name contains an underscore before the
	 *       extension (e.g. {@code icon_16.png}), the trailing {@code _<variant>} segment
	 *       is stripped (yielding {@code icon.png}) and the lookup is retried.</li>
	 * </ol>
	 * When neither pass resolves a file the returned resource will have no backing file
	 * and the superclass will respond with a 404.
	 * </p>
	 *
	 * @param request the current HTTP request
	 * @param params  parsed request parameters carrying the resource file name,
	 *                customer/module context, and optional image dimensions
	 * @return a {@link FileResource} (possibly with no backing file if not found)
	 * @throws Exception if repository access fails
	 */
	@Override
	@SuppressWarnings("java:S3776") // complexity is fine
	protected Resource createResource(HttpServletRequest request, RequestParams params) throws Exception {
		FileResource resource = new FileResource();
		resource.imageWidth = params.imageWidth();
		resource.imageHeight = params.imageHeight();

		if (params.resourceFileName() == null) {
			LOGGER.error("No resource file name or data file name in the URL");
		}
		else {
			Repository repository = CORE.getRepository();
			File tempFile = repository.findResourceFile(params.resourceFileName(), params.customerName(), params.moduleName());
			if (tempFile.exists()) {
				resource.file = tempFile;
			}
			else {
				int underscoreIndex = params.resourceFileName().lastIndexOf('_');
				if (underscoreIndex > 0) {
					int dotIndex = params.resourceFileName().lastIndexOf('.');
					if (dotIndex > underscoreIndex) {
						String baseFileName = params.resourceFileName().substring(0, underscoreIndex) +
												params.resourceFileName().substring(dotIndex);
						tempFile = repository.findResourceFile(baseFileName, params.customerName(), params.moduleName());
						if (tempFile.exists()) {
							resource.file = tempFile;
						}
					}
				}
			}
		}
		return resource;
	}
}

