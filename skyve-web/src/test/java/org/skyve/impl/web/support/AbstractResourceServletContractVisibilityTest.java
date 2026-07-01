package org.skyve.impl.web.support;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.io.IOException;
import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.Test;
import org.skyve.impl.web.AbstractResourceServlet;
import org.skyve.impl.web.AbstractResourceServlet.AbstractResource;
import org.skyve.impl.web.AbstractResourceServlet.RequestParams;
import org.skyve.impl.web.AbstractResourceServlet.Resource;
import org.skyve.util.Thumbnail;

@SuppressWarnings("static-method")
class AbstractResourceServletContractVisibilityTest {
	@Test
	void resourceContractTypesAreUsableOutsideServletPackage() throws Exception {
		RequestParams params = new RequestParams(
				"admin",
				"User",
				"contact",
				"avatar",
				0,
				0,
				null,
				null,
				null);
		Resource resource = new ExternalResource();

		assertEquals("admin", params.moduleName());
		assertArrayEquals("external".getBytes(StandardCharsets.UTF_8), resource.getBytes());
		assertEquals("text/plain", resource.getContentType());
		assertEquals("external.txt", resource.getFileName());
		resource.dispose();
	}

	@Test
	void abstractResourceCanBeExtendedOutsideServletPackage() throws Exception {
		ExternalAbstractResource resource = new ExternalAbstractResource();

		assertNull(resource.getBytes());
		assertEquals("image/svg+xml", resource.getContentType());
		assertEquals("placeholder.svg", resource.getFileName());
	}

	private static final class ExternalResource implements AbstractResourceServlet.Resource {
		@Override
		public void dispose() {
			// No external resource is held.
		}

		@Override
		public byte[] getBytes() {
			return "external".getBytes(StandardCharsets.UTF_8);
		}

		@Override
		public String getContentType() {
			return "text/plain";
		}

		@Override
		public String getFileName() {
			return "external.txt";
		}
	}

	private static final class ExternalAbstractResource extends AbstractResource {
		@Override
		protected Thumbnail load() throws IOException {
			return null;
		}

		@Override
		protected String resolveContentType() {
			return "image/svg+xml";
		}

		@Override
		protected String resolveFileName() {
			return "placeholder.svg";
		}
	}
}
