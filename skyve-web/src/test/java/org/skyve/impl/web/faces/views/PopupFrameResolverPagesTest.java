package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class PopupFrameResolverPagesTest {
	private static final Path UPLOAD_PAGE = webappPage("upload.xhtml");
	private static final Path BIZ_IMPORT_PAGE = webappPage("bizImport.xhtml");
	private static final Path IMAGE_MARKUP_PAGE = webappPage("imageMarkup.xhtml");

	@Test
	void uploadPageLoadsCommonBundleBeforeActionUploadCallback() throws IOException {
		String page = Files.readString(UPLOAD_PAGE);

		assertTrue(page.contains("skyve/prime/skyve-min.js?v=#{_skyveContent.webResourceFileVersion}"), page);
		assertTrue(page.indexOf("skyve/prime/skyve-min.js") < page.indexOf("function skyveAfterActionUpload"), page);
		assertTrue(page.contains("var owner = SKYVE.Util.findSmartClientWindow();"), page);
		assertTrue(page.contains("oncomplete=\"skyveAfterActionUpload('#{_skyveContent.uploadState.uploadKind}')\""), page);
		assertFalse(page.contains("window.parent.isc"), page);
	}

	@Test
	void bizImportPageLoadsCommonBundleBeforeCompletionCallback() throws IOException {
		String page = Files.readString(BIZ_IMPORT_PAGE);

		assertTrue(page.contains("skyve/prime/skyve-min.js?v=#{_skyveBizImport.webResourceFileVersion}"), page);
		assertTrue(page.indexOf("skyve/prime/skyve-min.js") < page.indexOf("function skyveAfterBizImport"), page);
		assertTrue(page.contains("var owner = SKYVE.Util.findSmartClientWindow();"), page);
		assertTrue(page.contains("oncomplete=\"skyveAfterBizImport()\""), page);
		assertFalse(page.contains("window.parent.isc"), page);
	}

	@Test
	void imageMarkupPageLoadsCommonBundleForApplyCallback() throws IOException {
		String page = Files.readString(IMAGE_MARKUP_PAGE);

		assertTrue(page.contains("skyve/prime/skyve-min.js?v=#{_skyveMarkup.webResourceFileVersion}"), page);
		assertFalse(page.contains("window.parent.isc"), page);
		assertFalse(page.contains("top.SKYVE"), page);
	}

	private static Path webappPage(String fileName) {
		Path fromModule = Path.of("../skyve-war/src/main/webapp", fileName);
		if (Files.exists(fromModule)) {
			return fromModule;
		}
		return Path.of("skyve-war/src/main/webapp", fileName);
	}
}
