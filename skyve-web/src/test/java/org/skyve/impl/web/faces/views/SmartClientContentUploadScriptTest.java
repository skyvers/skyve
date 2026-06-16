package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class SmartClientContentUploadScriptTest {
	private static final Path TYPES_SCRIPT = Path.of("src/js/desktop/types.js");
	private static final Path UTIL_SCRIPT = Path.of("src/js/desktop/util.js");
	private static final Path COMMON_UTIL_SCRIPT = Path.of("src/js/common/util.js");

	@Test
	void contentUploadLinkPreviewIsVerticallyCentredInEditor() throws IOException {
		String script = Files.readString(TYPES_SCRIPT);

		assertTrue(script.contains("_linkPreviewContents: function (contents)"), script);
		assertTrue(script.contains("const height = this._preview ? this._preview.getHeight() : this._previewHeight;"), script);
		assertTrue(script.contains("'<div style=\"line-height:' +"), script);
		assertTrue(script.contains("height +"), script);
		assertTrue(script.contains("this._linkPreviewContents(this.canvas.linkHTML(url, \"Content\", \"_blank\"))"), script);
		assertTrue(script.contains("this._preview.setContents(this._linkPreviewContents(\"&lt;Empty&gt;\"));"), script);
	}

	@Test
	void contentUploadUrlsUnsanitiseCompoundBindings() throws IOException {
		String commonUtil = Files.readString(COMMON_UTIL_SCRIPT);
		String desktopUtil = Files.readString(UTIL_SCRIPT);
		String types = Files.readString(TYPES_SCRIPT);

		assertTrue(commonUtil.contains("unsanitiseBinding: function (binding)"), commonUtil);
		assertTrue(commonUtil.contains("return binding.replace(/\\_(\\d*)\\_/g, \"[$1]\").replace(/\\_/g, \".\");"), commonUtil);
		assertTrue(desktopUtil.contains("upload.xhtml?_u=boundContent&_n=${SKYVE.Util.unsanitiseBinding("), desktopUtil);
		assertTrue(desktopUtil.contains("imageMarkup.xhtml?_n=${SKYVE.Util.unsanitiseBinding("), desktopUtil);
		assertTrue(desktopUtil.contains("url += `&_b=${SKYVE.Util.unsanitiseBinding(contentFormItem.form._view._b)}`;"), desktopUtil);
		assertTrue(types.contains("SKYVE.Util.unsanitiseBinding(this.name)"), types);
		assertTrue(types.contains("SKYVE.Util.unsanitiseBinding(this.name) +"), types);
	}
}
