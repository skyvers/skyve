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
		assertTrue(script.contains("this.emptyText = config.emptyText || \"No content\";"), script);
		assertTrue(script.contains("this._preview.setContents(this._emptyPreviewContents());"), script);
		assertTrue(script.contains("_emptyPreviewContents: function ()"), script);
		assertTrue(script.contains("String(this.emptyText)\n\t\t\t\t.replace(/&/g, \"&amp;\")\n\t\t\t\t.replace(/</g, \"&lt;\")\n\t\t\t\t.replace(/>/g, \"&gt;\")"), script);
		assertTrue(script.contains("border:1px solid #bfbfbf;box-sizing:border-box;width:${this._preview.getWidth()}px;height:${this._preview.getHeight()}px;object-fit:contain"), script);
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

	@Test
	void popupOwnerResolversSearchParentFrames() throws IOException {
		String commonUtil = Files.readString(COMMON_UTIL_SCRIPT);

		assertTrue(commonUtil.contains("findOwnerWindow: function (predicate)"), commonUtil);
		assertTrue(commonUtil.contains("let result = window.parent;"), commonUtil);
		assertTrue(commonUtil.contains("while (result && result !== window)"), commonUtil);
		assertTrue(commonUtil.contains("catch (ignore)"), commonUtil);
		assertTrue(commonUtil.contains("findSmartClientWindow: function ()"), commonUtil);
		assertTrue(commonUtil.contains("return !!(owner.isc && owner.isc.WindowStack);"), commonUtil);
		assertTrue(commonUtil.contains("findPrimeFacesWindow: function ()"), commonUtil);
		assertTrue(commonUtil.contains("return !!(owner.SKYVE && owner.SKYVE.PF);"), commonUtil);
		assertTrue(commonUtil.contains("findSkyveWindow: function ()"), commonUtil);
		assertTrue(commonUtil.contains("(owner.isc && owner.isc.WindowStack) ||"), commonUtil);
	}
}
