package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class PrimeFacesContentUploadScriptTest {
	private static final Path SCRIPT = Path.of("src/js/prime/pf.js");

	@Test
	void mediaKindCallbackUsesExactCompanionFieldWithoutFallingBackToContentField() throws IOException {
		String script = Files.readString(SCRIPT);

		assertTrue(script.contains("var setExactContentValue = function(root, id, binding, suffix, value)"), script);
		assertTrue(script.contains("var getClientIdSelector = function(localId)"), script);
		assertTrue(script.contains("return '[id=\"' + localId + '\"],[id$=\":' + localId + '\"]';"), script);
		assertTrue(script.contains("return getClientIdSelector(id ? id + '_' + binding + suffix : '_' + binding + suffix);"), script);
		assertFalse(script.contains("[id$=\"' + id + '_' + binding + suffix + '\"]"), script);
		assertTrue(script.contains("var unsanitiseBinding = function(binding)"), script);
		assertTrue(script.contains("return binding.replace(/\\_(\\d*)\\_/g, '[$1]').replace(/\\_/g, '.');"), script);
		assertTrue(script.contains("afterContentUpload: function(binding, contentId, modoc, fileName, mediaKind, companionBinding)"), script);
		assertTrue(script.contains("if (mediaKind && companionBinding) {\n\t\t\t\tsetExactContentValue(top, id, companionBinding, '_hidden', mediaKind);\n\t\t\t}"), script);
		assertTrue(script.contains("var showContentVideo = function(root, id, binding, url)"), script);
		assertTrue(script.contains("var video = container.children('video');"), script);
		assertTrue(script.contains("container.append('<video controls preload=\"metadata\" style=\"width:100%;height:100%;object-fit:contain\" src=\"' + url + '\"></video>');"), script);
		assertTrue(script.contains("var showContentVideoPlaceholder = function(root, id, binding)"), script);
		assertTrue(script.contains("root.$(this).children('video').remove();"), script);
		assertTrue(script.contains("var setAutoContentVisibility = function(root, id, binding, mediaKind)"), script);
		assertTrue(script.contains("link.toggleClass('skyveContentHidden', !! mediaKind && mediaKind !== 'link');"), script);
		assertTrue(script.contains("image.parent().toggleClass('skyveContentHidden', ! showImage);"), script);
		assertTrue(script.contains("video.parent().toggleClass('skyveContentHidden', ! showVideo);"), script);
		assertTrue(script.contains("var getContentMarkupItems = function(root, id, binding)"), script);
		assertTrue(script.contains("toggleClass('skyveContentHidden', mediaKind !== 'image');"), script);
		assertTrue(script.contains("showContentVideo(top, id, binding, url);"), script);
		assertTrue(script.contains("setAutoContentVisibility(top, id, binding, mediaKind);"), script);
		assertTrue(script.contains("setContentMarkupVisibility(top, id, binding, mediaKind);"), script);
		assertTrue(script.contains("showContentVideoPlaceholder(top, id, binding);"), script);
		assertTrue(script.contains("setAutoContentVisibility(top, id, binding, '');"), script);
		assertTrue(script.contains("setContentMarkupVisibility(top, id, binding, '');"), script);
		assertFalse(script.contains("replaceWith('<video"));
		assertFalse(script.contains("replaceWith('<div"));
		assertTrue(script.contains("setExactContentValue(top, id, companionBinding, '_hidden', 'image');"), script);
		assertTrue(script.contains("setAutoContentVisibility(top, id, binding, 'image');"), script);
		assertTrue(script.contains("setContentMarkupVisibility(top, id, binding, 'image');"), script);
		assertTrue(script.contains("'&_b=' + unsanitiseBinding(binding)"), script);
		assertFalse(script.contains("setContentValue(top, id, '_' + binding, '_hidden', mediaKind || '');"), script);
		assertFalse(script.contains("setContentValue(top, id, '_' + binding, '_hidden', 'image');"), script);
	}
}
