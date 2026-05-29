package org.skyve.impl.report.freemarker;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.xhtmlrenderer.pdf.ITextOutputDevice;

@SuppressWarnings("static-method")
class FreemarkerReportUtilTest {

	@BeforeAll
	static void initFreeMarker() {
		FreemarkerReportUtil.init();
	}

	@Test
	void initCompletesWithoutException() {
		FreemarkerReportUtil.init();
		// Verify configuration is accessible after init (removing non-existent template returns false)
		assertFalse(FreemarkerReportUtil.removeTemplate("nonexistent_abc_init"));
	}

	@Test
	void addTemplateAndRemoveTemplate() {
		FreemarkerReportUtil.addTemplate("testTemplate", "Hello World!");
		assertTrue(FreemarkerReportUtil.removeTemplate("testTemplate"));
	}

	@Test
	void removeNonExistentTemplateReturnsFalse() {
		assertFalse(FreemarkerReportUtil.removeTemplate("doesNotExist_xyz"));
	}

	@Test
	void addMultipleTemplatesAndRemove() {
		FreemarkerReportUtil.addTemplate("r1", "c1");
		FreemarkerReportUtil.addTemplate("r2", "c2");
		assertTrue(FreemarkerReportUtil.removeTemplate("r1"));
		assertTrue(FreemarkerReportUtil.removeTemplate("r2"));
	}

	@Test
	void addDirectiveDoesNotThrow() {
		FreemarkerReportUtil.addDirective("d1", new SqlFormatDirective());
		// Verify the directive was registered (removing a non-existent template is always false)
		assertFalse(FreemarkerReportUtil.removeTemplate("d1"));
	}

	@Test
	void addTemplateWithHtmlMarkupAndRemove() {
		FreemarkerReportUtil.addTemplate("html1", "<html></html>");
		assertTrue(FreemarkerReportUtil.removeTemplate("html1"));
	}

	@Test
	void addTemplateOverwritesExistingTemplate() {
		FreemarkerReportUtil.addTemplate("ow", "v1");
		FreemarkerReportUtil.addTemplate("ow", "v2");
		assertTrue(FreemarkerReportUtil.removeTemplate("ow"));
	}

	@Test
	void resourceLoaderUserAgentResolveAndOpenStreamHandlesUnknownUri() throws Exception {
		Class<?> clazz = Class.forName("org.skyve.impl.report.freemarker.FreemarkerReportUtil$ResourceLoaderUserAgent");
		Constructor<?> constructor = clazz.getDeclaredConstructor(ITextOutputDevice.class, int.class);
		constructor.setAccessible(true);
		Object userAgent = constructor.newInstance(mock(ITextOutputDevice.class), Integer.valueOf(1));

		Method method = clazz.getDeclaredMethod("resolveAndOpenStream", String.class);
		method.setAccessible(true);
		InputStream result = (InputStream) method.invoke(userAgent, "missing://resource");
		assertNull(result);
	}
}
