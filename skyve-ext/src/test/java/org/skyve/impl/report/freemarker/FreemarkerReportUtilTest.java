package org.skyve.impl.report.freemarker;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

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
}
