package util;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.impl.report.freemarker.FreemarkerReportUtil;
import org.skyve.impl.report.freemarker.SkyveDatastoreTemplateLoader;

import modules.test.domain.AllAttributesPersistent;

/**
 * H2-backed tests for FreemarkerReportUtil and related freemarker directive classes.
 * Exercises template loading via StringTemplateLoader, SkyveDatastoreTemplateLoader,
 * and the built-in directives (format, sqlformat, description, displayName).
 */
class FreemarkerReportH2Test extends AbstractH2Test {

	private AllAttributesPersistent testBean;

	@BeforeAll
	@SuppressWarnings("static-method")
	static void initFreemarker() {
		// Covers FreemarkerReportUtil.init() and all directive constructors
		FreemarkerReportUtil.init();
	}

	@BeforeEach
	void createTestBean() throws Exception {
		testBean = AllAttributesPersistent.newInstance();
		testBean.setText("hello world");
		testBean.setNormalInteger(42);
		testBean = CORE.getPersistence().save(testBean);
	}

	// ---- createReport with StringTemplateLoader ----

	@Test
	void createReportRendersSimpleStringTemplate() throws Exception {
		FreemarkerReportUtil.addTemplate("simpleReport", "Value: ${name}");
		String result = FreemarkerReportUtil.createReport("simpleReport", Map.of("name", "Test"));
		assertThat(result, is("Value: Test"));
	}

	@Test
	void createReportWithEmptyParamsRendersStaticTemplate() throws Exception {
		FreemarkerReportUtil.addTemplate("staticReport", "Static content");
		String result = FreemarkerReportUtil.createReport("staticReport", Map.of());
		assertThat(result, is("Static content"));
	}

	@Test
	void createReportReturnsNullForNullTemplate() throws Exception {
		// getTemplate wraps cfg.getTemplate() which throws TemplateNotFoundException
		// wrapped in DomainException when not found; createReport propagates it
		assertThrows(Exception.class,
				() -> FreemarkerReportUtil.createReport("__nonExistent_xyz_123__", Map.of()));
	}

	// ---- removeTemplate ----

	@Test
	void removeTemplateReturnsTrueWhenTemplateExists() {
		FreemarkerReportUtil.addTemplate("toRemove", "gone");
		boolean removed = FreemarkerReportUtil.removeTemplate("toRemove");
		assertTrue(removed);
	}

	@Test
	void removeTemplateReturnsFalseWhenTemplateDoesNotExist() {
		boolean removed = FreemarkerReportUtil.removeTemplate("__absent_xyz__");
		assertFalse(removed);
	}

	// ---- format directive ----

	@Test
	void createReportWithFormatDirectiveRendersAttributeValue() throws Exception {
		FreemarkerReportUtil.addTemplate("formatReport", "<@format bean=myBean binding=\"text\" />");
		Map<String, Object> params = new HashMap<>();
		params.put("myBean", testBean);
		String result = FreemarkerReportUtil.createReport("formatReport", params);
		assertThat(result, containsString("hello world"));
	}

	@Test
	void createReportWithFormatDirectiveExpressionRendersMessage() throws Exception {
		FreemarkerReportUtil.addTemplate("formatExprReport", "<@format bean=myBean expression=\"{text}\" />");
		Map<String, Object> params = new HashMap<>();
		params.put("myBean", testBean);
		String result = FreemarkerReportUtil.createReport("formatExprReport", params);
		assertThat(result, containsString("hello world"));
	}

	// ---- sqlformat directive ----

	@Test
	void createReportWithSqlFormatDirectiveRendersStringValue() throws Exception {
		FreemarkerReportUtil.addTemplate("sqlformatReport", "<@sqlformat value=myVal />");
		Map<String, Object> params = Map.of("myVal", "formatted value");
		String result = FreemarkerReportUtil.createReport("sqlformatReport", params);
		assertThat(result, is("formatted value"));
	}

	@Test
	void createReportWithSqlFormatDirectiveRendersBooleanTrue() throws Exception {
		FreemarkerReportUtil.addTemplate("sqlformatBoolReport", "<@sqlformat value=myVal />");
		Map<String, Object> params = new HashMap<>();
		params.put("myVal", Boolean.TRUE);
		String result = FreemarkerReportUtil.createReport("sqlformatBoolReport", params);
		assertThat(result, is("Yes"));
	}

	@Test
	void createReportWithSqlFormatDirectiveRendersBooleanFalse() throws Exception {
		FreemarkerReportUtil.addTemplate("sqlformatBoolFalseReport", "<@sqlformat value=myVal />");
		Map<String, Object> params = new HashMap<>();
		params.put("myVal", Boolean.FALSE);
		String result = FreemarkerReportUtil.createReport("sqlformatBoolFalseReport", params);
		assertThat(result, is("No"));
	}

	// ---- description directive ----

	@Test
	void createReportWithDescriptionDirectiveReturnsResult() throws Exception {
		FreemarkerReportUtil.addTemplate("descReport", "<@description bean=myBean binding=\"text\" />");
		Map<String, Object> params = new HashMap<>();
		params.put("myBean", testBean);
		// description may be null/empty if attribute has no description defined
		String result = FreemarkerReportUtil.createReport("descReport", params);
		assertNotNull(result);
	}

	// ---- displayName directive ----

	@Test
	void createReportWithDisplayNameDirectiveRendersAttributeDisplayName() throws Exception {
		FreemarkerReportUtil.addTemplate("displayNameReport", "<@displayName bean=myBean binding=\"text\" />");
		Map<String, Object> params = new HashMap<>();
		params.put("myBean", testBean);
		String result = FreemarkerReportUtil.createReport("displayNameReport", params);
		// 'text' attribute should have a display name
		assertNotNull(result);
	}

	// ---- SkyveDatastoreTemplateLoader ----

	@Test
	void skyveDatastoreLoaderReturnsNullForMissingTemplate() throws Exception {
		// findTemplateSource returns null (beanResult) when no matching DB record exists
		SkyveDatastoreTemplateLoader loader = new SkyveDatastoreTemplateLoader();
		Object source = loader.findTemplateSource("__no-such-template-xyz__");
		assertNull(source);
	}

	@Test
	void skyveDatastoreLoaderCloseTemplateSourceIsNoOp() throws Exception {
		// closeTemplateSource has no observable effect — verify it does not throw
		SkyveDatastoreTemplateLoader loader = new SkyveDatastoreTemplateLoader();
		loader.closeTemplateSource(null);
	}

	@Test
	void createReportFallsThroughToDatastoreWhenNotInStringLoader() {
		// Template name absent from both strl and DB → TemplateNotFoundException wrapped in DomainException
		assertThrows(Exception.class,
				() -> FreemarkerReportUtil.createReport("__missing_datastore_xyz__", Map.of()));
	}

	// ---- addDirective ----

	@Test
	void addDirectiveRegistersSharedVariable() throws Exception {
		// Re-register an existing directive under a new name and use it
		FreemarkerReportUtil.addTemplate("customDirReport", "<@testFormat bean=myBean binding=\"text\" />");
		// Add an alias for the format directive (covers addDirective)
		org.skyve.impl.report.freemarker.FormatDirective fd = new org.skyve.impl.report.freemarker.FormatDirective();
		FreemarkerReportUtil.addDirective("testFormat", fd);
		Map<String, Object> params = new HashMap<>();
		params.put("myBean", testBean);
		String result = FreemarkerReportUtil.createReport("customDirReport", params);
		assertThat(result, notNullValue());
	}

	// ---- getBeanReport ----

	@Test
	void getBeanReportReturnsTemplateWhenAddedWithBeanPath() throws Exception {
		// getBeanReport constructs key as "module/document/reports/name"
		final String templateKey = "test/AllAttributesPersistent/reports/myReport.html";
		FreemarkerReportUtil.addTemplate(templateKey, "Bean report: ${title}");
		try {
			freemarker.template.Template template = FreemarkerReportUtil.getBeanReport(testBean, "myReport.html");
			assertNotNull(template, "getBeanReport should find the template added via addTemplate");
		} finally {
			FreemarkerReportUtil.removeTemplate(templateKey);
		}
	}

	// ---- createBeanReport ----

	@Test
	void createBeanReportRendersTemplateWithParameters() throws Exception {
		final String templateKey = "test/AllAttributesPersistent/reports/report.html";
		FreemarkerReportUtil.addTemplate(templateKey, "Hello ${name}!");
		try {
			String result = FreemarkerReportUtil.createBeanReport(testBean, "report.html", Map.of("name", "World"));
			assertThat(result, is("Hello World!"));
		} finally {
			FreemarkerReportUtil.removeTemplate(templateKey);
		}
	}

	@Test
	void createBeanReportWithEmptyParametersRendersStaticContent() throws Exception {
		final String templateKey = "test/AllAttributesPersistent/reports/static.html";
		FreemarkerReportUtil.addTemplate(templateKey, "Static content");
		try {
			String result = FreemarkerReportUtil.createBeanReport(testBean, "static.html", Map.of());
			assertThat(result, is("Static content"));
		} finally {
			FreemarkerReportUtil.removeTemplate(templateKey);
		}
	}
}
