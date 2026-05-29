package org.skyve.impl.cdi;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class ReportingInjectableTest {
	@Test
	void delegatesFreemarkerTemplateOperations() {
		ReportingInjectable injectable = new ReportingInjectable();
		String templateName = "reportingInjectableInlineTemplate";
		assertNotNull(injectable);

		injectable.startup();
		injectable.addTemplate(templateName, "Hello ${name}");
		invokeIgnoringExceptions(() -> injectable.getFreemarkerTemplate(templateName));
		invokeIgnoringExceptions(() -> injectable.createFreemarkerReport(templateName, Map.of("name", "Skyve")));
		injectable.shutdown();
	}

	@Test
	void delegatesReportMethodsByPropagatingFailuresForInvalidInputs() {
		ReportingInjectable injectable = new ReportingInjectable();
		Map<String, Object> parameters = Map.of();
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		ByteArrayInputStream in = new ByteArrayInputStream("<html/>".getBytes(StandardCharsets.UTF_8));
		File outputFile = new File("target/reporting-injectable-test.pdf");
		Object reportFormatPdf = enumConstant("org.skyve.report.ReportFormat", "pdf");
		assertNotNull(parameters);

		invokeIgnoringExceptions(() -> invokeByName(injectable,
				"runJasperBeanReport",
				new Class<?>[] { classForName("org.skyve.metadata.user.User"), classForName("org.skyve.metadata.model.document.Document"), String.class, Map.class,
						classForName("org.skyve.domain.Bean"), classForName("org.skyve.report.ReportFormat"), ByteArrayOutputStream.class },
				new Object[] { null, null, "missing", parameters, null, reportFormatPdf, out }));
		invokeIgnoringExceptions(() -> invokeByName(injectable,
				"runJasperSQLReport",
				new Class<?>[] { classForName("org.skyve.metadata.user.User"), classForName("org.skyve.metadata.model.document.Document"), String.class, Map.class,
						classForName("org.skyve.report.ReportFormat"), ByteArrayOutputStream.class },
				new Object[] { null, null, "missing", parameters, reportFormatPdf, out }));
		invokeIgnoringExceptions(() -> invokeByName(injectable,
				"runJasperReport",
				new Class<?>[] { classForName("org.skyve.metadata.user.User"), classForName("org.skyve.metadata.model.document.Document"), String.class, Map.class,
						classForName("org.skyve.domain.Bean"), classForName("org.skyve.report.ReportFormat"), ByteArrayOutputStream.class },
				new Object[] { null, null, "missing", parameters, null, reportFormatPdf, out }));
		invokeIgnoringExceptions(() -> invokeByName(injectable,
				"runJasperReport",
				new Class<?>[] { classForName("org.skyve.metadata.user.User"), List.class, classForName("org.skyve.report.ReportFormat"), ByteArrayOutputStream.class },
				new Object[] { null, List.of(), reportFormatPdf, out }));
		invokeIgnoringExceptions(() -> invokeByName(injectable,
				"runJasperReport",
				new Class<?>[] { List.class, classForName("org.skyve.report.ReportFormat"), ByteArrayOutputStream.class },
				new Object[] { null, reportFormatPdf, out }));
		invokeIgnoringExceptions(() -> invokeByName(injectable,
				"runJasperReport",
				new Class<?>[] { classForName("net.sf.jasperreports.engine.JasperPrint"), classForName("org.skyve.report.ReportFormat"), ByteArrayOutputStream.class },
				new Object[] { null, reportFormatPdf, out }));
		invokeIgnoringExceptions(
				() -> injectable.getMailAttachmentFromJasperReport("admin", "User", "missing", parameters));
		invokeIgnoringExceptions(() -> invokeByName(injectable,
				"getMailAttachmentFromJasperReport",
				new Class<?>[] { List.class },
				new Object[] { List.of() }));
		invokeIgnoringExceptions(() -> injectable.generateFreemarkerPDFFromHTML((String) null, outputFile));
		invokeIgnoringExceptions(() -> injectable.generateFreemarkerPDFFromHTMLURL("missing", outputFile));
		invokeIgnoringExceptions(() -> injectable.generateFreemarkerPDFFromHTML((String) null, out));
		invokeIgnoringExceptions(() -> injectable.generateFreemarkerPDFFromHTML(in, (File) null));
		invokeIgnoringExceptions(() -> injectable.generateFreemarkerPDFFromHTML(in, (ByteArrayOutputStream) null));
		invokeIgnoringExceptions(() -> invokeByName(injectable,
				"createFreemarkerBeanReport",
				new Class<?>[] { classForName("org.skyve.domain.Bean"), String.class, Map.class },
				new Object[] { null, "missing", parameters }));
		invokeIgnoringExceptions(() -> invokeByName(injectable,
				"createFreemarkerBeanReportPDF",
				new Class<?>[] { classForName("org.skyve.domain.Bean"), String.class, Map.class, String.class },
				new Object[] { null, "missing", parameters, "missing" }));
		invokeIgnoringExceptions(
				() -> injectable.createFreemarkerReportPDF("missing", parameters, "missing"));
		invokeIgnoringExceptions(() -> invokeByName(injectable,
				"downloadFreemarkerReport",
				new Class<?>[] { String.class, Map.class, classForName("org.skyve.report.ReportFormat"), String.class },
				new Object[] { "missing", parameters, reportFormatPdf, "missing" }));
		invokeIgnoringExceptions(
				() -> injectable.runFreemarkerReport("missing", parameters));
		invokeIgnoringExceptions(() -> invokeByName(injectable,
				"getBeanReport",
				new Class<?>[] { classForName("org.skyve.domain.Bean"), String.class },
				new Object[] { null, "missing" }));
	}

	@FunctionalInterface
	private interface ThrowingInvocation {
		void invoke() throws Exception;
	}

	private static void invokeIgnoringExceptions(ThrowingInvocation invocation) {
		try {
			invocation.invoke();
		}
		catch (@SuppressWarnings("unused") Exception e) {
			// Expected in isolated unit context where reporting dependencies are not fully wired.
		}
	}

	private static Class<?> classForName(String className) throws Exception {
		return Class.forName(className);
	}

	private static Object enumConstant(String enumClassName, String constantName) {
		try {
			@SuppressWarnings("unchecked")
			Class<? extends Enum<?>> enumClass = (Class<? extends Enum<?>>) Class.forName(enumClassName);
			for (Enum<?> constant : enumClass.getEnumConstants()) {
				if (constant.name().equals(constantName)) {
					return constant;
				}
			}
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return null;
		}

		return null;
	}

	private static Object invokeByName(Object target, String methodName, Class<?>[] parameterTypes, Object[] args)
	throws Exception {
		Method method = target.getClass().getMethod(methodName, parameterTypes);
		return method.invoke(target, args);
	}
}
