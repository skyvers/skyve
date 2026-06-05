package org.skyve.impl.report.jasperreports;

import static org.junit.Assert.assertEquals;

import java.io.ByteArrayOutputStream;
import java.lang.reflect.Method;
import java.util.stream.Stream;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.skyve.report.ReportFormat;

import net.sf.jasperreports.engine.export.HtmlExporter;
import net.sf.jasperreports.engine.export.JRCsvExporter;
import net.sf.jasperreports.engine.export.JRRtfExporter;
import net.sf.jasperreports.engine.export.JRTextExporter;
import net.sf.jasperreports.engine.export.JRXmlExporter;
import net.sf.jasperreports.engine.export.oasis.JROdsExporter;
import net.sf.jasperreports.engine.export.oasis.JROdtExporter;
import net.sf.jasperreports.engine.export.ooxml.JRDocxExporter;
import net.sf.jasperreports.engine.export.ooxml.JRPptxExporter;
import net.sf.jasperreports.engine.export.ooxml.JRXlsxExporter;
import net.sf.jasperreports.pdf.JRPdfExporter;
import net.sf.jasperreports.poi.export.JRXlsExporter;

@RunWith(Parameterized.class)
public class JasperReportUtilTest {

	private final ReportFormat format;
	private final Class<?> exporterClass;

	public JasperReportUtilTest(ReportFormat format, Class<?> exporterClass) {
		this.format = format;
		this.exporterClass = exporterClass;
	}

	@Parameters(name = "{0}")
	public static Iterable<Object[]> formats() {
		return Stream.of(new Object[][] {
			{ReportFormat.txt, JRTextExporter.class},
			{ReportFormat.csv, JRCsvExporter.class},
			{ReportFormat.html, HtmlExporter.class},
			{ReportFormat.pdf, JRPdfExporter.class},
			{ReportFormat.xls, JRXlsExporter.class},
			{ReportFormat.rtf, JRRtfExporter.class},
			{ReportFormat.odt, JROdtExporter.class},
			{ReportFormat.ods, JROdsExporter.class},
			{ReportFormat.docx, JRDocxExporter.class},
			{ReportFormat.xlsx, JRXlsxExporter.class},
			{ReportFormat.pptx, JRPptxExporter.class},
			{ReportFormat.xml, JRXmlExporter.class}
		})::iterator;
	}

	@Test
	public void getExporterReturnsExpectedExporterForFormat() throws Exception {
		Method method = JasperReportUtil.class.getDeclaredMethod("getExporter", ReportFormat.class, java.io.OutputStream.class);
		method.setAccessible(true);

		Object exporter = method.invoke(null, format, new ByteArrayOutputStream());

		assertEquals(exporterClass, exporter.getClass());
	}
}
