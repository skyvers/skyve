package org.skyve.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.impl.report.jasperreports.JasperReportUtil;
import org.skyve.report.ReportFormat;

import net.sf.jasperreports.engine.JasperCompileManager;
import net.sf.jasperreports.engine.JasperPrint;
import net.sf.jasperreports.engine.JasperReport;
import net.sf.jasperreports.engine.design.JRDesignBand;
import net.sf.jasperreports.engine.design.JRDesignExpression;
import net.sf.jasperreports.engine.design.JRDesignField;
import net.sf.jasperreports.engine.design.JRDesignQuery;
import net.sf.jasperreports.engine.design.JRDesignSection;
import net.sf.jasperreports.engine.design.JRDesignTextField;
import net.sf.jasperreports.engine.design.JasperDesign;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class JasperSqlReportH2Test extends AbstractH2Test {
	@Test
	void fillsSqlReportFromH2Connection() throws Exception {
		JasperDesign design = new JasperDesign();
		design.setName("sqlReport");
		design.setPageWidth(200);
		design.setPageHeight(200);
		design.setColumnWidth(160);
		design.setLeftMargin(20);
		design.setRightMargin(20);
		design.setTopMargin(20);
		design.setBottomMargin(20);

		JRDesignQuery query = new JRDesignQuery();
		query.setLanguage("sql");
		query.setText("SELECT 'sql report' AS REPORT_VALUE");
		design.setQuery(query);

		JRDesignField field = new JRDesignField();
		field.setName("REPORT_VALUE");
		field.setValueClass(String.class);
		design.addField(field);

		JRDesignTextField text = new JRDesignTextField();
		text.setX(0);
		text.setY(0);
		text.setWidth(160);
		text.setHeight(20);
		text.setExpression(new JRDesignExpression("$F{REPORT_VALUE}"));
		JRDesignBand detail = new JRDesignBand();
		detail.setHeight(20);
		detail.addElement(text);
		((JRDesignSection) design.getDetailSection()).addBand(detail);

		JasperReport report = JasperCompileManager.compileReport(design);
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		JasperPrint print = JasperReportUtil.runReport(report, CORE.getPersistence().getUser(), null,
				new HashMap<>(), null, ReportFormat.csv, out);

		assertEquals(1, print.getPages().size());
		assertTrue(out.toString(StandardCharsets.UTF_8).contains("sql report"));
	}
}
