package org.skyve.impl.report.jasperreports;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.HashMap;
import java.util.List;
import java.util.Locale;

import org.junit.jupiter.api.Test;

import net.sf.jasperreports.engine.JasperCompileManager;
import net.sf.jasperreports.engine.JasperFillManager;
import net.sf.jasperreports.engine.JasperReport;
import net.sf.jasperreports.engine.data.JRBeanCollectionDataSource;
import net.sf.jasperreports.engine.design.JRDesignBand;
import net.sf.jasperreports.engine.design.JRDesignField;
import net.sf.jasperreports.engine.design.JRDesignSection;
import net.sf.jasperreports.engine.design.JRDesignTextField;
import net.sf.jasperreports.engine.design.JasperDesign;

class BeanCollectionReportTest {

	@Test
	@SuppressWarnings("static-method")
	void fillsReportFromBeanCollection() throws Exception {
		JasperDesign design = new JasperDesign();
		design.setName("beanCollection");
		design.setPageWidth(200);
		design.setPageHeight(200);
		design.setColumnWidth(160);
		design.setLeftMargin(20);
		design.setRightMargin(20);
		design.setTopMargin(20);
		design.setBottomMargin(20);

		JRDesignField field = new JRDesignField();
		field.setName("language");
		field.setValueClass(String.class);
		design.addField(field);

		JRDesignTextField text = new JRDesignTextField();
		text.setX(0);
		text.setY(0);
		text.setWidth(160);
		text.setHeight(20);
		text.setExpression(new net.sf.jasperreports.engine.design.JRDesignExpression("$F{language}"));
		JRDesignBand detail = new JRDesignBand();
		detail.setHeight(20);
		detail.addElement(text);
		((JRDesignSection) design.getDetailSection()).addBand(detail);

		JasperReport report = JasperCompileManager.compileReport(design);
		JRBeanCollectionDataSource dataSource = new JRBeanCollectionDataSource(List.of(Locale.ENGLISH));
		assertEquals(1, JasperFillManager.fillReport(report, new HashMap<>(), dataSource).getPages().size());
	}
}
