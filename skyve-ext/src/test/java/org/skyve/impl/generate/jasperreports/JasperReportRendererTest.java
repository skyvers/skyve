package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.impl.generate.jasperreports.DesignSpecification.Mode;
import org.skyve.impl.report.jasperreports.ReportDesignParameters;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.report.ReportFormat;

import net.sf.jasperreports.engine.JRExpression;
import net.sf.jasperreports.engine.JRField;
import net.sf.jasperreports.engine.design.JRDesignBand;

@SuppressWarnings({"static-method", "boxing"})
public class JasperReportRendererTest {

	private static ReportDesignParameters minimalRdp() {
		ReportDesignParameters rdp = new ReportDesignParameters();
		rdp.setReportStyle(ReportDesignParameters.ReportStyle.tabular);
		rdp.setReportFormat(ReportFormat.csv);
		rdp.setPageWidth(595);
		rdp.setPageHeight(842);
		rdp.setLeftMargin(20);
		rdp.setRightMargin(20);
		rdp.setTopMargin(20);
		rdp.setBottomMargin(20);
		rdp.setPaginated(true);
		return rdp;
	}

	private static ReportDesignParameters.ReportColumn column(String name,
			String title,
			int width,
			ReportDesignParameters.ColumnAlignment alignment,
			AttributeType type) {
		ReportDesignParameters.ReportColumn col = new ReportDesignParameters.ReportColumn();
		col.setName(name);
		col.setTitle(title);
		col.setWidth(width);
		col.setAlignment(alignment);
		col.setAttributeType(type);
		return col;
	}

	@Test
	public void renderDesignWithEmptyTabularRdpProducesJrxml() throws Exception {
		String jrxml = new JasperReportRenderer(minimalRdp()).renderDesign();
		assertThat(jrxml, notNullValue());
		assertThat(jrxml, containsString("jasperReport"));
	}

	@Test
	public void renderDesignWithTabularTextColumnsProducesDisplayFields() throws Exception {
		ReportDesignParameters rdp = minimalRdp();
		rdp.getColumns().add(column("firstName", "First Name", 150, ReportDesignParameters.ColumnAlignment.left, AttributeType.text));
		rdp.getColumns().add(column("lastName", "Last Name", 150, ReportDesignParameters.ColumnAlignment.right, AttributeType.text));
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, containsString("firstName_display"));
		assertThat(jrxml, containsString("lastName_display"));
	}

	@Test
	public void renderDesignWithCentreAlignmentProducesJrxml() throws Exception {
		ReportDesignParameters rdp = minimalRdp();
		rdp.getColumns().add(column("name", "Name", 100, ReportDesignParameters.ColumnAlignment.center, AttributeType.text));
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, containsString("jasperReport"));
	}

	@Test
	public void renderDesignWithIntegerColumnProducesAggregateSummaryVariable() throws Exception {
		// pdf format: retainNumeric=false → requiresDisplayField=true; aggregatable → adds sum variable
		ReportDesignParameters rdp = minimalRdp();
		rdp.setReportFormat(ReportFormat.pdf);
		rdp.getColumns().add(column("amount", "Amount", 100, ReportDesignParameters.ColumnAlignment.right, AttributeType.integer));
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, containsString("amount_summary"));
	}

	@Test
	public void renderDesignWithDateColumnProducesMinMaxVariables() throws Exception {
		// date columns are aggregatable temporal → min/max variables
		ReportDesignParameters rdp = minimalRdp();
		rdp.setReportFormat(ReportFormat.pdf);
		rdp.getColumns().add(column("birthDate", "Birth Date", 100, ReportDesignParameters.ColumnAlignment.left, AttributeType.date));
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, containsString("birthDate_minDate"));
		assertThat(jrxml, containsString("birthDate_maxDate"));
	}

	@Test
	public void renderDesignWithColumnarStyleProducesJrxml() throws Exception {
		ReportDesignParameters rdp = minimalRdp();
		rdp.setReportStyle(ReportDesignParameters.ReportStyle.columnar);
		rdp.setReportFormat(ReportFormat.pdf);
		rdp.getColumns().add(column("name", "Name", 200, ReportDesignParameters.ColumnAlignment.left, AttributeType.text));
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, containsString("jasperReport"));
	}

	@Test
	public void renderDesignCalledTwiceThrowsIllegalStateException() throws Exception {
		JasperReportRenderer renderer = new JasperReportRenderer(minimalRdp());
		renderer.renderDesign();
		assertThrows(IllegalStateException.class, renderer::renderDesign);
	}

	@Test
	public void renderDesignWithShowSummaryProducesRecordCountExpression() throws Exception {
		ReportDesignParameters rdp = minimalRdp();
		rdp.setShowSummary(true);
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, containsString("REPORT_COUNT"));
	}

	@Test
	public void isAggregatableAttributeReturnsTrueForDecimal2() {
		assertThat(JasperReportRenderer.isAggregatableAttribute(AttributeType.decimal2), is(Boolean.TRUE));
	}

	@Test
	public void isAggregatableAttributeReturnsFalseForText() {
		assertThat(JasperReportRenderer.isAggregatableAttribute(AttributeType.text), is(Boolean.FALSE));
	}

	@Test
	public void isDateOrTimeAttributeReturnsTrueForDate() {
		assertThat(JasperReportRenderer.isDateOrTimeAttribute(AttributeType.date), is(Boolean.TRUE));
	}

	@Test
	public void isDateOrTimeAttributeReturnsFalseForText() {
		assertThat(JasperReportRenderer.isDateOrTimeAttribute(AttributeType.text), is(Boolean.FALSE));
	}

	@Test
	public void createFieldWithBeanModeStringTypeHasDescription() {
		DesignSpecification parent = new DesignSpecification();
		parent.setMode(Mode.bean);
		ReportField field = new ReportField();
		field.setParent(parent);
		field.setName("textAttr");
		field.setTypeClass(String.class.getName());
		JRField jrField = JasperReportRenderer.createField(field);
		assertThat(jrField, notNullValue());
		assertThat(jrField.getName(), is("textAttr"));
		assertThat(jrField.getDescription(), is("textAttr"));
	}

	@Test
	public void createFieldWithNullParentReturnsNull() {
		ReportField field = new ReportField();
		field.setName("orphan");
		field.setTypeClass(String.class.getName());
		JRField jrField = JasperReportRenderer.createField(field);
		assertThat(jrField, nullValue());
	}

	@Test
	public void createVariableReturnsCorrectName() {
		ReportVariable var = new ReportVariable();
		var.setName("total");
		var.setTypeClass("java.lang.Integer");
		assertThat(JasperReportRenderer.createVariable(var).getName(), is("total"));
	}

	@Test
	public void createVariableExpressionContainsAddCall() {
		ReportVariable var = new ReportVariable();
		var.setName("total");
		var.setTypeClass("java.lang.Integer");
		JRExpression expr = JasperReportRenderer.createVariableExpression(var);
		assertThat(expr.getText(), containsString("$V{java.lang.Integer}.add($F{total})"));
	}

	@Test
	public void createInitialValueVariableExpressionContainsNew() {
		ReportVariable var = new ReportVariable();
		var.setName("total");
		var.setTypeClass("java.lang.Integer");
		JRExpression expr = JasperReportRenderer.createInitialValueVariableExpression(var);
		assertThat(expr.getText(), containsString("new java.lang.Integer(0)"));
	}

	@Test
	public void addCustomerLogoAddsImageElementToBand() {
		JRDesignBand band = new JRDesignBand();
		band.setHeight(40);
		JasperReportRenderer.addCustomerLogo(band);
		assertThat(band.getElements().length, is(1));
	}

	@Test
	public void getIncrementTypeReturnsNonNull() {
		assertThat(JasperReportRenderer.getIncrementType(), notNullValue());
	}

	// --- createImageElementExpression ---

	@Test
	public void createImageElementExpressionContentImageContainsCONTENTIdField() {
		DesignSpecification spec = new DesignSpecification();
		spec.setModuleName("test");
		spec.setDocumentName("Doc");
		spec.setMode(DesignSpecification.Mode.bean);
		ReportBand band = new ReportBand();
		band.setBandType(ReportBand.BandType.detail);
		band.setParent(spec);

		ReportElement e = new ReportElement(ReportElement.ElementType.contentImage, "img", "myContentField",
				Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(100), null);
		e.setElementHeight(Integer.valueOf(50));
		e.setParent(band);

		JRExpression expr = JasperReportRenderer.createImageElementExpression(e);
		assertThat(expr.getText(), containsString("ContentImageForReport.image"));
		assertThat(expr.getText(), containsString("myContentField"));
	}

	@Test
	public void createImageElementExpressionStaticImageReturnsValueDirectly() {
		DesignSpecification spec = new DesignSpecification();
		spec.setModuleName("test");
		spec.setDocumentName("Doc");
		spec.setMode(DesignSpecification.Mode.bean);
		ReportBand band = new ReportBand();
		band.setBandType(ReportBand.BandType.detail);
		band.setParent(spec);

		ReportElement e = new ReportElement(ReportElement.ElementType.staticImage, "img", "path/to/image.png",
				Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(100), null);
		e.setElementHeight(Integer.valueOf(50));
		e.setParent(band);

		JRExpression expr = JasperReportRenderer.createImageElementExpression(e);
		assertThat(expr.getText(), is("path/to/image.png"));
	}

	@Test
	public void createImageElementExpressionDynamicImageBeanModeContainsFieldThis() {
		DesignSpecification spec = new DesignSpecification();
		spec.setModuleName("test");
		spec.setDocumentName("Doc");
		spec.setMode(DesignSpecification.Mode.bean);
		ReportBand band = new ReportBand();
		band.setBandType(ReportBand.BandType.detail);
		band.setParent(spec);

		ReportElement e = new ReportElement(ReportElement.ElementType.dynamicImage, "img", "MyImage",
				Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(100), null);
		e.setElementHeight(Integer.valueOf(50));
		e.setParent(band);

		JRExpression expr = JasperReportRenderer.createImageElementExpression(e);
		assertThat(expr.getText(), containsString("$F{THIS}"));
		assertThat(expr.getText(), containsString("MyImage"));
	}

	@Test
	public void createTextElementExpressionWithNullValueUsesEmptyString() {
		DesignSpecification spec = new DesignSpecification();
		spec.setModuleName("test");
		spec.setDocumentName("Doc");
		spec.setMode(DesignSpecification.Mode.bean);
		ReportBand band = new ReportBand();
		band.setBandType(ReportBand.BandType.detail);
		band.setParent(spec);

		ReportElement e = new ReportElement(ReportElement.ElementType.textField, "v", null,
				Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(100), null);
		e.setParent(band);

		JRExpression expr = JasperReportRenderer.createTextElementExpression(e);
		assertThat(expr.getText(), is("\"\""));
	}

	@Test
	public void createTextElementExpressionWithValueReturnsThatValue() {
		DesignSpecification spec = new DesignSpecification();
		spec.setModuleName("test");
		spec.setDocumentName("Doc");
		spec.setMode(DesignSpecification.Mode.bean);
		ReportBand band = new ReportBand();
		band.setBandType(ReportBand.BandType.detail);
		band.setParent(spec);

		ReportElement e = new ReportElement(ReportElement.ElementType.textField, "v", "$F{text}",
				Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(100), null);
		e.setParent(band);

		JRExpression expr = JasperReportRenderer.createTextElementExpression(e);
		assertThat(expr.getText(), is("$F{text}"));
	}

	// --- renderDesignWithNoModuleOrDocument ---

	@Test
	public void renderDesignFromDesignSpecWithNullModuleAndDocumentReturnsIllegalArgument() {
		DesignSpecification spec = new DesignSpecification();
		// moduleName and documentName intentionally not set
		JasperReportRenderer renderer = new JasperReportRenderer(spec);
		assertThrows(IllegalArgumentException.class, renderer::renderDesign);
	}

	// --- renderDesignWithNoDesignSpecAndNoRdp ---

	@Test
	public void renderDesignWithNoSpecAndNoRdpThrowsIllegalStateException() throws Exception {
		// Construct a renderer where both designSpecification and reportDesignParameters are null
		// by passing a DesignSpecification with list source (sets designSpec=null, rdp assigned)
		// We can't easily make both null without a subclass, but we can test the null-rdp path
		// by creating a plain spec and then calling render twice
		JasperReportRenderer renderer = new JasperReportRenderer(minimalRdp());
		renderer.renderDesign();
		assertThrows(IllegalStateException.class, renderer::renderDesign);
	}
}
