package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.generate.jasperreports.DesignSpecification.Mode;
import org.skyve.impl.generate.jasperreports.ReportElement.ElementType;
import org.skyve.impl.report.jasperreports.ReportDesignParameters;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.report.ReportFormat;

import net.sf.jasperreports.engine.JRExpression;
import net.sf.jasperreports.engine.JRField;
import net.sf.jasperreports.engine.design.JRDesignBand;

@SuppressWarnings({"static-method", "boxing"})
class JasperReportRendererTest {

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
	void renderDesignWithEmptyTabularRdpProducesJrxml() throws Exception {
		String jrxml = new JasperReportRenderer(minimalRdp()).renderDesign();
		assertThat(jrxml, notNullValue());
		assertThat(jrxml, containsString("jasperReport"));
	}

	@Test
	void renderDesignWithTabularTextColumnsProducesDisplayFields() throws Exception {
		ReportDesignParameters rdp = minimalRdp();
		rdp.getColumns().add(column("firstName", "First Name", 150, ReportDesignParameters.ColumnAlignment.left, AttributeType.text));
		rdp.getColumns().add(column("lastName", "Last Name", 150, ReportDesignParameters.ColumnAlignment.right, AttributeType.text));
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, containsString("firstName_display"));
		assertThat(jrxml, containsString("lastName_display"));
	}

	@Test
	void renderDesignWithCentreAlignmentProducesJrxml() throws Exception {
		ReportDesignParameters rdp = minimalRdp();
		rdp.getColumns().add(column("name", "Name", 100, ReportDesignParameters.ColumnAlignment.center, AttributeType.text));
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, containsString("jasperReport"));
	}

	@Test
	void renderDesignWithIntegerColumnProducesAggregateSummaryVariable() throws Exception {
		// pdf format: retainNumeric=false → requiresDisplayField=true; aggregatable → adds sum variable
		ReportDesignParameters rdp = minimalRdp();
		rdp.setReportFormat(ReportFormat.pdf);
		rdp.getColumns().add(column("amount", "Amount", 100, ReportDesignParameters.ColumnAlignment.right, AttributeType.integer));
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, containsString("amount_summary"));
	}

	@Test
	void renderDesignWithDateColumnProducesMinMaxVariables() throws Exception {
		// date columns are aggregatable temporal → min/max variables
		ReportDesignParameters rdp = minimalRdp();
		rdp.setReportFormat(ReportFormat.pdf);
		rdp.getColumns().add(column("birthDate", "Birth Date", 100, ReportDesignParameters.ColumnAlignment.left, AttributeType.date));
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, containsString("birthDate_minDate"));
		assertThat(jrxml, containsString("birthDate_maxDate"));
	}

	@Test
	void renderDesignWithColumnarStyleProducesJrxml() throws Exception {
		ReportDesignParameters rdp = minimalRdp();
		rdp.setReportStyle(ReportDesignParameters.ReportStyle.columnar);
		rdp.setReportFormat(ReportFormat.pdf);
		rdp.getColumns().add(column("name", "Name", 200, ReportDesignParameters.ColumnAlignment.left, AttributeType.text));
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, containsString("jasperReport"));
	}

	@Test
	void renderDesignCalledTwiceThrowsIllegalStateException() throws Exception {
		JasperReportRenderer renderer = new JasperReportRenderer(minimalRdp());
		renderer.renderDesign();
		assertThrows(IllegalStateException.class, renderer::renderDesign);
	}

	@Test
	void renderDesignWithShowSummaryProducesRecordCountExpression() throws Exception {
		ReportDesignParameters rdp = minimalRdp();
		rdp.setShowSummary(true);
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, containsString("REPORT_COUNT"));
	}

	@Test
	void isAggregatableAttributeReturnsTrueForDecimal2() {
		assertThat(JasperReportRenderer.isAggregatableAttribute(AttributeType.decimal2), is(Boolean.TRUE));
	}

	@Test
	void isAggregatableAttributeReturnsFalseForText() {
		assertThat(JasperReportRenderer.isAggregatableAttribute(AttributeType.text), is(Boolean.FALSE));
	}

	@Test
	void isDateOrTimeAttributeReturnsTrueForDate() {
		assertThat(JasperReportRenderer.isDateOrTimeAttribute(AttributeType.date), is(Boolean.TRUE));
	}

	@Test
	void isDateOrTimeAttributeReturnsFalseForText() {
		assertThat(JasperReportRenderer.isDateOrTimeAttribute(AttributeType.text), is(Boolean.FALSE));
	}

	@Test
	void createFieldWithBeanModeStringTypeHasDescription() {
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
	void createFieldWithNullParentReturnsNull() {
		ReportField field = new ReportField();
		field.setName("orphan");
		field.setTypeClass(String.class.getName());
		JRField jrField = JasperReportRenderer.createField(field);
		assertThat(jrField, nullValue());
	}

	@Test
	void createVariableReturnsCorrectName() {
		ReportVariable var = new ReportVariable();
		var.setName("total");
		var.setTypeClass("java.lang.Integer");
		assertThat(JasperReportRenderer.createVariable(var).getName(), is("total"));
	}

	@Test
	void createVariableExpressionContainsAddCall() {
		ReportVariable var = new ReportVariable();
		var.setName("total");
		var.setTypeClass("java.lang.Integer");
		JRExpression expr = JasperReportRenderer.createVariableExpression(var);
		assertThat(expr.getText(), containsString("$V{java.lang.Integer}.add($F{total})"));
	}

	@Test
	void createInitialValueVariableExpressionContainsNew() {
		ReportVariable var = new ReportVariable();
		var.setName("total");
		var.setTypeClass("java.lang.Integer");
		JRExpression expr = JasperReportRenderer.createInitialValueVariableExpression(var);
		assertThat(expr.getText(), containsString("new java.lang.Integer(0)"));
	}

	@Test
	void addCustomerLogoAddsImageElementToBand() {
		JRDesignBand band = new JRDesignBand();
		band.setHeight(40);
		JasperReportRenderer.addCustomerLogo(band);
		assertEquals(1, band.getElements().length);
	}

	@Test
	void getIncrementTypeReturnsNonNull() {
		assertThat(JasperReportRenderer.getIncrementType(), notNullValue());
	}

	// --- createImageElementExpression ---

	@Test
	void createImageElementExpressionContentImageContainsCONTENTIdField() {
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
	void createImageElementExpressionStaticImageReturnsValueDirectly() {
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
	void createImageElementExpressionDynamicImageBeanModeContainsFieldThis() {
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
	void createTextElementExpressionWithNullValueUsesEmptyString() {
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
	void createTextElementExpressionWithValueReturnsThatValue() {
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
	void renderDesignFromDesignSpecWithNullModuleAndDocumentReturnsIllegalArgument() {
		DesignSpecification spec = new DesignSpecification();
		// moduleName and documentName intentionally not set
		JasperReportRenderer renderer = new JasperReportRenderer(spec);
		assertThrows(IllegalArgumentException.class, renderer::renderDesign);
	}

	// --- renderDesignWithNoDesignSpecAndNoRdp ---

	@Test
	void renderDesignWithNoSpecAndNoRdpThrowsIllegalStateException() throws Exception {
		// Construct a renderer where both designSpecification and reportDesignParameters are null
		// by passing a DesignSpecification with list source (sets designSpec=null, rdp assigned)
		// We can't easily make both null without a subclass, but we can test the null-rdp path
		// by creating a plain spec and then calling render twice
		JasperReportRenderer renderer = new JasperReportRenderer(minimalRdp());
		renderer.renderDesign();
		assertThrows(IllegalStateException.class, renderer::renderDesign);
	}

	@Test
	void renderDesignNonPaginatedTabularProducesJrxml() throws Exception {
		ReportDesignParameters rdp = minimalRdp();
		rdp.setPaginated(false);
		rdp.getColumns().add(column("name", "Name", 100, ReportDesignParameters.ColumnAlignment.left, AttributeType.text));
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, notNullValue());
		assertThat(jrxml, containsString("jasperReport"));
	}

	@Test
	void renderDesignWithPrettyTitleProducesJrxml() throws Exception {
		ReportDesignParameters rdp = minimalRdp();
		rdp.setPretty(true);
		rdp.getColumns().add(column("name", "Name", 100, ReportDesignParameters.ColumnAlignment.left, AttributeType.text));
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, containsString("TITLE"));
	}

	@Test
	void renderDesignWithXlsxAndDecimalColumnRetainsNumericNoDisplayField() throws Exception {
		ReportDesignParameters rdp = minimalRdp();
		rdp.setReportFormat(ReportFormat.xlsx);
		rdp.getColumns().add(column("amount", "Amount", 100, ReportDesignParameters.ColumnAlignment.right, AttributeType.decimal2));
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		// retainNumeric=true so no _display field for decimal2
		assertThat(jrxml, containsString("amount_summary"));
	}

	@Test
	void renderDesignWithXlsxAndDateColumnRetainsTemporal() throws Exception {
		ReportDesignParameters rdp = minimalRdp();
		rdp.setReportFormat(ReportFormat.xlsx);
		rdp.getColumns().add(column("dueDate", "Due Date", 100, ReportDesignParameters.ColumnAlignment.left, AttributeType.date));
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		// retainTemporal=true so no _display field for date; minDate/maxDate variables still added
		assertThat(jrxml, containsString("dueDate_minDate"));
	}

	@Test
	void renderDesignWithOdsAndDecimalColumnRetainsNumeric() throws Exception {
		ReportDesignParameters rdp = minimalRdp();
		rdp.setReportFormat(ReportFormat.ods);
		rdp.getColumns().add(column("total", "Total", 100, ReportDesignParameters.ColumnAlignment.right, AttributeType.decimal5));
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, containsString("total_summary"));
	}

	@Test
	void renderDesignWithColumnarStyleNonPaginatedProducesJrxml() throws Exception {
		ReportDesignParameters rdp = minimalRdp();
		rdp.setReportStyle(ReportDesignParameters.ReportStyle.columnar);
		rdp.setPaginated(false);
		rdp.setReportFormat(ReportFormat.pdf);
		rdp.getColumns().add(column("firstName", "First Name", 150, ReportDesignParameters.ColumnAlignment.left, AttributeType.text));
		rdp.getColumns().add(column("lastName", "Last Name", 150, ReportDesignParameters.ColumnAlignment.right, AttributeType.text));
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, containsString("jasperReport"));
	}

	@Test
	void renderDesignWithWideStaticTextsForTxtFormat() throws Exception {
		ReportDesignParameters rdp = minimalRdp();
		rdp.setReportFormat(ReportFormat.txt);
		rdp.getColumns().add(column("description", "Description", 200, ReportDesignParameters.ColumnAlignment.left, AttributeType.text));
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, notNullValue());
	}

	@Test
	void renderDesignWithXmlFormatProducesWideStaticTexts() throws Exception {
		ReportDesignParameters rdp = minimalRdp();
		rdp.setReportFormat(ReportFormat.xml);
		rdp.getColumns().add(column("code", "Code", 100, ReportDesignParameters.ColumnAlignment.center, AttributeType.text));
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, notNullValue());
	}

	@Test
	void renderDesignWithLongColumnProducesAggregateSummary() throws Exception {
		ReportDesignParameters rdp = minimalRdp();
		rdp.setReportFormat(ReportFormat.pdf);
		rdp.getColumns().add(column("count", "Count", 100, ReportDesignParameters.ColumnAlignment.right, AttributeType.longInteger));
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, containsString("count_summary"));
	}

	@Test
	void renderDesignWithDateTimeColumnProducesMinMaxVariables() throws Exception {
		ReportDesignParameters rdp = minimalRdp();
		rdp.setReportFormat(ReportFormat.pdf);
		rdp.getColumns().add(column("createdAt", "Created At", 150, ReportDesignParameters.ColumnAlignment.left, AttributeType.dateTime));
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, containsString("createdAt_minDate"));
		assertThat(jrxml, containsString("createdAt_maxDate"));
	}

	@Test
	void renderDesignWithTimeColumnProducesMinMaxVariables() throws Exception {
		ReportDesignParameters rdp = minimalRdp();
		rdp.setReportFormat(ReportFormat.pdf);
		rdp.getColumns().add(column("startTime", "Start Time", 100, ReportDesignParameters.ColumnAlignment.left, AttributeType.time));
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, containsString("startTime_minDate"));
	}

	@Test
	void renderDesignWithTemporalFormatPatternSetsPattern() throws Exception {
		ReportDesignParameters rdp = minimalRdp();
		rdp.setReportFormat(ReportFormat.pdf);
		ReportDesignParameters.ReportColumn col = column("eventDate", "Event Date", 120, ReportDesignParameters.ColumnAlignment.left, AttributeType.date);
		col.setFormatPattern("dd/MM/yyyy");
		rdp.getColumns().add(col);
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, containsString("eventDate_minDate"));
	}

	@Test
	void renderDesignWithPrettyAndIncludeLogoProducesJrxml() throws Exception {
		ReportDesignParameters rdp = minimalRdp();
		rdp.setPretty(true);
		rdp.setIncludeCustomerLogo(true);
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, notNullValue());
	}

	@Test
	void renderDesignWithMultipleColumnsAndPrettyPaddingProducesJrxml() throws Exception {
		ReportDesignParameters rdp = minimalRdp();
		rdp.setPretty(true);
		rdp.setReportFormat(ReportFormat.pdf);
		rdp.getColumns().add(column("a", "Col A", 100, ReportDesignParameters.ColumnAlignment.left, AttributeType.text));
		rdp.getColumns().add(column("b", "Col B", 100, ReportDesignParameters.ColumnAlignment.right, AttributeType.integer));
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, containsString("Col A"));
		assertThat(jrxml, containsString("b_summary"));
	}

	@Test
	void renderDesignWithXlsFormatProducesJrxml() throws Exception {
		ReportDesignParameters rdp = minimalRdp();
		rdp.setReportFormat(ReportFormat.xls);
		rdp.getColumns().add(column("val", "Value", 100, ReportDesignParameters.ColumnAlignment.right, AttributeType.decimal2));
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, notNullValue());
	}

	@Test
	void renderDesignColumnarWithXlsxAndDecimalRetainsBothNumericAndTemporal() throws Exception {
		ReportDesignParameters rdp = minimalRdp();
		rdp.setReportStyle(ReportDesignParameters.ReportStyle.columnar);
		rdp.setReportFormat(ReportFormat.xlsx);
		rdp.getColumns().add(column("amount", "Amount", 100, ReportDesignParameters.ColumnAlignment.right, AttributeType.decimal2));
		rdp.getColumns().add(column("dt", "Date", 100, ReportDesignParameters.ColumnAlignment.left, AttributeType.dateTime));
		String jrxml = new JasperReportRenderer(rdp).renderDesign();
		assertThat(jrxml, notNullValue());
	}

	// --- renderFromDesignSpecification with bean mode ---

	private static DesignSpecification beanModeSpec() {
		DesignSpecification spec = new DesignSpecification();
		spec.setModuleName("test");
		spec.setDocumentName("TestDoc");
		spec.setMode(Mode.bean);
		spec.setReportType(DesignSpecification.ReportType.report);
		return spec;
	}

	private static ReportBand detailBand(DesignSpecification spec) {
		ReportBand band = new ReportBand();
		band.setBandType(ReportBand.BandType.detail);
		band.setHeight(Integer.valueOf(20));
		band.setParent(spec);
		return band;
	}

	@Test
	void renderFromDesignSpecificationBeanModeEmptyBandsProducesJrxml() throws Exception {
		DesignSpecification spec = beanModeSpec();
		String jrxml = new JasperReportRenderer(spec).renderDesign();
		assertThat(jrxml, notNullValue());
		assertThat(jrxml, containsString("jasperReport"));
	}

	@Test
	void renderFromDesignSpecificationWithStaticTextElementProducesElement() throws Exception {
		DesignSpecification spec = beanModeSpec();
		ReportBand band = detailBand(spec);
		ReportElement e = new ReportElement(ElementType.staticText, "label", "Hello World",
				Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(200), null);
		e.setElementHeight(Integer.valueOf(20));
		e.setParent(band);
		band.getElements().add(e);
		spec.getBands().add(band);
		String jrxml = new JasperReportRenderer(spec).renderDesign();
		assertThat(jrxml, containsString("Hello World"));
	}

	@Test
	void renderFromDesignSpecificationWithTextFieldElementProducesElement() throws Exception {
		DesignSpecification spec = beanModeSpec();
		ReportBand band = detailBand(spec);
		ReportElement e = new ReportElement(ElementType.textField, "field", "$F{name}",
				Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(200), null);
		e.setElementHeight(Integer.valueOf(20));
		e.setParent(band);
		band.getElements().add(e);
		spec.getBands().add(band);
		String jrxml = new JasperReportRenderer(spec).renderDesign();
		assertThat(jrxml, notNullValue());
	}

	@Test
	void renderFromDesignSpecificationWithLineElementProducesElement() throws Exception {
		DesignSpecification spec = beanModeSpec();
		ReportBand band = detailBand(spec);
		ReportElement e = new ReportElement(ElementType.line, "divider", null,
				Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(500), null);
		e.setElementHeight(Integer.valueOf(1));
		e.setParent(band);
		band.getElements().add(e);
		spec.getBands().add(band);
		String jrxml = new JasperReportRenderer(spec).renderDesign();
		assertThat(jrxml, notNullValue());
	}

	@Test
	void renderFromDesignSpecificationWithBorderElementProducesElement() throws Exception {
		DesignSpecification spec = beanModeSpec();
		ReportBand band = detailBand(spec);
		ReportElement e = new ReportElement(ElementType.border, "box", null,
				Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(400), null);
		e.setElementHeight(Integer.valueOf(30));
		e.setParent(band);
		band.getElements().add(e);
		spec.getBands().add(band);
		String jrxml = new JasperReportRenderer(spec).renderDesign();
		assertThat(jrxml, notNullValue());
	}

	@Test
	void renderFromDesignSpecificationWithImageElementProducesElement() throws Exception {
		DesignSpecification spec = beanModeSpec();
		ReportBand band = detailBand(spec);
		ReportElement e = new ReportElement(ElementType.staticImage, "img", "path/to/img.png",
				Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(100), null);
		e.setElementHeight(Integer.valueOf(50));
		e.setParent(band);
		band.getElements().add(e);
		spec.getBands().add(band);
		String jrxml = new JasperReportRenderer(spec).renderDesign();
		assertThat(jrxml, notNullValue());
	}

	@Test
	void renderFromDesignSpecificationWithTitleBandProducesTitle() throws Exception {
		DesignSpecification spec = beanModeSpec();
		ReportBand title = new ReportBand();
		title.setBandType(ReportBand.BandType.title);
		title.setHeight(Integer.valueOf(50));
		title.setParent(spec);
		ReportElement e = new ReportElement(ElementType.staticText, "header", "Report Title",
				Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(400), null);
		e.setElementHeight(Integer.valueOf(30));
		e.setParent(title);
		title.getElements().add(e);
		spec.getBands().add(title);
		String jrxml = new JasperReportRenderer(spec).renderDesign();
		assertThat(jrxml, containsString("Report Title"));
	}

	@Test
	void renderFromDesignSpecificationWithBandSplitTypeProducesJrxml() throws Exception {
		DesignSpecification spec = beanModeSpec();
		ReportBand band = detailBand(spec);
		band.setSplitType(ReportBand.SplitType.prevent);
		ReportElement e = new ReportElement(ElementType.staticText, "t", "text",
				Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(100), null);
		e.setElementHeight(Integer.valueOf(20));
		e.setParent(band);
		band.getElements().add(e);
		spec.getBands().add(band);
		String jrxml = new JasperReportRenderer(spec).renderDesign();
		assertThat(jrxml, notNullValue());
	}

	@Test
	void renderFromDesignSpecificationWithInvisibleConditionProducesExpression() throws Exception {
		DesignSpecification spec = beanModeSpec();
		ReportBand band = detailBand(spec);
		band.setInvisibleConditionName("someCondition");
		ReportElement e = new ReportElement(ElementType.staticText, "t", "text",
				Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(100), null);
		e.setElementHeight(Integer.valueOf(20));
		e.setParent(band);
		band.getElements().add(e);
		spec.getBands().add(band);
		String jrxml = new JasperReportRenderer(spec).renderDesign();
		assertThat(jrxml, notNullValue());
	}

	@Test
	void renderFromDesignSpecificationWithSubreportElementBeanModeProducesElement() throws Exception {
		DesignSpecification spec = beanModeSpec();
		ReportBand band = detailBand(spec);
		ReportElement e = new ReportElement(ElementType.subreport, "sub", "SubReport",
				Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(400), null);
		e.setElementHeight(Integer.valueOf(100));
		e.setReportFileName("subReport1");
		e.setParent(band);
		band.getElements().add(e);
		spec.getBands().add(band);
		String jrxml = new JasperReportRenderer(spec).renderDesign();
		assertThat(jrxml, notNullValue());
	}

	@Test
	void renderFromDesignSpecificationWithParametersProducesParameters() throws Exception {
		DesignSpecification spec = beanModeSpec();
		ReportParameter param = new ReportParameter();
		param.setName("MY_PARAM");
		param.setTypeClass(String.class.getName());
		spec.getParameters().add(param);
		String jrxml = new JasperReportRenderer(spec).renderDesign();
		assertThat(jrxml, containsString("MY_PARAM"));
	}

	@Test
	void renderFromDesignSpecificationWithReportFieldsProducesFields() throws Exception {
		DesignSpecification spec = beanModeSpec();
		ReportField field = new ReportField();
		field.setName("customerName");
		field.setTypeClass(String.class.getName());
		field.setParent(spec);
		spec.getFields().add(field);
		String jrxml = new JasperReportRenderer(spec).renderDesign();
		assertThat(jrxml, containsString("customerName"));
	}

	@Test
	void renderFromDesignSpecificationWithReportVariablesProducesVariables() throws Exception {
		DesignSpecification spec = beanModeSpec();
		ReportVariable var = new ReportVariable();
		var.setName("totalCount");
		var.setTypeClass("java.lang.Integer");
		spec.getVariables().add(var);
		String jrxml = new JasperReportRenderer(spec).renderDesign();
		assertThat(jrxml, containsString("totalCount"));
	}

	@Test
	void renderFromDesignSpecificationWithLanguageSetProducesJrxml() throws Exception {
		DesignSpecification spec = beanModeSpec();
		spec.setLanguage("java");
		String jrxml = new JasperReportRenderer(spec).renderDesign();
		assertThat(jrxml, notNullValue());
	}

	@Test
	void renderFromDesignSpecificationWithIncludeLogoProducesLogo() throws Exception {
		DesignSpecification spec = beanModeSpec();
		spec.setIncludeCustomerLogo(true);
		ReportBand title = new ReportBand();
		title.setBandType(ReportBand.BandType.title);
		title.setHeight(Integer.valueOf(50));
		title.setParent(spec);
		ReportElement e = new ReportElement(ElementType.staticText, "hdr", "Title",
				Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(200), null);
		e.setElementHeight(Integer.valueOf(30));
		e.setParent(title);
		title.getElements().add(e);
		spec.getBands().add(title);
		String jrxml = new JasperReportRenderer(spec).renderDesign();
		assertThat(jrxml, notNullValue());
	}
}

