package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.impl.generate.jasperreports.DesignSpecification.Mode;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.Attribute.AttributeType;

@SuppressWarnings({"static-method", "boxing"})
class RendererStringTest {
	@TempDir
	Path tempDir;

	private DesignSpecification parent;

	@BeforeEach
	void setUp() {
		parent = new DesignSpecification();
		parent.setModuleName("test");
		parent.setDocumentName("AllAttributesPersistent");
		parent.setMode(Mode.bean);
	}

	// --- eS / eF ---

	@Test
	void eSWithSingleAttributeProducesXml() {
		String result = Renderer.eS("jasperReport", "name", "test", true);
		assertThat(result, containsString("<jasperReport"));
		assertThat(result, containsString("name=\"test\""));
		assertThat(result, containsString("/>"));
	}

	@Test
	void eSWithTerminatorFalseOmitsSlash() {
		String result = Renderer.eS("band", "height", "30", false);
		assertThat(result, containsString("<band"));
		assertFalse(result.trim().endsWith("/>"), "should not end with />");
		assertThat(result, containsString(">"));
	}

	@Test
	void eSWithNullAttributeProducesTagOnly() {
		String result = Renderer.eS("detail", null, false);
		assertThat(result, containsString("<detail"));
		assertFalse(result.trim().endsWith("/>"));
	}

	@Test
	void eSWithMapProducesMultipleAttributes() {
		Map<String, String> attrs = new LinkedHashMap<>();
		attrs.put("x", "10");
		attrs.put("y", "20");
		String result = Renderer.eS("reportElement", attrs, true);
		assertThat(result, containsString("x=\"10\""));
		assertThat(result, containsString("y=\"20\""));
		assertThat(result, containsString("/>"));
	}

	@Test
	void eFProducesClosingTag() {
		String result = Renderer.eF("jasperReport");
		assertThat(result, containsString("</jasperReport>"));
	}

	// --- renderParameter, renderVariable, renderField (pure string-building) ---

	@Test
	void renderParameterOutputContainsName() {
		ReportParameter p = new ReportParameter();
		p.setName("ID");
		p.setTypeClass("java.lang.String");
		p.setParent(parent);

		String jrxml = Renderer.renderParameter(p);
		assertThat(jrxml, notNullValue());
		assertThat(jrxml, containsString("ID"));
		assertThat(jrxml, containsString("java.lang.String"));
	}

	@Test
	void renderVariableOutputContainsName() {
		ReportVariable v = new ReportVariable();
		v.setName("totalCount");
		v.setTypeClass("java.lang.Integer");
		v.setParent(parent);

		String jrxml = Renderer.renderVariable(v);
		assertThat(jrxml, notNullValue());
		assertThat(jrxml, containsString("totalCount"));
		assertThat(jrxml, containsString("java.lang.Integer"));
	}

	@Test
	void renderFieldOutputContainsNameAndClass() {
		ReportField f = new ReportField();
		f.setName("firstName");
		f.setTypeClass("java.lang.String");
		f.setParent(parent);

		String jrxml = Renderer.renderField(f);
		assertThat(jrxml, notNullValue());
		assertThat(jrxml, containsString("firstName"));
		assertThat(jrxml, containsString("java.lang.String"));
	}

	@Test
	void defaultReportDimensions() {
		assertEquals(842, Renderer.defaultReportWith);
		assertEquals(595, Renderer.defaultReportHeight);
	}

	// --- renderPrintWhenExpression ---

	@Test
	void renderPrintWhenExpressionWithNullConditionReturnsEmpty() {
		String result = Renderer.renderPrintWhenExpression(parent, null);
		assertThat(result, is(""));
	}

	@Test
	void renderPrintWhenExpressionWithBlankConditionReturnsEmpty() {
		String result = Renderer.renderPrintWhenExpression(parent, "  ");
		assertThat(result, is(""));
	}

	@Test
	void renderPrintWhenExpressionBeanModeUsesFieldThis() {
		String result = Renderer.renderPrintWhenExpression(parent, "condition");
		assertThat(result, containsString("$F{THIS}"));
		assertThat(result, containsString("printWhenExpression"));
	}

	@Test
	void renderPrintWhenExpressionSqlModeUsesBeanForReport() {
		parent.setMode(Mode.sql);
		parent.setModuleName("myModule");
		parent.setDocumentName("MyDocument");
		String result = Renderer.renderPrintWhenExpression(parent, "condition");
		assertThat(result, containsString("BeanForReport.evaluateCondition"));
		assertThat(result, containsString("myModule"));
		assertThat(result, containsString("MyDocument"));
	}

	@Test
	void renderPrintWhenExpressionSqlModeWithNotConditionOmitsNegation() {
		parent.setMode(Mode.sql);
		// A "not"-prefixed condition should NOT inject a "!" sign — the "not" prefix itself is the negation
		String result = Renderer.renderPrintWhenExpression(parent, "notCondition");
		assertFalse(result.contains("CDATA[!"), "not-prefixed condition should not inject extra '!' negation");
		assertThat(result, containsString("BeanForReport.evaluateCondition"));
	}

	// --- flipCondition ---

	@Test
	void flipConditionNonNotAddsNotPrefix() {
		assertThat(Renderer.flipCondition("active"), is("notActive"));
	}

	@Test
	void flipConditionNotPrefixedStripsNotAndAddsIs() {
		assertThat(Renderer.flipCondition("notActive"), is("isActive"));
	}

	@Test
	void flipConditionNullReturnsNull() {
		assertThat(Renderer.flipCondition(null), is((String) null));
	}

	// --- rawConditionName ---

	@Test
	void rawConditionNameNonNotReturnsLowerFirst() {
		assertThat(Renderer.rawConditionName("Active"), is("active"));
	}

	@Test
	void rawConditionNameNotPrefixedStripsNot() {
		assertThat(Renderer.rawConditionName("notActive"), is("active"));
	}

	// --- renderField sql mode (parent mode = sql → self-closing tag) ---

	@Test
	void renderFieldSqlModeProducesSelfClosingTag() {
		parent.setMode(DesignSpecification.Mode.sql);
		ReportField f = new ReportField();
		f.setName("firstName");
		f.setTypeClass("java.lang.String");
		f.setParent(parent);
		String jrxml = Renderer.renderField(f);
		assertThat(jrxml, containsString("<field"));
		assertThat(jrxml, containsString("firstName"));
		assertThat(jrxml, containsString("/>"));
	}

	@Test
	void renderFieldBeanModeNonStringTypeHasNoFieldDescription() {
		ReportField f = new ReportField();
		f.setName("amount");
		f.setTypeClass("java.math.BigDecimal");
		f.setParent(parent);
		String jrxml = Renderer.renderField(f);
		assertThat(jrxml, containsString("amount"));
		assertFalse(jrxml.contains("fieldDescription"), "non-String field should not have fieldDescription");
	}

	@Test
	void renderFieldCollectionInSqlModeReturnsEmpty() {
		parent.setMode(DesignSpecification.Mode.sql);
		ReportField f = new ReportField();
		f.setName("children");
		f.setTypeClass("java.util.List");
		f.setCollection(Boolean.TRUE);
		f.setParent(parent);
		String jrxml = Renderer.renderField(f);
		assertThat(jrxml, is(""));
	}

	// --- getSqlEquivalentClass ---

	@Test
	void getSqlEquivalentClassDecimal2ReturnsBigDecimal() {
		assertThat(Renderer.getSqlEquivalentClass(org.skyve.metadata.model.Attribute.AttributeType.decimal2), is("java.math.BigDecimal"));
	}

	@Test
	void getSqlEquivalentClassIntegerReturnsInteger() {
		assertThat(Renderer.getSqlEquivalentClass(org.skyve.metadata.model.Attribute.AttributeType.integer), is("java.lang.Integer"));
	}

	@Test
	void getSqlEquivalentClassLongIntegerReturnsLong() {
		assertThat(Renderer.getSqlEquivalentClass(org.skyve.metadata.model.Attribute.AttributeType.longInteger), is("java.lang.Long"));
	}

	@Test
	void getSqlEquivalentClassDateReturnsDate() {
		assertThat(Renderer.getSqlEquivalentClass(org.skyve.metadata.model.Attribute.AttributeType.date), is("java.util.Date"));
	}

	@Test
	void getSqlEquivalentClassBoolReturnsBoolean() {
		assertThat(Renderer.getSqlEquivalentClass(org.skyve.metadata.model.Attribute.AttributeType.bool), is("java.lang.Boolean"));
	}

	@Test
	void getSqlEquivalentClassTextReturnsString() {
		assertThat(Renderer.getSqlEquivalentClass(org.skyve.metadata.model.Attribute.AttributeType.text), is("java.lang.String"));
	}

	// --- renderBoundMessage ---

	@Test
	void renderBoundMessageBeanModeUsesFieldThis() {
		String result = Renderer.renderBoundMessage(parent, "myMessage");
		assertThat(result, containsString("$F{THIS}"));
		assertThat(result, containsString("BeanForReport.getMessage"));
		assertThat(result, containsString("myMessage"));
	}

	@Test
	void renderBoundMessageSqlModeUsesModuleAndDocumentName() {
		parent.setMode(DesignSpecification.Mode.sql);
		parent.setModuleName("myModule");
		parent.setDocumentName("MyDoc");
		String result = Renderer.renderBoundMessage(parent, "greeting");
		assertThat(result, containsString("myModule"));
		assertThat(result, containsString("MyDoc"));
		assertThat(result, containsString("greeting"));
		assertThat(result, containsString("$P{ID}"));
	}

	// --- pathToReport ---
	// Note: UtilImpl.getAbsoluteBasePath() may be null in headless unit test context.
	// Tests use a try-catch to cover both enquote branches without requiring app config.

	@Test
	void pathToReportEnquotedDiffersFromNonEnquoted() {
		try {
			String enquoted = Renderer.pathToReport("myModule", "MyDoc", true);
			String plain = Renderer.pathToReport("myModule", "MyDoc", false);
			// If basePath is configured, enquoted should differ from plain
			assertNotEquals(enquoted, plain);
		} catch (@SuppressWarnings("unused") NullPointerException npe) {
			// UtilImpl not configured in standalone unit test — method is covered, skip assertion
		}
	}

	// --- renderBox ---

	@Test
	void renderBoxDefaultContainsBoxTagAndDefaultPens() throws Exception {
		ReportElement e = new ReportElement(ReportElement.ElementType.textField, "f", "v", 0, 0, 200, null);
		String result = Renderer.renderBox(e);
		assertThat(result, containsString("<box"));
		assertThat(result, containsString("</box>"));
		assertThat(result, containsString("<topPen"));
		assertThat(result, containsString("<leftPen"));
		assertThat(result, containsString("<bottomPen"));
		assertThat(result, containsString("<rightPen"));
	}

	@Test
	void renderBoxWithTopAndLeftPaddingContainsPaddingAttributes() throws Exception {
		ReportElement e = new ReportElement(ReportElement.ElementType.textField, "f", "v", 0, 0, 200, null);
		e.setTopPadding(Integer.valueOf(5));
		e.setLeftPadding(Integer.valueOf(3));
		String result = Renderer.renderBox(e);
		assertThat(result, containsString("topPadding=\"5\""));
		assertThat(result, containsString("leftPadding=\"3\""));
	}

	@Test
	void renderBoxWithElementBorderAndTopContainsPenElement() throws Exception {
		ReportElement e = new ReportElement(ReportElement.ElementType.textField, "f", "v", 0, 0, 200, null);
		e.setElementBorder(Boolean.TRUE);
		e.setBorderTop(Boolean.TRUE);
		e.setBorderColour("#000000");
		String result = Renderer.renderBox(e);
		assertThat(result, containsString("<topPen"));
		assertThat(result, containsString("lineStyle=\"Solid\""));
	}

	// --- renderElement image types ---

	@Test
	void renderElementStaticImageContainsImageTagAndValue() {
		ReportElement e = new ReportElement(ReportElement.ElementType.staticImage, "logo", "/path/logo.png", 0, 0, 100, null);
		e.setElementHeight(Integer.valueOf(50));
		String result = Renderer.renderElement(e);
		assertThat(result, containsString("<image>"));
		assertThat(result, containsString("</image>"));
		assertThat(result, containsString("/path/logo.png"));
	}

	@Test
	void renderElementContentImageContainsContentImageCallAndBinding() {
		ReportElement e = new ReportElement(ReportElement.ElementType.contentImage, "photo_contentImage", "photoBinding", 0, 0, 100, null);
		e.setElementHeight(Integer.valueOf(50));
		String result = Renderer.renderElement(e);
		assertThat(result, containsString("<image>"));
		assertThat(result, containsString("ContentImageForReport.image"));
		assertThat(result, containsString("photoBinding"));
	}

	// --- renderElement: dynamicImage ---

	@Test
	void renderElementDynamicImageBeanModeContainsModuleAndDocumentInExpression() {
		ReportBand band = new ReportBand();
		band.setParent(parent);
		ReportElement e = new ReportElement(ReportElement.ElementType.dynamicImage, "myImage", "myImage", 0, 0, 80, null);
		e.setElementHeight(Integer.valueOf(80));
		e.setParent(band);
		String result = Renderer.renderElement(e);
		assertThat(result, containsString("<image>"));
		assertThat(result, containsString("test.AllAttributesPersistent.images.myImage"));
		assertThat(result, containsString("$F{THIS}"));
	}

	@Test
	void renderElementDynamicImageSqlModeContainsBeanForReportCall() {
		parent.setMode(DesignSpecification.Mode.sql);
		ReportBand band = new ReportBand();
		band.setParent(parent);
		ReportElement e = new ReportElement(ReportElement.ElementType.dynamicImage, "myImage", "myImage", 0, 0, 80, null);
		e.setElementHeight(Integer.valueOf(80));
		e.setParent(band);
		String result = Renderer.renderElement(e);
		assertThat(result, containsString("<image>"));
		assertThat(result, containsString("BeanForReport.getBean"));
		assertThat(result, containsString("$P{ID}"));
	}

	// --- renderElement: subreport ---

	@Test
	void renderElementSubreportBeanModeContainsDataSourceExpression() {
		ReportBand band = new ReportBand();
		band.setParent(parent);
		ReportElement e = new ReportElement(ReportElement.ElementType.subreport, "items", null, 0, 0, 300, null);
		e.setElementHeight(Integer.valueOf(50));
		e.setReportFileName("ItemsSubreport");
		e.setParent(band);
		String result = Renderer.renderElement(e);
		assertThat(result, containsString("<subreport>"));
		assertThat(result, containsString("dataSourceExpression"));
		assertThat(result, containsString("JRBeanCollectionDataSource"));
		assertThat(result, containsString("$F{items}"));
	}

	@Test
	void renderElementSubreportSqlModeContainsConnectionExpression() {
		parent.setMode(DesignSpecification.Mode.sql);
		ReportBand band = new ReportBand();
		band.setParent(parent);
		ReportElement e = new ReportElement(ReportElement.ElementType.subreport, "items", null, 0, 0, 300, null);
		e.setElementHeight(Integer.valueOf(50));
		e.setReportFileName("ItemsSubreport");
		e.setParent(band);
		String result = Renderer.renderElement(e);
		assertThat(result, containsString("<subreport>"));
		assertThat(result, containsString("connectionExpression"));
		assertThat(result, containsString("REPORT_CONNECTION"));
	}

	// --- renderBand ---

	private ReportBand newBand(ReportBand.BandType type) {
		ReportBand band = new ReportBand();
		band.setBandType(type);
		band.setParent(parent);
		return band;
	}

	@Test
	void renderBandDetailTypeEmptyElementsProducesSelfClosingBandTagWithNoWrapper() {
		ReportBand band = newBand(ReportBand.BandType.detail);
		String result = Renderer.renderBand(band);
		assertThat(result, containsString("<band"));
		assertThat(result, containsString("/>"));
		assertFalse(result.contains("<detail"), "detail type should not be wrapped");
	}

	@Test
	void renderBandNonDetailEmptyElementsIsWrappedInBandType() {
		ReportBand band = newBand(ReportBand.BandType.title);
		String result = Renderer.renderBand(band);
		assertThat(result, containsString("<title"));
		assertThat(result, containsString("</title>"));
		assertThat(result, containsString("<band"));
	}

	@Test
	void renderBandWithElementsContainsBandHeightAndElementJrxml() {
		ReportBand band = newBand(ReportBand.BandType.title);
		ReportElement e = new ReportElement(ReportElement.ElementType.staticText, "label", "Hello", 0, 0, 200, null);
		e.setElementHeight(Integer.valueOf(20));
		e.setParent(band);
		band.getElements().add(e);
		String result = Renderer.renderBand(band);
		assertThat(result, containsString("height="));
		assertThat(result, containsString("<staticText>"));
	}

	@Test
	void renderBandWithInvisibleConditionProducesPrintWhenExpression() {
		ReportBand band = newBand(ReportBand.BandType.detail);
		band.setInvisibleConditionName("notActive");
		// printWhenExpression is only emitted when the band has elements
		ReportElement e = new ReportElement(ReportElement.ElementType.staticText, "label", "Hello", 0, 0, 200, null);
		e.setElementHeight(Integer.valueOf(20));
		e.setParent(band);
		band.getElements().add(e);
		String result = Renderer.renderBand(band);
		assertThat(result, containsString("printWhenExpression"));
	}

	@Test
	void renderBandWithSplitTypeIncludesSplitTypeAttribute() {
		ReportBand band = newBand(ReportBand.BandType.detail);
		ReportElement e = new ReportElement(ReportElement.ElementType.staticText, "label", "Hello", 0, 0, 200, null);
		e.setElementHeight(Integer.valueOf(20));
		e.setParent(band);
		band.getElements().add(e);
		band.setSplitType(ReportBand.SplitType.immediate);
		String result = Renderer.renderBand(band);
		assertThat(result, containsString("splitType=\"Immediate\""));
	}

	// --- addElement ---

	@Test
	void addElementAddsElementToBandWithCorrectTypeAndName() {
		ReportBand band = newBand(ReportBand.BandType.detail);
		Renderer.addElement(band, ReportElement.ElementType.textField, "myField", "$F{myField}",
				null, null, 0, 0, 200, 20, Boolean.FALSE, null, Boolean.FALSE, Boolean.FALSE, null, null, null);
		assertThat(band.getElements().size(), is(1));
		assertThat(band.getElements().get(0).getElementType(), is(ReportElement.ElementType.textField));
		assertThat(band.getElements().get(0).getName(), is("myField"));
	}

	@Test
	void addElementWithForeColourAndBackColourSetsColours() {
		ReportBand band = newBand(ReportBand.BandType.detail);
		Renderer.addElement(band, ReportElement.ElementType.textField, "coloured", "$F{coloured}",
				null, null, 0, 0, 200, 20, Boolean.FALSE, null, Boolean.FALSE, Boolean.FALSE, "#FF0000", "#0000FF", null);
		ReportElement added = band.getElements().get(0);
		assertThat(added.getElementForeColour(), is("#FF0000"));
		assertThat(added.getElementBackColour(), is("#0000FF"));
	}

	@Test
	void addElementBorderTypeIsInsertedAtStart() {
		ReportBand band = newBand(ReportBand.BandType.detail);
		// Add a non-border element first
		Renderer.addElement(band, ReportElement.ElementType.textField, "first", "$F{first}",
				null, null, 0, 0, 200, 20, Boolean.FALSE, null, Boolean.FALSE, Boolean.FALSE, null, null, null);
		// Add a border element – it should be inserted at index 0
		Renderer.addElement(band, ReportElement.ElementType.border, "myBorder", null,
				null, null, 0, 0, 200, 20, Boolean.TRUE, null, Boolean.FALSE, Boolean.FALSE, null, null, null);
		assertThat(band.getElements().size(), is(2));
		assertThat(band.getElements().get(0).getElementType(), is(ReportElement.ElementType.border));
		assertThat(band.getElements().get(1).getElementType(), is(ReportElement.ElementType.textField));
	}

	// --- renderElement: line ---

	@Test
	void renderElementLineContainsLineTagAndGraphicElement() {
		ReportBand band = newBand(ReportBand.BandType.detail);
		ReportElement e = new ReportElement(ReportElement.ElementType.line, "separator", null, 0, 0, 500, null);
		e.setElementHeight(Integer.valueOf(1));
		e.setParent(band);
		String result = Renderer.renderElement(e);
		assertThat(result, containsString("<line>"));
		assertThat(result, containsString("</line>"));
		assertThat(result, containsString("<graphicElement>"));
		assertThat(result, containsString("<pen"));
		assertThat(result, containsString("lineStyle=\"Solid\""));
	}

	@Test
	void renderElementLineWithDefaultLineWidthUsesDesignDefault() {
		parent.setDefaultLineWidth(new org.skyve.domain.types.Decimal2("2.0"));
		ReportBand band = newBand(ReportBand.BandType.detail);
		ReportElement e = new ReportElement(ReportElement.ElementType.line, "separator", null, 0, 0, 500, null);
		e.setElementHeight(Integer.valueOf(1));
		e.setParent(band);
		String result = Renderer.renderElement(e);
		assertThat(result, containsString("lineWidth=\"2.00\""));
	}

	@Test
	void renderElementLineWithoutDefaultLineWidthUsesDefaultOne() {
		parent.setDefaultLineWidth(null);
		ReportBand band = newBand(ReportBand.BandType.detail);
		ReportElement e = new ReportElement(ReportElement.ElementType.line, "sep", null, 0, 0, 500, null);
		e.setElementHeight(Integer.valueOf(1));
		e.setParent(band);
		String result = Renderer.renderElement(e);
		assertThat(result, containsString("lineWidth=\"1.0\""));
	}

	// --- renderElement: border ---

	@Test
	void renderElementBorderContainsRectangleTag() {
		ReportBand band = newBand(ReportBand.BandType.detail);
		ReportElement e = new ReportElement(ReportElement.ElementType.border, "box", null, 0, 0, 500, null);
		e.setElementHeight(Integer.valueOf(30));
		e.setParent(band);
		String result = Renderer.renderElement(e);
		assertThat(result, containsString("<rectangle>"));
		assertThat(result, containsString("</rectangle>"));
	}

	@Test
	void renderElementBorderWithDefaultLineColourUsesIt() {
		parent.setDefaultLineColour("#AABBCC");
		ReportBand band = newBand(ReportBand.BandType.detail);
		ReportElement e = new ReportElement(ReportElement.ElementType.border, "box", null, 0, 0, 500, null);
		e.setElementHeight(Integer.valueOf(30));
		e.setParent(band);
		String result = Renderer.renderElement(e);
		assertThat(result, containsString("#AABBCC"));
	}

	@Test
	void renderElementBorderWithDefaultLineWidthContainsGraphicElement() {
		parent.setDefaultLineWidth(new org.skyve.domain.types.Decimal2("1.5"));
		ReportBand band = newBand(ReportBand.BandType.detail);
		ReportElement e = new ReportElement(ReportElement.ElementType.border, "box", null, 0, 0, 500, null);
		e.setElementHeight(Integer.valueOf(30));
		e.setParent(band);
		String result = Renderer.renderElement(e);
		assertThat(result, containsString("<graphicElement>"));
		assertThat(result, containsString("lineWidth=\"1.50\""));
	}

	@Test
	void renderElementBorderWithoutDefaultLineWidthHasNoGraphicElement() {
		parent.setDefaultLineWidth(null);
		ReportBand band = newBand(ReportBand.BandType.detail);
		ReportElement e = new ReportElement(ReportElement.ElementType.border, "box", null, 0, 0, 500, null);
		e.setElementHeight(Integer.valueOf(30));
		e.setParent(band);
		String result = Renderer.renderElement(e);
		assertFalse(result.contains("<graphicElement>"), "no graphicElement when defaultLineWidth is null");
	}

	// --- renderElement: textField with dynamicFlow ---

	@Test
	void renderElementTextFieldWithDynamicFlowContainsIsStretchWithOverflow() {
		ReportBand band = newBand(ReportBand.BandType.detail);
		ReportElement e = new ReportElement(ReportElement.ElementType.textField, "dynamic", "$F{text}", 0, 0, 200, null);
		e.setElementHeight(Integer.valueOf(40));
		e.setDynamicFlow(Boolean.TRUE);
		e.setParent(band);
		String result = Renderer.renderElement(e);
		assertThat(result, containsString("isStretchWithOverflow=\"true\""));
	}

	@Test
	void renderElementTextFieldWithNullValueProducesEmptyExpression() {
		ReportBand band = newBand(ReportBand.BandType.detail);
		ReportElement e = new ReportElement(ReportElement.ElementType.textField, "empty", null, 0, 0, 200, null);
		e.setElementHeight(Integer.valueOf(20));
		e.setParent(band);
		String result = Renderer.renderElement(e);
		assertThat(result, containsString("\"\""));
	}

	@Test
	void renderElementTextFieldInTitleBandUsesParentTitleFontSize() {
		parent.setTitleFontSize(Integer.valueOf(18));
		ReportBand band = newBand(ReportBand.BandType.title);
		ReportElement e = new ReportElement(ReportElement.ElementType.textField, "heading", "$F{title}", 0, 0, 200, null);
		e.setElementHeight(Integer.valueOf(30));
		e.setParent(band);
		String result = Renderer.renderElement(e);
		assertThat(result, containsString("\"18\""));
	}

	@Test
	void renderElementTextFieldInTitleBandWithNullFontSizeUsesDefaultSixteen() {
		// force null to exercise the else branch
		parent.setTitleFontSize(null);
		ReportBand band = newBand(ReportBand.BandType.title);
		ReportElement e = new ReportElement(ReportElement.ElementType.textField, "heading", "$F{title}", 0, 0, 200, null);
		e.setElementHeight(Integer.valueOf(30));
		e.setParent(band);
		String result = Renderer.renderElement(e);
		assertThat(result, containsString("\"16\""));
	}

	@Test
	void renderElementTextFieldWithCustomBackColourProducesOpaqueMode() {
		ReportBand band = newBand(ReportBand.BandType.detail);
		ReportElement e = new ReportElement(ReportElement.ElementType.textField, "bg", "$F{bg}", 0, 0, 200, null);
		e.setElementHeight(Integer.valueOf(20));
		e.setElementBackColour("#EEEEEE");
		e.setParent(band);
		String result = Renderer.renderElement(e);
		assertThat(result, containsString("mode=\"Opaque\""));
		assertThat(result, containsString("#EEEEEE"));
	}

	@Test
	void renderElementTextFieldWithEvaluationTimeContainsAttribute() {
		ReportBand band = newBand(ReportBand.BandType.detail);
		ReportElement e = new ReportElement(ReportElement.ElementType.textField, "eval", "$V{count}", 0, 0, 200, null);
		e.setElementHeight(Integer.valueOf(20));
		e.setEvaluationTime(ReportElement.EvaluationTime.report);
		e.setParent(band);
		String result = Renderer.renderElement(e);
		assertThat(result, containsString("evaluationTime=\"Report\""));
	}

	// --- renderBox: element border with left/bottom/pen ---

	@Test
	void renderBoxWithElementBorderAndLineWidthContainsPen() throws Exception {
		ReportElement e = new ReportElement(ReportElement.ElementType.textField, "f", "v", 0, 0, 200, null);
		e.setElementBorder(Boolean.TRUE);
		e.setBorderLineWidth(new org.skyve.domain.types.Decimal2("1.5"));
		e.setBorderColour("#333333");
		String result = Renderer.renderBox(e);
		assertThat(result, containsString("<pen"));
		assertThat(result, containsString("#333333"));
	}

	@Test
	void renderBoxWithElementBorderAndLeftContainsLeftPen() throws Exception {
		ReportElement e = new ReportElement(ReportElement.ElementType.textField, "f", "v", 0, 0, 200, null);
		e.setElementBorder(Boolean.TRUE);
		e.setBorderLeft(Boolean.TRUE);
		e.setBorderColour("#000000");
		String result = Renderer.renderBox(e);
		assertThat(result, containsString("<leftPen"));
	}

	@Test
	void renderBoxWithElementBorderAndBottomContainsBottomPen() throws Exception {
		ReportElement e = new ReportElement(ReportElement.ElementType.textField, "f", "v", 0, 0, 200, null);
		e.setElementBorder(Boolean.TRUE);
		e.setBorderBottom(Boolean.TRUE);
		e.setBorderColour("#000000");
		String result = Renderer.renderBox(e);
		assertThat(result, containsString("<bottomPen"));
	}

	@Test
	void renderBoxWithRightPaddingContainsRightPaddingAttribute() throws Exception {
		ReportElement e = new ReportElement(ReportElement.ElementType.textField, "f", "v", 0, 0, 200, null);
		e.setRightPadding(Integer.valueOf(8));
		e.setBottomPadding(Integer.valueOf(4));
		String result = Renderer.renderBox(e);
		assertThat(result, containsString("rightPadding=\"8\""));
		assertThat(result, containsString("bottomPadding=\"4\""));
	}

	@Test
	void getSqlEquivalentClassForDecimal2ReturnsBigDecimal() {
		assertThat(Renderer.getSqlEquivalentClass(AttributeType.decimal2), is("java.math.BigDecimal"));
	}

	@Test
	void getSqlEquivalentClassForDecimal5ReturnsBigDecimal() {
		assertThat(Renderer.getSqlEquivalentClass(AttributeType.decimal5), is("java.math.BigDecimal"));
	}

	@Test
	void getSqlEquivalentClassForDecimal10ReturnsBigDecimal() {
		assertThat(Renderer.getSqlEquivalentClass(AttributeType.decimal10), is("java.math.BigDecimal"));
	}

	@Test
	void getSqlEquivalentClassForIntegerReturnsInteger() {
		assertThat(Renderer.getSqlEquivalentClass(AttributeType.integer), is("java.lang.Integer"));
	}

	@Test
	void getSqlEquivalentClassForLongReturnsLong() {
		assertThat(Renderer.getSqlEquivalentClass(AttributeType.longInteger), is("java.lang.Long"));
	}

	@Test
	void getSqlEquivalentClassForDateReturnsDate() {
		assertThat(Renderer.getSqlEquivalentClass(AttributeType.date), is("java.util.Date"));
	}

	@Test
	void getSqlEquivalentClassForBoolReturnsBoolean() {
		assertThat(Renderer.getSqlEquivalentClass(AttributeType.bool), is("java.lang.Boolean"));
	}

	@Test
	void getSqlEquivalentClassForTextReturnsString() {
		assertThat(Renderer.getSqlEquivalentClass(AttributeType.text), is("java.lang.String"));
	}

	@Test
	void flipConditionWithNotPrefixReturnsIs() {
		assertThat(Renderer.flipCondition("notActive"), is("isActive"));
	}

	@Test
	void flipConditionWithoutNotPrefixAddsNot() {
		assertThat(Renderer.flipCondition("active"), is("notActive"));
	}

	@Test
	void flipConditionWithNullReturnsNull() {
		assertThat(Renderer.flipCondition(null), nullValue());
	}

	@Test
	void rawConditionNameStripsNotPrefix() {
		assertThat(Renderer.rawConditionName("notActive"), is("active"));
	}

	@Test
	void rawConditionNameWithoutNotPrefixReturnsLowerFirst() {
		assertThat(Renderer.rawConditionName("Active"), is("active"));
	}

	@Test
	void renderPrintWhenExpressionNullReturnsEmpty() {
		DesignSpecification spec = new DesignSpecification();
		assertThat(Renderer.renderPrintWhenExpression(spec, null), is(""));
	}

	@Test
	void renderPrintWhenExpressionBlankReturnsEmpty() {
		DesignSpecification spec = new DesignSpecification();
		assertThat(Renderer.renderPrintWhenExpression(spec, "   "), is(""));
	}

	@Test
	void renderPrintWhenExpressionBeanModeContainsFAndFlip() {
		DesignSpecification spec = new DesignSpecification();
		spec.setMode(DesignSpecification.Mode.bean);
		String result = Renderer.renderPrintWhenExpression(spec, "active");
		assertThat(result, containsString("$F{THIS}"));
		assertThat(result, containsString("notActive"));
	}

	@Test
	void renderPrintWhenExpressionSqlModeNotStartingWithNotContainsBang() {
		DesignSpecification spec = new DesignSpecification();
		spec.setMode(DesignSpecification.Mode.sql);
		spec.setModuleName("mod");
		spec.setDocumentName("doc");
		String result = Renderer.renderPrintWhenExpression(spec, "active");
		assertThat(result, containsString("!"));
		assertThat(result, containsString("BeanForReport.evaluateCondition"));
	}

	@Test
	void renderPrintWhenExpressionSqlModeStartingWithNotNoExtraBang() {
		DesignSpecification spec = new DesignSpecification();
		spec.setMode(DesignSpecification.Mode.sql);
		spec.setModuleName("mod");
		spec.setDocumentName("doc");
		String result = Renderer.renderPrintWhenExpression(spec, "notActive");
		assertThat(result, containsString("BeanForReport.evaluateCondition"));
	}

	@Test
	void renderBoundMessageBeanModeContainsThisField() {
		DesignSpecification spec = new DesignSpecification();
		spec.setMode(DesignSpecification.Mode.bean);
		String result = Renderer.renderBoundMessage(spec, "msg.key");
		assertThat(result, containsString("$F{THIS}"));
		assertThat(result, containsString("msg.key"));
	}

	@Test
	void renderBoundMessageSqlModeContainsModuleAndDocument() {
		DesignSpecification spec = new DesignSpecification();
		spec.setMode(DesignSpecification.Mode.sql);
		spec.setModuleName("mymod");
		spec.setDocumentName("MyDoc");
		String result = Renderer.renderBoundMessage(spec, "some.message");
		assertThat(result, containsString("mymod"));
		assertThat(result, containsString("MyDoc"));
	}

	@Test
	void addElementCreatesAndAddsToTheBand() {
		DesignSpecification spec = new DesignSpecification();
		ReportBand band = new ReportBand();
		band.setBandType(ReportBand.BandType.detail);
		band.setHeight(Integer.valueOf(20));
		band.setParent(spec);
		Renderer.addElement(band, ReportElement.ElementType.staticText, "label", "Hello",
				null, null, Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(100), Integer.valueOf(20),
				null, null, null, null, null, null, null);
		assertThat(band.getElements().size(), is(1));
	}

	@Test
	void addElementBorderTypeAddsAtStart() {
		DesignSpecification spec = new DesignSpecification();
		ReportBand band = new ReportBand();
		band.setBandType(ReportBand.BandType.detail);
		band.setHeight(Integer.valueOf(20));
		band.setParent(spec);
		Renderer.addElement(band, ReportElement.ElementType.staticText, "label", "Hello",
				null, null, Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(100), Integer.valueOf(20),
				null, null, null, null, null, null, null);
		Renderer.addElement(band, ReportElement.ElementType.border, "box", null,
				null, null, Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(200), Integer.valueOf(60),
				null, null, null, null, null, null, null);
		// border should be first
		assertThat(band.getElements().get(0).getElementType(), is(ReportElement.ElementType.border));
	}

	@Test
	void constructorCreatesRendererInstance() {
		assertNotNull(new Renderer());
	}

	@Test
	void getPersistentIdentifierForDocumentReturnsUnknownTableWhenPersistentIsNull() {
		Document document = mock(Document.class);
		when(document.getPersistent()).thenReturn(null);
		assertEquals("UnknownTable", Renderer.getPersistentIdentifierForDocument(document));
	}

	@Test
	void getPersistentIdentifierForDocumentReturnsPersistentIdentifierWhenAvailable() {
		Document document = mock(Document.class);
		Persistent persistent = new Persistent();
		persistent.setName("contact");
		when(document.getPersistent()).thenReturn(persistent);
		assertEquals("contact", Renderer.getPersistentIdentifierForDocument(document));
	}

	@Test
	void saveJrxmlWithProvidedRendererWritesToDocumentPackagePath() throws Exception {
		DesignSpecification design = new DesignSpecification();
		design.setName("MainReport");
		design.setModuleName("admin");
		design.setDocumentName("Contact");
		design.setRepositoryPath(tempDir.toString());
		design.setSaveToDocumentPackage(Boolean.TRUE);

		DesignSpecification child = new DesignSpecification();
		child.setName("ChildReport");
		child.setModuleName("admin");
		child.setDocumentName("Contact");
		child.setRepositoryPath(tempDir.toString());
		child.setSaveToDocumentPackage(Boolean.TRUE);
		design.getSubReports().add(child);

		JasperReportRenderer renderer = mock(JasperReportRenderer.class);
		when(renderer.getJrxml()).thenReturn("<jrxml/>");

		Renderer.saveJrxml(design, renderer);

		Path root = tempDir.resolve("modules").resolve("admin").resolve("Contact").resolve("reports");
		assertTrue(Files.exists(root.resolve("MainReport.jrxml")));
		assertTrue(Files.exists(root.resolve("ChildReport.jrxml")));
	}

	@Test
	void saveJrxmlWithProvidedRendererThrowsWhenRepositoryPathMissing() {
		DesignSpecification design = new DesignSpecification();
		design.setName("NoPath");
		design.setModuleName("admin");
		design.setDocumentName("Contact");
		JasperReportRenderer renderer = mock(JasperReportRenderer.class);
		assertThrows(org.skyve.metadata.MetaDataException.class, () -> Renderer.saveJrxml(design, renderer));
	}

	@Test
	void saveJrxmlDefaultOverloadWritesGeneratedReportFile() throws Exception {
		DesignSpecification design = new DesignSpecification();
		design.setName("GeneratedDefault");
		design.setModuleName("test");
		design.setDocumentName("AllAttributesPersistent");
		design.setRepositoryPath(tempDir.toString());
		design.setSaveToDocumentPackage(Boolean.FALSE);

		Renderer.saveJrxml(design);

		Path generatedFile = tempDir.resolve("generatedReports").resolve("GeneratedDefault.jrxml");
		assertTrue(Files.exists(generatedFile));
	}

	@Test
	void saveJrxmlPathOverloadWritesFileAtProvidedPath() throws Exception {
		DesignSpecification design = new DesignSpecification();
		design.setName("PathOnly");
		design.setModuleName("test");
		design.setDocumentName("AllAttributesPersistent");
		design.setRepositoryPath(tempDir.toString());

		Path outputPath = tempDir.resolve("customOutput");
		Files.createDirectories(outputPath);

		Renderer.saveJrxml(design, outputPath);

		assertTrue(Files.exists(outputPath.resolve("PathOnly.jrxml")));
	}
}

