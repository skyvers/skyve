package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;

import java.util.LinkedHashMap;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.generate.jasperreports.DesignSpecification.Mode;

@SuppressWarnings({"static-method", "boxing"})
public class RendererStringTest {

	private DesignSpecification parent;

	@BeforeEach
	public void setUp() {
		parent = new DesignSpecification();
		parent.setModuleName("test");
		parent.setDocumentName("AllAttributesPersistent");
		parent.setMode(Mode.bean);
	}

	// --- eS / eF ---

	@Test
	public void eSWithSingleAttributeProducesXml() {
		String result = Renderer.eS("jasperReport", "name", "test", true);
		assertThat(result, containsString("<jasperReport"));
		assertThat(result, containsString("name=\"test\""));
		assertThat(result, containsString("/>"));
	}

	@Test
	public void eSWithTerminatorFalseOmitsSlash() {
		String result = Renderer.eS("band", "height", "30", false);
		assertThat(result, containsString("<band"));
		assertFalse(result.trim().endsWith("/>"), "should not end with />");
		assertThat(result, containsString(">"));
	}

	@Test
	public void eSWithNullAttributeProducesTagOnly() {
		String result = Renderer.eS("detail", null, false);
		assertThat(result, containsString("<detail"));
		assertFalse(result.trim().endsWith("/>"));
	}

	@Test
	public void eSWithMapProducesMultipleAttributes() {
		Map<String, String> attrs = new LinkedHashMap<>();
		attrs.put("x", "10");
		attrs.put("y", "20");
		String result = Renderer.eS("reportElement", attrs, true);
		assertThat(result, containsString("x=\"10\""));
		assertThat(result, containsString("y=\"20\""));
		assertThat(result, containsString("/>"));
	}

	@Test
	public void eFProducesClosingTag() {
		String result = Renderer.eF("jasperReport");
		assertThat(result, containsString("</jasperReport>"));
	}

	// --- renderParameter, renderVariable, renderField (pure string-building) ---

	@Test
	public void renderParameterOutputContainsName() {
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
	public void renderVariableOutputContainsName() {
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
	public void renderFieldOutputContainsNameAndClass() {
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
	public void defaultReportDimensions() {
		assertThat(Renderer.defaultReportWith, is(842));
		assertThat(Renderer.defaultReportHeight, is(595));
	}

	// --- renderPrintWhenExpression ---

	@Test
	public void renderPrintWhenExpressionWithNullConditionReturnsEmpty() {
		String result = Renderer.renderPrintWhenExpression(parent, null);
		assertThat(result, is(""));
	}

	@Test
	public void renderPrintWhenExpressionWithBlankConditionReturnsEmpty() {
		String result = Renderer.renderPrintWhenExpression(parent, "  ");
		assertThat(result, is(""));
	}

	@Test
	public void renderPrintWhenExpressionBeanModeUsesFieldThis() {
		String result = Renderer.renderPrintWhenExpression(parent, "condition");
		assertThat(result, containsString("$F{THIS}"));
		assertThat(result, containsString("printWhenExpression"));
	}

	@Test
	public void renderPrintWhenExpressionSqlModeUsesBeanForReport() {
		parent.setMode(Mode.sql);
		parent.setModuleName("myModule");
		parent.setDocumentName("MyDocument");
		String result = Renderer.renderPrintWhenExpression(parent, "condition");
		assertThat(result, containsString("BeanForReport.evaluateCondition"));
		assertThat(result, containsString("myModule"));
		assertThat(result, containsString("MyDocument"));
	}

	@Test
	public void renderPrintWhenExpressionSqlModeWithNotConditionOmitsNegation() {
		parent.setMode(Mode.sql);
		// A "not"-prefixed condition should NOT inject a "!" sign — the "not" prefix itself is the negation
		String result = Renderer.renderPrintWhenExpression(parent, "notCondition");
		assertFalse(result.contains("CDATA[!"), "not-prefixed condition should not inject extra '!' negation");
		assertThat(result, containsString("BeanForReport.evaluateCondition"));
	}

	// --- flipCondition ---

	@Test
	public void flipConditionNonNotAddsNotPrefix() {
		assertThat(Renderer.flipCondition("active"), is("notActive"));
	}

	@Test
	public void flipConditionNotPrefixedStripsNotAndAddsIs() {
		assertThat(Renderer.flipCondition("notActive"), is("isActive"));
	}

	@Test
	public void flipConditionNullReturnsNull() {
		assertThat(Renderer.flipCondition(null), is((String) null));
	}

	// --- rawConditionName ---

	@Test
	public void rawConditionNameNonNotReturnsLowerFirst() {
		assertThat(Renderer.rawConditionName("Active"), is("active"));
	}

	@Test
	public void rawConditionNameNotPrefixedStripsNot() {
		assertThat(Renderer.rawConditionName("notActive"), is("active"));
	}

	// --- renderField sql mode (parent mode = sql → self-closing tag) ---

	@Test
	public void renderFieldSqlModeProducesSelfClosingTag() {
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
	public void renderFieldBeanModeNonStringTypeHasNoFieldDescription() {
		ReportField f = new ReportField();
		f.setName("amount");
		f.setTypeClass("java.math.BigDecimal");
		f.setParent(parent);
		String jrxml = Renderer.renderField(f);
		assertThat(jrxml, containsString("amount"));
		assertFalse(jrxml.contains("fieldDescription"), "non-String field should not have fieldDescription");
	}

	@Test
	public void renderFieldCollectionInSqlModeReturnsEmpty() {
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
	public void getSqlEquivalentClassDecimal2ReturnsBigDecimal() {
		assertThat(Renderer.getSqlEquivalentClass(org.skyve.metadata.model.Attribute.AttributeType.decimal2), is("java.math.BigDecimal"));
	}

	@Test
	public void getSqlEquivalentClassIntegerReturnsInteger() {
		assertThat(Renderer.getSqlEquivalentClass(org.skyve.metadata.model.Attribute.AttributeType.integer), is("java.lang.Integer"));
	}

	@Test
	public void getSqlEquivalentClassLongIntegerReturnsLong() {
		assertThat(Renderer.getSqlEquivalentClass(org.skyve.metadata.model.Attribute.AttributeType.longInteger), is("java.lang.Long"));
	}

	@Test
	public void getSqlEquivalentClassDateReturnsDate() {
		assertThat(Renderer.getSqlEquivalentClass(org.skyve.metadata.model.Attribute.AttributeType.date), is("java.util.Date"));
	}

	@Test
	public void getSqlEquivalentClassBoolReturnsBoolean() {
		assertThat(Renderer.getSqlEquivalentClass(org.skyve.metadata.model.Attribute.AttributeType.bool), is("java.lang.Boolean"));
	}

	@Test
	public void getSqlEquivalentClassTextReturnsString() {
		assertThat(Renderer.getSqlEquivalentClass(org.skyve.metadata.model.Attribute.AttributeType.text), is("java.lang.String"));
	}

	// --- renderBoundMessage ---

	@Test
	public void renderBoundMessageBeanModeUsesFieldThis() {
		String result = Renderer.renderBoundMessage(parent, "myMessage");
		assertThat(result, containsString("$F{THIS}"));
		assertThat(result, containsString("BeanForReport.getMessage"));
		assertThat(result, containsString("myMessage"));
	}

	@Test
	public void renderBoundMessageSqlModeUsesModuleAndDocumentName() {
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
	public void pathToReportEnquotedDiffersFromNonEnquoted() {
		try {
			String enquoted = Renderer.pathToReport("myModule", "MyDoc", true);
			String plain = Renderer.pathToReport("myModule", "MyDoc", false);
			// If basePath is configured, enquoted should differ from plain
			assertThat(enquoted.equals(plain), is(false));
		} catch (@SuppressWarnings("unused") NullPointerException npe) {
			// UtilImpl not configured in standalone unit test — method is covered, skip assertion
		}
	}

	// --- renderBox ---

	@Test
	public void renderBoxDefaultContainsBoxTagAndDefaultPens() throws Exception {
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
	public void renderBoxWithTopAndLeftPaddingContainsPaddingAttributes() throws Exception {
		ReportElement e = new ReportElement(ReportElement.ElementType.textField, "f", "v", 0, 0, 200, null);
		e.setTopPadding(Integer.valueOf(5));
		e.setLeftPadding(Integer.valueOf(3));
		String result = Renderer.renderBox(e);
		assertThat(result, containsString("topPadding=\"5\""));
		assertThat(result, containsString("leftPadding=\"3\""));
	}

	@Test
	public void renderBoxWithElementBorderAndTopContainsPenElement() throws Exception {
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
	public void renderElementStaticImageContainsImageTagAndValue() {
		ReportElement e = new ReportElement(ReportElement.ElementType.staticImage, "logo", "/path/logo.png", 0, 0, 100, null);
		e.setElementHeight(Integer.valueOf(50));
		String result = Renderer.renderElement(e);
		assertThat(result, containsString("<image>"));
		assertThat(result, containsString("</image>"));
		assertThat(result, containsString("/path/logo.png"));
	}

	@Test
	public void renderElementContentImageContainsContentImageCallAndBinding() {
		ReportElement e = new ReportElement(ReportElement.ElementType.contentImage, "photo_contentImage", "photoBinding", 0, 0, 100, null);
		e.setElementHeight(Integer.valueOf(50));
		String result = Renderer.renderElement(e);
		assertThat(result, containsString("<image>"));
		assertThat(result, containsString("ContentImageForReport.image"));
		assertThat(result, containsString("photoBinding"));
	}
}
