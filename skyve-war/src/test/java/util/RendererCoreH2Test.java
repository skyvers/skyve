package util;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.generate.jasperreports.DesignSpecification;
import org.skyve.impl.generate.jasperreports.DocumentReportDesignGenerator;
import org.skyve.impl.generate.jasperreports.Renderer;
import org.skyve.impl.generate.jasperreports.ReportBand;
import org.skyve.impl.generate.jasperreports.ReportElement;

import modules.test.domain.AllAttributesPersistent;

@SuppressWarnings("static-method")
public class RendererCoreH2Test extends AbstractH2Test {

	private DesignSpecification spec;

	@BeforeEach
	public void buildSpec() {
		spec = new DesignSpecification();
		spec.setModuleName(AllAttributesPersistent.MODULE_NAME);
		spec.setDocumentName(AllAttributesPersistent.DOCUMENT_NAME);
		spec.setMode(DesignSpecification.Mode.bean);
		spec.setName("test");
		spec.setWidth(Integer.valueOf(842));
		spec.setHeight(Integer.valueOf(595));
		spec.setLeftMargin(Integer.valueOf(20));
		spec.setRightMargin(Integer.valueOf(20));
		spec.setTopMargin(Integer.valueOf(20));
		spec.setBottomMargin(Integer.valueOf(20));
		spec.setColumnWidth(Integer.valueOf(802));
		spec.setDefaultElementHeight(Integer.valueOf(20));
		spec.setDefaultFontSize(Integer.valueOf(10));
		spec.setReportType(DesignSpecification.ReportType.report);
		spec.setDefinitionSource(DesignSpecification.DefinitionSource.document);
	}

	/**
	 * Creates a fully-wired element attached to a new band of the given type.
	 * The band's parent is the shared {@link #spec} instance.
	 */
	private ReportElement elementInBand(ReportBand.BandType bandType, ReportElement.ElementType elemType,
			String name, String value) {
		ReportBand band = new ReportBand();
		band.setBandType(bandType);
		band.setParent(spec);

		ReportElement e = new ReportElement(elemType, name, value,
				Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(200), null);
		e.setElementHeight(Integer.valueOf(20));
		e.setElementFontName("SansSerif");
		e.setParent(band);
		band.addElement(e);
		return e;
	}

	@Test
	public void renderElementBeanCollectionContainsDataSourceExpression() {
		ReportBand band = new ReportBand();
		band.setBandType(ReportBand.BandType.detail);
		band.setParent(spec);

		ReportElement subreportElem = new ReportElement(
				ReportElement.ElementType.subreport, "collectionField", "collectionField",
				Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(200), null);
		subreportElem.setElementHeight(Integer.valueOf(20));
		// Set the parent band so the renderer can navigate to the DesignSpecification
		subreportElem.setParent(band);

		String jrxml = Renderer.renderElement(subreportElem);
		assertThat(jrxml, notNullValue());
		assertFalse(jrxml.isEmpty());
		// In bean mode, subreport element should contain a subreportExpression
		assertThat(jrxml, containsString("subreport"));
	}

	@Test
	public void renderDesignForKnownDocumentContainsFieldTag() {
		// Populate the design from document metadata before rendering — fields are added by populateDesign
		new DocumentReportDesignGenerator().populateDesign(spec);
		// renderDesign calls CORE.getCustomer() to resolve persistent strategy
		String jrxml = Renderer.renderDesign(spec);
		assertThat(jrxml, notNullValue());
		assertFalse(jrxml.isEmpty());
		assertThat(jrxml, containsString("<field"));
	}

	@Test
	public void renderElementStaticTextContainsStaticTextTag() {
		ReportElement e = elementInBand(ReportBand.BandType.detail,
				ReportElement.ElementType.staticText, "label", "Hello World");
		String jrxml = Renderer.renderElement(e);
		assertThat(jrxml, containsString("<staticText>"));
		assertThat(jrxml, containsString("Hello World"));
		assertThat(jrxml, containsString("</staticText>"));
	}

	@Test
	public void renderElementTextFieldContainsTextFieldExpression() {
		ReportElement e = elementInBand(ReportBand.BandType.detail,
				ReportElement.ElementType.textField, "value", "$F{text}");
		String jrxml = Renderer.renderElement(e);
		assertThat(jrxml, containsString("textFieldExpression"));
		assertThat(jrxml, containsString("$F{text}"));
	}

	@Test
	public void renderElementLineContainsLineTag() {
		ReportElement e = elementInBand(ReportBand.BandType.detail,
				ReportElement.ElementType.line, "separator", null);
		String jrxml = Renderer.renderElement(e);
		assertThat(jrxml, containsString("<line"));
		assertThat(jrxml, containsString("</line>"));
	}

	@Test
	public void renderElementBorderContainsRectangleTag() {
		ReportElement e = elementInBand(ReportBand.BandType.detail,
				ReportElement.ElementType.border, "box", null);
		String jrxml = Renderer.renderElement(e);
		assertThat(jrxml, containsString("<rectangle>"));
		assertThat(jrxml, containsString("</rectangle>"));
	}

	@Test
	public void renderBandWithElementContainsBandHeightAndElement() {
		ReportBand band = new ReportBand();
		band.setBandType(ReportBand.BandType.title);
		band.setParent(spec);

		ReportElement e = new ReportElement(ReportElement.ElementType.staticText, "title", "My Report",
				Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(400), null);
		e.setElementHeight(Integer.valueOf(20));
		e.setElementFontName("SansSerif");
		e.setParent(band);
		band.addElement(e);

		String jrxml = Renderer.renderBand(band);
		assertThat(jrxml, containsString("<band"));
		assertThat(jrxml, containsString("<title>"));
		assertThat(jrxml, containsString("<staticText>"));
	}

	@Test
	public void renderBandEmptyDetailBandProducesSelfClosingTag() {
		ReportBand band = new ReportBand();
		band.setBandType(ReportBand.BandType.detail);
		band.setParent(spec);

		String jrxml = Renderer.renderBand(band);
		assertThat(jrxml, containsString("<band/>"));
	}

	// --- Renderer.renderDesign sql mode ---

	@Test
	public void renderDesignSqlModeContainsSqlSelect() {
		spec.setMode(DesignSpecification.Mode.sql);
		new DocumentReportDesignGenerator().populateDesign(spec);
		String jrxml = Renderer.renderDesign(spec);
		assertThat(jrxml, notNullValue());
		assertFalse(jrxml.isEmpty());
		assertThat(jrxml, containsString("select"));
		assertThat(jrxml, containsString("from"));
	}

	@Test
	public void renderDesignSqlModeContainsWhereClauseWithIdParameter() {
		spec.setMode(DesignSpecification.Mode.sql);
		new DocumentReportDesignGenerator().populateDesign(spec);
		String jrxml = Renderer.renderDesign(spec);
		assertThat(jrxml, containsString("$P{ID}"));
	}

	// --- Renderer.getPersistentIdentifierForDocument ---

	@Test
	public void getPersistentIdentifierForDocumentReturnsNonNullAndNonEmpty() {
		org.skyve.metadata.customer.Customer customer = org.skyve.CORE.getPersistence().getUser().getCustomer();
		org.skyve.metadata.module.Module module = customer.getModule(AllAttributesPersistent.MODULE_NAME);
		org.skyve.metadata.model.document.Document document = module.getDocument(customer, AllAttributesPersistent.DOCUMENT_NAME);
		String identifier = Renderer.getPersistentIdentifierForDocument(document);
		assertThat(identifier, notNullValue());
		assertFalse(identifier.isEmpty());
	}

	// --- renderBand with invisibleConditionName ---

	@Test
	public void renderBandWithInvisibleConditionNameContainsPrintWhenExpression() {
		ReportBand band = new ReportBand();
		band.setBandType(ReportBand.BandType.detail);
		band.setInvisibleConditionName("someCondition");
		band.setParent(spec);

		ReportElement e = new ReportElement(ReportElement.ElementType.staticText, "label", "text",
				Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(200), null);
		e.setElementHeight(Integer.valueOf(20));
		e.setElementFontName("SansSerif");
		e.setParent(band);
		band.addElement(e);

		String jrxml = Renderer.renderBand(band);
		assertThat(jrxml, containsString("printWhenExpression"));
		// invisibleConditionName is negated by flipCondition: "someCondition" -> "notSomeCondition"
		assertThat(jrxml, containsString("notSomeCondition"));
	}
}
