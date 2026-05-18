package org.skyve.impl.script;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.emptyString;

import org.apache.commons.text.StringEscapeUtils;

import org.commonmark.node.Node;
import org.commonmark.parser.Parser;
import org.commonmark.renderer.html.HtmlRenderer;
import org.junit.Test;

public class SkyveDocumentNodeRendererTest {

	private static final Parser PARSER = Parser.builder().build();

	private static String render(String markdown) {
		Node document = PARSER.parse(markdown);
		HtmlRenderer renderer = HtmlRenderer.builder()
				.nodeRendererFactory(SkyveDocumentNodeRenderer::new)
				.build();
		return StringEscapeUtils.unescapeHtml4(renderer.render(document));
	}

	private static String renderWithSpacing(String markdown) {
		Node document = PARSER.parse(markdown);
		HtmlRenderer renderer = HtmlRenderer.builder()
				.nodeRendererFactory(c -> new SkyveDocumentNodeRenderer(c, true))
				.build();
		return StringEscapeUtils.unescapeHtml4(renderer.render(document));
	}

	// ── Heading level 2 (document definition) ──────────────────────────────

	@Test
	@SuppressWarnings("static-method")
	public void testHeading2CreatesDocumentDefinition() {
		String result = render("## MyDocument");
		assertThat(result, containsString("<document name=\"MyDocument\""));
		assertThat(result, containsString("xmlns=\"http://www.skyve.org/xml/document\""));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testHeading2CreatesSingularAlias() {
		String result = render("## myDocument");
		assertThat(result, containsString("<singularAlias>My Document</singularAlias>"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testHeading2CreatesPluralAlias() {
		String result = render("## myDocument");
		assertThat(result, containsString("<pluralAlias>My Documents</pluralAlias>"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testHeading2CreatesIconElement() {
		String result = render("## MyDocument");
		assertThat(result, containsString("<iconStyleClass>"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testHeading2CreatesBizKey() {
		String result = render("## MyDocument");
		assertThat(result, containsString("<bizKey expression=\"MyDocument\"/>"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testHeading2WithPersistentNameCreatesPersistentElement() {
		// Heading2 with inline code creates a persistent name
		String result = render("## MyDocument `MY_TABLE`");
		assertThat(result, containsString("<persistent name=\"MY_TABLE\" />"));
	}

	// ── Bullet list attributes (-) ──────────────────────────────────────────

	@Test
	@SuppressWarnings({"static-method", "java:S5976"})
	public void testBulletListCreatesAttributesElement() {
		String markdown = "## MyDocument\n\n- name text 50\n";
		String result = render(markdown);
		assertThat(result, containsString("<attributes>"));
	}

	@Test
	@SuppressWarnings({"static-method", "java:S5976"})
	public void testTextAttributeWithLength() {
		String markdown = "## MyDocument\n\n- name text 50\n";
		String result = render(markdown);
		assertThat(result, containsString("<text name=\"name\""));
		assertThat(result, containsString("<length>50</length>"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testTextAttributeWithLengthDisplayName() {
		String markdown = "## MyDocument\n\n- firstName text 100\n";
		String result = render(markdown);
		assertThat(result, containsString("<displayName>First Name</displayName>"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testTextAttributeMissingLengthEmitsAlert() {
		String markdown = "## MyDocument\n\n- name text\n";
		String result = render(markdown);
		assertThat(result, containsString("missing required field length"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testBooleanAttribute() {
		String markdown = "## MyDocument\n\n- active boolean\n";
		String result = render(markdown);
		assertThat(result, containsString("<boolean name=\"active\""));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testDateAttribute() {
		String markdown = "## MyDocument\n\n- birthDate date\n";
		String result = render(markdown);
		assertThat(result, containsString("<date name=\"birthDate\""));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testDateTimeAttribute() {
		String markdown = "## MyDocument\n\n- createdAt dateTime\n";
		String result = render(markdown);
		assertThat(result, containsString("<dateTime name=\"createdAt\""));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testDecimal10Attribute() {
		String markdown = "## MyDocument\n\n- amount decimal10\n";
		String result = render(markdown);
		assertThat(result, containsString("<decimal10 name=\"amount\""));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testDecimal2Attribute() {
		String markdown = "## MyDocument\n\n- price decimal2\n";
		String result = render(markdown);
		assertThat(result, containsString("<decimal2 name=\"price\""));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testDecimal5Attribute() {
		String markdown = "## MyDocument\n\n- ratio decimal5\n";
		String result = render(markdown);
		assertThat(result, containsString("<decimal5 name=\"ratio\""));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGeometryAttribute() {
		String markdown = "## MyDocument\n\n- location geometry\n";
		String result = render(markdown);
		assertThat(result, containsString("<geometry name=\"location\""));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testIdAttribute() {
		String markdown = "## MyDocument\n\n- externalId id\n";
		String result = render(markdown);
		assertThat(result, containsString("<id name=\"externalId\""));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testIntegerAttribute() {
		String markdown = "## MyDocument\n\n- count integer\n";
		String result = render(markdown);
		assertThat(result, containsString("<integer name=\"count\""));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testLongIntegerAttribute() {
		String markdown = "## MyDocument\n\n- bigCount longInteger\n";
		String result = render(markdown);
		assertThat(result, containsString("<longInteger name=\"bigCount\""));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testMemoAttribute() {
		String markdown = "## MyDocument\n\n- notes memo\n";
		String result = render(markdown);
		assertThat(result, containsString("<memo name=\"notes\""));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testMarkupAttribute() {
		String markdown = "## MyDocument\n\n- body markup\n";
		String result = render(markdown);
		assertThat(result, containsString("<markup name=\"body\""));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testTimeAttribute() {
		String markdown = "## MyDocument\n\n- startTime time\n";
		String result = render(markdown);
		assertThat(result, containsString("<time name=\"startTime\""));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testTimestampAttribute() {
		String markdown = "## MyDocument\n\n- created timestamp\n";
		String result = render(markdown);
		assertThat(result, containsString("<timestamp name=\"created\""));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testEnumAttributeWithValues() {
		String markdown = "## MyDocument\n\n- status enum (active,inactive)\n";
		String result = render(markdown);
		assertThat(result, containsString("<enum name=\"status\""));
		assertThat(result, containsString("<value code=\"active\"/>"));
		assertThat(result, containsString("<value code=\"inactive\"/>"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testEnumAttributeMissingBracketsEmitsAlert() {
		String markdown = "## MyDocument\n\n- status enum noBrackets\n";
		String result = render(markdown);
		assertThat(result, containsString("missing required value array"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testAssociationAttribute() {
		// Uppercase type in a dash list = association
		String markdown = "## MyDocument\n\n- customer Customer\n";
		String result = render(markdown);
		assertThat(result, containsString("<association name=\"customer\""));
		assertThat(result, containsString("<documentName>Customer</documentName>"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testUnsupportedAttributeTypeEmitsAlert() {
		String markdown = "## MyDocument\n\n- weirdField unknownType\n";
		String result = render(markdown);
		assertThat(result, containsString("Unsupported attribute"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testRequiredAttributeViaEmphasis() {
		// emphasis (*name*) marks attribute as required
		String markdown = "## MyDocument\n\n- *name* text 50\n";
		String result = render(markdown);
		assertThat(result, containsString("required=\"true\""));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testDocumentEndWrittenWhenNoMoreNodes() {
		String markdown = "## MyDocument\n\n- name text 50\n";
		String result = render(markdown);
		assertThat(result, containsString("</attributes>"));
		assertThat(result, containsString("</document>"));
	}

	// ── Ordered list (1. items) ─────────────────────────────────────────────

	@Test
	@SuppressWarnings("static-method")
	public void testOrderedListEmitsUnknownTypeAlert() {
		String markdown = "## MyDocument\n\n1. someItem unknownType\n";
		String result = render(markdown);
		assertThat(result, containsString("Unknown list item type"));
	}

	// ── Unknown bullet marker ───────────────────────────────────────────────

	@Test
	@SuppressWarnings("static-method")
	public void testGetNodeTypesIncludesHeadingAndListTypes() {
		// Verify the renderer handles Heading/BulletList/OrderedList/ListItem
		// by rendering a heading and checking output is produced
		String result = render("## Document");
		assertThat(result, not(emptyString()));
	}

	// ── htmlSpacing mode ────────────────────────────────────────────────────

	@Test
	@SuppressWarnings("static-method")
	public void testHtmlSpacingModeEmitsBreakTags() {
		String markdown = "## MyDocument";
		String result = renderWithSpacing(markdown);
		assertThat(result, containsString("<br"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testHtmlSpacingModeEmitsEmspForIndent() {
		String markdown = "## MyDocument\n\n- name text 50\n";
		String result = renderWithSpacing(markdown);
		assertThat(result, containsString("\u2003")); // em space (&emsp;)
	}

	// ── Collection (+) ──────────────────────────────────────────────────────

	@Test
	@SuppressWarnings("static-method")
	public void testPlusMarkerListCreatesCollection() {
		String markdown = "## MyDocument\n\n+ lines LineItem\n";
		String result = render(markdown);
		assertThat(result, containsString("<collection name=\"lines\""));
		assertThat(result, containsString("<documentName>LineItem</documentName>"));
	}

	// ── Multiple documents ──────────────────────────────────────────────────

	@Test
	@SuppressWarnings("static-method")
	public void testMultipleDocumentHeadingsProduceMultipleDocumentElements() {
		String markdown = "## DocA\n\n## DocB\n";
		String result = render(markdown);
		assertThat(result, containsString("name=\"DocA\""));
		assertThat(result, containsString("name=\"DocB\""));
	}
}
