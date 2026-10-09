package org.skyve.impl.web.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.LinkItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.impl.metadata.module.query.AbstractMetaDataQueryColumn;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.repository.view.ViewMetaData;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.repository.DelegatingProvidedRepositoryChain;
import org.skyve.metadata.repository.MutableRepository;
import org.skyve.metadata.user.User;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import modules.test.AbstractSkyveTest;

/**
 * Renders fixture metadata through {@code MetaDataServlet} and parses the result with a strict JSON parser.
 *
 * <p>Each test renders the smallest view or menu that reaches one piece of servlet output, so that a failure names
 * the output at fault. Fixture views are put into the repository under a UX/UI name nothing else uses, and menus are
 * rendered for a user of this test's own, whose menus are a private copy.
 */
@SuppressWarnings("java:S1192") // Repeated values are deliberate metadata rendering fixtures.
class MetaDataServletJsonValidityH2Test extends AbstractSkyveTest {
	private static final String FIXTURE_UXUI = "jsonValidityFixture";
	private static final String MENU_UXUI = "external";
	private static final String MODULE_NAME = "kitchensink";
	private static final String DOCUMENT_NAME = "KitchenSink";
	private static final String LIST_DOCUMENT_NAME = "ListAttributes";

	/** A value holding the two characters that end or corrupt a JSON string when written raw. */
	private static final String NASTY = "say \"hi\" C:\\dir";
	/** {@link #NASTY} as it is written in an XML attribute or element. */
	private static final String NASTY_XML = "say &quot;hi&quot; C:\\dir";

	private static final ObjectMapper STRICT_JSON = new ObjectMapper()
			.enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION)
			.enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

	private SuperUser menuUser;

	@BeforeEach
	void createMenuUser() {
		menuUser = new SuperUser();
		menuUser.setCustomerName(c.getName());
		menuUser.setName(u.getName());
		menuUser.setId(u.getId());
		ProvidedRepositoryFactory.get().resetMenus(menuUser);
	}

	@Test
	void sliderSettingsAreNumbers() throws Exception {
		JsonNode slider = widget(renderFixture("", item("<slider binding=\"slider\" min=\"1\" max=\"10\""
				+ " numberOfDiscreteValues=\"10\" roundingPrecision=\"0\" vertical=\"false\" />"), ""), "slider");

		assertEquals(1.0, number(slider, "min").doubleValue());
		assertEquals(10.0, number(slider, "max").doubleValue());
		assertEquals(10, number(slider, "numberOfDiscreteValues").intValue());
		assertEquals(0, number(slider, "roundingPrecision").intValue());
		assertFalse(slider.get("vertical").booleanValue());
	}

	@Test
	void lookupDescriptionDropDownColumnsAreObjects() throws Exception {
		JsonNode lookup = widget(renderFixture("", item("<lookupDescription binding=\"lookupDescription\""
				+ " descriptionBinding=\"description\"><dropDown><column filterable=\"true\">description</column>"
				+ "<column>bizKey</column></dropDown></lookupDescription>"), ""), "lookupDescription");

		JsonNode columns = lookup.get("dropDownColumns");
		assertNotNull(columns, lookup.toString());
		assertEquals(2, columns.size());
		assertEquals("description", columns.get(0).get("name").textValue());
		assertTrue(columns.get(0).get("filterable").booleanValue());
		assertEquals("bizKey", columns.get(1).get("name").textValue());
		assertFalse(columns.get(1).has("filterable"));
	}

	@Test
	void chartOrderAndTopAreObjects() throws Exception {
		JsonNode chart = widget(renderFixture("", chart("label=\"Values\"", "<noBucket /><top by=\"value\""
				+ " sort=\"descending\" top=\"5\" includeOthers=\"true\" /><order by=\"category\" sort=\"ascending\" />"), ""), "chart");

		JsonNode top = chart.get("top");
		assertNotNull(top, chart.toString());
		assertEquals("value", top.get("by").textValue());
		assertEquals("descending", top.get("sort").textValue());
		assertEquals(5, number(top, "top").intValue());
		assertTrue(top.get("includeOthers").booleanValue());
		JsonNode order = chart.get("order");
		assertNotNull(order, chart.toString());
		assertEquals("category", order.get("by").textValue());
		assertEquals("ascending", order.get("sort").textValue());
	}

	@Test
	void chartTextStartsWithBucketIsAnObject() throws Exception {
		JsonNode chart = widget(renderFixture("", chart("label=\"Values\"",
				"<textStartsWithBucket length=\"2\" caseSensitive=\"true\" />"), ""), "chart");

		JsonNode bucket = chart.get("categoryBucket");
		assertNotNull(bucket, chart.toString());
		assertEquals("textStartsWithBucket", bucket.get("type").textValue());
		assertEquals(2, number(bucket, "length").intValue());
		assertTrue(bucket.get("caseSensitive").booleanValue());
	}

	@Test
	void chartPostProcessorClassNamesAreSeparateMembers() throws Exception {
		JsonNode chart = widget(renderFixture("", chart("label=\"Values\"", "<noBucket /><order by=\"category\""
				+ " sort=\"ascending\" /><JFreeChartPostProcessorClassName>fixture.JFree</JFreeChartPostProcessorClassName>"
				+ "<primeFacesChartPostProcessorClassName>fixture.PrimeFaces</primeFacesChartPostProcessorClassName>"),
				""), "chart");

		assertEquals("fixture.JFree", text(chart, "jFreeChartPostProcessorClassName"));
		assertEquals("fixture.PrimeFaces", text(chart, "primeFacesChartPostProcessorClassName"));
	}

	@Test
	void chartPostProcessorClassNameFollowsTheLastSimpleMember() throws Exception {
		JsonNode chart = widget(renderFixture("", chart("label=\"Values\"", "<noBucket />"
				+ "<primeFacesChartPostProcessorClassName>fixture.PrimeFaces</primeFacesChartPostProcessorClassName>"),
				""), "chart");

		assertEquals("fixture.PrimeFaces", text(chart, "primeFacesChartPostProcessorClassName"));
	}

	@Test
	void chartTitleAndLabelAreEscaped() throws Exception {
		JsonNode chart = widget(renderFixture("", chart("title=\"" + NASTY_XML + " title\" label=\""
				+ NASTY_XML + " label\"", "<noBucket />"), ""), "chart");

		assertEquals(NASTY + " title", text(chart, "title"));
		assertEquals(NASTY + " label", text(chart, "label"));
	}

	@Test
	void viewRefreshTimeHasItsOwnMember() throws Exception {
		JsonNode view = renderFixture("helpURL=\"https://example.com/help\" refreshTimeInSeconds=\"30\"",
				item("<textField binding=\"text\" />"), "");

		assertEquals("https://example.com/help", text(view, "helpURL"));
		assertEquals(30, number(view, "refreshTimeInSeconds").intValue());
	}

	@Test
	void viewHelpLocationsAreEscaped() throws Exception {
		JsonNode view = renderFixture("helpRelativeFileName=\"" + NASTY_XML + ".html\" helpURL=\"https://example.com/?q="
				+ NASTY_XML + "\"", item("<textField binding=\"text\" />"), "");

		assertEquals(NASTY + ".html", text(view, "helpRelativeFileName"));
		assertEquals("https://example.com/?q=" + NASTY, text(view, "helpURL"));
	}

	@Test
	void formItemRequiredMessageIsEscaped() throws Exception {
		JsonNode view = renderFixture("", form("<item required=\"true\" requiredMessage=\"" + NASTY_XML
				+ "\"><textField binding=\"text\" /></item>"), "");

		assertEquals(NASTY, text(find(view, "type", "item"), "requiredMessage"));
	}

	@Test
	void externalLinkHrefIsEscaped() throws Exception {
		JsonNode link = widget(renderFixture("", item("<link value=\"External\"><externalReference"
				+ " href=\"https://example.com/?q=" + NASTY_XML + "\" /></link>"), ""), "link");

		assertEquals("https://example.com/?q=" + NASTY, text(link.get("reference"), "href"));
	}

	@Test
	void dialogButtonDialogNameAndCommandAreEscaped() throws Exception {
		JsonNode button = widget(renderFixture("", item("<dialogButton displayName=\"Dialog\" dialogName=\""
				+ NASTY_XML + " dialog\" command=\"" + NASTY_XML + " command\" />"), ""), "dialogButton");

		assertEquals(NASTY + " dialog", text(button, "dialogName"));
		assertEquals(NASTY + " command", text(button, "command"));
	}

	@Test
	void contentSignatureColoursAreEscaped() throws Exception {
		JsonNode signature = widget(renderFixture("", item("<contentSignature binding=\"contentSignature\""
				+ " rgbHexBackgroundColour=\"" + NASTY_XML + " back\" rgbHexForegroundColour=\"" + NASTY_XML
				+ " fore\" />"), ""), "contentSignature");

		assertEquals(NASTY + " back", text(signature, "rgbHexBackgroundColour"));
		assertEquals(NASTY + " fore", text(signature, "rgbHexForegroundColour"));
	}

	@Test
	void widgetIdIsEscaped() throws Exception {
		JsonNode view = renderFixture("", "<form widgetId=\"" + NASTY_XML + "\"><column /><column /><row><item>"
				+ "<textField binding=\"text\" /></item></row></form>", "");

		assertEquals(NASTY, text(find(view, "type", "form"), "widgetId"));
	}

	@Test
	void dynamicImageNameIsEscaped() throws Exception {
		JsonNode image = widget(renderFixture("", "<dynamicImage name=\"" + NASTY_XML + "\" />", ""), "dynamicImage");

		assertEquals(NASTY, text(image, "name"));
	}

	@Test
	void actionFontIconIsEscaped() throws Exception {
		JsonNode view = renderFixture("", item("<textField binding=\"text\" />"),
				"<actions><save iconStyleClass=\"fa " + NASTY_XML + "\" /></actions>");

		JsonNode actions = view.get("actions");
		assertNotNull(actions, view.toString());
		assertEquals("fa " + NASTY, text(actions.get(0), "fontIcon"));
	}

	@Test
	void listGridQueryColumnHasAnEscapedLabel() throws Exception {
		@SuppressWarnings("null")
		AbstractMetaDataQueryColumn column = (AbstractMetaDataQueryColumn) module().getMetaDataQuery("qEscapingFixture")
				.getColumns().get(0);
		String displayName = column.getDisplayName();
		column.setDisplayName(NASTY);
		try {
			JsonNode grid = widget(renderFixture("", "<listGrid query=\"qEscapingFixture\""
					+ " continueConversation=\"false\" />", ""), "listGrid");

			JsonNode gridColumn = grid.get("columns").get(0);
			assertEquals(NASTY, text(gridColumn, "label"));
			assertFalse(gridColumn.has("lobel"), gridColumn.toString());
		}
		finally {
			column.setDisplayName(displayName);
		}
	}

	@Test
	void menuLinkHrefIsEscaped() throws Exception {
		LinkItem link = menuItems().stream().filter(LinkItem.class::isInstance).map(LinkItem.class::cast).findFirst()
				.orElseThrow();
		link.setHref("fixture?q=" + NASTY);

		// The menu renderer puts the context URL before a relative href.
		String href = text(menuItem("link", link.getName()), "href");
		assertTrue(href.endsWith("/fixture?q=" + NASTY), href);
	}

	@Test
	void menuItemFontIconIsEscapedForEveryItemType() throws Exception {
		addTreeMapAndCalendarItems();
		DocumentImpl edited = document(DOCUMENT_NAME);
		DocumentImpl listed = document(LIST_DOCUMENT_NAME);
		String editedIcon = edited.getIconStyleClass();
		String listedIcon = listed.getIconStyleClass();
		edited.setIconStyleClass("fa " + NASTY);
		listed.setIconStyleClass("fa " + NASTY);
		try {
			JsonNode menu = menu();

			assertEquals("fa " + NASTY, text(find(menu, "edit", "Kitchen Sink"), "fontIcon"));
			assertEquals("fa " + NASTY, text(find(menu, "list", "List"), "fontIcon"));
			assertEquals("fa " + NASTY, text(find(menu, "tree", "Fixture tree"), "fontIcon"));
			assertEquals("fa " + NASTY, text(find(menu, "map", "Fixture map"), "fontIcon"));
			assertEquals("fa " + NASTY, text(find(menu, "calendar", "Fixture calendar"), "fontIcon"));
		}
		finally {
			edited.setIconStyleClass(editedIcon);
			listed.setIconStyleClass(listedIcon);
		}
	}

	@Test
	void menuItemIcon16IsEscapedForEveryItemType() throws Exception {
		addTreeMapAndCalendarItems();
		DocumentImpl edited = document(DOCUMENT_NAME);
		DocumentImpl listed = document(LIST_DOCUMENT_NAME);
		String editedIcon = edited.getIconStyleClass();
		String listedIcon = listed.getIconStyleClass();
		String editedIcon16 = edited.getIcon16x16RelativeFileName();
		String listedIcon16 = listed.getIcon16x16RelativeFileName();
		edited.setIconStyleClass(null);
		listed.setIconStyleClass(null);
		edited.setIcon16x16RelativeFileName(NASTY + ".png");
		listed.setIcon16x16RelativeFileName(NASTY + ".png");
		try {
			JsonNode menu = menu();

			assertEquals(NASTY + ".png", text(find(menu, "edit", "Kitchen Sink"), "icon16"));
			assertEquals(NASTY + ".png", text(find(menu, "list", "List"), "icon16"));
			assertEquals(NASTY + ".png", text(find(menu, "tree", "Fixture tree"), "icon16"));
			assertEquals(NASTY + ".png", text(find(menu, "map", "Fixture map"), "icon16"));
			assertEquals(NASTY + ".png", text(find(menu, "calendar", "Fixture calendar"), "icon16"));
		}
		finally {
			edited.setIconStyleClass(editedIcon);
			listed.setIconStyleClass(listedIcon);
			edited.setIcon16x16RelativeFileName(editedIcon16);
			listed.setIcon16x16RelativeFileName(listedIcon16);
		}
	}

	@Test
	void userContactAvatarInitialsAreEscaped() throws Exception {
		menuUser.setContactName("\"Quoted\" \\Slashed");

		assertEquals("\"\\", text(parse(renderMetadata()), "userContactAvatarInitials"));
	}

	private Module module() {
		return c.getModule(MODULE_NAME);
	}

	private DocumentImpl document(String documentName) {
		return (DocumentImpl) module().getDocument(c, documentName);
	}

	private List<MenuItem> menuItems() {
		return menuUser.getModuleMenu(MODULE_NAME).getItems();
	}

	/**
	 * Adds one menu item of each type the kitchen sink menu lacks, each over the list document.
	 */
	private void addTreeMapAndCalendarItems() {
		TreeItem tree = new TreeItem();
		tree.setName("Fixture tree");
		tree.setDocumentName(LIST_DOCUMENT_NAME);
		MapItem map = new MapItem();
		map.setName("Fixture map");
		map.setDocumentName(LIST_DOCUMENT_NAME);
		map.setGeometryBinding("geometry");
		CalendarItem calendar = new CalendarItem();
		calendar.setName("Fixture calendar");
		calendar.setDocumentName(LIST_DOCUMENT_NAME);
		calendar.setStartBinding("date");
		calendar.setEndBinding("date");

		menuItems().addAll(List.of(tree, map, calendar));
	}

	private JsonNode menu() throws Exception {
		JsonNode menus = parse(renderMetadata()).get("menus");
		for (JsonNode menu : menus) {
			if (MODULE_NAME.equals(menu.get("module").textValue())) {
				return menu;
			}
		}
		return fail("No " + MODULE_NAME + " menu in " + menus);
	}

	private JsonNode menuItem(String type, String name) throws Exception {
		return find(menu(), type, name);
	}

	private JsonNode renderFixture(String viewAttributes, String contained, String actions) throws Exception {
		String xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><view name=\"edit\" title=\"JSON validity fixture\" "
				+ viewAttributes + " xmlns=\"http://www.skyve.org/xml/view\">" + contained + actions + "</view>";
		ViewMetaData view = XMLMetaData.unmarshalViewString(xml);
		mutableRepository().putView(c, FIXTURE_UXUI, document(DOCUMENT_NAME), view);
		return parse(invokeView(u, FIXTURE_UXUI, MODULE_NAME, DOCUMENT_NAME));
	}

	private static String form(String item) {
		return "<form><column /><column /><row>" + item + "</row></form>";
	}

	private static String item(String widget) {
		return form("<item>" + widget + "</item>");
	}

	private static String chart(String modelAttributes, String modelElements) {
		return "<chart type=\"bar\"><model " + modelAttributes + " moduleName=\"" + MODULE_NAME + "\" documentName=\""
				+ LIST_DOCUMENT_NAME + "\" categoryBinding=\"text\" valueBinding=\"normalInteger\" valueFunction=\"Sum\">"
				+ modelElements + "</model></chart>";
	}

	private static JsonNode widget(JsonNode view, String type) {
		return find(view, "type", type);
	}

	/**
	 * Finds the first object, depth first, whose named member has the given text value.
	 */
	private static JsonNode find(JsonNode node, String memberName, String memberValue) {
		JsonNode result = findOrNull(node, memberName, memberValue);
		assertNotNull(result, "No object with " + memberName + " = " + memberValue + " in " + node);
		return result;
	}

	private static JsonNode findOrNull(JsonNode node, String memberName, String memberValue) {
		if (node.isObject()) {
			JsonNode member = node.get(memberName);
			if ((member != null) && member.isTextual() && memberValue.equals(member.textValue())) {
				return node;
			}
		}
		for (JsonNode child : node) {
			JsonNode result = findOrNull(child, memberName, memberValue);
			if (result != null) {
				return result;
			}
		}
		return null;
	}

	private static String text(JsonNode node, String memberName) {
		JsonNode member = node.get(memberName);
		assertNotNull(member, "No " + memberName + " in " + node);
		assertTrue(member.isTextual(), memberName + " is not a string in " + node);
		return member.textValue();
	}

	private static JsonNode number(JsonNode node, String memberName) {
		JsonNode member = node.get(memberName);
		assertNotNull(member, "No " + memberName + " in " + node);
		assertTrue(member.isNumber(), memberName + " is not a number in " + node);
		return member;
	}

	private static JsonNode parse(String json) {
		try {
			return STRICT_JSON.readTree(json);
		}
		catch (JsonProcessingException e) {
			return fail("Not valid JSON: " + e.getOriginalMessage() + " in " + json, e);
		}
	}

	private String renderMetadata() throws Exception {
		StringWriter body = new StringWriter();
		try (PrintWriter writer = new PrintWriter(body)) {
			invoke(servletMethod("metadata", User.class, String.class, String.class, PrintWriter.class),
					menuUser, MENU_UXUI, MODULE_NAME, writer);
		}
		return body.toString();
	}

	private static String invokeView(User user, String uxui, String moduleName, String documentName) throws Exception {
		return invoke(servletMethod("view", User.class, String.class, String.class, String.class, boolean.class),
				user, uxui, moduleName, documentName, Boolean.FALSE).toString();
	}

	@SuppressWarnings("java:S3011") // Reflection exercises the private servlet rendering seams.
	private static Method servletMethod(String name, Class<?>... parameterTypes) throws Exception {
		Method result = Class.forName("org.skyve.impl.web.service.MetaDataServlet").getDeclaredMethod(name, parameterTypes);
		result.setAccessible(true);
		return result;
	}

	private static Object invoke(Method method, Object... arguments) throws Exception {
		try {
			return method.invoke(null, arguments);
		}
		catch (InvocationTargetException e) {
			if (e.getCause() instanceof Exception exception) {
				throw exception;
			}
			throw e;
		}
	}

	/**
	 * Returns the repository in the delegating chain that accepts metadata put at run time.
	 */
	@SuppressWarnings("java:S3011") // The chain does not expose its delegates.
	private static MutableRepository mutableRepository() throws Exception {
		Field delegates = DelegatingProvidedRepositoryChain.class.getDeclaredField("delegates");
		delegates.setAccessible(true);
		for (Object delegate : (List<?>) delegates.get(ProvidedRepositoryFactory.get())) {
			if (delegate instanceof MutableRepository mutable) {
				return mutable;
			}
		}
		return fail("No mutable repository in the delegating chain");
	}
}
