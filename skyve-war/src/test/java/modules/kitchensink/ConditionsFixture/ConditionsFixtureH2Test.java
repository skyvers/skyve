package modules.kitchensink.ConditionsFixture;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.beans.Introspector;
import java.io.InputStream;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.opentest4j.AssertionFailedError;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.impl.metadata.repository.view.ViewMetaData;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.component.Component;
import org.skyve.impl.sail.mock.MockApplication;
import org.skyve.impl.sail.mock.MockFacesContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.faces.models.BeanMapAdapter;
import org.skyve.impl.web.faces.pipeline.FacesViewRenderer;
import org.skyve.impl.web.faces.pipeline.component.ResponsiveComponentBuilder;
import org.skyve.impl.web.faces.pipeline.layout.ResponsiveLayoutBuilder;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.impl.web.service.smartclient.SmartClientGeneratorServlet;
import org.skyve.impl.web.service.smartclient.SmartClientViewRenderer;
import org.skyve.impl.web.service.smartclient.ViewJSONManipulator;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.JSON;
import org.skyve.web.UserAgentType;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import jakarta.el.ELContext;
import jakarta.el.ExpressionFactory;
import jakarta.el.StandardELContext;
import jakarta.el.ValueExpression;
import jakarta.faces.application.Application;
import jakarta.faces.component.UIComponent;
import jakarta.faces.context.FacesContext;
import modules.kitchensink.domain.ConditionsFixture;
import modules.kitchensink.domain.ConditionsFixture.Choice;
import modules.kitchensink.domain.ConditionsFixtureRow;
import util.AbstractH2Test;

/**
 * Exercises every view attribute that takes a condition, with the Kitchen Sink conditions fixture as the subject.
 *
 * <p>{@link #EXPECTED} says when each condition the fixture's views author is true. Every test derives what it
 * expects from that table and fails on a condition the table does not know, so the table and the views cannot
 * drift apart. The opposite sense of a condition is never listed; it is expected to be the strict complement.
 *
 * <p>The tests follow a condition from the XML, through the loaded metadata and its evaluation against a bean,
 * to what SmartClient, PrimeFaces and {@code MetaDataServlet} make of it. No browser is involved.
 */
@SuppressWarnings({"java:S1192", "java:S3011"}) // Repeated condition text is the fixture's vocabulary; reflection reads every condition accessor.
class ConditionsFixtureH2Test extends AbstractH2Test {
	private static final String SMART_CLIENT_UXUI = "desktop";
	private static final String PRIME_FACES_UXUI = "external";
	private static final String VIEWS = "modules/kitchensink/ConditionsFixture/views/";
	private static final String EDIT_VIEW = VIEWS + "edit.xml";
	private static final String COMPONENT_VIEW = VIEWS + "_conditions.xml";
	private static final String COMPONENT_VIEW_NAME = "_conditions";

	/** The key of the stash entry and of the user attribute the fixture's views refer to. */
	private static final String CONTEXT_KEY = "conditionsFixture";

	/**
	 * Condition attributes of view.xsd, in a form, that the fixture does not author, each for the reason given.
	 */
	private static final Set<String> NOT_AUTHORED = Set.of(
			// a view has one sidebar and one refreshIf, each authored once, with a condition name
			"container/sidebar@visible#expression",
			"container/sidebar@invisible#name",
			"container/sidebar@invisible#expression",
			"view@refreshIf#expression");

	private static final String[] VISIBILITY = {"visible", "invisible"};
	private static final String[] ABILITY = {"enabled", "disabled"};
	private static final String[] VISIBILITY_AND_ABILITY = {"visible", "invisible", "enabled", "disabled"};
	private static final String[] EDIT = {"enableEdit", "disableEdit"};
	private static final String[] POST_REFRESH = {"postRefresh"};
	private static final String[] LOOKUP_FLAGS = {"enablePick", "disablePick", "enableClear", "disableClear", "enableEdit", "disableEdit", "enableAdd", "disableAdd"};
	/** The widgets a bound column of a data grid can take as its input. */
	private static final String[] INPUT_WIDGETS = {"input/checkBox", "input/colourPicker", "input/combo", "input/content", "input/geometry",
													"input/html", "input/lookupDescription", "input/password", "input/radio", "input/richText",
													"input/slider", "input/spinner", "input/textArea", "input/textField"};

	/**
	 * Condition attributes of the fixture's edit view, as {@code context/element@attribute}, that the SmartClient
	 * view script does not name. Each is what the renderer does today, for the reason given; none is a requirement.
	 */
	private static final Set<String> SMART_CLIENT_SCRIPT_DOES_NOT_NAME = new TreeSet<>();
	static {
		// a container column's widgets are rendered on the server for each row, into markup that the data carries
		attributes(SMART_CLIENT_SCRIPT_DOES_NOT_NAME, VISIBILITY, "containerColumn/blurb", "containerColumn/label", "containerColumn/link",
					"containerColumn/staticImage", "containerColumn/dynamicImage");
		attributes(SMART_CLIENT_SCRIPT_DOES_NOT_NAME, VISIBILITY_AND_ABILITY, "containerColumn/content");
		// the widget of a bound column input makes the column's editor, which is given none of its conditions
		attributes(SMART_CLIENT_SCRIPT_DOES_NOT_NAME, VISIBILITY_AND_ABILITY, INPUT_WIDGETS);
		attributes(SMART_CLIENT_SCRIPT_DOES_NOT_NAME, LOOKUP_FLAGS, "input/lookupDescription");
		// rendered without their conditions, or as placeholders; SmartClientViewRenderer marks most as still to be done
		attributes(SMART_CLIENT_SCRIPT_DOES_NOT_NAME, VISIBILITY, "item/staticImage", "container/staticImage",
					"container/link", "container/chart", "container/map");
		attributes(SMART_CLIENT_SCRIPT_DOES_NOT_NAME, VISIBILITY_AND_ABILITY, "container/checkMembership", "container/dialogButton");
		// the add action is never put in the action panel
		attributes(SMART_CLIENT_SCRIPT_DOES_NOT_NAME, VISIBILITY_AND_ABILITY, "actions/add");
		// a component's contents are visited, but its own condition is not applied to them
		attributes(SMART_CLIENT_SCRIPT_DOES_NOT_NAME, VISIBILITY, "container/component");
		// read by MetaDataServlet only
		SMART_CLIENT_SCRIPT_DOES_NOT_NAME.add("view@refreshIf");
	}

	/**
	 * Condition attributes that the SmartClient view script names but whose value is not in the data it is sent,
	 * because {@code ViewJSONManipulator} does nothing for a spacer. The spacer follows its condition only where
	 * something else in the view uses the same condition.
	 */
	private static final Set<String> SMART_CLIENT_DATA_DOES_NOT_CARRY =
			Set.of("item/spacer@visible", "item/spacer@invisible", "container/spacer@visible", "container/spacer@invisible");

	/**
	 * Condition attributes of the fixture's edit view, as {@code context/element@attribute}, that no expression of a
	 * PrimeFaces component refers to. Each is what the faces pipeline does today, for the reason given; none is a requirement.
	 */
	private static final Set<String> PRIME_FACES_COMPONENTS_DO_NOT_REFER_TO = new TreeSet<>();
	static {
		// not rendered: add and new are commented out in FacesViewRenderer, and print is still to be done
		attributes(PRIME_FACES_COMPONENTS_DO_NOT_REFER_TO, VISIBILITY_AND_ABILITY, "actions/add", "actions/new", "actions/print");
		// placeholders that FacesViewRenderer marks as still to be done: a link to a resource, check membership, a comparison and a tree grid
		attributes(PRIME_FACES_COMPONENTS_DO_NOT_REFER_TO, VISIBILITY, "containerColumn/link");
		attributes(PRIME_FACES_COMPONENTS_DO_NOT_REFER_TO, ABILITY, "container/checkMembership", "container/comparison", "container/treeGrid");
		attributes(PRIME_FACES_COMPONENTS_DO_NOT_REFER_TO, new String[] {"enableAdd", "disableAdd", "enableZoom", "disableZoom", "enableRemove", "disableRemove"}, "container/treeGrid");
		// the disabled condition of these is not applied; a form's is marked as still to be done
		attributes(PRIME_FACES_COMPONENTS_DO_NOT_REFER_TO, ABILITY, "container/form", "container/tabPane", "container/listMembership");
		// nothing in the faces pipeline reads these conditions
		attributes(PRIME_FACES_COMPONENTS_DO_NOT_REFER_TO, LOOKUP_FLAGS, "item/lookupDescription", "input/lookupDescription");
		attributes(PRIME_FACES_COMPONENTS_DO_NOT_REFER_TO, EDIT, "container/dataGrid", "container/listGrid", "container/treeGrid");
		attributes(PRIME_FACES_COMPONENTS_DO_NOT_REFER_TO, new String[] {"enableRemove", "disableRemove"}, "container/listGrid");
		attributes(PRIME_FACES_COMPONENTS_DO_NOT_REFER_TO, POST_REFRESH, "container/listGrid", "container/listRepeater", "container/treeGrid");
		// in a column of a data grid, a widget's disabled condition is applied but its invisible condition is not
		attributes(PRIME_FACES_COMPONENTS_DO_NOT_REFER_TO, VISIBILITY, INPUT_WIDGETS);
		attributes(PRIME_FACES_COMPONENTS_DO_NOT_REFER_TO, VISIBILITY, "containerColumn/content");
		// every kind of event handler is rendered without the conditions of its setDisabled and setInvisible actions
		attributes(PRIME_FACES_COMPONENTS_DO_NOT_REFER_TO, ABILITY, handlers("setDisabled"));
		attributes(PRIME_FACES_COMPONENTS_DO_NOT_REFER_TO, VISIBILITY, handlers("setInvisible"));
		// a component's contents are visited, but its own condition is not applied to them
		attributes(PRIME_FACES_COMPONENTS_DO_NOT_REFER_TO, VISIBILITY, "container/component");
		// read by MetaDataServlet only
		PRIME_FACES_COMPONENTS_DO_NOT_REFER_TO.add("view@refreshIf");
	}

	private static String[] handlers(String eventAction) {
		String[] result = {"Added", "Blur", "Changed", "Cleared", "Deleted", "Edited", "Focus", "Picked", "Removed", "Selected"};
		for (int i = 0; i < result.length; i++) {
			result[i] = "on" + result[i] + "Handlers/" + eventAction;
		}
		return result;
	}

	private static void attributes(Set<String> addTo, String[] attributes, String... elements) {
		for (String element : elements) {
			for (String attribute : attributes) {
				addTo.add(element + '@' + attribute);
			}
		}
	}

	/** When each condition the fixture's views author is true. */
	private static final Map<String, Predicate<ConditionsFixture>> EXPECTED = new TreeMap<>();
	static {
		Predicate<ConditionsFixture> shown = bean -> Boolean.TRUE.equals(bean.getShow());
		Predicate<ConditionsFixture> allowed = bean -> Boolean.TRUE.equals(bean.getAllow());
		Predicate<ConditionsFixture> nestedShown = bean -> (bean.getNested() != null) && Boolean.TRUE.equals(bean.getNested().getShow());
		expectForms("show", "shown", shown);
		expectForms("allow", "allowed", allowed);

		// implicit conditions
		EXPECTED.put(Bean.PERSISTED_KEY, Bean::isPersisted);
		EXPECTED.put(Bean.CREATED_KEY, Bean::isCreated);
		EXPECTED.put(Bean.CHANGED_KEY, Bean::isChanged);

		// expressions that do not depend on the bean
		EXPECTED.put("{role:kitchensink.dev}", bean -> CORE.getUser().isInRole(ConditionsFixture.MODULE_NAME, "dev"));
		EXPECTED.put("{stash:conditionsFixture}", bean -> Boolean.TRUE.equals(CORE.getStash().get(CONTEXT_KEY)));
		EXPECTED.put("{user:conditionsFixture}", bean -> Boolean.TRUE.equals(CORE.getUser().getAttributes().get(CONTEXT_KEY)));
		// an i18n expression yields text, which is never true
		EXPECTED.put("{i18n:conditionsFixture}", bean -> false);

		// expressions across a relation, which is also where a component with a binding takes an expression
		EXPECTED.put("{nested.show}", nestedShown);
		EXPECTED.put("{el:bean.nested.show}", nestedShown);
		EXPECTED.put("{el:not bean.nested.show}", nestedShown.negate());
		EXPECTED.put("{el:bean.nested.text eq 'mapped'}", bean -> (bean.getNested() != null) && "mapped".equals(bean.getNested().getText()));

		// expressions with operators
		EXPECTED.put("{el:bean.show and bean.allow}", shown.and(allowed));
		EXPECTED.put("{el:bean.allow and bean.show}", shown.and(allowed));
		EXPECTED.put("{el:bean.show or bean.allow}", shown.or(allowed));
		EXPECTED.put("{el:bean.text eq 'x'}", bean -> "x".equals(bean.getText()));
		EXPECTED.put("{el:bean.text eq 'mapped'}", bean -> "mapped".equals(bean.getText()));
		EXPECTED.put("{el:bean.number gt 5}", bean -> (bean.getNumber() != null) && (bean.getNumber().intValue() > 5));
		EXPECTED.put("{el:bean.choice eq Choice.one}", bean -> Choice.one == bean.getChoice());
		EXPECTED.put("{el:empty bean.dataGrid1}", bean -> bean.getDataGrid1().isEmpty());
	}

	/**
	 * Expects every form the fixture uses to say a boolean attribute is set, and to say it is not.
	 * An attribute with no value is not set.
	 */
	private static void expectForms(String attribute, String condition, Predicate<ConditionsFixture> set) {
		EXPECTED.put(condition, set);
		EXPECTED.put(condition + "ByExpression", set);
		EXPECTED.put('{' + attribute + '}', set);
		EXPECTED.put("{el:bean." + attribute + '}', set);
		EXPECTED.put("{rtel:bean." + attribute + '}', set);
		EXPECTED.put("{el:bean." + attribute + " eq true}", set);

		Predicate<ConditionsFixture> notSet = set.negate();
		EXPECTED.put("{el:not bean." + attribute + '}', notSet);
		EXPECTED.put("{rtel:not bean." + attribute + '}', notSet);
		EXPECTED.put("{el:empty bean." + attribute + " or bean." + attribute + " eq false}", notSet);
	}

	/**
	 * A bean to evaluate conditions against, with the stash entry and user attribute to evaluate them with.
	 */
	private record Scenario(String name, ConditionsFixture bean, Object stashed, Object userAttribute) {
		/**
		 * Puts this scenario's stash entry and user attribute in place, removing either that it does not have.
		 */
		void enter() {
			put(CORE.getStash(), stashed);
			put(CORE.getUser().getAttributes(), userAttribute);
		}

		private static void put(Map<String, Object> map, Object value) {
			if (value == null) {
				map.remove(CONTEXT_KEY);
			}
			else {
				map.put(CONTEXT_KEY, value);
			}
		}
	}

	/**
	 * A condition, or a pair of opposite condition attributes, of one piece of view metadata.
	 *
	 * @param owner	The metadata.
	 * @param attribute	The attribute authored in the positive sense, or the only attribute where there is no opposite.
	 * @param authored	What that attribute holds.
	 * @param oppositeAttribute	The attribute authored in the negative sense, or {@code null} where there is no opposite.
	 * @param oppositeAuthored	What that attribute holds.
	 * @param derived	The negative sense as renderers get it, or what the only attribute holds.
	 */
	private record Condition(Object owner, String attribute, String authored, String oppositeAttribute, String oppositeAuthored, String derived) {
		String describe() {
			StringBuilder result = new StringBuilder(128).append(owner.getClass().getSimpleName());
			if (authored != null) {
				result.append(' ').append(attribute).append("=\"").append(authored).append('"');
			}
			if (oppositeAuthored != null) {
				result.append(' ').append(oppositeAttribute).append("=\"").append(oppositeAuthored).append('"');
			}
			return result.toString();
		}
	}

	/**
	 * Gives this package the instance data SmartClient is sent for a bean.
	 */
	private static final class Payload extends ViewJSONManipulator {
		Payload(User user, Module module, Document document, View view, Bean bean) {
			super(user, module, document, view, SMART_CLIENT_UXUI, bean, 0, 0, false);
		}

		Map<String, Object> values(AbstractWebContext webContext) throws Exception {
			@SuppressWarnings("unchecked")
			Map<String, Object> result = (Map<String, Object>) JSON.unmarshall(toJSON(webContext, null));
			return result;
		}
	}

	/**
	 * A faces context whose expressions keep their text, which the mock context discards.
	 */
	private static final class ExpressionFacesContext extends MockFacesContext {
		private final ExpressionFactory factory = ExpressionFactory.newInstance();
		private final ELContext elContext = new StandardELContext(factory);
		private final Application application = new MockApplication() {
			@Override
			public ExpressionFactory getExpressionFactory() {
				return factory;
			}
		};

		@Override
		public Application getApplication() {
			return application;
		}

		@Override
		public ELContext getELContext() {
			return elContext;
		}

		static void setCurrent(FacesContext context) {
			setCurrentInstance(context);
		}
	}

	private User user;
	private Customer customer;
	private Module module;
	private Document document;

	@BeforeEach
	void resolveMetaData() {
		user = CORE.getUser();
		customer = CORE.getCustomer();
		module = customer.getModule(ConditionsFixture.MODULE_NAME);
		document = module.getDocument(customer, ConditionsFixture.DOCUMENT_NAME);
	}

	@AfterEach
	@SuppressWarnings("static-method")
	void clearContext() {
		CORE.getStash().remove(CONTEXT_KEY);
		CORE.getUser().getAttributes().remove(CONTEXT_KEY);
	}

	@Test
	@SuppressWarnings("static-method")
	void theFixtureAuthorsEveryConditionAttributeTheSchemaDeclaresByNameAndByExpression() throws Exception {
		Set<String> declared = declaredConditionAttributes();
		Set<String> required = new TreeSet<>();
		for (String attribute : declared) {
			required.add(attribute + "#name");
			required.add(attribute + "#expression");
		}

		Set<String> authored = new TreeSet<>();
		Set<String> undeclared = new TreeSet<>();
		for (String resource : new String[] {EDIT_VIEW, COMPONENT_VIEW}) {
			for (String condition : authoredConditions(resource)) {
				String attribute = condition.substring(0, condition.indexOf('='));
				String value = condition.substring(condition.indexOf('=') + 1);
				if (! declared.contains(attribute)) {
					undeclared.add(attribute);
				}
				if (! isLiteral(value)) {
					authored.add(attribute + (BindUtil.isSkyveExpression(value) ? "#expression" : "#name"));
				}
			}
		}

		Set<String> missing = new TreeSet<>(required);
		missing.removeAll(authored);
		missing.removeAll(NOT_AUTHORED);
		Set<String> stale = new TreeSet<>(NOT_AUTHORED);
		stale.removeIf(attribute -> required.contains(attribute) && (! authored.contains(attribute)));

		assertAll(
				() -> assertFalse(declared.isEmpty(), "No condition attributes were found in view.xsd"),
				() -> assertEquals(Set.of(), missing, "Condition attributes of view.xsd the fixture does not author in that form"),
				() -> assertEquals(Set.of(), stale, "Condition attributes listed as not authored that are authored, or are not in view.xsd"),
				() -> assertEquals(Set.of(), undeclared, "Condition attributes the fixture authors that view.xsd does not declare"));
	}

	@Test
	void theLoadedViewHoldsEveryConditionAsAuthored() throws Exception {
		for (String[] resourceAndViewName : new String[][] {{EDIT_VIEW, ViewType.edit.toString()}, {COMPONENT_VIEW, COMPONENT_VIEW_NAME}}) {
			List<String> authored = new ArrayList<>();
			for (String condition : authoredConditions(resourceAndViewName[0])) {
				authored.add(condition.substring(condition.indexOf('@') + 1));
			}
			Collections.sort(authored);

			for (String uxui : new String[] {SMART_CLIENT_UXUI, PRIME_FACES_UXUI}) {
				List<String> held = new ArrayList<>();
				for (Condition condition : conditions(document.getView(uxui, customer, resourceAndViewName[1]), uxui, false)) {
					if (condition.authored() != null) {
						held.add(condition.attribute() + '=' + condition.authored());
					}
					if (condition.oppositeAuthored() != null) {
						held.add(condition.oppositeAttribute() + '=' + condition.oppositeAuthored());
					}
				}
				Collections.sort(held);
				assertEquals(authored, held, resourceAndViewName[0] + " for " + uxui);
			}
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void theViewsMarshalBackWithEveryConditionAsAuthored() throws Exception {
		for (String resource : new String[] {EDIT_VIEW, COMPONENT_VIEW}) {
			ViewMetaData view = XMLMetaData.unmarshalViewString(read(resource));
			String marshalled = XMLMetaData.marshalView(view, false, false);
			assertEquals(authoredConditions(resource), authoredConditions(parse(marshalled)), resource);
		}
	}

	@Test
	void theViewsAreValid() {
		ProvidedRepository repository = (ProvidedRepository) CORE.getRepository();
		for (String uxui : new String[] {SMART_CLIENT_UXUI, PRIME_FACES_UXUI}) {
			for (String viewName : new String[] {ViewType.edit.toString(), COMPONENT_VIEW_NAME}) {
				View view = document.getView(uxui, customer, viewName);
				// as domain generation validates it
				assertDoesNotThrow(() -> repository.validateViewForGenerateDomain(customer, document, view, uxui), viewName + " for " + uxui);
			}
		}
	}

	@Test
	void everyConditionEvaluatesAsItsFormSays() throws Exception {
		List<Condition> conditions = conditions(document.getView(SMART_CLIENT_UXUI, customer, ViewType.edit.toString()), SMART_CLIENT_UXUI, true);
		assertFalse(conditions.isEmpty());

		List<Executable> assertions = new ArrayList<>();
		for (Scenario scenario : scenarios()) {
			scenario.enter();
			ConditionsFixture bean = scenario.bean();
			for (Condition condition : conditions) {
				String context = scenario.name() + ": " + condition.describe();
				// evaluate now, while the scenario's stash entry and user attribute are in place
				if (condition.authored() != null) {
					boolean expected = expected(condition.authored(), bean);
					boolean actual = bean.evaluateCondition(condition.authored());
					assertions.add(() -> assertEquals(Boolean.valueOf(expected), Boolean.valueOf(actual), context));
				}
				if (condition.oppositeAuthored() != null) {
					boolean expected = expected(condition.oppositeAuthored(), bean);
					boolean actual = bean.evaluateCondition(condition.oppositeAuthored());
					assertions.add(() -> assertEquals(Boolean.valueOf(expected), Boolean.valueOf(actual), context));
				}
				if (condition.oppositeAttribute() != null) {
					// what a renderer is given is the negative sense, whichever sense was authored
					String derived = condition.derived();
					assertNotNull(derived, context);
					if (condition.oppositeAuthored() != null) {
						assertEquals(condition.oppositeAuthored(), derived, context);
					}
					else if (isLiteral(condition.authored())) {
						assertEquals(String.valueOf(! Boolean.parseBoolean(condition.authored())), derived, context);
					}
					else {
						// the same condition in the other sense
						assertEquals(positive(condition.authored()), positive(derived), context);
						assertNotEquals(condition.authored(), derived, context);
					}
					boolean expected = expected(derived, bean);
					boolean actual = bean.evaluateCondition(derived);
					assertions.add(() -> assertEquals(Boolean.valueOf(expected), Boolean.valueOf(actual), context + " as " + derived));
				}
			}
		}
		assertAll(assertions);
	}

	@Test
	void smartClientIsSentTheValueOfEveryConditionItsViewNames() throws Exception {
		View view = document.getView(SMART_CLIENT_UXUI, customer, ViewType.edit.toString());
		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(user, module, document, view, SMART_CLIENT_UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();

		// what the view script calls each condition, against the condition
		Map<String, String> named = new TreeMap<>();
		List<Executable> assertions = new ArrayList<>();
		for (Condition condition : conditions(view, SMART_CLIENT_UXUI, true)) {
			String derived = condition.derived();
			if (isLiteral(derived)) {
				continue;
			}
			String identifier = BindUtil.conditionIdentifier(derived);
			named.put(identifier, derived);
			assertions.add(() -> assertTrue(code.contains('\'' + identifier + '\''),
												condition.describe() + " is not in the view script as " + identifier));
		}
		Matcher matcher = Pattern.compile("Condition:'([^']+)'").matcher(code);
		while (matcher.find()) {
			String name = matcher.group(1);
			if (! isLiteral(name)) {
				// a condition the renderer adds for itself is known by its own name
				named.putIfAbsent(name, name);
			}
		}
		for (String expression : EXPECTED.keySet()) {
			if (BindUtil.isSkyveExpression(expression)) {
				assertions.add(() -> assertFalse(code.contains(expression), expression + " is in the view script"));
			}
		}
		// a widget that writes a statement where it should write the properties of a form item breaks the whole script;
		// a statement can follow a property, or open an object literal, but not open a function or block body
		Matcher statementInAnObject = Pattern.compile(",\\s*var |[\\[(:]\\s*\\{\\s*var ").matcher(code);
		assertions.add(() -> assertFalse(statementInAnObject.find(), "A statement inside an object in the view script"));

		for (Scenario scenario : scenarios()) {
			scenario.enter();
			ConditionsFixture bean = scenario.bean();
			Map<String, Object> values = payload(view, bean);
			for (Map.Entry<String, String> entry : named.entrySet()) {
				Boolean expected = Boolean.valueOf(expected(entry.getValue(), bean));
				Object actual = values.get(entry.getKey());
				assertions.add(() -> assertEquals(expected, actual, scenario.name() + ": " + entry.getValue() + " sent as " + entry.getKey()));
			}
		}
		assertAll(assertions);
	}

	@Test
	void primeFacesComponentsEvaluateEveryCondition() throws Exception {
		View view = document.getView(PRIME_FACES_UXUI, customer, ViewType.edit.toString());

		try (ExpressionFacesContext context = new ExpressionFacesContext()) {
			ExpressionFacesContext.setCurrent(context);
			// the conditions the component tree refers to, in the sense it refers to them
			Set<String> referenced = conditionsReferencedByPrimeFaces(view, new TreeSet<>());

			List<Executable> assertions = new ArrayList<>();
			Set<String> referencedInEitherSense = new TreeSet<>();
			for (String condition : referenced) {
				referencedInEitherSense.add(positive(condition));
			}
			for (Condition condition : conditions(view, PRIME_FACES_UXUI, true)) {
				String derived = condition.derived();
				if (! isLiteral(derived)) {
					assertions.add(() -> assertTrue(referencedInEitherSense.contains(positive(derived)),
														condition.describe() + " is not in an expression of any component"));
				}
			}

			for (Scenario scenario : scenarios()) {
				scenario.enter();
				ConditionsFixture bean = scenario.bean();
				BeanMapAdapter adapter = new BeanMapAdapter(bean, mockWebContext());
				for (String condition : referenced) {
					Boolean expected = Boolean.valueOf(expected(condition, bean));
					// as the expression does it: an inline expression through the adapter's method, a name as a map key
					Object actual = (BindUtil.isSkyveExpression(condition) || BindUtil.isNegatedSkyveExpression(condition)) ?
										Boolean.valueOf(adapter.evaluateCondition(condition)) :
										adapter.get(condition);
					assertions.add(() -> assertEquals(expected, actual, scenario.name() + ": " + condition));
				}
			}
			assertAll(assertions);
		}
		finally {
			ExpressionFacesContext.setCurrent(null);
		}
	}

	@Test
	void everyConditionAttributeReachesSmartClient() throws Exception {
		List<Position> positions = new ArrayList<>();
		View view = positionedView(SMART_CLIENT_UXUI, positions);
		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(user, module, document, view, SMART_CLIENT_UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();

		Map<String, Object> shownAndAllowed = payload(view, bean(Boolean.TRUE, Boolean.TRUE, null, null, null));
		Map<String, Object> shownNotAllowed = payload(view, bean(Boolean.TRUE, Boolean.FALSE, null, null, null));

		List<Executable> assertions = new ArrayList<>();
		Set<String> staleExceptions = new TreeSet<>();
		for (Position position : positions) {
			String name = BindUtil.conditionIdentifier(position.derived());
			boolean inScript = code.contains('\'' + name + '\'');
			if (SMART_CLIENT_SCRIPT_DOES_NOT_NAME.contains(position.attribute())) {
				if (inScript) {
					staleExceptions.add(position.attribute());
				}
			}
			else if (SMART_CLIENT_DATA_DOES_NOT_CARRY.contains(position.attribute())) {
				assertions.add(() -> assertTrue(inScript, position.attribute() + " is not in the view script"));
				if (shownAndAllowed.containsKey(name)) {
					staleExceptions.add(position.attribute());
				}
			}
			else {
				assertions.add(() -> assertTrue(inScript, position.attribute() + " is not in the view script"));
				// everything is shown, so nothing is left out of the data for being hidden
				assertions.add(() -> assertEquals(Boolean.valueOf(position.derivedWhen(true, true)), shownAndAllowed.get(name),
													position.attribute() + " in the data of a bean that is shown and allowed"));
				assertions.add(() -> assertEquals(Boolean.valueOf(position.derivedWhen(true, false)), shownNotAllowed.get(name),
													position.attribute() + " in the data of a bean that is shown and not allowed"));
			}
		}
		assertions.add(() -> assertEquals(Set.of(), staleExceptions, "Attributes listed as not named by the view script, or not in its data, that are"));
		assertAll(assertions);
	}

	@Test
	void everyConditionAttributeReachesPrimeFaces() throws Exception {
		List<Position> positions = new ArrayList<>();
		View view = positionedView(PRIME_FACES_UXUI, positions);

		try (ExpressionFacesContext context = new ExpressionFacesContext()) {
			ExpressionFacesContext.setCurrent(context);
			Set<String> referencedOfARow = new TreeSet<>();
			Set<String> referenced = conditionsReferencedByPrimeFaces(view, referencedOfARow);

			List<Executable> assertions = new ArrayList<>();
			Set<String> staleExceptions = new TreeSet<>();
			Set<String> evaluatedAgainstARow = new TreeSet<>();
			for (Position position : positions) {
				if (referencedOfARow.contains(position.expression()) || referencedOfARow.contains("not" + position.expression())) {
					evaluatedAgainstARow.add(position.attribute());
				}
				boolean inComponents = referenced.contains(position.expression()) || referenced.contains("not" + position.expression());
				if (PRIME_FACES_COMPONENTS_DO_NOT_REFER_TO.contains(position.attribute())) {
					if (inComponents) {
						staleExceptions.add(position.attribute());
					}
				}
				else {
					assertions.add(() -> assertTrue(inComponents, position.attribute() + " is not in an expression of any component"));
				}
			}
			assertions.add(() -> assertEquals(Set.of(), staleExceptions, "Attributes listed as not referred to by components that are"));
			// a condition in a data widget is evaluated against the view's bean, as it is validated
			assertions.add(() -> assertEquals(Set.of(), evaluatedAgainstARow, "Attributes evaluated against the row of a data widget"));
			assertAll(assertions);
		}
		finally {
			ExpressionFacesContext.setCurrent(null);
		}
	}

	@Test
	void primeFacesShowsDeleteOnlyForAPersistedBeanAsSmartClientDoes() throws Exception {
		List<Position> positions = new ArrayList<>();
		View view = positionedView(PRIME_FACES_UXUI, positions);

		try (ExpressionFacesContext context = new ExpressionFacesContext()) {
			ExpressionFacesContext.setCurrent(context);
			List<String> expressions = new ArrayList<>();
			for (UIComponent root : renderForPrimeFaces(view)) {
				collectRenderedExpressions(root, expressions);
			}

			List<Executable> assertions = new ArrayList<>();
			for (Position position : positions) {
				String attribute = position.attribute();
				boolean delete = attribute.startsWith("actions/delete@");
				if ((delete || attribute.startsWith("actions/save@")) && (attribute.endsWith("visible") || attribute.endsWith("Visible"))) {
					// the button's rendered expression is the one that refers to the attribute's own expression
					List<String> buttons = expressions.stream().filter(expression -> expression.contains(position.expression())).toList();
					assertions.add(() -> assertFalse(buttons.isEmpty(), attribute + " is not in a rendered expression"));
					for (String button : buttons) {
						assertions.add(() -> assertEquals(Boolean.valueOf(delete), Boolean.valueOf(button.contains(VIEW_BEAN + "['persisted']")),
															attribute + (delete ? " is not hidden until persisted: " : " is hidden until persisted: ") + button));
					}
				}
			}
			assertAll(assertions);
		}
		finally {
			ExpressionFacesContext.setCurrent(null);
		}
	}

	/**
	 * A condition attribute of the fixture's edit view, given an expression that no other attribute has.
	 *
	 * @param attribute	The attribute, as {@code context/element@attribute}.
	 * @param expression	Its expression, which is true when what it depends on has the value that makes the attribute apply:
	 * 						a positive attribute's when show (for visibility) or allow (for anything else) is set,
	 * 						and a negative attribute's when it is not.
	 * @param derived	The condition a renderer is given for it, which is in the negative sense where the attribute has an opposite.
	 */
	private record Position(String attribute, String expression, String derived) {
		/**
		 * Says what the derived condition should be for a bean.
		 */
		boolean derivedWhen(boolean show, boolean allow) {
			boolean visibility = attribute.endsWith("@visible") || attribute.endsWith("@invisible");
			boolean on = visibility ? show : allow;
			// a condition with no opposite is true when on; the negative sense of any other is true when off
			return (attribute.endsWith("@postRefresh") || attribute.endsWith("@refreshIf")) ? on : (! on);
		}
	}

	/**
	 * Returns the fixture's edit view with every condition attribute replaced by an expression of its own,
	 * so that what a renderer does with each attribute can be told apart from what it does with every other.
	 * With show and allow both set, everything in the view is shown and enabled.
	 *
	 * @param positions	Receives the attributes, in document order.
	 */
	private View positionedView(String uxui, List<Position> positions) throws Exception {
		org.w3c.dom.Document xml = parse(read(EDIT_VIEW));
		NodeList elements = xml.getElementsByTagNameNS("*", "*");
		for (int i = 0, l = elements.getLength(); i < l; i++) {
			Element element = (Element) elements.item(i);
			NamedNodeMap attributes = element.getAttributes();
			for (int j = 0, m = attributes.getLength(); j < m; j++) {
				Node attribute = attributes.item(j);
				String name = attribute.getNodeName();
				if (CONDITION_ATTRIBUTE.matcher(name).matches()) {
					boolean visibility = "visible".equals(name) || "invisible".equals(name);
					boolean negative = "invisible".equals(name) || name.startsWith("disable");
					boolean hasOpposite = (! "postRefresh".equals(name)) && (! "refreshIf".equals(name));
					int number = positions.size();
					String expression = "{el:" + (negative ? "not " : "") + "bean." + (visibility ? "show" : "allow") + " and " + number + " eq " + number + '}';
					attribute.setNodeValue(expression);
					positions.add(new Position(context(element.getParentNode()) + element.getLocalName() + '@' + name,
												expression,
												(hasOpposite && (! negative)) ? "not" + expression : expression));
				}
			}
		}

		StringWriter positioned = new StringWriter();
		TransformerFactory.newInstance().newTransformer().transform(new DOMSource(xml), new StreamResult(positioned));
		ViewImpl result = XMLMetaData.unmarshalViewString(positioned.toString()).convert("the positioned conditions fixture view");
		result.resolve(uxui, customer, module, document, false);
		return result;
	}

	private Map<String, Object> payload(View view, ConditionsFixture bean) throws Exception {
		Payload payload = new Payload(user, module, document, view, bean);
		payload.visit();
		AbstractWebContext webContext = mockWebContext();
		webContext.setCurrentBean(bean);
		return payload.values(webContext);
	}

	@Test
	void viewMetadataNamesEveryInlineConditionByItsIdentifierWithItsExpression() throws Exception {
		Method viewMethod = Class.forName("org.skyve.impl.web.service.MetaDataServlet")
									.getDeclaredMethod("view", User.class, String.class, String.class, String.class, boolean.class);
		viewMethod.setAccessible(true);

		for (String uxui : new String[] {SMART_CLIENT_UXUI, PRIME_FACES_UXUI}) {
			String json = viewMethod.invoke(null, user, uxui, ConditionsFixture.MODULE_NAME, ConditionsFixture.DOCUMENT_NAME, Boolean.FALSE).toString();

			Set<String> expressions = new TreeSet<>();
			Set<String> names = new TreeSet<>();
			List<Executable> assertions = new ArrayList<>();
			collectMetadataConditions(new ObjectMapper().readTree(json), expressions, names, assertions);

			Set<String> authoredExpressions = new TreeSet<>();
			Set<String> authoredNames = new TreeSet<>();
			for (Condition condition : conditions(document.getView(uxui, customer, ViewType.edit.toString()), uxui, true)) {
				String derived = condition.derived();
				if (BindUtil.isSkyveExpression(derived)) {
					authoredExpressions.add(derived);
				}
				else if (BindUtil.isNegatedSkyveExpression(derived)) {
					authoredExpressions.add(derived.substring(3));
				}
				else {
					authoredNames.add(derived);
				}
			}
			assertions.add(() -> assertEquals(authoredExpressions, expressions, "Inline condition expressions in the view metadata for " + uxui));
			assertions.add(() -> assertEquals(authoredNames, names, "Condition names in the view metadata for " + uxui));
			assertAll(assertions);
		}
	}

	/**
	 * Collects every condition member of some view metadata, depth first.
	 * A member that has an expression beside it must be named by the identifier of that expression, in either sense.
	 */
	private static void collectMetadataConditions(JsonNode node, Set<String> expressions, Set<String> names, List<Executable> assertions) {
		if (node.isObject()) {
			for (Map.Entry<String, JsonNode> member : node.properties()) {
				String memberName = member.getKey();
				JsonNode value = member.getValue();
				if (memberName.endsWith("Condition") && value.isTextual()) {
					String name = value.textValue();
					JsonNode expressionMember = node.get(memberName + "Expression");
					if (expressionMember == null) {
						names.add(name);
					}
					else {
						String expression = expressionMember.textValue();
						String identifier = BindUtil.conditionIdentifier(expression);
						expressions.add(expression);
						assertions.add(() -> assertTrue(name.equals(identifier) || name.equals("not" + identifier),
															memberName + " of " + expression + " is " + name));
					}
				}
			}
		}
		for (JsonNode child : node) {
			collectMetadataConditions(child, expressions, names, assertions);
		}
	}

	/** The expression for the view's bean in a PrimeFaces component expression. */
	private static final String VIEW_BEAN = "skyve.currentBean";
	private static final Pattern NAMED_CONDITION = Pattern.compile("([\\w.]+)\\['([^']+)'\\]");
	private static final Pattern INLINE_CONDITION = Pattern.compile("([\\w.]+)\\.evaluateCondition\\('((?:[^'\\\\]|\\\\.)*)'\\)");

	/**
	 * The component properties that PrimeFaces rendering puts a condition in.
	 * A component does not say which of its properties have expressions, so one that is not listed here goes unseen.
	 */
	private static final String[] CONDITIONAL_PROPERTIES = {"rendered", "disabled", "readonly", "styleClass", "onclick", "onShow", "selectionMode", "value"};

	/**
	 * Renders a view for PrimeFaces and collects the conditions its components refer to.
	 * A faces context that keeps expression text must be current.
	 *
	 * @param ofARow	Receives the conditions that are evaluated against something other than the view's bean,
	 * 					which is the row of a data widget.
	 * @return	The conditions that are evaluated against the view's bean.
	 */
	private Set<String> conditionsReferencedByPrimeFaces(View view, Set<String> ofARow) {
		Set<String> result = new TreeSet<>();
		for (UIComponent root : renderForPrimeFaces(view)) {
			collectReferencedConditions(root, result, ofARow);
		}
		return result;
	}

	/**
	 * Renders a view for PrimeFaces and returns its component trees: the view and the sidebar.
	 * A faces context that keeps expression text must be current.
	 */
	private List<UIComponent> renderForPrimeFaces(View view) {
		ResponsiveComponentBuilder componentBuilder = new ResponsiveComponentBuilder();
		ResponsiveLayoutBuilder layoutBuilder = new ResponsiveLayoutBuilder();
		FacesView managedBean = new FacesView();
		componentBuilder.setUserAgentType(UserAgentType.desktop);
		layoutBuilder.setUserAgentType(UserAgentType.desktop);
		componentBuilder.setSAILManagedBean(managedBean);
		layoutBuilder.setSAILManagedBean(managedBean);
		FacesViewRenderer renderer = new FacesViewRenderer(user, module, document, view, PRIME_FACES_UXUI, null, componentBuilder, layoutBuilder);
		renderer.visit();

		UIComponent sidebar = renderer.getSidebar();
		assertNotNull(sidebar);
		return List.of(renderer.getFacesView(), sidebar);
	}

	private static void collectRenderedExpressions(UIComponent component, List<String> expressions) {
		ValueExpression rendered = component.getValueExpression("rendered");
		if (rendered != null) {
			expressions.add(rendered.getExpressionString());
		}
		Iterator<UIComponent> children = component.getFacetsAndChildren();
		while (children.hasNext()) {
			collectRenderedExpressions(children.next(), expressions);
		}
	}

	/**
	 * Collects the conditions that the expressions of a component tree refer to.
	 * A name that is not a condition of the fixture document is the binding of a value, and is left out.
	 */
	private void collectReferencedConditions(UIComponent component, Set<String> ofTheViewBean, Set<String> ofARow) {
		for (String property : CONDITIONAL_PROPERTIES) {
			ValueExpression expression = component.getValueExpression(property);
			if (expression != null) {
				String text = expression.getExpressionString();
				Matcher matcher = NAMED_CONDITION.matcher(text);
				while (matcher.find()) {
					if (VIEW_BEAN.equals(matcher.group(1)) && isConditionName(matcher.group(2))) {
						ofTheViewBean.add(matcher.group(2));
					}
				}
				matcher = INLINE_CONDITION.matcher(text);
				while (matcher.find()) {
					// undo the escaping of an EL string literal
					String condition = matcher.group(2).replace("\\'", "'").replace("\\\\", "\\");
					(VIEW_BEAN.equals(matcher.group(1)) ? ofTheViewBean : ofARow).add(condition);
				}
			}
		}
		Iterator<UIComponent> children = component.getFacetsAndChildren();
		while (children.hasNext()) {
			collectReferencedConditions(children.next(), ofTheViewBean, ofARow);
		}
	}

	/**
	 * Returns beans that between them make every condition in {@link #EXPECTED} both true and false.
	 */
	private static List<Scenario> scenarios() {
		List<Scenario> result = new ArrayList<>();

		ConditionsFixture bean = bean(Boolean.TRUE, Boolean.TRUE, "x", Integer.valueOf(6), Choice.one);
		bean.setNested(bean(Boolean.TRUE, null, "mapped", null, null));
		result.add(new Scenario("everything set", bean, Boolean.TRUE, Boolean.TRUE));

		bean = bean(Boolean.FALSE, Boolean.FALSE, "mapped", Integer.valueOf(5), Choice.two);
		bean.setNested(bean(Boolean.FALSE, null, "x", null, null));
		bean.addDataGrid1Element(ConditionsFixtureRow.newInstance());
		result.add(new Scenario("everything cleared", bean, Boolean.FALSE, Boolean.FALSE));

		result.add(new Scenario("nothing has a value", bean(null, null, null, null, null), null, null));

		bean = bean(Boolean.TRUE, Boolean.FALSE, null, null, null);
		bean.setNested(bean(null, null, null, null, null));
		// text is not a Boolean, so it is not true
		result.add(new Scenario("shown but not allowed", bean, "true", "true"));

		result.add(new Scenario("allowed but not shown", bean(Boolean.FALSE, Boolean.TRUE, null, null, null), null, null));

		return result;
	}

	private static ConditionsFixture bean(Boolean show, Boolean allow, String text, Integer number, Choice choice) {
		ConditionsFixture result = ConditionsFixture.newInstance();
		result.setShow(show);
		result.setAllow(allow);
		result.setText(text);
		result.setNumber(number);
		result.setChoice(choice);
		return result;
	}

	/**
	 * Says whether a condition should be true for a bean, from {@link #EXPECTED}.
	 *
	 * @param condition	A condition as authored, or as derived for the opposite sense.
	 */
	private static boolean expected(String condition, ConditionsFixture bean) {
		if (isLiteral(condition)) {
			return Boolean.parseBoolean(condition);
		}
		Predicate<ConditionsFixture> expectation = EXPECTED.get(condition);
		if (expectation != null) {
			return expectation.test(bean);
		}
		String positive = positive(condition);
		if (positive.equals(condition)) {
			throw new AssertionFailedError("No expectation for condition " + condition);
		}
		return (! expected(positive, bean));
	}

	/**
	 * Says whether a name, in either sense, is a condition the fixture document declares or every document has.
	 */
	private boolean isConditionName(String name) {
		String positive = positive(name);
		return document.getConditionNames().contains(positive) ||
				Bean.PERSISTED_KEY.equals(positive) || Bean.CREATED_KEY.equals(positive) || Bean.CHANGED_KEY.equals(positive);
	}

	private static boolean isLiteral(String condition) {
		return "true".equals(condition) || "false".equals(condition);
	}

	/**
	 * Returns the condition that a negated condition negates, or the condition itself where it is not negated.
	 */
	private static String positive(String condition) {
		if (BindUtil.isNegatedSkyveExpression(condition)) {
			return condition.substring(3);
		}
		if ((! BindUtil.isSkyveExpression(condition)) && condition.startsWith("not") &&
				(condition.length() > 3) && Character.isUpperCase(condition.charAt(3))) {
			return Introspector.decapitalize(condition.substring(3));
		}
		return condition;
	}

	private static final Pattern AUTHORED_POSITIVE_ACCESSOR = Pattern.compile("getXml(Visible|Enabled|Enable[A-Z]\\w*)Condition");

	/**
	 * Returns every condition of a view's metadata, found by reflection so that no kind of metadata is missed.
	 *
	 * @param expandComponents	Whether to include what each component expands to, as a renderer would visit it.
	 */
	private List<Condition> conditions(View view, String uxui, boolean expandComponents) throws Exception {
		List<Condition> result = new ArrayList<>();
		collectConditions(view, uxui, expandComponents, Collections.newSetFromMap(new IdentityHashMap<>()), result);
		return result;
	}

	private void collectConditions(Object metaData, String uxui, boolean expandComponents, Set<Object> visited, List<Condition> result)
	throws Exception {
		if (metaData instanceof Collection<?> collection) {
			for (Object element : collection) {
				collectConditions(element, uxui, expandComponents, visited, result);
			}
			return;
		}
		if (metaData instanceof Map<?, ?> map) {
			collectConditions(map.values(), uxui, expandComponents, visited, result);
			return;
		}
		if ((metaData == null) || (! isViewMetaData(metaData.getClass())) || (! visited.add(metaData))) {
			return;
		}

		Class<?> type = metaData.getClass();
		for (Method method : type.getMethods()) {
			if ((method.getParameterCount() != 0) || (! method.getName().startsWith("get")) || (method.getDeclaringClass() == Object.class)) {
				continue;
			}
			String name = method.getName();
			Matcher matcher = AUTHORED_POSITIVE_ACCESSOR.matcher(name);
			if (matcher.matches()) {
				String positive = matcher.group(1);
				String negative = "Visible".equals(positive) ? "Invisible" : ("Enabled".equals(positive) ? "Disabled" : "Disable" + positive.substring(6));
				String authored = (String) get(metaData, method);
				String oppositeAuthored = (String) get(metaData, type.getMethod("getXml" + negative + "Condition"));
				String derived = (String) get(metaData, type.getMethod("get" + negative + "Condition"));
				if ((authored != null) || (oppositeAuthored != null) || (derived != null)) {
					result.add(new Condition(metaData, Introspector.decapitalize(positive), authored, Introspector.decapitalize(negative), oppositeAuthored, derived));
				}
			}
			else if ("getPostRefreshCondition".equals(name) || "getRefreshIfCondition".equals(name)) {
				String authored = (String) get(metaData, method);
				if (authored != null) {
					result.add(new Condition(metaData, Introspector.decapitalize(name.substring(3, name.length() - 9)), authored, null, null, authored));
				}
			}
			else if (mayHoldViewMetaData(method.getReturnType())) {
				collectConditions(get(metaData, method), uxui, expandComponents, visited, result);
			}
		}

		if (expandComponents && (metaData instanceof Component component)) {
			collectConditions(component.getFragment((CustomerImpl) customer, uxui), uxui, true, visited, result);
		}
	}

	/**
	 * Says whether an accessor of this type could return view metadata, or a collection or map of it.
	 * The declared type is often an interface from outside the view packages, so only what is returned can say for sure.
	 */
	private static boolean mayHoldViewMetaData(Class<?> type) {
		return Collection.class.isAssignableFrom(type) || Map.class.isAssignableFrom(type) ||
				((! type.isEnum()) && type.getName().startsWith("org.skyve."));
	}

	private static boolean isViewMetaData(Class<?> type) {
		Package typePackage = type.getPackage();
		return (typePackage != null) && typePackage.getName().startsWith("org.skyve.") && typePackage.getName().contains(".view") && (! type.isEnum());
	}

	private static Object get(Object metaData, Method accessor) throws IllegalAccessException {
		try {
			return accessor.invoke(metaData);
		}
		catch (InvocationTargetException e) {
			return fail(accessor + " failed on " + metaData, e.getCause());
		}
	}

	private static final Pattern CONDITION_ATTRIBUTE = Pattern.compile("visible|invisible|enabled|disabled|postRefresh|refreshIf|(enable|disable)[A-Z]\\w*");

	/**
	 * Returns every condition attribute authored in a view on the classpath, as {@code context/element@attribute=value},
	 * in document order. See {@link #context(Node)}.
	 */
	private static List<String> authoredConditions(String resource) throws Exception {
		return authoredConditions(parse(read(resource)));
	}

	/**
	 * Returns where an element is, for what a renderer makes of it: the element that contains it and a slash,
	 * except that every element that lays out widgets one after another is {@code container}. The view itself is nowhere.
	 */
	private static String context(Node parent) {
		if (! (parent instanceof Element)) {
			return "";
		}
		return context(parent.getLocalName());
	}

	private static String context(String parentName) {
		return switch (parentName) {
			case "view", "vbox", "hbox", "tab", "sidebar" -> "container/";
			default -> parentName + '/';
		};
	}

	private static List<String> authoredConditions(org.w3c.dom.Document xml) {
		List<String> result = new ArrayList<>();
		NodeList elements = xml.getElementsByTagNameNS("*", "*");
		for (int i = 0, l = elements.getLength(); i < l; i++) {
			Element element = (Element) elements.item(i);
			NamedNodeMap attributes = element.getAttributes();
			// attribute order is not significant in XML
			Set<String> conditions = new TreeSet<>();
			for (int j = 0, m = attributes.getLength(); j < m; j++) {
				Node attribute = attributes.item(j);
				if (CONDITION_ATTRIBUTE.matcher(attribute.getNodeName()).matches()) {
					conditions.add(context(element.getParentNode()) + element.getLocalName() + '@' + attribute.getNodeName() + '=' + attribute.getNodeValue());
				}
			}
			result.addAll(conditions);
		}
		return result;
	}

	private static final String XSD = "http://www.w3.org/2001/XMLSchema";

	/**
	 * Returns every condition attribute view.xsd declares, as {@code context/element@attribute}, for every context
	 * the schema allows the element in. See {@link #context(Node)}.
	 */
	private static Set<String> declaredConditionAttributes() throws Exception {
		Element schema = parse(read("schemas/view.xsd")).getDocumentElement();
		Map<String, Element> types = new TreeMap<>();
		Map<String, Element> globalElements = new TreeMap<>();
		for (Node child = schema.getFirstChild(); child != null; child = child.getNextSibling()) {
			if (child instanceof Element declaration) {
				if ("complexType".equals(declaration.getLocalName())) {
					types.put(declaration.getAttribute("name"), declaration);
				}
				else if ("element".equals(declaration.getLocalName())) {
					globalElements.put(declaration.getAttribute("name"), declaration);
				}
			}
		}

		Set<String> result = new TreeSet<>();
		// the view is in no context
		for (String attribute : declaredConditionAttributes(globalElements.get("view"), types)) {
			result.add("view@" + attribute);
		}
		// every element declaration, global or local, is a possible parent
		NodeList parents = schema.getElementsByTagNameNS(XSD, "element");
		for (int i = 0, l = parents.getLength(); i < l; i++) {
			Element parent = (Element) parents.item(i);
			Element parentType = type(parent, types);
			if ((! parent.hasAttribute("name")) || (parentType == null)) {
				continue;
			}
			List<Element> children = new ArrayList<>();
			collectChildElements(parentType, types, children);
			for (Element child : children) {
				String name = child.hasAttribute("ref") ? localName(child.getAttribute("ref")) : child.getAttribute("name");
				Element declaration = child.hasAttribute("ref") ? globalElements.get(name) : child;
				for (String attribute : declaredConditionAttributes(declaration, types)) {
					result.add(context(parent.getAttribute("name")) + name + '@' + attribute);
				}
			}
		}
		return result;
	}

	private static Set<String> declaredConditionAttributes(Element elementDeclaration, Map<String, Element> types) {
		Set<String> result = new TreeSet<>();
		Element type = (elementDeclaration == null) ? null : type(elementDeclaration, types);
		if (type != null) {
			collectDeclaredConditionAttributes(type, types, result);
		}
		return result;
	}

	/**
	 * Returns the type of a schema element declaration, which is named or is declared within it.
	 */
	private static Element type(Element elementDeclaration, Map<String, Element> types) {
		Element result = types.get(localName(elementDeclaration.getAttribute("type")));
		if (result == null) {
			for (Node child = elementDeclaration.getFirstChild(); child != null; child = child.getNextSibling()) {
				if ((child instanceof Element element) && "complexType".equals(element.getLocalName())) {
					result = element;
				}
			}
		}
		return result;
	}

	/**
	 * Collects the element declarations and references directly within a schema type and the types it extends.
	 */
	private static void collectChildElements(Element node, Map<String, Element> types, List<Element> children) {
		for (Node child = node.getFirstChild(); child != null; child = child.getNextSibling()) {
			if (child instanceof Element element) {
				String localName = element.getLocalName();
				if ("element".equals(localName)) {
					children.add(element);
					continue;
				}
				if ("extension".equals(localName)) {
					Element base = types.get(localName(element.getAttribute("base")));
					if (base != null) {
						collectChildElements(base, types, children);
					}
				}
				collectChildElements(element, types, children);
			}
		}
	}

	/**
	 * Collects the condition attributes of a schema type and of the types it extends, but not of the elements it contains.
	 */
	private static void collectDeclaredConditionAttributes(Element node, Map<String, Element> types, Set<String> attributes) {
		for (Node child = node.getFirstChild(); child != null; child = child.getNextSibling()) {
			if (child instanceof Element element) {
				String localName = element.getLocalName();
				if ("element".equals(localName)) {
					continue;
				}
				if ("attribute".equals(localName) && CONDITION_ATTRIBUTE.matcher(element.getAttribute("name")).matches()) {
					attributes.add(element.getAttribute("name"));
				}
				else if ("extension".equals(localName)) {
					Element base = types.get(localName(element.getAttribute("base")));
					if (base != null) {
						collectDeclaredConditionAttributes(base, types, attributes);
					}
				}
				collectDeclaredConditionAttributes(element, types, attributes);
			}
		}
	}

	private static String localName(String qualifiedName) {
		return qualifiedName.substring(qualifiedName.indexOf(':') + 1);
	}

	private static String read(String resource) throws Exception {
		try (InputStream in = Thread.currentThread().getContextClassLoader().getResourceAsStream(resource)) {
			assertNotNull(in, resource + " is not on the classpath");
			return new String(in.readAllBytes(), StandardCharsets.UTF_8);
		}
	}

	private static org.w3c.dom.Document parse(String xml) throws Exception {
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setNamespaceAware(true);
		factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
		return factory.newDocumentBuilder().parse(new java.io.ByteArrayInputStream(xml.getBytes(StandardCharsets.UTF_8)));
	}
}
