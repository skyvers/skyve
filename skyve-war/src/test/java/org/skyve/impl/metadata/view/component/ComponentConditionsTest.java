package org.skyve.impl.metadata.view.component;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.ExpressionEvaluator;

import modules.test.AbstractSkyveTest;
import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.AllAttributesPersistent.Enum3;
import modules.test.domain.DeleteDuringPostDelete;

/**
 * Conditions in component views, against real documents and generated classes.
 *
 * <p>DeleteDuringPostDelete stands in for a containing document: it has an association,
 * {@code aggregatedAssociation}, to AllAttributesPersistent, which stands in for the component's
 * document and declares the {@code Enum3} enumeration that DeleteDuringPostDelete does not.
 */
class ComponentConditionsTest extends AbstractSkyveTest {
	private static final String BINDING = DeleteDuringPostDelete.aggregatedAssociationPropertyName;

	private Document containing;
	private DeleteDuringPostDelete bean;

	@BeforeEach
	void setUpBean() {
		containing = m.getDocument(c, DeleteDuringPostDelete.DOCUMENT_NAME);
		bean = DeleteDuringPostDelete.newInstance();
		AllAttributesPersistent associated = AllAttributesPersistent.newInstance();
		associated.setEnum3(Enum3.two);
		associated.setBooleanFlag(Boolean.TRUE);
		bean.setAggregatedAssociation(associated);
	}

	/**
	 * Rewrites a spacer with the given visible condition as a component bound to the association would.
	 */
	private Spacer inComponent(String visibleCondition, ComponentTranslation... conditions) {
		ViewImpl view = new ViewImpl();
		Spacer result = new Spacer();
		result.setXmlVisibleCondition(visibleCondition);
		view.getContained().add(result);
		new ComponentViewVisitor((CustomerImpl) c,
									(ModuleImpl) m,
									(DocumentImpl) aapd,
									view,
									"desktop",
									BINDING,
									Collections.emptyList(),
									List.of(conditions),
									null).visit();
		return result;
	}

	private static ComponentTranslation mapping(String fromComponent, String mappedTo) {
		ComponentTranslation result = new ComponentTranslation();
		result.setFromComponent(fromComponent);
		result.setMappedTo(mappedTo);
		return result;
	}

	private String validate(String condition, Document document) {
		return ExpressionEvaluator.validate(condition, Boolean.class, c, m, document);
	}

	private void assertVisibleInContainingView(Spacer spacer, boolean visible) {
		String condition = spacer.getXmlVisibleCondition();
		assertNotNull(condition);
		assertNull(validate(condition, containing), condition);
		assertEquals(Boolean.valueOf(visible), Boolean.valueOf(BindUtil.evaluateCondition(bean, condition)), condition);
		// the derived condition renderers read is the strict complement
		String invisibleCondition = spacer.getInvisibleCondition();
		assertNotNull(invisibleCondition);
		assertEquals(Boolean.valueOf(! visible), Boolean.valueOf(BindUtil.evaluateCondition(bean, invisibleCondition)));
	}

	// ---- enumerations of related documents ----

	@Test
	void anEnumerationOfARelatedDocumentValidatesAndEvaluatesFromTheContainingDocument() {
		String condition = "{el:bean.aggregatedAssociation.enum3 eq Enum3.two}";
		assertNull(validate(condition, containing));
		assertTrue(BindUtil.evaluateCondition(bean, condition));
		assertFalse(BindUtil.evaluateCondition(bean, "{el:bean.aggregatedAssociation.enum3 eq Enum3.one}"));
		assertTrue(BindUtil.evaluateCondition(bean, "{rtel:bean.aggregatedAssociation.enum3 ne Enum3.three}"));
		assertEquals(Enum3.three, ExpressionEvaluator.evaluate("{el:Enum3.three}", bean));
	}

	@Test
	void anUnknownValueOfARelatedEnumerationIsAValidationError() {
		assertNotNull(validate("{el:bean.aggregatedAssociation.enum3 eq Enum3.four}", containing));
	}

	@Test
	void anEnumerationThatNoRelatedDocumentDeclaresIsStillAValidationError() {
		String error = validate("{el:bean.aggregatedAssociation.enum3 eq NoSuchEnum.two}", containing);
		assertNotNull(error);
		assertTrue(error.contains("NoSuchEnum"), error);
	}

	@Test
	void theDocumentsOwnEnumerationStillResolvesAsBefore() {
		assertNull(validate("{el:bean.enum3 eq Enum3.two}", aapd));
		assertTrue(BindUtil.evaluateCondition(bean.getAggregatedAssociation(), "{el:bean.enum3 eq Enum3.two}"));
	}

	@Test
	void anEnumerationOfARelatedAdminDocumentValidates() {
		Module admin = c.getModule("admin");
		Document configuration = admin.getDocument(c, "Configuration");
		Document startup = admin.getDocument(c, "Startup");
		assertNull(ExpressionEvaluator.validate("{el:bean.mapType eq MapType.gmap}", Boolean.class, c, admin, startup));
		assertNull(ExpressionEvaluator.validate("{el:bean.startup.mapType eq MapType.gmap}", Boolean.class, c, admin, configuration));
		assertNotNull(ExpressionEvaluator.validate("{el:bean.startup.mapType eq MapType.noSuchMap}", Boolean.class, c, admin, configuration));
	}

	// ---- inline conditions in a component with a binding ----

	@Test
	void anInlineConditionFollowsTheComponentBinding() {
		String authored = "{el:bean.enum3 eq Enum3.two}";
		assertNull(validate(authored, aapd));
		Spacer spacer = inComponent(authored);
		assertEquals("{el:bean.aggregatedAssociation.enum3 eq Enum3.two}", spacer.getXmlVisibleCondition());
		assertVisibleInContainingView(spacer, true);
	}

	@Test
	void anInlineConditionUsingBracketNotationFollowsTheComponentBinding() {
		Spacer spacer = inComponent("{el:bean['enum3'] eq Enum3.one}");
		assertEquals("{el:bean.aggregatedAssociation['enum3'] eq Enum3.one}", spacer.getXmlVisibleCondition());
		assertVisibleInContainingView(spacer, false);
	}

	@Test
	void anInlineConditionOnTheBeanItselfFollowsTheComponentBinding() {
		Spacer spacer = inComponent("{el:not empty bean and bean.booleanFlag}");
		assertEquals("{el:not empty bean.aggregatedAssociation and bean.aggregatedAssociation.booleanFlag}", spacer.getXmlVisibleCondition());
		assertVisibleInContainingView(spacer, true);

		bean.setAggregatedAssociation(null);
		assertVisibleInContainingView(spacer, false);
	}

	@Test
	void aStringLiteralInAnInlineConditionIsNotRewritten() {
		bean.getAggregatedAssociation().setText("bean.text");
		Spacer spacer = inComponent("{el:bean.text eq 'bean.text'}");
		assertEquals("{el:bean.aggregatedAssociation.text eq 'bean.text'}", spacer.getXmlVisibleCondition());
		assertVisibleInContainingView(spacer, true);
	}

	@Test
	void aPlainBindingConditionFollowsTheComponentBinding() {
		Spacer spacer = inComponent("{booleanFlag}");
		assertEquals("{aggregatedAssociation.booleanFlag}", spacer.getXmlVisibleCondition());
		assertVisibleInContainingView(spacer, true);
	}

	// ---- condition mappings ----

	@Test
	void anInlineConditionCanBeMappedToAnExpressionOfTheContainingDocument() {
		String authored = "{el:bean.enum3 eq Enum3.two}";
		String mappedTo = "{el:bean.aggregatedAssociation.enum3 eq Enum3.three}";
		Spacer spacer = inComponent(authored, mapping(authored, mappedTo));
		// what a condition is mapped to is used as it is
		assertEquals(mappedTo, spacer.getXmlVisibleCondition());
		assertVisibleInContainingView(spacer, false);
	}

	@Test
	void aConditionNameCanBeMappedToAnExpressionOfTheContainingDocument() {
		// AllAttributesPersistent declares "condition"; DeleteDuringPostDelete does not
		String mappedTo = "{el:bean.aggregatedAssociation.booleanFlag}";
		Spacer spacer = inComponent("condition", mapping("condition", mappedTo));
		assertEquals(mappedTo, spacer.getXmlVisibleCondition());
		assertVisibleInContainingView(spacer, true);

		// the inverse of an expression is not stored, so the opposite attribute carries it
		Spacer inverse = inComponent("notCondition", mapping("condition", mappedTo));
		assertNull(inverse.getXmlVisibleCondition());
		assertEquals(mappedTo, inverse.getXmlInvisibleCondition());
		String invisibleCondition = inverse.getInvisibleCondition();
		assertNotNull(invisibleCondition);
		assertTrue(BindUtil.evaluateCondition(bean, invisibleCondition));
	}

	// ---- the admin views that map actions ----

	private static void collectComponents(List<? extends MetaData> contained, List<Component> components) {
		for (MetaData widget : contained) {
			if (widget instanceof Component component) {
				components.add(component);
			}
			else if (widget instanceof VBox vbox) {
				collectComponents(vbox.getContained(), components);
			}
		}
	}

	@Test
	void theAdminUserCreateViewMapsItsComponentActions() {
		ProvidedRepository repository = (ProvidedRepository) CORE.getRepository();
		Module admin = c.getModule("admin");
		Document user = admin.getDocument(c, "User");
		for (String uxui : new String[] {"desktop", "external"}) {
			ViewImpl view = (ViewImpl) repository.getView(uxui, c, user, ViewType.create.toString());
			assertNotNull(view);
			List<Component> components = new ArrayList<>();
			collectComponents(view.getContained(), components);
			assertEquals(3, components.size());
			for (Component component : components) {
				assertEquals(2, component.getActions().size(), component.getName());
				assertTrue(component.getConditions().isEmpty(), component.getName());
				assertFalse(component.getFragment((CustomerImpl) c, uxui).getContained().isEmpty(), component.getName());
			}
			assertDoesNotThrow(() -> repository.validateViewForGenerateDomain(c, user, view, uxui));
		}
	}

	@Test
	void aComponentThatTargetsAWidgetMapsItsActions() {
		CustomerImpl customer = (CustomerImpl) c;
		ModuleImpl admin = (ModuleImpl) c.getModule("admin");
		DocumentImpl controlPanel = (DocumentImpl) admin.getDocument(c, "ControlPanel");
		for (String uxui : new String[] {"desktop", "external"}) {
			Component component = new Component();
			component.setName("_generateTestData");
			component.setWidgetId("generationParameters");
			ComponentTranslation action = mapping("GenerateTestData", "DeleteTestData");
			component.getActions().add(action);
			component.link(uxui, customer, admin, controlPanel, ViewType.edit.toString());

			List<? extends MetaData> contained = component.getFragment(customer, uxui).getContained();
			assertEquals(1, contained.size());
			VBox vbox = (VBox) contained.get(0);
			assertEquals("generationParameters", vbox.getWidgetId());
			List<String> actionNames = new ArrayList<>();
			for (FormRow row : ((Form) vbox.getContained().get(0)).getRows()) {
				for (FormItem item : row.getItems()) {
					if (item.getWidget() instanceof Button button) {
						actionNames.add(button.getActionName());
					}
				}
			}
			assertEquals(List.of("DeleteTestData", "DeleteTestData"), actionNames);
		}
	}
}
