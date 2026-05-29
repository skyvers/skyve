package org.skyve.impl.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.ConditionImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.model.chart.ChartBuilderMetaData;
import org.skyve.impl.metadata.view.event.Addable;
import org.skyve.impl.metadata.view.event.Changeable;
import org.skyve.impl.metadata.view.event.EventAction;
import org.skyve.impl.metadata.view.event.Focusable;
import org.skyve.impl.metadata.view.event.Removable;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.Selectable;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;
import org.skyve.impl.metadata.view.reference.DefaultListViewReference;
import org.skyve.impl.metadata.view.reference.EditViewReference;
import org.skyve.impl.metadata.view.reference.QueryListViewReference;
import org.skyve.impl.metadata.view.reference.ReportReference;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.Comparison;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.container.Collapsible;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.FormatterName;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.domain.Bean;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.view.Action.ActionShow;
import org.skyve.metadata.view.model.chart.ChartModel;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.impl.metadata.model.document.field.Text;

@ExtendWith(MockitoExtension.class)
@SuppressWarnings("java:S5976") // separate tests preferred over parameterized here as junit-jupiter-params is not on the classpath
class ViewValidatorTest {

	@Mock
	private ProvidedRepository repository;

	@Mock
	private CustomerImpl customer;

	@Mock
	private ModuleImpl module;

	private DocumentImpl document;
	private ViewImpl view;

	@BeforeEach
	void setUp() {
		when(repository.getModule(any(), anyString())).thenReturn(module);
		ProvidedRepositoryFactory.set(repository);

		document = new DocumentImpl();
		document.setOwningModuleName("testMod");
		document.setName("TestDoc");

		view = new ViewImpl();
		view.setName("edit");
	}

	private ViewValidator newValidator() {
		return new ViewValidator(view, repository, customer, document, "desktop");
	}

	private ViewValidator newValidator(DocumentImpl contextDocument) {
		return new ViewValidator(view, repository, customer, contextDocument, "desktop");
	}

	private static Object invokeValidateBinding(ViewValidator v,
													org.skyve.metadata.module.Module contextModule,
													org.skyve.metadata.model.document.Document contextDocument,
													String bindingPrefix,
													String binding,
													boolean bindingRequired,
													boolean compoundBindingInvalid,
													boolean domainValuesRequired,
													boolean scalarBindingOnly,
													String widgetIdentifier,
													org.skyve.metadata.model.Attribute.AttributeType... assertTypes) {
		try {
			Method m = ViewValidator.class.getDeclaredMethod("validateBinding",
					org.skyve.metadata.module.Module.class,
					org.skyve.metadata.model.document.Document.class,
					String.class,
					String.class,
					boolean.class,
					boolean.class,
					boolean.class,
					boolean.class,
					String.class,
					org.skyve.metadata.model.Attribute.AttributeType[].class);
			m.setAccessible(true);
			return m.invoke(v,
					contextModule,
					contextDocument,
					bindingPrefix,
					binding,
					Boolean.valueOf(bindingRequired),
					Boolean.valueOf(compoundBindingInvalid),
					Boolean.valueOf(domainValuesRequired),
					Boolean.valueOf(scalarBindingOnly),
					widgetIdentifier,
					assertTypes);
		}
		catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof RuntimeException) {
				throw (RuntimeException) cause;
			}
			throw new RuntimeException(cause);
		}
		catch (ReflectiveOperationException e) {
			throw new RuntimeException(e);
		}
	}

	private static Object invokeValidateBinding(ViewValidator v,
											String bindingPrefix,
											String binding,
											boolean bindingRequired,
											boolean compoundBindingInvalid,
											boolean domainValuesRequired,
											boolean scalarBindingOnly,
											String widgetIdentifier,
											org.skyve.metadata.model.Attribute.AttributeType... assertTypes) {
		return invokeValidateBinding(v, null, null, bindingPrefix, binding, bindingRequired, compoundBindingInvalid, domainValuesRequired, scalarBindingOnly, widgetIdentifier, assertTypes);
	}

	// ---- empty view ----

	@Test
	void emptyView_visit_noException() {
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	// ---- visitView title / help ----

	@Test
	void viewWithTitle_noException() {
		view.setTitle("My Title");
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void viewBothHelpUrlAndFileName_throws() {
		view.setHelpURL("http://example.com/help");
		view.setHelpRelativeFileName("help.html");
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void viewOnlyHelpUrl_noException() {
		view.setHelpURL("http://example.com/help");
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void viewOnlyHelpRelativeFileName_noException() {
		view.setHelpRelativeFileName("help.html");
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	// ---- visitSpacer — condition names ----

	@Test
	void spacer_noCondition_noException() {
		Spacer s = new Spacer();
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void spacer_trueCondition_noException() {
		Spacer s = new Spacer();
		s.setInvisibleConditionName("true");
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void spacer_falseCondition_noException() {
		Spacer s = new Spacer();
		s.setInvisibleConditionName("false");
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void spacer_persistedCondition_noException() {
		Spacer s = new Spacer();
		s.setInvisibleConditionName("persisted");
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void spacer_createdCondition_noException() {
		Spacer s = new Spacer();
		s.setInvisibleConditionName("created");
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void spacer_changedCondition_noException() {
		Spacer s = new Spacer();
		s.setInvisibleConditionName("changed");
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void spacer_notCondition_validConditionInDocument_noException() {
		document.getConditions().put("myFlag", new ConditionImpl());
		Spacer s = new Spacer();
		s.setInvisibleConditionName("notMyFlag");
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void spacer_validConditionInDocument_noException() {
		document.getConditions().put("myFlag", new ConditionImpl());
		Spacer s = new Spacer();
		s.setInvisibleConditionName("myFlag");
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void spacer_unknownCondition_throws() {
		Spacer s = new Spacer();
		s.setInvisibleConditionName("noSuchCondition");
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void spacer_notConditionBadCamelCase_throws() {
		// "notxxx" — 'x' is lower-case after "not" → must be MetaDataException
		Spacer s = new Spacer();
		s.setInvisibleConditionName("notxxx");
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	// ---- validateSize — Spacer pixel width ----

	@Test
	void spacer_positivePixelWidth_noException() {
		Spacer s = new Spacer();
		s.setPixelWidth(Integer.valueOf(100));
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void spacer_zeroPixelWidth_throws() {
		Spacer s = new Spacer();
		s.setPixelWidth(Integer.valueOf(0));
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void spacer_negativePixelWidth_throws() {
		Spacer s = new Spacer();
		s.setPixelWidth(Integer.valueOf(-5));
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	// ---- visitBlurb ----

	@Test
	void blurb_withMarkup_noException() {
		Blurb b = new Blurb();
		b.setMarkup("<p>Hello</p>");
		view.getContained().add(b);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void blurb_noMarkup_throws() {
		Blurb b = new Blurb();
		view.getContained().add(b);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void blurb_withConditionInDocument_noException() {
		document.getConditions().put("showBlurb", new ConditionImpl());
		Blurb b = new Blurb();
		b.setMarkup("<p>Content</p>");
		b.setInvisibleConditionName("showBlurb");
		view.getContained().add(b);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void validateBinding_requiredNullBinding_throws() {
		ViewValidator v = newValidator();

		assertThrows(MetaDataException.class,
				() -> invokeValidateBinding(v,
						null,
						null,
						true,
						false,
						false,
						false,
						"widget"));
	}

	@Test
	void validateBinding_notRequiredNullBinding_returnsNull() {
		ViewValidator v = newValidator();

		Object result = invokeValidateBinding(v,
				null,
				null,
				false,
				false,
				false,
				false,
				"widget");

		org.junit.jupiter.api.Assertions.assertNull(result);
	}

	@Test
	void validateBinding_compoundNotAllowed_throws() {
		ViewValidator v = newValidator();

		assertThrows(MetaDataException.class,
				() -> invokeValidateBinding(v,
						null,
						"a.b",
						false,
						true,
						false,
						false,
						"widget"));
	}

	@Test
	void validateBinding_invalidBindingWrapsAsMetadataException() {
		ViewValidator v = newValidator();

		assertThrows(MetaDataException.class,
				() -> invokeValidateBinding(v,
						"prefix",
						"!invalid",
						false,
						false,
						false,
						false,
						"widget"));
	}

	@Test
	void validateBinding_bindingPrefix_resolvesNestedBinding() {
		DocumentImpl parentDocument = new DocumentImpl();
		parentDocument.setOwningModuleName("testMod");
		parentDocument.setName("ParentDoc");
		Text parentName = new Text();
		parentName.setName("name");
		parentName.setLength(20);
		parentDocument.putAttribute(parentName);

		DocumentImpl childDocument = new DocumentImpl();
		childDocument.setOwningModuleName("testMod");
		childDocument.setName("ChildDoc");
		childDocument.setParentDocumentName("ParentDoc");

		when(module.getDocument(eq(customer), eq("ParentDoc"))).thenReturn(parentDocument);

		ViewValidator v = newValidator(childDocument);
		Object result = invokeValidateBinding(v,
				module,
				childDocument,
				"parent",
				"name",
				false,
				false,
				false,
				false,
				"widget");

		assertEquals(String.class, result);
	}

	@Test
	void validateBinding_domainValuesRequiredImplicitAttribute_throws() {
		ViewValidator v = newValidator();

		assertThrows(MetaDataException.class,
				() -> invokeValidateBinding(v,
						module,
						document,
						null,
						"created",
						false,
						false,
						true,
						false,
						"widget"));
	}

	@Test
	void validateBinding_domainValuesRequiredWithoutDomainType_throws() {
		Text title = new Text();
		title.setName("title");
		title.setLength(20);
		document.putAttribute(title);

		ViewValidator v = newValidator();

		assertThrows(MetaDataException.class,
				() -> invokeValidateBinding(v,
						module,
						document,
						null,
						"title",
						false,
						false,
						true,
						false,
						"widget"));
	}

	@Test
	void validateBinding_assertTypesOnImplicitAttribute_throws() {
		ViewValidator v = newValidator();

		assertThrows(MetaDataException.class,
				() -> invokeValidateBinding(v,
						module,
						document,
						null,
						"created",
						false,
						false,
						false,
						false,
						"widget",
						org.skyve.metadata.model.Attribute.AttributeType.text));
	}

	@Test
	void validateBinding_assertTypesMismatchOnScalarAttribute_throws() {
		Text title = new Text();
		title.setName("title");
		title.setLength(20);
		document.putAttribute(title);

		ViewValidator v = newValidator();

		assertThrows(MetaDataException.class,
				() -> invokeValidateBinding(v,
						module,
						document,
						null,
						"title",
						false,
						false,
						false,
						false,
						"widget",
						org.skyve.metadata.model.Attribute.AttributeType.integer));
	}

	@Test
	void validateBinding_parentBindingAllowsAssociationAssertion() {
		DocumentImpl childDocument = new DocumentImpl();
		childDocument.setOwningModuleName("testMod");
		childDocument.setName("ChildDoc");
		childDocument.setParentDocumentName("ParentDoc");

		ViewValidator v = newValidator(childDocument);
		Object result = invokeValidateBinding(v,
				module,
				childDocument,
				null,
				org.skyve.domain.ChildBean.PARENT_NAME,
				false,
				false,
				false,
				false,
				"widget",
				org.skyve.metadata.model.Attribute.AttributeType.association);

		assertEquals(org.skyve.domain.Bean.class, result);
	}

	@Test
	void validateBinding_parentBindingWithoutAssociationAssertion_throws() {
		DocumentImpl childDocument = new DocumentImpl();
		childDocument.setOwningModuleName("testMod");
		childDocument.setName("ChildDoc");
		childDocument.setParentDocumentName("ParentDoc");

		ViewValidator v = newValidator(childDocument);

		assertThrows(MetaDataException.class,
				() -> invokeValidateBinding(v,
						module,
						childDocument,
						null,
						org.skyve.domain.ChildBean.PARENT_NAME,
						false,
						false,
						false,
						false,
						"widget",
						org.skyve.metadata.model.Attribute.AttributeType.text));
	}

	@Test
	void validateBinding_scalarBindingOnlyRejectsCollection() {
		CollectionImpl children = new CollectionImpl();
		children.setName("children");
		children.setDocumentName("ChildDoc");
		children.setType(org.skyve.metadata.model.document.Collection.CollectionType.child);
		document.putAttribute(children);

		ViewValidator v = newValidator();

		assertThrows(MetaDataException.class,
				() -> invokeValidateBinding(v,
						module,
						document,
						null,
						"children",
						false,
						false,
						false,
						true,
						"widget"));
	}

	@Test
	void validateBinding_scalarBindingOnlyWithAssertTypeAllowsCollection() {
		CollectionImpl children = new CollectionImpl();
		children.setName("children");
		children.setDocumentName("ChildDoc");
		children.setType(org.skyve.metadata.model.document.Collection.CollectionType.child);
		document.putAttribute(children);

		ViewValidator v = newValidator();

		assertDoesNotThrow(() -> invokeValidateBinding(v,
				module,
				document,
				null,
				"children",
				false,
				false,
				false,
				true,
				"widget",
				org.skyve.metadata.model.Attribute.AttributeType.collection));
	}

	@Test
	void validateBinding_domainValuesRequiredWithDomainType_noException() {
		Text status = new Text();
		status.setName("status");
		status.setLength(20);
		status.setDomainType(org.skyve.metadata.model.document.DomainType.constant);
		document.putAttribute(status);

		ViewValidator v = newValidator();

		assertDoesNotThrow(() -> invokeValidateBinding(v,
				module,
				document,
				null,
				"status",
				false,
				false,
				true,
				false,
				"widget"));
	}

	@Test
	void validateBinding_assertTypesMatchOnScalarAttribute_noException() {
		Text title = new Text();
		title.setName("title");
		title.setLength(20);
		document.putAttribute(title);

		ViewValidator v = newValidator();

		assertDoesNotThrow(() -> invokeValidateBinding(v,
				module,
				document,
				null,
				"title",
				false,
				false,
				false,
				false,
				"widget",
				org.skyve.metadata.model.Attribute.AttributeType.text));
	}

	@Test
	void spacer_notConditionWithInvalidCamelCase_throws() {
		Spacer s = new Spacer();
		s.setInvisibleConditionName("notmyFlag");
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void spacer_conditionResolvedFromExtendedDocument_noException() {
		DocumentImpl baseDocument = new DocumentImpl();
		baseDocument.setOwningModuleName("testMod");
		baseDocument.setName("BaseDoc");
		baseDocument.getConditions().put("baseFlag", new ConditionImpl());

		DocumentImpl childDocument = new DocumentImpl();
		childDocument.setOwningModuleName("testMod");
		childDocument.setName("ChildDoc");
		Extends extension = new Extends();
		extension.setDocumentName("BaseDoc");
		childDocument.setExtends(extension);

		when(module.getDocument(eq(customer), eq("BaseDoc"))).thenReturn(baseDocument);

		Spacer s = new Spacer();
		s.setInvisibleConditionName("baseFlag");
		view.getContained().add(s);

		ViewValidator v = newValidator(childDocument);
		assertDoesNotThrow(v::visit);
	}

	@Test
	void spacer_conditionMissingInExtendedHierarchy_throws() {
		DocumentImpl baseDocument = new DocumentImpl();
		baseDocument.setOwningModuleName("testMod");
		baseDocument.setName("BaseDoc");

		DocumentImpl childDocument = new DocumentImpl();
		childDocument.setOwningModuleName("testMod");
		childDocument.setName("ChildDoc");
		Extends extension = new Extends();
		extension.setDocumentName("BaseDoc");
		childDocument.setExtends(extension);

		when(module.getDocument(eq(customer), eq("BaseDoc"))).thenReturn(baseDocument);

		Spacer s = new Spacer();
		s.setInvisibleConditionName("missingFlag");
		view.getContained().add(s);

		ViewValidator v = newValidator(childDocument);
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void blurb_unknownCondition_throws() {
		Blurb b = new Blurb();
		b.setMarkup("<p>Content</p>");
		b.setInvisibleConditionName("noSuchCond");
		view.getContained().add(b);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	// ---- visitButton ----

	@Test
	void button_nullActionName_noException() {
		// null action name → validateActionName does nothing
		Button btn = new Button();
		view.getContained().add(btn);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void button_nonExistentAction_throws() {
		Button btn = new Button();
		btn.setActionName("notAnAction");
		view.getContained().add(btn);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void button_existingAction_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("myAction");
		action.setImplicitName(ImplicitActionName.Save);
		view.putAction(action);

		Button btn = new Button();
		btn.setActionName("myAction");
		view.getContained().add(btn);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	// ---- HBox ----

	@Test
	void hbox_empty_noException() {
		HBox hbox = new HBox();
		view.getContained().add(hbox);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void hbox_unknownCondition_throws() {
		HBox hbox = new HBox();
		hbox.setInvisibleConditionName("badCondition");
		view.getContained().add(hbox);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void hbox_collapsible_noTitle_throws() {
		HBox hbox = new HBox();
		hbox.setCollapsible(Collapsible.open);
		view.getContained().add(hbox);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void hbox_negativePixelWidth_throws() {
		HBox hbox = new HBox();
		hbox.setPixelWidth(Integer.valueOf(-1));
		view.getContained().add(hbox);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	// ---- VBox ----

	@Test
	void vbox_empty_noException() {
		VBox vbox = new VBox();
		view.getContained().add(vbox);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void vbox_unknownCondition_throws() {
		VBox vbox = new VBox();
		vbox.setInvisibleConditionName("badCond");
		view.getContained().add(vbox);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void vbox_withSpacerInside_noException() {
		VBox vbox = new VBox();
		Spacer s = new Spacer();
		vbox.getContained().add(s);
		view.getContained().add(vbox);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	// ---- HBox inside VBox ----

	@Test
	void vbox_withHBoxInside_noException() {
		VBox vbox = new VBox();
		HBox hbox = new HBox();
		vbox.getContained().add(hbox);
		view.getContained().add(vbox);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	// ---- TabPane and Tab ----

	@Test
	void tabPane_empty_noException() {
		TabPane tabPane = new TabPane();
		view.getContained().add(tabPane);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void tabPane_unknownCondition_throws() {
		TabPane tabPane = new TabPane();
		tabPane.setInvisibleConditionName("badCondition");
		view.getContained().add(tabPane);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void tabPane_negativePixelWidth_throws() {
		TabPane tabPane = new TabPane();
		tabPane.setPixelWidth(Integer.valueOf(-1));
		view.getContained().add(tabPane);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void tab_withSpacerInside_noException() {
		Tab tab = new Tab();
		Spacer s = new Spacer();
		tab.getContained().add(s);
		TabPane tabPane = new TabPane();
		tabPane.getTabs().add(tab);
		view.getContained().add(tabPane);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void tab_unknownDisabledCondition_throws() {
		Tab tab = new Tab();
		tab.setDisabledConditionName("badCond");
		TabPane tabPane = new TabPane();
		tabPane.getTabs().add(tab);
		view.getContained().add(tabPane);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	// ---- Actions on the view ----

	@Test
	void action_saveImplicit_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("Save");
		action.setImplicitName(ImplicitActionName.Save);
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void action_cancelImplicit_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("Cancel");
		action.setImplicitName(ImplicitActionName.Cancel);
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void action_okImplicit_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("OK");
		action.setImplicitName(ImplicitActionName.OK);
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void action_deleteImplicit_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("Delete");
		action.setImplicitName(ImplicitActionName.Delete);
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void action_addImplicit_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("Add");
		action.setImplicitName(ImplicitActionName.Add);
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void action_editImplicit_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("Edit");
		action.setImplicitName(ImplicitActionName.Edit);
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void action_removeImplicit_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("Remove");
		action.setImplicitName(ImplicitActionName.Remove);
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void action_newImplicit_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("New");
		action.setImplicitName(ImplicitActionName.New);
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void action_zoomOutImplicit_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("ZoomOut");
		action.setImplicitName(ImplicitActionName.ZoomOut);
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void action_iconShowWithIconStyleClass_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("myAction");
		action.setImplicitName(ImplicitActionName.Save);
		action.setShow(ActionShow.icon);
		action.setIconStyleClass("fa fa-save");
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void action_iconShowWithNoIcon_throws() {
		ActionImpl action = new ActionImpl();
		action.setName("myAction");
		action.setImplicitName(ImplicitActionName.Save);
		action.setShow(ActionShow.icon);
		view.putAction(action);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void action_iconShowWithRelativeIconFileName_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("myAction");
		action.setImplicitName(ImplicitActionName.Save);
		action.setShow(ActionShow.icon);
		action.setRelativeIconFileName("save.png");
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void action_unknownConditionOnAction_throws() {
		ActionImpl action = new ActionImpl();
		action.setName("myAction");
		action.setImplicitName(ImplicitActionName.Save);
		action.setDisabledConditionName("noSuchCondition");
		view.putAction(action);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void action_validConditionOnAction_noException() {
		document.getConditions().put("canSave", new ConditionImpl());
		ActionImpl action = new ActionImpl();
		action.setName("myAction");
		action.setImplicitName(ImplicitActionName.Save);
		action.setDisabledConditionName("canSave");
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	// ---- DEFAULTS implicit action ----

	@Test
	void action_defaultsImplicit_editView_noException() {
		// DEFAULTS on an edit view expands to all edit-view implicit actions
		ActionImpl action = new ActionImpl();
		action.setName("defaults");
		action.setImplicitName(ImplicitActionName.DEFAULTS);
		view.setName("edit");
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void action_defaultsImplicit_listView_noException() {
		// DEFAULTS on a list view expands to New
		ActionImpl action = new ActionImpl();
		action.setName("defaults");
		action.setImplicitName(ImplicitActionName.DEFAULTS);
		view.setName("list");
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	// ---- Button icon show mode ----

	@Test
	void button_iconShowWithNoIcon_throws() {
		ActionImpl action = new ActionImpl();
		action.setName("doSomething");
		action.setImplicitName(ImplicitActionName.Save);
		view.putAction(action);

		Button btn = new Button();
		btn.setActionName("doSomething");
		btn.setShow(ActionShow.icon);
		view.getContained().add(btn);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void button_iconShowWithIconStyleClass_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("doSomething");
		action.setImplicitName(ImplicitActionName.Save);
		action.setIconStyleClass("fa fa-check");
		view.putAction(action);

		Button btn = new Button();
		btn.setActionName("doSomething");
		btn.setShow(ActionShow.icon);
		view.getContained().add(btn);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	// ---- refreshConditionName on view ----

	@Test
	void view_refreshConditionName_unknown_throws() {
		view.setRefreshConditionName("noSuch");
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void view_refreshConditionName_valid_noException() {
		document.getConditions().put("refreshable", new ConditionImpl());
		view.setRefreshConditionName("refreshable");
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	// ---- refreshActionName on view ----

	@Test
	void view_refreshActionName_nonExistent_throws() {
		view.setRefreshActionName("noSuchAction");
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void view_refreshActionName_existingAction_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("refreshNow");
		action.setImplicitName(ImplicitActionName.Save);
		view.putAction(action);
		view.setRefreshActionName("refreshNow");
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	// ---- nested containers with conditions ----

	@Test
	void hboxInsideTabWithCondition_noException() {
		document.getConditions().put("isVisible", new ConditionImpl());
		Tab tab = new Tab();
		HBox hbox = new HBox();
		hbox.setInvisibleConditionName("isVisible");
		tab.getContained().add(hbox);
		TabPane tabPane = new TabPane();
		tabPane.getTabs().add(tab);
		view.getContained().add(tabPane);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void multipleWidgetsInView_noException() {
		document.getConditions().put("flag", new ConditionImpl());
		Spacer s1 = new Spacer();
		Spacer s2 = new Spacer();
		s2.setInvisibleConditionName("flag");
		Blurb b = new Blurb();
		b.setMarkup("<p>ok</p>");
		view.getContained().add(s1);
		view.getContained().add(s2);
		view.getContained().add(b);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void formRow_empty_throws() {
		Form form = new Form();
		FormRow row = new FormRow();
		form.getRows().add(row);
		view.getContained().add(form);

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void formRow_withItem_noException() {
		Form form = new Form();
		FormRow row = new FormRow();
		FormItem item = new FormItem();
		item.setWidget(new Spacer());
		row.getItems().add(item);
		form.getRows().add(row);
		view.getContained().add(form);

		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void link_queryListReference_unknownQuery_throws() {
		Link link = new Link();
		link.setValue("Open");
		QueryListViewReference reference = new QueryListViewReference();
		reference.setQueryName("missingQuery");
		link.setReference(reference);
		view.getContained().add(link);

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void link_queryListReference_existingQuery_noException() {
		when(module.getMetaDataQuery("existingQuery")).thenReturn(mock(MetaDataQueryDefinition.class));

		Link link = new Link();
		link.setValue("Open");
		QueryListViewReference reference = new QueryListViewReference();
		reference.setQueryName("existingQuery");
		link.setReference(reference);
		view.getContained().add(link);

		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void tabPane_selectedTabIndexBinding_unknown_throws() {
		TabPane tabPane = new TabPane();
		tabPane.setSelectedTabIndexBinding("unknownBinding");
		view.getContained().add(tabPane);

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void parameter_withoutValueOrBinding_throws() {
		Parameter parameter = mock(Parameter.class);
		when(parameter.getName()).thenReturn("p");
		when(parameter.getValue()).thenReturn(null);
		when(parameter.getValueBinding()).thenReturn(null);

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitParameter(parameter, true, true));
	}

	@Test
	void parameter_withValue_noException() {
		Parameter parameter = mock(Parameter.class);
		when(parameter.getValue()).thenReturn("v");

		ViewValidator v = newValidator();
		assertDoesNotThrow(() -> v.visitParameter(parameter, true, true));
	}

	@Test
	void filterParameter_operatorEqualWithoutValueOrBinding_throws() {
		FilterParameter parameter = mock(FilterParameter.class);
		when(parameter.getFilterBinding()).thenReturn("status");
		when(parameter.getOperator()).thenReturn(FilterOperator.equal);
		when(parameter.getValue()).thenReturn(null);
		when(parameter.getValueBinding()).thenReturn(null);

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitFilterParameter(parameter, true, true));
	}

	@Test
	void filterParameter_operatorIsNullWithoutValueOrBinding_noException() {
		FilterParameter parameter = mock(FilterParameter.class);
		when(parameter.getOperator()).thenReturn(FilterOperator.isNull);

		ViewValidator v = newValidator();
		assertDoesNotThrow(() -> v.visitFilterParameter(parameter, true, true));
	}

	@Test
	void link_editViewReference_invalidModule_throws() {
		when(repository.getModule(any(), eq("missingModule"))).thenReturn(null);

		EditViewReference reference = new EditViewReference();
		reference.setModuleName("missingModule");
		reference.setDocumentName("AnyDoc");

		Link link = new Link();
		link.setValue("Open");
		link.setReference(reference);
		view.getContained().add(link);

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void link_defaultListViewReference_invalidDocument_throws() {
		when(repository.getModule(any(), eq("knownModule"))).thenReturn(module);
		when(module.getDocument(any(), eq("missingDocument"))).thenReturn(null);

		DefaultListViewReference reference = new DefaultListViewReference();
		reference.setModuleName("knownModule");
		reference.setDocumentName("missingDocument");

		Link link = new Link();
		link.setValue("List");
		link.setReference(reference);
		view.getContained().add(link);

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void link_editViewReference_expressionModuleAndDocument_noException() {
		EditViewReference reference = new EditViewReference();
		reference.setModuleName("{moduleExpr}");
		reference.setDocumentName("{documentExpr}");

		Link link = new Link();
		link.setValue("Open");
		link.setReference(reference);
		view.getContained().add(link);

		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void link_defaultListViewReference_expressionModuleAndDocument_noException() {
		DefaultListViewReference reference = new DefaultListViewReference();
		reference.setModuleName("{moduleExpr}");
		reference.setDocumentName("{documentExpr}");

		Link link = new Link();
		link.setValue("List");
		link.setReference(reference);
		view.getContained().add(link);

		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void link_reportReference_expressionModuleAndDocument_noException() {
		ReportReference reference = new ReportReference();
		reference.setModuleName("{moduleExpr}");
		reference.setDocumentName("{documentExpr}");
		reference.setReportName("{reportExpr}");

		Link link = new Link();
		link.setValue("Report");
		link.setReference(reference);
		view.getContained().add(link);

		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void link_defaultListViewReference_validModuleAndDocument_noException() {
		DocumentImpl targetDocument = new DocumentImpl();
		targetDocument.setName("TargetDoc");
		when(repository.getModule(any(), eq("knownModule"))).thenReturn(module);
		when(module.getDocument(any(), eq("TargetDoc"))).thenReturn(targetDocument);

		DefaultListViewReference reference = new DefaultListViewReference();
		reference.setModuleName("knownModule");
		reference.setDocumentName("TargetDoc");

		Link link = new Link();
		link.setValue("List");
		link.setReference(reference);
		view.getContained().add(link);

		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void link_reportReference_invalidReportName_throws() {
		DocumentImpl targetDocument = new DocumentImpl();
		targetDocument.setName("TargetDoc");
		when(repository.getModule(any(), eq("knownModule"))).thenReturn(module);
		when(module.getName()).thenReturn("knownModule");
		when(module.getDocument(any(), eq("TargetDoc"))).thenReturn(targetDocument);
		when(repository.getReportFileName(any(), eq(targetDocument), eq("missingReport"))).thenReturn(null);

		ReportReference reference = new ReportReference();
		reference.setModuleName("knownModule");
		reference.setDocumentName("TargetDoc");
		reference.setReportName("missingReport");

		Link link = new Link();
		link.setValue("Report");
		link.setReference(reference);
		view.getContained().add(link);

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void link_editViewReference_invalidBinding_throws() {
		EditViewReference reference = new EditViewReference();
		reference.setModuleName("{moduleExpr}");
		reference.setDocumentName("{documentExpr}");
		reference.setBinding("noSuchBinding");

		Link link = new Link();
		link.setValue("Open");
		link.setReference(reference);
		view.getContained().add(link);

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void link_defaultListViewReference_expressionDocumentWithConcreteModule_noException() {
		when(repository.getModule(any(), eq("knownModule"))).thenReturn(module);

		DefaultListViewReference reference = new DefaultListViewReference();
		reference.setModuleName("knownModule");
		reference.setDocumentName("{documentExpr}");

		Link link = new Link();
		link.setValue("List");
		link.setReference(reference);
		view.getContained().add(link);

		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void link_editViewReference_validBinding_noException() {
		EditViewReference reference = new EditViewReference();
		reference.setModuleName("{moduleExpr}");
		reference.setDocumentName("{documentExpr}");
		reference.setBinding(Bean.DOCUMENT_ID);

		Link link = new Link();
		link.setValue("Open");
		link.setReference(reference);
		view.getContained().add(link);

		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void dataGridBoundColumn_formatterAndCustomFormatter_throws() {
		DataGridBoundColumn column = new DataGridBoundColumn();
		column.setTitle("Amount");
		column.setFormatterName(FormatterName.Integer);
		column.setCustomFormatterName("AnyFormatter");

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitDataGridBoundColumn(column, true, true));
	}

	@Test
	void dataGridBoundColumn_unknownCustomFormatter_throws() {
		DataGridBoundColumn column = new DataGridBoundColumn();
		column.setTitle("Amount");
		column.setCustomFormatterName("NoSuchFormatter");

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitDataGridBoundColumn(column, true, true));
	}

	@Test
	void chart_withoutModelNameAndModel_throws() {
		Chart chart = new Chart();
		view.getContained().add(chart);

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void chart_withModelNameAndModel_throws() {
		Chart chart = new Chart();
		chart.setModelName("namedModel");
		chart.setModel(new ChartBuilderMetaData());
		view.getContained().add(chart);

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void chart_inlineModel_withoutDocumentOrQuery_throws() {
		ChartBuilderMetaData model = new ChartBuilderMetaData();
		model.setLabel("Inline");
		model.setModuleName("testMod");
		model.setCategoryBinding("name");
		model.setValueBinding("amount");

		Chart chart = new Chart();
		chart.setModel(model);
		view.getContained().add(chart);

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void chart_inlineModel_unknownQuery_throws() {
		ChartBuilderMetaData model = new ChartBuilderMetaData();
		model.setLabel("Inline");
		model.setModuleName("testMod");
		model.setQueryName("missingQuery");
		model.setCategoryBinding("name");
		model.setValueBinding("amount");

		Chart chart = new Chart();
		chart.setModel(model);
		view.getContained().add(chart);

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void chart_modelName_missing_throws() {
		when(repository.getChartModel(any(), any(), eq("missingChartModel"), eq(false))).thenThrow(new RuntimeException("missing"));

		Chart chart = new Chart();
		chart.setModelName("missingChartModel");
		view.getContained().add(chart);

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void chart_modelName_existing_noException() {
		when(repository.getChartModel(any(), any(), eq("existingChartModel"), eq(false))).thenReturn(mock(ChartModel.class));

		Chart chart = new Chart();
		chart.setModelName("existingChartModel");
		view.getContained().add(chart);

		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void chart_inlineModel_invalidModule_throws() {
		when(repository.getModule(any(), eq("badModule"))).thenThrow(new RuntimeException("bad module"));

		ChartBuilderMetaData model = new ChartBuilderMetaData();
		model.setLabel("Inline");
		model.setModuleName("badModule");
		model.setDocumentName("AnyDoc");
		model.setCategoryBinding("name");
		model.setValueBinding("amount");

		Chart chart = new Chart();
		chart.setModel(model);
		view.getContained().add(chart);

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void chart_inlineModel_invalidDocument_throws() {
		when(module.getDocument(any(), eq("MissingDoc"))).thenThrow(new RuntimeException("bad document"));

		ChartBuilderMetaData model = new ChartBuilderMetaData();
		model.setLabel("Inline");
		model.setModuleName("testMod");
		model.setDocumentName("MissingDoc");
		model.setCategoryBinding("name");
		model.setValueBinding("amount");

		Chart chart = new Chart();
		chart.setModel(model);
		view.getContained().add(chart);

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void customAction_printNameClash_throws() {
		ActionImpl action = new ActionImpl();
		action.setResourceName(ImplicitActionName.Print.toString());

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitCustomAction(action));
	}

	@Test
	void customAction_missingClass_throws() {
		ActionImpl action = new ActionImpl();
		action.setResourceName("MissingCustomAction");

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitCustomAction(action));
	}

	@Test
	void customAction_existingClass_noException() {
		doReturn(ActionImpl.class).when(repository).getJavaClass(any(), anyString());

		ActionImpl action = new ActionImpl();
		action.setResourceName("ExistingCustomAction");

		ViewValidator v = newValidator();
		assertDoesNotThrow(() -> v.visitCustomAction(action));
	}

	@Test
	void classBackedActionVisitors_existingClass_noException() {
		doReturn(ActionImpl.class).when(repository).getJavaClass(any(), anyString());

		ActionImpl action = new ActionImpl();
		action.setName("Export");
		action.setResourceName("ExistingClassAction");

		ViewValidator v = newValidator();
		assertDoesNotThrow(() -> v.visitBizExportAction(action));
		assertDoesNotThrow(() -> v.visitBizImportAction(action));
		assertDoesNotThrow(() -> v.visitDownloadAction(action));
		assertDoesNotThrow(() -> v.visitUploadAction(action));
	}

	@Test
	void classBackedActionVisitor_printNameClash_throws() {
		ActionImpl action = new ActionImpl();
		action.setName("Export");
		action.setResourceName(ImplicitActionName.Print.toString());

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitBizExportAction(action));
	}

	@Test
	void simpleActionVisitors_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("DoIt");

		ViewValidator v = newValidator();
		assertDoesNotThrow(() -> v.visitAddAction(action));
		assertDoesNotThrow(() -> v.visitCancelAction(action));
		assertDoesNotThrow(() -> v.visitDeleteAction(action));
		assertDoesNotThrow(() -> v.visitEditAction(action));
		assertDoesNotThrow(() -> v.visitNavigateAction(action));
		assertDoesNotThrow(() -> v.visitNewAction(action));
		assertDoesNotThrow(() -> v.visitOKAction(action));
		assertDoesNotThrow(() -> v.visitRemoveAction(action));
	}

	@Test
	void customAction_withMetaDataAction_noException() {
		when(repository.getMetaDataAction(any(), any(), eq("metaAction")))
				.thenReturn(mock(org.skyve.impl.metadata.repository.behaviour.ActionMetaData.class));

		ActionImpl action = new ActionImpl();
		action.setName("DoMeta");
		action.setResourceName("metaAction");

		ViewValidator v = newValidator();
		assertDoesNotThrow(() -> v.visitCustomAction(action));
	}

	@Test
	void customAction_withMetaDataAction_iconShowNoIcon_throws() {
		when(repository.getMetaDataAction(any(), any(), eq("metaAction")))
				.thenReturn(mock(org.skyve.impl.metadata.repository.behaviour.ActionMetaData.class));

		ActionImpl action = new ActionImpl();
		action.setName("DoMeta");
		action.setResourceName("metaAction");
		action.setShow(ActionShow.icon);

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitCustomAction(action));
	}

	@Test
	void visitedNoOpMethods_noException() {
		ViewValidator v = newValidator();
		assertDoesNotThrow(() -> {
			v.visitedCheckBox(null, true, true);
			v.visitedCheckMembership(null, true, true);
			v.visitedColourPicker(null, true, true);
			v.visitedCombo(null, true, true);
			v.visitedDataGridBoundColumn(null, true, true);
			v.visitedDataGridContainerColumn(null, true, true);
			v.visitedGeometry(null, true, true);
			v.visitedGeometryMap(null, true, true);
			v.visitedListMembership(null, true, true);
			v.visitedLookupDescription(null, true, true);
			v.visitedPassword(null, true, true);
			v.visitedRadio(null, true, true);
			v.visitedRichText(null, true, true);
			v.visitedSlider(null, true, true);
			v.visitedSpinner(null, true, true);
			v.visitedTextArea(null, true, true);
			v.visitedTextField(null, true, true);
			v.visitedDataGrid(null, true, true);
			v.visitedDataRepeater(null, true, true);
			v.visitedForm(null, true, true);
			v.visitedFormItem(null, true, true);
			v.visitedFormRow(null, true, true);
			v.visitedHBox(null, true, true);
			v.visitedListGrid(null, true, true);
			v.visitedListRepeater(null, true, true);
			v.visitedTreeGrid(null, true, true);
			v.visitedTab(null, true, true);
			v.visitedTabPane(null, true, true);
			v.visitedVBox(null, true, true);
			v.visitedView();
			v.visitedOnChangedEventHandler(null, true, true);
			v.visitedOnFocusEventHandler(null, true, true);
			v.visitedOnBlurEventHandler(null, true, true);
			v.visitedOnAddedEventHandler(null, true, true);
			v.visitedOnEditedEventHandler(null, true, true);
			v.visitedOnRemovedEventHandler(null, true, true);
			v.visitedOnSelectedEventHandler(null, true, true);
			v.visitedOnPickedEventHandler(null, true, true);
			v.visitedOnClearedEventHandler(null, true, true);
			v.visitedSidebar(null, true, true);
		});
	}

	@Test
	void reportAction_invalidDocument_throws() {
		ActionImpl action = new ActionImpl();
		action.setResourceName("SalesReport");

		ParameterImpl moduleParam = new ParameterImpl();
		moduleParam.setName(AbstractWebContext.MODULE_NAME);
		moduleParam.setValue("missingModule");
		action.getParameters().add(moduleParam);

		ParameterImpl documentParam = new ParameterImpl();
		documentParam.setName(AbstractWebContext.DOCUMENT_NAME);
		documentParam.setValue("MissingDocument");
		action.getParameters().add(documentParam);

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitReportAction(action));
	}

	@Test
	void onChanged_rerenderNotLast_throws() {
		Changeable changeable = mock(Changeable.class);
		when(changeable.getBinding()).thenReturn("status");
		when(changeable.getChangedActions()).thenReturn(java.util.List.of(new RerenderEventAction(), new RerenderEventAction()));

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitOnChangedEventHandler(changeable, true, true));
	}

	@Test
	void onFocus_serverNotLast_throws() {
		Focusable focusable = mock(Focusable.class);
		ServerSideActionEventAction server = new ServerSideActionEventAction();
		server.setActionName("doIt");
		when(focusable.getFocusActions()).thenReturn(java.util.List.of(server, new RerenderEventAction()));

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitOnFocusEventHandler(focusable, true, true));
	}

	@Test
	void onBlur_singleServerLast_noException() {
		Focusable focusable = mock(Focusable.class);
		ServerSideActionEventAction server = new ServerSideActionEventAction();
		server.setActionName("doIt");
		when(focusable.getBlurActions()).thenReturn(java.util.List.of(server));

		ViewValidator v = newValidator();
		assertDoesNotThrow(() -> v.visitOnBlurEventHandler(focusable, true, true));
	}

	@Test
	void onAdded_onRemoved_onSelected_serverNotLast_throw() {
		ServerSideActionEventAction server = new ServerSideActionEventAction();
		server.setActionName("doIt");
		java.util.List<EventAction> actions = java.util.List.of(server, new RerenderEventAction());

		Addable addable = mock(Addable.class);
		when(addable.getAddedActions()).thenReturn(actions);

		Removable removable = mock(Removable.class);
		when(removable.getRemovedActions()).thenReturn(actions);

		Selectable selectable = mock(Selectable.class);
		when(selectable.getSelectedActions()).thenReturn(actions);

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitOnAddedEventHandler(addable, true, true));
		assertThrows(MetaDataException.class, () -> v.visitOnRemovedEventHandler(removable, true, true));
		assertThrows(MetaDataException.class, () -> v.visitOnSelectedEventHandler(selectable, true, true));
	}

	@Test
	void serverSideEventAction_unknownAction_throws() {
		ServerSideActionEventAction server = new ServerSideActionEventAction();
		server.setActionName("missingAction");

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitServerSideActionEventAction(server, true, true));
	}

	@Test
	void serverSideEventAction_existingAction_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("existingAction");
		view.putAction(action);

		ServerSideActionEventAction server = new ServerSideActionEventAction();
		server.setActionName("existingAction");

		ViewValidator v = newValidator();
		assertDoesNotThrow(() -> v.visitServerSideActionEventAction(server, true, true));
		assertDoesNotThrow(() -> v.visitRerenderEventAction(new RerenderEventAction(), null, true, true));
	}

	@Test
	void sidebar_requiresWidth_throws() {
		Sidebar sidebar = new Sidebar();

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitSidebar(sidebar, true, true));
	}

	@Test
	void sidebar_withWidth_noException() {
		Sidebar sidebar = new Sidebar();
		sidebar.setPixelWidth(Integer.valueOf(300));

		ViewValidator v = newValidator();
		assertDoesNotThrow(() -> v.visitSidebar(sidebar, true, true));
	}

	@Test
	void onPicked_rerenderNotLast_throws() {
		LookupDescription lookup = mock(LookupDescription.class);
		when(lookup.getBinding()).thenReturn("contact");
		when(lookup.getPickedActions()).thenReturn(java.util.List.of(new RerenderEventAction(), new RerenderEventAction()));

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitOnPickedEventHandler(lookup, true, true));
	}

	@Test
	void onCleared_singleServerLast_noException() {
		LookupDescription lookup = mock(LookupDescription.class);
		when(lookup.getBinding()).thenReturn("contact");
		ServerSideActionEventAction server = new ServerSideActionEventAction();
		server.setActionName("existingAction");
		when(lookup.getClearedActions()).thenReturn(java.util.List.of(server));

		ActionImpl action = new ActionImpl();
		action.setName("existingAction");
		view.putAction(action);

		ViewValidator v = newValidator();
		assertDoesNotThrow(() -> v.visitOnClearedEventHandler(lookup, true, true));
	}

	@Test
	void setAndToggleEventActions_invalidBinding_throw() {
		SetDisabledEventAction setDisabled = new SetDisabledEventAction();
		setDisabled.setBinding("missingBool");
		setDisabled.setDisabledConditionName("flag");

		SetInvisibleEventAction setInvisible = new SetInvisibleEventAction();
		setInvisible.setBinding("missingBool");
		setInvisible.setInvisibleConditionName("flag");

		ToggleDisabledEventAction toggleDisabled = new ToggleDisabledEventAction();
		toggleDisabled.setBinding("missingBool");

		ToggleVisibilityEventAction toggleVisibility = new ToggleVisibilityEventAction();
		toggleVisibility.setBinding("missingBool");

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitSetDisabledEventAction(setDisabled, true, true));
		assertThrows(MetaDataException.class, () -> v.visitSetInvisibleEventAction(setInvisible, true, true));
		assertThrows(MetaDataException.class, () -> v.visitToggleDisabledEventAction(toggleDisabled, true, true));
		assertThrows(MetaDataException.class, () -> v.visitToggleVisibilityEventAction(toggleVisibility, true, true));
	}

	@Test
	void saveAndZoomOutActionVisitors_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("A");

		ViewValidator v = newValidator();
		assertDoesNotThrow(() -> v.visitSaveAction(action));
		assertDoesNotThrow(() -> v.visitZoomOutAction(action));
	}

	@Test
	void onEdited_onRemoved_onSelected_listGrid_serverNotLast_throw() {
		ServerSideActionEventAction server = new ServerSideActionEventAction();
		server.setActionName("existingAction");

		ListGrid grid = new ListGrid();
		grid.setQueryName("Q");
		grid.getEditedActions().add(server);
		grid.getEditedActions().add(new RerenderEventAction());
		grid.getRemovedActions().add(server);
		grid.getRemovedActions().add(new RerenderEventAction());
		grid.getSelectedActions().add(server);
		grid.getSelectedActions().add(new RerenderEventAction());

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitOnEditedEventHandler(grid, true, true));
		assertThrows(MetaDataException.class, () -> v.visitOnRemovedEventHandler(grid, true, true));
		assertThrows(MetaDataException.class, () -> v.visitOnSelectedEventHandler(grid, true, true));
	}

	@Test
	void onCleared_rerenderNotLast_throws() {
		LookupDescription lookup = mock(LookupDescription.class);
		when(lookup.getBinding()).thenReturn("contact");
		when(lookup.getClearedActions()).thenReturn(java.util.List.of(new RerenderEventAction(), new RerenderEventAction()));

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitOnClearedEventHandler(lookup, true, true));
	}

	@Test
	void onPicked_singleServerLast_noException() {
		LookupDescription lookup = mock(LookupDescription.class);
		when(lookup.getBinding()).thenReturn("contact");
		ServerSideActionEventAction server = new ServerSideActionEventAction();
		server.setActionName("existingAction");
		when(lookup.getPickedActions()).thenReturn(java.util.List.of(server));

		ActionImpl action = new ActionImpl();
		action.setName("existingAction");
		view.putAction(action);

		ViewValidator v = newValidator();
		assertDoesNotThrow(() -> v.visitOnPickedEventHandler(lookup, true, true));
	}

	@Test
	void sidebar_unknownInvisibleCondition_throws() {
		Sidebar sidebar = new Sidebar();
		sidebar.setPixelWidth(Integer.valueOf(300));
		sidebar.setInvisibleConditionName("missingCondition");

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitSidebar(sidebar, true, true));
	}

	@Test
	void visitLabel_withNoBindings_noException() {
		Label label = new Label();

		ViewValidator v = newValidator();
		assertDoesNotThrow(() -> v.visitLabel(label, true, true));
	}

	@Test
	void visitContentLink_withNoBinding_noException() {
		ContentLink link = new ContentLink();

		ViewValidator v = newValidator();
		assertDoesNotThrow(() -> v.visitContentLink(link, true, true));
	}

	@Test
	void visitListGrid_withoutQueryOrModel_throws() {
		ListGrid grid = new ListGrid();

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitListGrid(grid, true, true));
	}

	@Test
	void visitListRepeater_withoutQueryOrModel_throws() {
		ListRepeater repeater = new ListRepeater();

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitListRepeater(repeater, true, true));
	}

	@Test
	void visitTreeGrid_withoutQueryOrModel_throws() {
		TreeGrid grid = new TreeGrid();

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitTreeGrid(grid, true, true));
	}

	@Test
	void visitZoomIn_withoutBinding_throws() {
		ZoomIn zoomIn = new ZoomIn();

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitZoomIn(zoomIn, true, true));
	}

	@Test
	void sidebar_knownInvisibleCondition_noException() {
		document.getConditions().put("flag", new ConditionImpl());
		Sidebar sidebar = new Sidebar();
		sidebar.setPixelWidth(Integer.valueOf(300));
		sidebar.setInvisibleConditionName("flag");

		ViewValidator v = newValidator();
		assertDoesNotThrow(() -> v.visitSidebar(sidebar, true, true));
	}

	@Test
	void visitInputWidgets_withoutBinding_throw() {
		ViewValidator v = newValidator();
		CheckBox checkBox = new CheckBox();
		ColourPicker colourPicker = new ColourPicker();
		Combo combo = new Combo();
		ContentImage contentImage = new ContentImage();
		ContentSignature contentSignature = new ContentSignature();
		HTML html = new HTML();
		Password password = new Password();
		Radio radio = new Radio();
		RichText richText = new RichText();
		Slider slider = new Slider();
		Spinner spinner = new Spinner();
		TextArea textArea = new TextArea();
		TextField textField = new TextField();

		assertThrows(MetaDataException.class, () -> v.visitCheckBox(checkBox, true, true));
		assertThrows(MetaDataException.class, () -> v.visitColourPicker(colourPicker, true, true));
		assertThrows(MetaDataException.class, () -> v.visitCombo(combo, true, true));
		assertThrows(MetaDataException.class, () -> v.visitContentImage(contentImage, true, true));
		assertThrows(MetaDataException.class, () -> v.visitContentSignature(contentSignature, true, true));
		assertThrows(MetaDataException.class, () -> v.visitHTML(html, true, true));
		assertThrows(MetaDataException.class, () -> v.visitPassword(password, true, true));
		assertThrows(MetaDataException.class, () -> v.visitRadio(radio, true, true));
		assertThrows(MetaDataException.class, () -> v.visitRichText(richText, true, true));
		assertThrows(MetaDataException.class, () -> v.visitSlider(slider, true, true));
		assertThrows(MetaDataException.class, () -> v.visitSpinner(spinner, true, true));
		assertThrows(MetaDataException.class, () -> v.visitTextArea(textArea, true, true));
		assertThrows(MetaDataException.class, () -> v.visitTextField(textField, true, true));
	}

	@Test
	void visitListGrid_withQueryAndModel_throws() {
		ListGrid grid = new ListGrid();
		grid.setQueryName("Q");
		grid.setModelName("M");

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitListGrid(grid, true, true));
	}

	@Test
	void visitListRepeater_withQueryAndModel_throws() {
		ListRepeater repeater = new ListRepeater();
		repeater.setQueryName("Q");
		repeater.setModelName("M");

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitListRepeater(repeater, true, true));
	}

	@Test
	void visitTreeGrid_withQueryAndModel_throws() {
		TreeGrid grid = new TreeGrid();
		grid.setQueryName("Q");
		grid.setModelName("M");

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitTreeGrid(grid, true, true));
	}

	@Test
	void visitListGrid_withInvalidListModel_throws() {
		ListGrid grid = new ListGrid();
		grid.setModelName("badModel");

		when(repository.getListModel(any(), any(), eq("badModel"), eq(false))).thenThrow(new RuntimeException("missing model"));

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitListGrid(grid, true, true));
	}

	@Test
	void visitMap_withInvalidModel_throws() {
		MapDisplay map = new MapDisplay();
		map.setModelName("badMapModel");

		when(repository.getMapModel(any(), any(), eq("badMapModel"), eq(false))).thenThrow(new RuntimeException("missing model"));

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitMap(map, true, true));
	}

	@Test
	void visitDynamicStaticAndDialogWidgets_noException() {
		org.skyve.impl.metadata.view.widget.DynamicImage dynamic = new org.skyve.impl.metadata.view.widget.DynamicImage();
		dynamic.setName("logo");
		StaticImage stat = new StaticImage();
		stat.setRelativeFile("images/logo.png");
		DialogButton dialog = new DialogButton();
		dialog.setDialogName("myDialog");

		ViewValidator v = newValidator();
		assertDoesNotThrow(() -> v.visitDynamicImage(dynamic, true, true));
		assertDoesNotThrow(() -> v.visitStaticImage(stat, true, true));
		assertDoesNotThrow(() -> v.visitDialogButton(dialog, true, true));
	}

	@Test
	void visitDataGridAndDataRepeater_withoutBinding_throw() {
		DataGrid grid = new DataGrid();
		DataRepeater repeater = new DataRepeater();

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitDataGrid(grid, true, true));
		assertThrows(MetaDataException.class, () -> v.visitDataRepeater(repeater, true, true));
	}

	@Test
	void visitProgressComparisonAndMembership_withoutBinding_throw() {
		ProgressBar progressBar = new ProgressBar();
		Comparison comparison = new Comparison();
		CheckMembership checkMembership = new CheckMembership();
		ListMembership listMembership = new ListMembership();

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitProgressBar(progressBar, true, true));
		assertThrows(MetaDataException.class, () -> v.visitComparison(comparison, true, true));
		assertThrows(MetaDataException.class, () -> v.visitCheckMembership(checkMembership, true, true));
		assertThrows(MetaDataException.class, () -> v.visitListMembership(listMembership, true, true));
	}

	@Test
	void visitGeometryAndGeometryMap_withoutBinding_throw() {
		Geometry geometry = new Geometry();
		GeometryMap geometryMap = new GeometryMap();

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitGeometry(geometry, true, true));
		assertThrows(MetaDataException.class, () -> v.visitGeometryMap(geometryMap, true, true));
	}

	@Test
	void visitLookupDescription_withoutBindingAndQuery_throws() {
		LookupDescription lookup = new LookupDescription();
		lookup.setDescriptionBinding(Bean.BIZ_KEY);

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitLookupDescription(lookup, true, true));
	}

	@Test
	void visitListGrid_filterParameterWithColon_throws() {
		ListGrid grid = new ListGrid();
		grid.setModelName("existingModel");

		FilterParameter filter = mock(FilterParameter.class);
		when(filter.getFilterBinding()).thenReturn("status:bad");
		when(filter.getValueBinding()).thenReturn(null);
		grid.getFilterParameters().add(filter);

		ListModel<org.skyve.domain.Bean> model = mock(ListModel.class);
		when(model.getDrivingDocument()).thenReturn(null);
		when(repository.getListModel(any(), any(), eq("existingModel"), eq(false))).thenReturn(model);

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitListGrid(grid, true, true));
	}

	@Test
	void visitListRepeater_parameterWithColon_throws() {
		ListRepeater repeater = new ListRepeater();
		repeater.setModelName("existingModel");

		ParameterImpl parameter = new ParameterImpl();
		parameter.setName("bad:param");
		repeater.getParameters().add(parameter);

		ListModel<org.skyve.domain.Bean> model = mock(ListModel.class);
		when(model.getDrivingDocument()).thenReturn(null);
		when(repository.getListModel(any(), any(), eq("existingModel"), eq(false))).thenReturn(model);

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitListRepeater(repeater, true, true));
	}

	@Test
	void visitListGrid_withUnknownQuery_throws() {
		ListGrid grid = new ListGrid();
		grid.setQueryName("missingQuery");

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitListGrid(grid, true, true));
	}

	@Test
	void visitListGrid_withValidQuery_noException() {
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		when(module.getMetaDataQuery("existingQuery")).thenReturn(query);
		when(query.getDocumentModule(customer)).thenReturn(module);
		when(query.getDocumentName()).thenReturn("TestDoc");
		when(module.getDocument(customer, "TestDoc")).thenReturn(document);

		ListGrid grid = new ListGrid();
		grid.setQueryName("existingQuery");

		ViewValidator v = newValidator();
		assertDoesNotThrow(() -> v.visitListGrid(grid, true, true));
	}

	@Test
	void visitFormColumn_withInvalidResponsiveWidth_throws() {
		FormColumn column = new FormColumn();
		column.setResponsiveWidth(Integer.valueOf(-1));

		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, () -> v.visitFormColumn(column, true, true));
	}

	@Test
	void visitDataGridAndDataRepeater_withCollectionBinding_noException() {
		CollectionImpl children = new CollectionImpl();
		children.setName("children");
		children.setDocumentName("ChildDoc");
		children.setType(org.skyve.metadata.model.document.Collection.CollectionType.child);
		document.putAttribute(children);

		DataGrid grid = new DataGrid();
		grid.setBinding("children");

		DataRepeater repeater = new DataRepeater();
		repeater.setBinding("children");

		ViewValidator v = newValidator();
		assertDoesNotThrow(() -> v.visitDataGrid(grid, true, true));
		assertDoesNotThrow(() -> v.visitDataRepeater(repeater, true, true));
	}

	@Test
	void visitNoOpWidgets_noException() {
		DataGridContainerColumn containerColumn = new DataGridContainerColumn();
		Inject inject = new Inject();

		ViewValidator v = newValidator();
		assertDoesNotThrow(() -> v.visitDataGridContainerColumn(containerColumn, true, true));
		assertDoesNotThrow(() -> v.visitInject(inject, true, true));
	}
}
