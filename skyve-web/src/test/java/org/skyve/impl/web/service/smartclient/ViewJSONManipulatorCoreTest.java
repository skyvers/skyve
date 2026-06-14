package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.InjectBinding;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.reference.ExternalReference;
import org.skyve.impl.metadata.view.reference.ReferenceTarget;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Invisible;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.metadata.user.User;
import org.skyve.util.JSON;
import org.skyve.web.BackgroundTask;

@SuppressWarnings("static-method")
class ViewJSONManipulatorCoreTest {
	@Test
	void visitViewAddsCoreConditionBindings() throws Exception {
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), null, 2, 3);

		manipulator.visitView();

		ViewBindings bindingTree = bindingTree(manipulator);
		assertTrue(bindingTree.getBindings().contains(Bean.PERSISTED_KEY));
		assertTrue(bindingTree.getBindings().contains(Bean.CREATED_KEY));
		assertTrue(bindingTree.getBindings().contains(Bean.NOT_CREATED_KEY));
	}

	@Test
	void visitTabPaneAddsConditionsAndSelectedIndexBinding() throws Exception {
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), null, 0, 0);
		TabPane tabPane = new TabPane();
		tabPane.setInvisibleConditionName("invisibleWhen");
		tabPane.setDisabledConditionName("disabledWhen");
		tabPane.setSelectedTabIndexBinding("selectedTab");

		manipulator.visitTabPane(tabPane, true, true);

		ViewBindings bindingTree = bindingTree(manipulator);
		assertTrue(bindingTree.getBindings().contains("invisibleWhen"));
		assertTrue(bindingTree.getBindings().contains("disabledWhen"));
		assertTrue(bindingTree.getBindings().contains("selectedTab"));
	}

	@Test
	void visitedFormIncrementsTheCorrectIdCounter() throws Exception {
		Form form = new Form();
		ViewJSONManipulator createManipulator = newManipulator(ViewType.create.toString(), null, 10, 20);
		createManipulator.visitedForm(form, true, true);
		assertEquals(10, intField(createManipulator, "editIdCounter"));
		assertEquals(21, intField(createManipulator, "createIdCounter"));

		ViewJSONManipulator editManipulator = newManipulator(ViewType.edit.toString(), null, 10, 20);
		editManipulator.visitedForm(form, true, true);
		assertEquals(11, intField(editManipulator, "editIdCounter"));
		assertEquals(20, intField(editManipulator, "createIdCounter"));
	}

	@Test
	void visibleAndEnabledNegateBeanConditionEvaluation() {
		Bean bean = mock(Bean.class);
		doReturn(Boolean.TRUE).when(bean).evaluateCondition("hideWhen");
		doReturn(Boolean.FALSE).when(bean).evaluateCondition("disableWhen");

		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), bean, 0, 0);

		Invisible invisible = mock(Invisible.class);
		when(invisible.getInvisibleConditionName()).thenReturn("hideWhen");
		assertFalse(manipulator.visible(invisible));

		Disableable disableable = mock(Disableable.class);
		when(disableable.getDisabledConditionName()).thenReturn("disableWhen");
		assertTrue(manipulator.enabled(disableable));
	}

	@Test
	void addConditionSkipsTrueFalseAndAddsDynamicCondition() throws Exception {
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), null, 0, 0);

		manipulator.addCondition(null);
		manipulator.addCondition("true");
		manipulator.addCondition("false");
		manipulator.addCondition("showWhen");

		ViewBindings bindingTree = bindingTree(manipulator);
		assertFalse(bindingTree.getBindings().contains("true"));
		assertFalse(bindingTree.getBindings().contains("false"));
		assertTrue(bindingTree.getBindings().contains("showWhen"));
	}

	@Test
	void addNamedAndAnonymousFormatStorePerBindingPrefixEntries() throws Exception {
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), null, 0, 0);

		manipulator.addNamedFormat("myFormat", "display:false;Hi", false, Sanitisation.none);
		manipulator.addAnonymousFormat("display:true;Anon", true, Sanitisation.text);

		Map<String, Map<String, ViewFormat>> formats = formats(manipulator);
		Map<String, ViewFormat> rootFormats = formats.get("");
		assertNotNull(rootFormats);

		ViewFormat named = rootFormats.get("myFormat");
		assertNotNull(named);
		assertEquals("display:false;Hi", named.getFormat());
		assertFalse(named.isEscape());
		assertEquals(Sanitisation.none, named.getSanitise());

		ViewFormat anonymous = rootFormats.get("_0");
		assertNotNull(anonymous);
		assertEquals("display:true;Anon", anonymous.getFormat());
		assertTrue(anonymous.isEscape());
		assertEquals(Sanitisation.text, anonymous.getSanitise());
	}

	@Test
	void visitSimpleInputWidgetsAddsMutableBindingsAndConditions() throws Exception {
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), visibleBean(), 0, 0);

		CheckBox checkBox = new CheckBox();
		checkBox.setBinding("active");
		checkBox.setDisabledConditionName("activeDisabled");
		checkBox.setInvisibleConditionName("activeHidden");
		manipulator.visitCheckBox(checkBox, true, true);

		ColourPicker colour = new ColourPicker();
		colour.setBinding("colour");
		colour.setDisabledConditionName("colourDisabled");
		colour.setInvisibleConditionName("colourHidden");
		manipulator.visitColourPicker(colour, true, true);

		RichText richText = new RichText();
		richText.setBinding("description");
		richText.setSanitise(Sanitisation.relaxed);
		richText.setDisabledConditionName("descriptionDisabled");
		richText.setInvisibleConditionName("descriptionHidden");
		manipulator.visitRichText(richText, true, true);

		HTML html = new HTML();
		html.setBinding("markup");
		html.setSanitise(Sanitisation.none);
		html.setDisabledConditionName("markupDisabled");
		html.setInvisibleConditionName("markupHidden");
		manipulator.visitHTML(html, true, true);

		ViewBindings bindingTree = bindingTree(manipulator);
		assertMutableBinding(bindingTree, "active", Sanitisation.none);
		assertMutableBinding(bindingTree, "colour", Sanitisation.none);
		assertMutableBinding(bindingTree, "description", Sanitisation.relaxed);
		assertMutableBinding(bindingTree, "markup", Sanitisation.none);
		assertTrue(bindingTree.getBindings().contains("activeDisabled"));
		assertTrue(bindingTree.getBindings().contains("activeHidden"));
		assertTrue(bindingTree.getBindings().contains("colourDisabled"));
		assertTrue(bindingTree.getBindings().contains("colourHidden"));
		assertTrue(bindingTree.getBindings().contains("descriptionDisabled"));
		assertTrue(bindingTree.getBindings().contains("descriptionHidden"));
		assertTrue(bindingTree.getBindings().contains("markupDisabled"));
		assertTrue(bindingTree.getBindings().contains("markupHidden"));
	}

	@Test
	void visitTextualAndNumericInputWidgetsAddsExpectedSanitisation() throws Exception {
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), null, 0, 0);

		Password password = new Password();
		password.setBinding("password");
		manipulator.visitPassword(password, true, true);

		Radio radio = new Radio();
		radio.setBinding("choice");
		manipulator.visitRadio(radio, true, true);

		Slider slider = new Slider();
		slider.setBinding("score");
		manipulator.visitSlider(slider, true, true);

		Spinner spinner = new Spinner();
		spinner.setBinding("quantity");
		manipulator.visitSpinner(spinner, true, true);

		TextArea textArea = new TextArea();
		textArea.setBinding("notes");
		manipulator.visitTextArea(textArea, true, true);

		TextField textField = new TextField();
		textField.setBinding("name");
		manipulator.visitTextField(textField, true, true);

		ViewBindings bindingTree = bindingTree(manipulator);
		assertMutableBinding(bindingTree, "password", Sanitisation.none);
		assertMutableBinding(bindingTree, "choice", Sanitisation.none);
		assertMutableBinding(bindingTree, "score", Sanitisation.text);
		assertMutableBinding(bindingTree, "quantity", Sanitisation.text);
		assertMutableBinding(bindingTree, "notes", Sanitisation.none);
		assertMutableBinding(bindingTree, "name", Sanitisation.none);
	}

	@Test
	void visitContentWidgetsAddsFileBindingsAndFrameworkIdentityBindings() throws Exception {
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), visibleBean(), 0, 0);

		ContentImage image = new ContentImage();
		image.setBinding("photo");
		image.setDisabledConditionName("photoDisabled");
		image.setInvisibleConditionName("photoHidden");
		manipulator.visitContentImage(image, true, true);

		ContentSignature signature = new ContentSignature();
		signature.setBinding("signature");
		signature.setDisabledConditionName("signatureDisabled");
		signature.setInvisibleConditionName("signatureHidden");
		manipulator.visitContentSignature(signature, true, true);

		ContentLink link = new ContentLink();
		link.setBinding("attachment");
		link.setDisabledConditionName("attachmentDisabled");
		link.setInvisibleConditionName("attachmentHidden");
		manipulator.visitContentLink(link, true, true);

		ViewBindings bindingTree = bindingTree(manipulator);
		assertMutableBinding(bindingTree, "photo", Sanitisation.text);
		assertMutableBinding(bindingTree, "signature", Sanitisation.text);
		assertMutableBinding(bindingTree, "attachment", Sanitisation.text);
		assertTrue(bindingTree.getBindings().contains(Bean.MODULE_KEY));
		assertTrue(bindingTree.getBindings().contains(Bean.DOCUMENT_KEY));
		assertTrue(bindingTree.getBindings().contains(Bean.DATA_GROUP_ID));
		assertTrue(bindingTree.getBindings().contains(Bean.USER_ID));
		assertTrue(bindingTree.getBindings().contains("photoDisabled"));
		assertTrue(bindingTree.getBindings().contains("photoHidden"));
		assertTrue(bindingTree.getBindings().contains("signatureDisabled"));
		assertTrue(bindingTree.getBindings().contains("signatureHidden"));
		assertTrue(bindingTree.getBindings().contains("attachmentDisabled"));
		assertTrue(bindingTree.getBindings().contains("attachmentHidden"));
	}

	@Test
	void visitReadOnlyApplyInputWidgetsAddsConditionsButSkipsMutableBinding() throws Exception {
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), visibleBean(), 0, 0, true);

		TextField readOnlyText = new TextField();
		readOnlyText.setBinding("name");
		readOnlyText.setEditable(Boolean.FALSE);
		readOnlyText.setDisabledConditionName("nameDisabled");
		readOnlyText.setInvisibleConditionName("nameHidden");
		manipulator.visitTextField(readOnlyText, true, true);

		ContentLink readOnlyLink = new ContentLink();
		readOnlyLink.setBinding("attachment");
		readOnlyLink.setEditable(Boolean.FALSE);
		readOnlyLink.setDisabledConditionName("attachmentDisabled");
		readOnlyLink.setInvisibleConditionName("attachmentHidden");
		manipulator.visitContentLink(readOnlyLink, true, true);

		ViewBindings bindingTree = bindingTree(manipulator);
		assertFalse(bindingTree.getBindings().contains("name"));
		assertFalse(bindingTree.getBindings().contains("attachment"));
		assertTrue(bindingTree.getBindings().contains("nameDisabled"));
		assertTrue(bindingTree.getBindings().contains("nameHidden"));
		assertTrue(bindingTree.getBindings().contains("attachmentDisabled"));
		assertTrue(bindingTree.getBindings().contains("attachmentHidden"));
	}

	@Test
	void visitZoomInAndProgressBarAddReadOnlyBindings() throws Exception {
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), visibleBean(), 0, 0);

		ZoomIn zoomIn = new ZoomIn();
		zoomIn.setBinding("contact");
		zoomIn.setDisabledConditionName("contactDisabled");
		zoomIn.setInvisibleConditionName("contactHidden");
		manipulator.visitZoomIn(zoomIn, true, true);

		ProgressBar progressBar = new ProgressBar();
		progressBar.setBinding("completion");
		progressBar.setInvisibleConditionName("completionHidden");
		manipulator.visitProgressBar(progressBar, true, true);

		ViewBindings bindingTree = bindingTree(manipulator);
		assertReadOnlyBinding(bindingTree, "contact", Sanitisation.text);
		assertReadOnlyBinding(bindingTree, "contact.bizModule", Sanitisation.none);
		assertReadOnlyBinding(bindingTree, "contact.bizDocument", Sanitisation.none);
		assertReadOnlyBinding(bindingTree, "completion", Sanitisation.none);
		assertTrue(bindingTree.getBindings().contains("contactDisabled"));
		assertTrue(bindingTree.getBindings().contains("contactHidden"));
		assertTrue(bindingTree.getBindings().contains("completionHidden"));
	}

	@Test
	void visitListWidgetsAddsCrudConditionsAndSelectionBindings() throws Exception {
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), visibleBean(), 0, 0);

		ListGrid listGrid = new ListGrid();
		listGrid.setDisabledConditionName("listDisabled");
		listGrid.setInvisibleConditionName("listHidden");
		listGrid.setDisableAddConditionName("listAddDisabled");
		listGrid.setDisableEditConditionName("listEditDisabled");
		listGrid.setDisableZoomConditionName("listZoomDisabled");
		listGrid.setDisableRemoveConditionName("listRemoveDisabled");
		listGrid.setPostRefreshConditionName("listPostRefresh");
		listGrid.setSelectedIdBinding("selectedListId");
		manipulator.visitListGrid(listGrid, true, true);

		ListRepeater repeater = new ListRepeater();
		repeater.setInvisibleConditionName("repeaterHidden");
		repeater.setPostRefreshConditionName("repeaterPostRefresh");
		manipulator.visitListRepeater(repeater, true, true);

		ViewBindings bindingTree = bindingTree(manipulator);
		assertTrue(bindingTree.getBindings().contains("listDisabled"));
		assertTrue(bindingTree.getBindings().contains("listHidden"));
		assertTrue(bindingTree.getBindings().contains("listAddDisabled"));
		assertTrue(bindingTree.getBindings().contains("listEditDisabled"));
		assertTrue(bindingTree.getBindings().contains("listZoomDisabled"));
		assertTrue(bindingTree.getBindings().contains("listRemoveDisabled"));
		assertTrue(bindingTree.getBindings().contains("listPostRefresh"));
		assertMutableBinding(bindingTree, "selectedListId", Sanitisation.text);
		assertTrue(bindingTree.getBindings().contains("repeaterHidden"));
		assertTrue(bindingTree.getBindings().contains("repeaterPostRefresh"));
	}

	@Test
	void visitTreeGridAddsRootIdBinding() throws Exception {
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), visibleBean(), 0, 0);

		TreeGrid treeGrid = new TreeGrid();
		treeGrid.setDisabledConditionName("treeDisabled");
		treeGrid.setInvisibleConditionName("treeHidden");
		treeGrid.setDisableAddConditionName("treeAddDisabled");
		treeGrid.setDisableEditConditionName("treeEditDisabled");
		treeGrid.setDisableZoomConditionName("treeZoomDisabled");
		treeGrid.setDisableRemoveConditionName("treeRemoveDisabled");
		treeGrid.setPostRefreshConditionName("treePostRefresh");
		treeGrid.setSelectedIdBinding("selectedTreeId");
		treeGrid.setRootIdBinding("rootTreeId");
		manipulator.visitTreeGrid(treeGrid, true, true);

		ViewBindings bindingTree = bindingTree(manipulator);
		assertTrue(bindingTree.getBindings().contains("treeDisabled"));
		assertTrue(bindingTree.getBindings().contains("treeHidden"));
		assertTrue(bindingTree.getBindings().contains("treeAddDisabled"));
		assertTrue(bindingTree.getBindings().contains("treeEditDisabled"));
		assertTrue(bindingTree.getBindings().contains("treeZoomDisabled"));
		assertTrue(bindingTree.getBindings().contains("treeRemoveDisabled"));
		assertTrue(bindingTree.getBindings().contains("treePostRefresh"));
		assertMutableBinding(bindingTree, "selectedTreeId", Sanitisation.text);
		assertReadOnlyBinding(bindingTree, "rootTreeId", Sanitisation.text);
	}

	@Test
	void visitContainerAndMiscWidgetsAddsConditionsAndParameterBindings() throws Exception {
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), visibleBean(), 0, 0);

		Tab tab = new Tab();
		tab.setInvisibleConditionName("tabHidden");
		tab.setDisabledConditionName("tabDisabled");
		manipulator.visitTab(tab, true, true);

		VBox vbox = new VBox();
		vbox.setInvisibleConditionName("vboxHidden");
		manipulator.visitVBox(vbox, true, true);

		HBox hbox = new HBox();
		hbox.setInvisibleConditionName("hboxHidden");
		manipulator.visitHBox(hbox, true, true);

		Form form = new Form();
		form.setInvisibleConditionName("formHidden");
		form.setDisabledConditionName("formDisabled");
		manipulator.visitForm(form, true, true);

		MapDisplay map = new MapDisplay();
		map.setInvisibleConditionName("mapHidden");
		manipulator.visitMap(map, true, true);

		Chart chart = new Chart();
		chart.setInvisibleConditionName("chartHidden");
		manipulator.visitChart(chart, true, true);

		DialogButton dialogButton = new DialogButton();
		dialogButton.setInvisibleConditionName("dialogHidden");
		dialogButton.setDisabledConditionName("dialogDisabled");
		manipulator.visitDialogButton(dialogButton, true, true);

		Parameter parameter = mock(Parameter.class);
		when(parameter.getValueBinding()).thenReturn("parameterValue");
		manipulator.visitParameter(parameter, true, true);

		FilterParameter filterParameter = mock(FilterParameter.class);
		when(filterParameter.getValueBinding()).thenReturn("filterValue");
		manipulator.visitFilterParameter(filterParameter, true, true);

		ViewBindings bindingTree = bindingTree(manipulator);
		assertTrue(bindingTree.getBindings().contains("tabHidden"));
		assertTrue(bindingTree.getBindings().contains("tabDisabled"));
		assertTrue(bindingTree.getBindings().contains("vboxHidden"));
		assertTrue(bindingTree.getBindings().contains("hboxHidden"));
		assertTrue(bindingTree.getBindings().contains("formHidden"));
		assertTrue(bindingTree.getBindings().contains("formDisabled"));
		assertTrue(bindingTree.getBindings().contains("mapHidden"));
		assertTrue(bindingTree.getBindings().contains("chartHidden"));
		assertTrue(bindingTree.getBindings().contains("dialogHidden"));
		assertTrue(bindingTree.getBindings().contains("dialogDisabled"));
		assertReadOnlyBinding(bindingTree, "parameterValue", Sanitisation.none);
		assertReadOnlyBinding(bindingTree, "filterValue", Sanitisation.none);
	}

	@Test
	void visitButtonReadsActionConditionsFromOwningView() throws Exception {
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), null, 0, 0);
		ViewImpl view = (ViewImpl) objectField(manipulator, "view");
		ActionImpl action = new ActionImpl();
		action.setName("approve");
		action.setInvisibleConditionName("approveHidden");
		action.setDisabledConditionName("approveDisabled");
		view.putAction(action);

		Button button = new Button();
		button.setActionName("approve");
		manipulator.visitButton(button, true, true);

		ViewBindings bindingTree = bindingTree(manipulator);
		assertTrue(bindingTree.getBindings().contains("approveHidden"));
		assertTrue(bindingTree.getBindings().contains("approveDisabled"));
	}

	@Test
	void visitGeometryWidgetsHonoursDataWidgetAndApplyGuards() throws Exception {
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), visibleBean(), 0, 0);

		Geometry geometry = new Geometry();
		geometry.setBinding("shape");
		geometry.setInvisibleConditionName("shapeHidden");
		geometry.setDisabledConditionName("shapeDisabled");
		manipulator.visitGeometry(geometry, true, true);

		GeometryMap geometryMap = new GeometryMap();
		geometryMap.setBinding("location");
		geometryMap.setInvisibleConditionName("locationHidden");
		geometryMap.setDisabledConditionName("locationDisabled");
		manipulator.visitGeometryMap(geometryMap, true, true);

		ViewBindings bindingTree = bindingTree(manipulator);
		assertMutableBinding(bindingTree, "shape", Sanitisation.none);
		assertMutableBinding(bindingTree, "location", Sanitisation.none);
		assertTrue(bindingTree.getBindings().contains("shapeHidden"));
		assertTrue(bindingTree.getBindings().contains("shapeDisabled"));
		assertTrue(bindingTree.getBindings().contains("locationHidden"));
		assertTrue(bindingTree.getBindings().contains("locationDisabled"));

		ViewJSONManipulator dataWidgetManipulator = newManipulator(ViewType.edit.toString(), visibleBean(), 0, 0);
		setField(dataWidgetManipulator, "visitingDataWidget", Boolean.TRUE);
		dataWidgetManipulator.visitGeometry(geometry, true, true);
		assertFalse(bindingTree(dataWidgetManipulator).getBindings().contains("shape"));

		ViewJSONManipulator applyManipulator = newManipulator(ViewType.edit.toString(), visibleBean(), 0, 0, true);
		applyManipulator.visitGeometryMap(geometryMap, true, false);
		assertFalse(bindingTree(applyManipulator).getBindings().contains("location"));
	}

	@Test
	void dataWidgetHtmlVisitorsAppendFragmentsToSharedBuffer() throws Exception {
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), visibleBean(), 0, 0);
		setField(manipulator, "visitingDataWidget", Boolean.TRUE);
		setField(manipulator, "processingDataWidget", Boolean.TRUE);
		setField(manipulator, "htmlGuts", new StringBuilder());

		DynamicImage dynamicImage = new DynamicImage();
		dynamicImage.setName("avatar");
		dynamicImage.setPixelHeight(Integer.valueOf(44));
		dynamicImage.setImageInitialPixelWidth(Integer.valueOf(22));
		dynamicImage.setImageInitialPixelHeight(Integer.valueOf(33));
		dynamicImage.setInvisibleConditionName("avatarHidden");
		manipulator.visitDynamicImage(dynamicImage, true, true);

		DynamicImage initialSizeImage = new DynamicImage();
		initialSizeImage.setName("avatarInitial");
		initialSizeImage.setImageInitialPixelWidth(Integer.valueOf(28));
		initialSizeImage.setImageInitialPixelHeight(Integer.valueOf(29));
		manipulator.visitDynamicImage(initialSizeImage, true, true);

		StaticImage staticImage = new StaticImage();
		staticImage.setRelativeFile("icons/static.png");
		staticImage.setPixelWidth(Integer.valueOf(16));
		staticImage.setPixelHeight(Integer.valueOf(17));
		staticImage.setInvisibleConditionName("staticHidden");
		manipulator.visitStaticImage(staticImage, true, true);

		ContentImage contentImage = new ContentImage();
		contentImage.setBinding("photo");
		contentImage.setPixelWidth(Integer.valueOf(24));
		contentImage.setPixelHeight(Integer.valueOf(25));
		contentImage.setInvisibleConditionName("photoHidden");
		manipulator.visitContentImage(contentImage, true, true);

		Blurb blurb = new Blurb();
		blurb.setMarkup("Hello");
		blurb.setTextAlignment(HorizontalAlignment.right);
		blurb.setInvisibleConditionName("blurbHidden");
		manipulator.visitBlurb(blurb, true, true);

		Label bindingLabel = new Label();
		bindingLabel.setBinding("status");
		bindingLabel.setInvisibleConditionName("statusHidden");
		manipulator.visitLabel(bindingLabel, true, true);

		Label valueLabel = new Label();
		valueLabel.setValue("Ready");
		valueLabel.setInvisibleConditionName("true");
		manipulator.visitLabel(valueLabel, true, true);

		Label forLabel = new Label();
		forLabel.setFor("fallbackField");
		manipulator.visitLabel(forLabel, true, true);

		ExternalReference reference = new ExternalReference();
		reference.setHref("https://example.test/details");
		Link link = new Link();
		link.setReference(reference);
		link.setValue("Details");
		link.setInvisibleConditionName("detailsHidden");
		manipulator.visitLink(link, true, true);

		String html = objectField(manipulator, "htmlGuts").toString();
		assertTrue(html.contains("dynamic.png?_n=avatar"), html);
		assertTrue(html.contains("dynamic.png?_n=avatarInitial"), html);
		assertTrue(html.contains("_w=28"), html);
		assertTrue(html.contains("_h=29"), html);
		assertTrue(html.contains("resources?_n=icons/static.png"), html);
		assertTrue(html.contains("content?_n={photo}"), html);
		assertTrue(html.contains("<div"), html);
		assertTrue(html.contains("text-align:right"), html);
		assertTrue(html.contains("<span"), html);
		assertTrue(html.contains("{status}"), html);
		assertTrue(html.contains(">Ready</span>"), html);
		assertTrue(html.contains("display:none"), html);
		assertTrue(html.contains(">fallbackField</span>"), html);
		assertTrue(html.contains("<a href=\"https://example.test/details\""), html);
		assertTrue(html.contains("&nbsp;"), html);
	}

	@Test
	void nonDataImageVisitorsOnlyAddVisibilityConditions() throws Exception {
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), visibleBean(), 0, 0);

		DynamicImage dynamicImage = new DynamicImage();
		dynamicImage.setInvisibleConditionName("dynamicHidden");
		manipulator.visitDynamicImage(dynamicImage, true, true);

		StaticImage staticImage = new StaticImage();
		staticImage.setInvisibleConditionName("staticHidden");
		manipulator.visitStaticImage(staticImage, true, true);

		ViewBindings bindingTree = bindingTree(manipulator);
		assertTrue(bindingTree.getBindings().contains("dynamicHidden"));
		assertTrue(bindingTree.getBindings().contains("staticHidden"));
	}

	@Test
	void visitInjectHonoursReadOnlyFlagWhenAddingBindings() throws Exception {
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), null, 0, 0);

		Inject inject = new Inject();
		InjectBinding mutable = new InjectBinding();
		mutable.setBinding("scriptWritable");
		mutable.setReadOnly(Boolean.FALSE);
		InjectBinding readOnly = new InjectBinding();
		readOnly.setBinding("scriptReadOnly");
		readOnly.setReadOnly(Boolean.TRUE);
		inject.getBindings().add(mutable);
		inject.getBindings().add(readOnly);

		manipulator.visitInject(inject, true, true);

		ViewBindings bindingTree = bindingTree(manipulator);
		assertMutableBinding(bindingTree, "scriptWritable", Sanitisation.none);
		assertReadOnlyBinding(bindingTree, "scriptReadOnly", Sanitisation.none);
	}

	@Test
	void visitActionsAddsCommonActionConditions() throws Exception {
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), null, 0, 0);
		ActionImpl action = new ActionImpl();
		action.setDisabledConditionName("actionDisabled");
		action.setInvisibleConditionName("actionHidden");

		manipulator.visitCustomAction(action);

		ViewBindings bindingTree = bindingTree(manipulator);
		assertTrue(bindingTree.getBindings().contains("actionDisabled"));
		assertTrue(bindingTree.getBindings().contains("actionHidden"));
	}

	@Test
	void visitBlurbWithExpressionStoresAnonymousFormatAndCondition() throws Exception {
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), visibleBean(), 0, 0);
		Blurb blurb = new Blurb();
		blurb.setMarkup("Hello {name}");
		blurb.setEscape(Boolean.FALSE);
		blurb.setSanitise(Sanitisation.none);
		blurb.setInvisibleConditionName("blurbHidden");

		manipulator.visitBlurb(blurb, true, true);

		ViewFormat format = formats(manipulator).get("").get("_0");
		assertNotNull(format);
		assertEquals("Hello {name}", format.getFormat());
		assertFalse(format.isEscape());
		assertEquals(Sanitisation.none, format.getSanitise());
		assertTrue(bindingTree(manipulator).getBindings().contains("blurbHidden"));
	}

	@Test
	void visitBlurbAndLabelKeepFormatCounterInSyncWhenSkipped() throws Exception {
		ViewJSONManipulator hiddenManipulator = newManipulator(ViewType.edit.toString(), visibleBean(), 0, 0);
		Blurb hiddenBlurb = new Blurb();
		hiddenBlurb.setMarkup("Hidden {text}");
		hiddenBlurb.setInvisibleConditionName("hiddenWhen");
		hiddenManipulator.visitBlurb(hiddenBlurb, false, true);
		assertEquals(1, intField(hiddenManipulator, "formatCounter"));
		assertFalse(formats(hiddenManipulator).containsKey(""));

		ViewJSONManipulator disabledApplyManipulator = newManipulator(ViewType.edit.toString(), visibleBean(), 0, 0, true);
		Label disabledLabel = new Label();
		disabledLabel.setValue("Disabled {text}");
		disabledApplyManipulator.visitLabel(disabledLabel, true, false);
		assertEquals(1, intField(disabledApplyManipulator, "formatCounter"));
		assertFalse(formats(disabledApplyManipulator).containsKey(""));
	}

	@Test
	void visitLabelStoresExpressionAsFormatAndPlainBindingAsReadOnlyBinding() throws Exception {
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), visibleBean(), 0, 0);
		Label expressionLabel = new Label();
		expressionLabel.setValue("Hi {name}");
		expressionLabel.setEscape(Boolean.TRUE);
		expressionLabel.setSanitise(Sanitisation.text);
		expressionLabel.setInvisibleConditionName("expressionHidden");
		manipulator.visitLabel(expressionLabel, true, true);

		Label bindingLabel = new Label();
		bindingLabel.setBinding("status");
		bindingLabel.setEscape(Boolean.FALSE);
		bindingLabel.setSanitise(Sanitisation.none);
		bindingLabel.setInvisibleConditionName("statusHidden");
		manipulator.visitLabel(bindingLabel, true, true);

		ViewFormat format = formats(manipulator).get("").get("_0");
		assertNotNull(format);
		assertEquals("Hi {name}", format.getFormat());
		assertTrue(format.isEscape());
		assertEquals(Sanitisation.text, format.getSanitise());

		ViewBindings bindingTree = bindingTree(manipulator);
		assertReadOnlyBinding(bindingTree, "status", Sanitisation.none);
		assertTrue(bindingTree.getBindings().contains("expressionHidden"));
		assertTrue(bindingTree.getBindings().contains("statusHidden"));
	}

	@Test
	void visitLinkStoresExternalReferenceAsAnonymousFormatAndCondition() throws Exception {
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), null, 0, 0);
		ExternalReference reference = new ExternalReference();
		reference.setHref("https://example.test/help");
		Link link = new Link();
		link.setReference(reference);
		link.setValue("Help");
		link.setInvisibleConditionName("helpHidden");

		manipulator.visitLink(link, true, true);

		ViewFormat format = formats(manipulator).get("").get("_0");
		assertNotNull(format);
		assertTrue(format.getFormat().contains("<a href=\"https://example.test/help\""), format.getFormat());
		assertTrue(format.getFormat().contains(">Help</a>"), format.getFormat());
		assertFalse(format.isEscape());
		assertEquals(Sanitisation.none, format.getSanitise());
		assertTrue(bindingTree(manipulator).getBindings().contains("helpHidden"));
	}

	@Test
	void visitLinkWritesTargetsAndSelfClosingMarkup() throws Exception {
		ViewJSONManipulator blankTargetManipulator = newManipulator(ViewType.edit.toString(), null, 0, 0);
		ExternalReference blankReference = new ExternalReference();
		blankReference.setHref("https://example.test/blank");
		ReferenceTarget blankTarget = new ReferenceTarget();
		blankTarget.setType(ReferenceTarget.ReferenceTargetType.blankFrame);
		Link blankLink = new Link();
		blankLink.setReference(blankReference);
		blankLink.setTarget(blankTarget);
		blankTargetManipulator.visitLink(blankLink, true, true);
		String blankFormat = formats(blankTargetManipulator).get("").get("_0").getFormat();
		assertTrue(blankFormat.contains("target=\"_blank\""));
		assertTrue(blankFormat.endsWith("/>"));

		ViewJSONManipulator namedTargetManipulator = newManipulator(ViewType.edit.toString(), null, 0, 0);
		ExternalReference namedReference = new ExternalReference();
		namedReference.setHref("https://example.test/named");
		ReferenceTarget namedTarget = new ReferenceTarget();
		namedTarget.setType(ReferenceTarget.ReferenceTargetType.namedFame);
		namedTarget.setName("detailFrame");
		Link namedLink = new Link();
		namedLink.setReference(namedReference);
		namedLink.setTarget(namedTarget);
		namedLink.setValue("Named");
		namedTargetManipulator.visitLink(namedLink, true, true);
		String namedFormat = formats(namedTargetManipulator).get("").get("_0").getFormat();
		assertTrue(namedFormat.contains("target=\"detailFrame\""));
		assertTrue(namedFormat.contains(">Named</a>"));
	}

	@Test
	void addBindingsAndFormatValuesEscapesBeansAndFormatsMessages() throws Exception {
		TestOwnerBean owner = new TestOwnerBean();
		owner.setBizId("owner-1");
		owner.setText("<b>Alpha</b>");
		TestRelatedBean association = new TestRelatedBean();
		association.setBizId("related-1");
		owner.setAssociation(association);

		PersistentBean constructorBean = mock(PersistentBean.class);
		when(constructorBean.getBizId()).thenReturn("owner-1");
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), constructorBean, 0, 0);
		ViewBindings rootBindings = bindingTree(manipulator);
		rootBindings.putBinding("text", false, true, Sanitisation.none, false);
		rootBindings.putBinding("association", false, false, Sanitisation.none, false);
		manipulator.addNamedFormat("summary",
				"display:false;{CONTEXT};src=\"content?_n=&_doc=test\"",
				false,
				Sanitisation.none);

		Map<String, Object> values = new LinkedHashMap<>();
		manipulator.addBindingsAndFormatValues(rootBindings, owner, values, "ctx-9");

		assertEquals("&lt;b&gt;Alpha&lt;/b&gt;", values.get("text"));
		assertEquals("related-1", values.get("association"));
		assertEquals("display:none;ctx-9;src=\"images/blank.gif\"", values.get("summary"));
	}

	@Test
	void toJSONIncludesContextRedirectDirtyFlagAndMessages() throws Exception {
		PersistentBean constructorBean = mock(PersistentBean.class);
		when(constructorBean.getBizId()).thenReturn("biz-1");
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), constructorBean, 0, 0);

		TestWebContext webContext = new TestWebContext("ctx-1");
		PersistentBean currentBean = mock(PersistentBean.class);
		when(currentBean.getBizId()).thenReturn("biz-1");
		doReturn(Boolean.TRUE).when(currentBean).hasChanged();
		webContext.setCurrentBean(currentBean);
		webContext.setGrowls(List.of(Map.of("severity", "info", "summary", "ok")));
		webContext.setMessages(List.of(Map.of("severity", "warn", "summary", "note")));

		String json = manipulator.toJSON(webContext, "https://skyve.org/redirect");
		@SuppressWarnings("unchecked")
		Map<String, Object> result = (Map<String, Object>) JSON.unmarshall(json);

		assertEquals("https://skyve.org/redirect", result.get("_redirectUrl"));
		assertEquals("ctx-1biz-1", result.get(AbstractWebContext.CONTEXT_NAME));
		assertEquals(Boolean.TRUE, result.get("_changed"));
		assertTrue(result.containsKey("_growls"));
		assertTrue(result.containsKey("_messages"));
	}

	@Test
	void toJSONIncludesChildBindingsValueMapsAndComparisons() throws Exception {
		TestOwnerBean owner = new TestOwnerBean();
		owner.setBizId("owner-1");
		owner.setText("A & B");

		TestRelatedBean association = new TestRelatedBean();
		association.setBizId("assoc-1");
		owner.setAssociation(association);

		TestRelatedBean child = new TestRelatedBean();
		child.setBizId("child-1");
		owner.getChildren().add(child);

		PersistentBean constructorBean = mock(PersistentBean.class);
		when(constructorBean.getBizId()).thenReturn("owner-1");
		ViewJSONManipulator manipulator = newManipulator(ViewType.edit.toString(), constructorBean, 0, 0);
		setField(manipulator, "bean", owner);

		ViewBindings rootBindings = bindingTree(manipulator);
		rootBindings.putBinding("text", true, false, Sanitisation.text, false);
		DocumentImpl relatedDocument = new DocumentImpl();
		relatedDocument.setName("RelatedDoc");
		relatedDocument.setPersistent(new Persistent());
		rootBindings.putOrGetChild("association", relatedDocument);
		rootBindings.putOrGetChild("children", relatedDocument);

		Map<String, LinkedHashMap<String, String>> valueMaps = new LinkedHashMap<>();
		LinkedHashMap<String, String> mapEntries = new LinkedHashMap<>();
		mapEntries.put("one", "1");
		valueMaps.put("text", mapEntries);
		setField(manipulator, "valueMaps", valueMaps);

		Map<String, Iterable<Map<String, Object>>> comparisons = new LinkedHashMap<>();
		comparisons.put("comparisonKey", List.of(Map.of("lhs", "rhs")));
		setField(manipulator, "comparisons", comparisons);

		TestWebContext webContext = new TestWebContext("ctx-2");
		webContext.setCurrentBean(constructorBean);

		String json = manipulator.toJSON(webContext, null);
		@SuppressWarnings("unchecked")
		Map<String, Object> result = (Map<String, Object>) JSON.unmarshall(json);

		assertEquals("ctx-2owner-1", result.get(AbstractWebContext.CONTEXT_NAME));
		assertEquals("A & B", result.get("text"));
		assertTrue(result.containsKey("association"));
		assertTrue(result.containsKey("children"));
		assertTrue(result.containsKey("_valueMaps"));
		assertTrue(result.containsKey("comparisonKey"));
	}

	private static final class TestWebContext extends AbstractWebContext {
		private static final long serialVersionUID = 1L;
		private List<Map<String, String>> growls;
		private List<Map<String, String>> messages;

		private TestWebContext(String key) {
			super(key);
		}

		private void setGrowls(List<Map<String, String>> growls) {
			this.growls = growls;
		}

		private void setMessages(List<Map<String, String>> messages) {
			this.messages = messages;
		}

		@Override
		public List<Map<String, String>> getGrowls() {
			return growls;
		}

		@Override
		public List<Map<String, String>> getMessages() {
			return messages;
		}

		@Override
		public void message(org.skyve.domain.messages.MessageSeverity severity, String message) {
			// no-op for focused unit tests
		}

		@Override
		public void growl(org.skyve.domain.messages.MessageSeverity severity, String message) {
			// no-op for focused unit tests
		}

		@Override
		public void cacheConversation() {
			// no-op for focused unit tests
		}

		@Override
		public <T extends Bean> void background(Class<? extends BackgroundTask<T>> taskClass) {
			// no-op for focused unit tests
		}

		@Override
		public <T extends Bean> void backgroundWithoutCachingConversation(Class<? extends BackgroundTask<T>> taskClass) {
			// no-op for focused unit tests
		}
	}

	public static final class TestOwnerBean extends org.skyve.impl.domain.AbstractPersistentBean {
		private static final long serialVersionUID = 1L;

		private String text;
		private TestRelatedBean association;
		private final List<TestRelatedBean> children = new ArrayList<>();

		@Override
		public String getBizKey() {
			return getBizId();
		}

		@Override
		public String getBizModule() {
			return "testModule";
		}

		@Override
		public String getBizDocument() {
			return "OwnerDoc";
		}

		public String getText() {
			return text;
		}

		public void setText(String text) {
			this.text = text;
		}

		public TestRelatedBean getAssociation() {
			return association;
		}

		public void setAssociation(TestRelatedBean association) {
			this.association = association;
		}

		public List<TestRelatedBean> getChildren() {
			return children;
		}
	}

	public static final class TestRelatedBean extends org.skyve.impl.domain.AbstractPersistentBean {
		private static final long serialVersionUID = 1L;

		@Override
		public String getBizKey() {
			return getBizId();
		}

		@Override
		public String getBizModule() {
			return "testModule";
		}

		@Override
		public String getBizDocument() {
			return "RelatedDoc";
		}
	}

	private static ViewJSONManipulator newManipulator(String viewName, Bean bean, int editIdCounter, int createIdCounter) {
		return newManipulator(viewName, bean, editIdCounter, createIdCounter, false);
	}

	private static ViewJSONManipulator newManipulator(String viewName, Bean bean, int editIdCounter, int createIdCounter, boolean forApply) {
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(new CustomerImpl());

		ModuleImpl module = new ModuleImpl();
		module.setName("testModule");

		DocumentImpl document = new DocumentImpl();
		document.setName("TestDoc");
		document.setPersistent(new Persistent());

		ViewImpl view = new ViewImpl();
		view.setName(viewName);
		view.setTitle("Test Title");

		return new ViewJSONManipulator(user,
				module,
				document,
				view,
				"external",
				bean,
				editIdCounter,
				createIdCounter,
				forApply);
	}

	private static Bean visibleBean() {
		Bean bean = mock(Bean.class);
		doReturn(Boolean.FALSE).when(bean).evaluateCondition(anyString());
		return bean;
	}

	private static void assertMutableBinding(ViewBindings bindings, String binding, Sanitisation sanitisation) {
		ViewBinding viewBinding = bindings.getBinding(binding);
		assertNotNull(viewBinding, binding);
		assertTrue(viewBinding.isMutable(), binding);
		assertEquals(sanitisation, viewBinding.getSanitise(), binding);
	}

	private static void assertReadOnlyBinding(ViewBindings bindings, String binding, Sanitisation sanitisation) {
		ViewBinding viewBinding = bindings.getBinding(binding);
		assertNotNull(viewBinding, binding);
		assertFalse(viewBinding.isMutable(), binding);
		assertEquals(sanitisation, viewBinding.getSanitise(), binding);
	}

	private static ViewBindings bindingTree(ViewJSONManipulator manipulator) throws Exception {
		Field field = ViewJSONManipulator.class.getDeclaredField("bindingTree");
		field.setAccessible(true);
		return (ViewBindings) field.get(manipulator);
	}

	@SuppressWarnings("unchecked")
	private static Map<String, Map<String, ViewFormat>> formats(ViewJSONManipulator manipulator) throws Exception {
		Field field = ViewJSONManipulator.class.getDeclaredField("formats");
		field.setAccessible(true);
		return (Map<String, Map<String, ViewFormat>>) field.get(manipulator);
	}

	private static int intField(ViewJSONManipulator manipulator, String name) throws Exception {
		Field field = ViewJSONManipulator.class.getDeclaredField(name);
		field.setAccessible(true);
		return field.getInt(manipulator);
	}

	private static void setField(ViewJSONManipulator manipulator, String name, Object value) throws Exception {
		Field field = findField(name);
		field.setAccessible(true);
		field.set(manipulator, value);
	}

	private static Object objectField(ViewJSONManipulator manipulator, String name) throws Exception {
		Field field = findField(name);
		field.setAccessible(true);
		return field.get(manipulator);
	}

	private static Field findField(String name) throws NoSuchFieldException {
		Class<?> type = ViewJSONManipulator.class;
		while (type != null) {
			try {
				return type.getDeclaredField(name);
			}
			catch (@SuppressWarnings("unused") NoSuchFieldException e) {
				type = type.getSuperclass();
			}
		}
		throw new NoSuchFieldException(name);
	}
}
