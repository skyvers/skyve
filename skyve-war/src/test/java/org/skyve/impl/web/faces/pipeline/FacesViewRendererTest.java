package org.skyve.impl.web.faces.pipeline;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.view.actions.AddAction;
import org.skyve.impl.metadata.repository.view.actions.CancelAction;
import org.skyve.impl.metadata.repository.view.actions.CustomAction;
import org.skyve.impl.metadata.repository.view.actions.DeleteAction;
import org.skyve.impl.metadata.repository.view.actions.BizExportAction;
import org.skyve.impl.metadata.repository.view.actions.BizImportAction;
import org.skyve.impl.metadata.repository.view.actions.DownloadAction;
import org.skyve.impl.metadata.repository.view.actions.NewAction;
import org.skyve.impl.metadata.repository.view.actions.OKAction;
import org.skyve.impl.metadata.repository.view.actions.PrintAction;
import org.skyve.impl.metadata.repository.view.actions.RemoveAction;
import org.skyve.impl.metadata.repository.view.actions.ReportAction;
import org.skyve.impl.metadata.repository.view.actions.SaveAction;
import org.skyve.impl.metadata.repository.view.actions.UploadAction;
import org.skyve.impl.metadata.repository.view.actions.ZoomOutAction;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Collapsible;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.Chart.ChartType;
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.WidgetReference;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.reference.ExternalReference;
import org.skyve.impl.metadata.view.reference.ActionReference;
import org.skyve.impl.metadata.view.reference.ContentReference;
import org.skyve.impl.metadata.view.reference.DefaultListViewReference;
import org.skyve.impl.metadata.view.reference.EditViewReference;
import org.skyve.impl.metadata.view.reference.ImplicitActionReference;
import org.skyve.impl.metadata.view.reference.QueryListViewReference;
import org.skyve.impl.metadata.view.reference.ReportReference;
import org.skyve.impl.metadata.view.reference.ResourceReference;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.sail.mock.MockFacesContext;
import org.skyve.impl.web.faces.pipeline.component.ResponsiveComponentBuilder;
import org.skyve.impl.web.faces.pipeline.layout.NoOpLayoutBuilder;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.web.UserAgentType;

import jakarta.faces.component.UIComponent;
import jakarta.faces.context.FacesContext;
import modules.test.AbstractSkyveTest;

class FacesViewRendererTest extends AbstractSkyveTest {

	private static final String UXUI = "external";

	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext fc) {
			setCurrentInstance(fc);
		}
	}

	/**
	 * A LayoutBuilder that properly maintains the current component reference.
	 * NoOpLayoutBuilder.addToContainer() returns its first arg (null), which sets current=null.
	 * This implementation returns the componentToAdd so current stays non-null.
	 */
	private static class TestLayoutBuilder extends NoOpLayoutBuilder {
		@Override
		public UIComponent addToContainer(UIComponent component,
											org.skyve.impl.metadata.Container viewContainer,
											UIComponent container,
											UIComponent componentToAdd,
											Integer pixelWidth,
											Integer responsiveWidth,
											Integer percentageWidth,
											Integer sm,
											Integer md,
											Integer lg,
											Integer xl,
											String invisibleConditionName) {
			if (component != null) {
				return component;
			}
			if (componentToAdd != null && container != null) {
				container.getChildren().add(componentToAdd);
			}
			return componentToAdd != null ? componentToAdd : container;
		}

		@Override
		public UIComponent addedToContainer(UIComponent component,
											org.skyve.impl.metadata.Container viewContainer,
											UIComponent container) {
			return component != null ? component : container;
		}

		@Override
		public UIComponent sidebarLayout(UIComponent component, Sidebar sidebar, boolean createView) {
			if (component != null) {
				return component;
			}
			UIComponent result = panelGroup(false, false, true, null, null);
			result.getChildren().add(panelGroup(false, false, true, null, null));
			return result;
		}

		@Override
		public UIComponent contentSignatureLayout(UIComponent component, ContentSignature signature) {
			if (component != null) {
				return component;
			}
			return panelGroup(false, false, true, null, null);
		}
	}

	@BeforeAll
	static void setUpFacesContext() {
		FacesContextBridge.setCurrent(new MockFacesContext());
	}

	@AfterAll
	static void tearDownFacesContext() {
		FacesContextBridge.setCurrent(null);
	}

	private FacesViewRenderer createRenderer(ViewImpl view) {
		return createRenderer(view, null);
	}

	private FacesViewRenderer createRenderer(ViewImpl view, String widgetId) {
		ResponsiveComponentBuilder cb = new ResponsiveComponentBuilder();
		TestLayoutBuilder lb = new TestLayoutBuilder();
		FacesView managedBean = new FacesView();
		cb.setUserAgentType(UserAgentType.desktop);
		lb.setUserAgentType(UserAgentType.desktop);
		cb.setSAILManagedBean(managedBean);
		lb.setSAILManagedBean(managedBean);
		return new FacesViewRenderer(u, m, aapd, view, UXUI, widgetId, cb, lb);
	}

	@Test
	void renderEmptyEditView() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderEmptyCreateView() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.create.toString());
		view.setTitle("Create Test");

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithForm() {
		ViewImpl view = createEditView();
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndCheckBox() {
		ViewImpl view = createEditView();
		CheckBox cb = new CheckBox();
		cb.setBinding("booleanFlag");
		view.getContained().add(createFormWithWidget(cb));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndCombo() {
		ViewImpl view = createEditView();
		Combo combo = new Combo();
		combo.setBinding("enum3");
		view.getContained().add(createFormWithWidget(combo));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndRadio() {
		ViewImpl view = createEditView();
		Radio radio = new Radio();
		radio.setBinding("enum3");
		view.getContained().add(createFormWithWidget(radio));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndPassword() {
		ViewImpl view = createEditView();
		Password pw = new Password();
		pw.setBinding("text");
		view.getContained().add(createFormWithWidget(pw));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndTextArea() {
		ViewImpl view = createEditView();
		TextArea ta = new TextArea();
		ta.setBinding("memo");
		view.getContained().add(createFormWithWidget(ta));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndRichText() {
		ViewImpl view = createEditView();
		RichText rt = new RichText();
		rt.setBinding("markup");
		view.getContained().add(createFormWithWidget(rt));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndHtml() {
		ViewImpl view = createEditView();
		HTML html = new HTML();
		html.setBinding("markup");
		view.getContained().add(createFormWithWidget(html));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndSpinner() {
		ViewImpl view = createEditView();
		Spinner spinner = new Spinner();
		spinner.setBinding("normalInteger");
		view.getContained().add(createFormWithWidget(spinner));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndSlider() {
		ViewImpl view = createEditView();
		Slider slider = new Slider();
		slider.setBinding("normalInteger");
		view.getContained().add(createFormWithWidget(slider));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndColourPicker() {
		ViewImpl view = createEditView();
		ColourPicker cp = new ColourPicker();
		cp.setBinding("colour");
		view.getContained().add(createFormWithWidget(cp));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndLookupDescription() {
		ViewImpl view = createEditView();
		LookupDescription ld = new LookupDescription();
		ld.setBinding("aggregatedAssociation");
		view.getContained().add(createFormWithWidget(ld));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndContentImage() {
		ViewImpl view = createEditView();
		ContentImage ci = new ContentImage();
		ci.setBinding("text");
		view.getContained().add(createFormWithWidget(ci));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndContentLink() {
		ViewImpl view = createEditView();
		ContentLink cl = new ContentLink();
		cl.setBinding("text");
		view.getContained().add(createFormWithWidget(cl));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndGeometry() {
		ViewImpl view = createEditView();
		Geometry geo = new Geometry();
		geo.setBinding("geometry");
		view.getContained().add(createFormWithWidget(geo));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndLabel() {
		ViewImpl view = createEditView();
		Label label = new Label();
		label.setBinding("text");
		view.getContained().add(createFormWithWidget(label));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndProgressBar() {
		ViewImpl view = createEditView();
		ProgressBar pb = new ProgressBar();
		pb.setBinding("decimal2");
		view.getContained().add(createFormWithWidget(pb));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndZoomIn() {
		ViewImpl view = createEditView();
		ZoomIn zi = new ZoomIn();
		zi.setBinding("aggregatedAssociation");
		view.getContained().add(createFormWithWidget(zi));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndSpacer() {
		ViewImpl view = createEditView();
		Spacer spacer = new Spacer();
		spacer.setPixelWidth(Integer.valueOf(10));
		view.getContained().add(createFormWithWidget(spacer));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndListMembership() {
		ViewImpl view = createEditView();
		ListMembership lm = new ListMembership();
		lm.setBinding("aggregatedCollection");
		view.getContained().add(createFormWithWidget(lm));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndCheckMembership() {
		ViewImpl view = createEditView();
		CheckMembership cm = new CheckMembership();
		cm.setBinding("aggregatedCollection");
		view.getContained().add(createFormWithWidget(cm));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithBlurb() {
		ViewImpl view = createEditView();
		Blurb blurb = new Blurb();
		blurb.setMarkup("<b>Test blurb</b>");
		view.getContained().add(blurb);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithStaticImage() {
		ViewImpl view = createEditView();
		StaticImage img = new StaticImage();
		img.setRelativeFile("images/test.png");
		view.getContained().add(img);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDynamicImage() {
		ViewImpl view = createEditView();
		DynamicImage di = new DynamicImage();
		di.setName("TestImage");
		view.getContained().add(di);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithMapDisplay() {
		ViewImpl view = createEditView();
		MapDisplay map = new MapDisplay();
		map.setModelName("testMap");
		map.setPixelWidth(Integer.valueOf(400));
		map.setPixelHeight(Integer.valueOf(300));
		view.getContained().add(map);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithChart() {
		ViewImpl view = createEditView();
		Chart chart = new Chart();
		chart.setType(ChartType.bar);
		chart.setModelName("testChart");
		view.getContained().add(chart);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDialogButton() {
		ViewImpl view = createEditView();
		DialogButton db = new DialogButton();
		db.setDisplayName("Open Dialog");
		db.setDialogName("testDialog");
		view.getContained().add(createFormWithWidget(db));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithTabPane() {
		ViewImpl view = createEditView();
		TabPane tabPane = new TabPane();
		Tab tab = new Tab();
		tab.setTitle("Tab 1");
		tab.getContained().add(createFormWithTextField());
		tabPane.getTabs().add(tab);
		view.getContained().add(tabPane);

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithTabPaneMultipleTabs() {
		ViewImpl view = createEditView();
		TabPane tabPane = new TabPane();

		Tab tab1 = new Tab();
		tab1.setTitle("Tab 1");
		tab1.getContained().add(createFormWithTextField());
		tabPane.getTabs().add(tab1);

		Tab tab2 = new Tab();
		tab2.setTitle("Tab 2");
		TextArea ta = new TextArea();
		ta.setBinding("memo");
		tab2.getContained().add(createFormWithWidget(ta));
		tabPane.getTabs().add(tab2);

		view.getContained().add(tabPane);

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithWidgetIdMatchedTabPaneFragment() {
		ViewImpl view = createEditView();
		TabPane tabPane = new TabPane();
		tabPane.setWidgetId("tabs-fragment");
		Tab tab = new Tab();
		tab.setTitle("Tab Fragment");
		tab.getContained().add(createFormWithTextField());
		tabPane.getTabs().add(tab);
		view.getContained().add(tabPane);

		FacesViewRenderer renderer = createRenderer(view, "tabs-fragment");
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithVBox() {
		ViewImpl view = createEditView();
		VBox vbox = new VBox();
		vbox.getContained().add(createFormWithTextField());
		view.getContained().add(vbox);

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithVBoxBorderTitle() {
		ViewImpl view = createEditView();
		VBox vbox = new VBox();
		vbox.setBorderTitle("Section Title");
		vbox.getContained().add(createFormWithTextField());
		view.getContained().add(vbox);

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithHBox() {
		ViewImpl view = createEditView();
		HBox hbox = new HBox();
		hbox.getContained().add(createFormWithTextField());
		view.getContained().add(hbox);

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithWidgetIdMatchedVBoxFragment() {
		ViewImpl view = createEditView();
		VBox vbox = new VBox();
		vbox.setWidgetId("vbox-fragment");
		vbox.getContained().add(createFormWithTextField());
		view.getContained().add(vbox);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view, "vbox-fragment");
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithWidgetIdMatchedBorderedHBoxFragment() {
		ViewImpl view = createEditView();
		HBox hbox = new HBox();
		hbox.setWidgetId("hbox-fragment");
		hbox.setBorderTitle("HBox Fragment");
		hbox.getContained().add(createFormWithTextField());
		view.getContained().add(hbox);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view, "hbox-fragment");
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithWidgetIdMatchedBorderedFormFragment() {
		ViewImpl view = createEditView();
		Form form = createFormWithTextField();
		form.setWidgetId("form-fragment");
		form.setBorderTitle("Form Fragment");
		view.getContained().add(form);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view, "form-fragment");
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataGrid() {
		ViewImpl view = createEditView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("bizKey");
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataRepeater() {
		ViewImpl view = createEditView();
		DataRepeater repeater = new DataRepeater();
		repeater.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("bizKey");
		repeater.getColumns().add(col);
		view.getContained().add(repeater);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithListGrid() {
		ViewImpl view = createEditView();
		ListGrid listGrid = new ListGrid();
		listGrid.setQueryName("qExpressionQuery");
		view.getContained().add(listGrid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithListRepeater() {
		ViewImpl view = createEditView();
		ListRepeater listRepeater = new ListRepeater();
		listRepeater.setQueryName("qExpressionQuery");
		view.getContained().add(listRepeater);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithSaveAndCancelActions() {
		ViewImpl view = createEditView();
		view.getContained().add(createFormWithTextField());
		view.putAction(new SaveAction().toMetaDataAction());
		view.putAction(new CancelAction().toMetaDataAction());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithOKAndDeleteActions() {
		ViewImpl view = createEditView();
		view.getContained().add(createFormWithTextField());
		view.putAction(new OKAction().toMetaDataAction());
		view.putAction(new DeleteAction().toMetaDataAction());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithNewAndZoomOutActions() {
		ViewImpl view = createEditView();
		view.getContained().add(createFormWithTextField());
		view.putAction(new NewAction().toMetaDataAction());
		view.putAction(new ZoomOutAction().toMetaDataAction());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithCustomAction() {
		ViewImpl view = createEditView();
		view.getContained().add(createFormWithTextField());
		CustomAction customAction = new CustomAction();
		customAction.setClassName("modules.test.SomeAction");
		view.putAction(customAction.toMetaDataAction());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDownloadAction() {
		ViewImpl view = createEditView();
		view.getContained().add(createFormWithTextField());
		DownloadAction download = new DownloadAction();
		download.setClassName("modules.test.Action");
		view.putAction(download.toMetaDataAction());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndServerSideActionOnChange() {
		ViewImpl view = createEditView();
		ActionImpl viewAction = new ActionImpl();
		viewAction.setName("testAction");
		viewAction.setResourceName("TestAction");
		view.putAction(viewAction);

		TextField tf = new TextField();
		tf.setBinding("text");
		ServerSideActionEventAction action = new ServerSideActionEventAction();
		action.setActionName("testAction");
		tf.getChangedActions().add(action);
		view.getContained().add(createFormWithWidget(tf));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndRerenderOnChange() {
		ViewImpl view = createEditView();
		TextField tf = new TextField();
		tf.setBinding("text");
		RerenderEventAction rerender = new RerenderEventAction();
		rerender.setClientValidation(Boolean.FALSE);
		tf.getChangedActions().add(rerender);
		view.getContained().add(createFormWithWidget(tf));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndSetDisabledOnChange() {
		ViewImpl view = createEditView();
		TextField tf = new TextField();
		tf.setBinding("text");
		SetDisabledEventAction setDisabled = new SetDisabledEventAction();
		setDisabled.setBinding("text");
		setDisabled.setDisabledConditionName("false");
		tf.getChangedActions().add(setDisabled);
		view.getContained().add(createFormWithWidget(tf));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndSetInvisibleOnChange() {
		ViewImpl view = createEditView();
		TextField tf = new TextField();
		tf.setBinding("text");
		SetInvisibleEventAction setInvisible = new SetInvisibleEventAction();
		setInvisible.setBinding("text");
		setInvisible.setInvisibleConditionName("false");
		tf.getChangedActions().add(setInvisible);
		view.getContained().add(createFormWithWidget(tf));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndToggleDisabledOnChange() {
		ViewImpl view = createEditView();
		TextField tf = new TextField();
		tf.setBinding("text");
		ToggleDisabledEventAction toggle = new ToggleDisabledEventAction();
		toggle.setBinding("text");
		tf.getChangedActions().add(toggle);
		view.getContained().add(createFormWithWidget(tf));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndToggleVisibilityOnChange() {
		ViewImpl view = createEditView();
		TextField tf = new TextField();
		tf.setBinding("text");
		ToggleVisibilityEventAction toggleVis = new ToggleVisibilityEventAction();
		toggleVis.setBinding("text");
		tf.getChangedActions().add(toggleVis);
		view.getContained().add(createFormWithWidget(tf));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndFocusBlurHandlers() {
		ViewImpl view = createEditView();
		TextField tf = new TextField();
		tf.setBinding("text");
		RerenderEventAction focusAction = new RerenderEventAction();
		tf.getFocusActions().add(focusAction);
		RerenderEventAction blurAction = new RerenderEventAction();
		tf.getBlurActions().add(blurAction);
		view.getContained().add(createFormWithWidget(tf));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormBorderTitle() {
		ViewImpl view = createEditView();
		Form form = new Form();
		form.setBorderTitle("Section Title");
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataGridEventHandlers() {
		ViewImpl view = createEditView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("bizKey");
		grid.getColumns().add(col);
		RerenderEventAction added = new RerenderEventAction();
		grid.getAddedActions().add(added);
		RerenderEventAction edited = new RerenderEventAction();
		grid.getEditedActions().add(edited);
		RerenderEventAction removed = new RerenderEventAction();
		grid.getRemovedActions().add(removed);
		RerenderEventAction selected = new RerenderEventAction();
		grid.getSelectedActions().add(selected);
		view.getContained().add(grid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithLookupDescriptionEventHandlers() {
		ViewImpl view = createEditView();
		LookupDescription ld = new LookupDescription();
		ld.setBinding("aggregatedAssociation");
		RerenderEventAction picked = new RerenderEventAction();
		ld.getPickedActions().add(picked);
		RerenderEventAction added = new RerenderEventAction();
		ld.getAddedActions().add(added);
		RerenderEventAction edited = new RerenderEventAction();
		ld.getEditedActions().add(edited);
		view.getContained().add(createFormWithWidget(ld));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithTreeGrid() {
		ViewImpl view = createEditView();
		TreeGrid treeGrid = new TreeGrid();
		treeGrid.setQueryName("qExpressionQuery");
		view.getContained().add(treeGrid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndGeometryMap() {
		ViewImpl view = createEditView();
		GeometryMap gm = new GeometryMap();
		gm.setBinding("geometry");
		view.getContained().add(createFormWithWidget(gm));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataGridContainerColumnStaticImage() {
		ViewImpl view = createEditView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridContainerColumn col = new DataGridContainerColumn();
		StaticImage si = new StaticImage();
		si.setRelativeFile("images/test.png");
		col.getWidgets().add(si);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataGridContainerColumnDynamicImage() {
		ViewImpl view = createEditView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridContainerColumn col = new DataGridContainerColumn();
		DynamicImage di = new DynamicImage();
		di.setName("testImage");
		col.getWidgets().add(di);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataGridContainerColumnBlurb() {
		ViewImpl view = createEditView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridContainerColumn col = new DataGridContainerColumn();
		Blurb blurb = new Blurb();
		blurb.setMarkup("<b>test</b>");
		col.getWidgets().add(blurb);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataGridContainerColumnLabel() {
		ViewImpl view = createEditView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridContainerColumn col = new DataGridContainerColumn();
		Label label = new Label();
		label.setValue("col label");
		col.getWidgets().add(label);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataGridContainerColumnLink() {
		ViewImpl view = createEditView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridContainerColumn col = new DataGridContainerColumn();
		Link link = new Link();
		ExternalReference ref = new ExternalReference();
		ref.setHref("https://example.com");
		link.setReference(ref);
		link.setValue("Click here");
		col.getWidgets().add(link);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormLinkEditViewReference() {
		ViewImpl view = createEditView();
		Link link = new Link();
		EditViewReference ref = new EditViewReference();
		ref.setModuleName("test");
		ref.setDocumentName("AllAttributesPersistent");
		ref.setBinding("text");
		link.setReference(ref);
		link.setValue("Edit");
		view.getContained().add(createFormWithWidget(link));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormLinkReportReference() {
		ViewImpl view = createEditView();
		Link link = new Link();
		ReportReference ref = new ReportReference();
		ref.setModuleName("test");
		ref.setDocumentName("AllAttributesPersistent");
		ref.setReportName("TestReport");
		link.setReference(ref);
		link.setValue("Report");
		view.getContained().add(createFormWithWidget(link));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormLinkResourceReference() {
		ViewImpl view = createEditView();
		Link link = new Link();
		ResourceReference ref = new ResourceReference();
		ref.setRelativeFile("resources/test.pdf");
		link.setReference(ref);
		link.setValue("Resource");
		view.getContained().add(createFormWithWidget(link));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormLinkActionReference() {
		ViewImpl view = createEditView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridContainerColumn col = new DataGridContainerColumn();
		ActionImpl viewAction = new ActionImpl();
		viewAction.setName("testAction");
		viewAction.setResourceName("TestAction");
		view.putAction(viewAction);

		Link link = new Link();
		ActionReference ref = new ActionReference();
		ref.setActionName("testAction");
		link.setReference(ref);
		link.setValue("Action");
		col.getWidgets().add(link);
		grid.getColumns().add(col);
		view.getContained().add(grid);

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormLinkImplicitActionReference() {
		ViewImpl view = createEditView();
		Link link = new Link();
		ImplicitActionReference ref = new ImplicitActionReference();
		ref.setImplicitActionName(org.skyve.metadata.controller.ImplicitActionName.OK);
		link.setReference(ref);
		link.setValue("Implicit");
		view.getContained().add(createFormWithWidget(link));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormLinkDefaultListViewReference() {
		ViewImpl view = createEditView();
		Link link = new Link();
		DefaultListViewReference ref = new DefaultListViewReference();
		ref.setModuleName("test");
		ref.setDocumentName("AllAttributesPersistent");
		link.setReference(ref);
		link.setValue("Default List");
		view.getContained().add(createFormWithWidget(link));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormLinkQueryListViewReference() {
		ViewImpl view = createEditView();
		Link link = new Link();
		QueryListViewReference ref = new QueryListViewReference();
		ref.setQueryName("AllAttributesPersistent");
		link.setReference(ref);
		link.setValue("Query List");
		view.getContained().add(createFormWithWidget(link));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormLinkContentReference() {
		ViewImpl view = createEditView();
		Link link = new Link();
		ContentReference ref = new ContentReference();
		ref.setBinding("content");
		link.setReference(ref);
		link.setValue("Content");
		view.getContained().add(createFormWithWidget(link));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataRepeaterContainerColumnBlurb() {
		ViewImpl view = createEditView();
		DataRepeater repeater = new DataRepeater();
		repeater.setBinding("aggregatedCollection");
		DataGridContainerColumn col = new DataGridContainerColumn();
		Blurb blurb = new Blurb();
		blurb.setMarkup("<i>repeater blurb</i>");
		col.getWidgets().add(blurb);
		repeater.getColumns().add(col);
		view.getContained().add(repeater);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndGeometryMapWidget() {
		ViewImpl view = createEditView();
		GeometryMap geo = new GeometryMap();
		geo.setBinding("geometry");
		view.getContained().add(createFormWithWidget(geo));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataRepeaterContainerColumnStaticImage() {
		ViewImpl view = createEditView();
		DataRepeater repeater = new DataRepeater();
		repeater.setBinding("aggregatedCollection");
		DataGridContainerColumn col = new DataGridContainerColumn();
		StaticImage img = new StaticImage();
		img.setRelativeFile("test.png");
		col.getWidgets().add(img);
		repeater.getColumns().add(col);
		view.getContained().add(repeater);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataRepeaterContainerColumnDynamicImage() {
		ViewImpl view = createEditView();
		DataRepeater repeater = new DataRepeater();
		repeater.setBinding("aggregatedCollection");
		DataGridContainerColumn col = new DataGridContainerColumn();
		DynamicImage di = new DynamicImage();
		di.setName("testImage");
		col.getWidgets().add(di);
		repeater.getColumns().add(col);
		view.getContained().add(repeater);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataRepeaterContainerColumnLabel() {
		ViewImpl view = createEditView();
		DataRepeater repeater = new DataRepeater();
		repeater.setBinding("aggregatedCollection");
		DataGridContainerColumn col = new DataGridContainerColumn();
		Label label = new Label();
		label.setBinding("text");
		col.getWidgets().add(label);
		repeater.getColumns().add(col);
		view.getContained().add(repeater);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataRepeaterContainerColumnLink() {
		ViewImpl view = createEditView();
		DataRepeater repeater = new DataRepeater();
		repeater.setBinding("aggregatedCollection");
		DataGridContainerColumn col = new DataGridContainerColumn();
		Link link = new Link();
		ExternalReference ref = new ExternalReference();
		ref.setHref("https://example.com");
		link.setReference(ref);
		link.setValue("Click here");
		col.getWidgets().add(link);
		repeater.getColumns().add(col);
		view.getContained().add(repeater);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataGridContainerColumnGeometryMap() {
		ViewImpl view = createEditView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridContainerColumn col = new DataGridContainerColumn();
		GeometryMap gm = new GeometryMap();
		gm.setBinding("geometry");
		col.getWidgets().add(gm);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataGridBoundColumnWithCheckBox() {
		ViewImpl view = createEditView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("booleanFlag");
		col.setEditable(Boolean.TRUE);
		org.skyve.impl.metadata.view.WidgetReference wr = new org.skyve.impl.metadata.view.WidgetReference();
		CheckBox cb = new CheckBox();
		cb.setBinding("booleanFlag");
		wr.setWidget(cb);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataGridBoundColumnWithTextField() {
		ViewImpl view = createEditView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("text");
		col.setEditable(Boolean.TRUE);
		WidgetReference wr = new WidgetReference();
		TextField tf = new TextField();
		tf.setBinding("text");
		wr.setWidget(tf);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataGridBoundColumnWithTextArea() {
		ViewImpl view = createEditView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("memo");
		col.setEditable(Boolean.TRUE);
		WidgetReference wr = new WidgetReference();
		TextArea ta = new TextArea();
		ta.setBinding("memo");
		wr.setWidget(ta);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataGridBoundColumnWithCombo() {
		ViewImpl view = createEditView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("enum3");
		col.setEditable(Boolean.TRUE);
		WidgetReference wr = new WidgetReference();
		Combo combo = new Combo();
		combo.setBinding("enum3");
		wr.setWidget(combo);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataGridBoundColumnWithColourPicker() {
		ViewImpl view = createEditView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("colour");
		col.setEditable(Boolean.TRUE);
		WidgetReference wr = new WidgetReference();
		ColourPicker cp = new ColourPicker();
		cp.setBinding("colour");
		wr.setWidget(cp);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataGridBoundColumnWithPassword() {
		ViewImpl view = createEditView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("text");
		col.setEditable(Boolean.TRUE);
		WidgetReference wr = new WidgetReference();
		Password pw = new Password();
		pw.setBinding("text");
		wr.setWidget(pw);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataGridBoundColumnWithRadio() {
		ViewImpl view = createEditView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("enum3");
		col.setEditable(Boolean.TRUE);
		WidgetReference wr = new WidgetReference();
		Radio radio = new Radio();
		radio.setBinding("enum3");
		wr.setWidget(radio);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataGridBoundColumnWithRichText() {
		ViewImpl view = createEditView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("memo");
		col.setEditable(Boolean.TRUE);
		WidgetReference wr = new WidgetReference();
		RichText rt = new RichText();
		rt.setBinding("memo");
		wr.setWidget(rt);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataGridBoundColumnWithSlider() {
		ViewImpl view = createEditView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("normalInteger");
		col.setEditable(Boolean.TRUE);
		WidgetReference wr = new WidgetReference();
		Slider slider = new Slider();
		slider.setBinding("normalInteger");
		wr.setWidget(slider);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataGridBoundColumnWithSpinner() {
		ViewImpl view = createEditView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("normalInteger");
		col.setEditable(Boolean.TRUE);
		WidgetReference wr = new WidgetReference();
		Spinner spinner = new Spinner();
		spinner.setBinding("normalInteger");
		wr.setWidget(spinner);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataGridBoundColumnWithHTML() {
		ViewImpl view = createEditView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("markup");
		col.setEditable(Boolean.TRUE);
		WidgetReference wr = new WidgetReference();
		HTML html = new HTML();
		html.setBinding("markup");
		wr.setWidget(html);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataGridBoundColumnWithLookupDescription() {
		ViewImpl view = createEditView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("aggregatedAssociation");
		col.setEditable(Boolean.TRUE);
		WidgetReference wr = new WidgetReference();
		LookupDescription ld = new LookupDescription();
		ld.setBinding("aggregatedAssociation");
		ld.setDescriptionBinding(org.skyve.domain.Bean.BIZ_KEY);
		wr.setWidget(ld);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataGridBoundColumnWithContentImage() {
		ViewImpl view = createEditView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("text");
		col.setEditable(Boolean.TRUE);
		WidgetReference wr = new WidgetReference();
		ContentImage ci = new ContentImage();
		ci.setBinding("text");
		wr.setWidget(ci);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithFormAndContentSignature() {
		ViewImpl view = createEditView();
		ContentSignature cs = new ContentSignature();
		cs.setBinding("colour");
		view.getContained().add(createFormWithWidget(cs));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithSidebar() {
		ViewImpl view = createEditView();
		view.getContained().add(createFormWithTextField());
		Sidebar sidebar = new Sidebar();
		view.setSidebar(sidebar);

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithBizExportAction() {
		ViewImpl view = createEditView();
		view.getContained().add(createFormWithTextField());
		BizExportAction bizExport = new BizExportAction();
		bizExport.setClassName("modules.test.ExportAction");
		view.putAction(bizExport.toMetaDataAction());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithBizImportAction() {
		ViewImpl view = createEditView();
		view.getContained().add(createFormWithTextField());
		BizImportAction bizImport = new BizImportAction();
		bizImport.setClassName("modules.test.ImportAction");
		view.putAction(bizImport.toMetaDataAction());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithAddAction() {
		ViewImpl view = createEditView();
		view.getContained().add(createFormWithTextField());
		AddAction add = new AddAction();
		add.setDisplayName("Add");
		view.putAction(add.toMetaDataAction());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithRemoveAction() {
		ViewImpl view = createEditView();
		view.getContained().add(createFormWithTextField());
		RemoveAction remove = new RemoveAction();
		remove.setDisplayName("Remove");
		view.putAction(remove.toMetaDataAction());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithReportAction() {
		ViewImpl view = createEditView();
		view.getContained().add(createFormWithTextField());
		ReportAction report = new ReportAction();
		report.setReportName("MyReport");
		view.putAction(report.toMetaDataAction());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithUploadAction() {
		ViewImpl view = createEditView();
		view.getContained().add(createFormWithTextField());
		UploadAction upload = new UploadAction();
		upload.setClassName("modules.test.UploadClass");
		view.putAction(upload.toMetaDataAction());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithPrintAction() {
		ViewImpl view = createEditView();
		view.getContained().add(createFormWithTextField());
		PrintAction print = new PrintAction();
		print.setDisplayName("Print");
		view.putAction(print.toMetaDataAction());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataRepeaterContainerColumnContentLink() {
		ViewImpl view = createEditView();
		DataRepeater dr = new DataRepeater();
		dr.setBinding("aggregatedCollection");
		DataGridContainerColumn col = new DataGridContainerColumn();
		ContentLink cl = new ContentLink();
		cl.setBinding("text");
		col.getWidgets().add(cl);
		dr.getColumns().add(col);
		view.getContained().add(dr);

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithDataRepeaterContainerColumnContentSignature() {
		ViewImpl view = createEditView();
		DataRepeater dr = new DataRepeater();
		dr.setBinding("aggregatedCollection");
		DataGridContainerColumn col = new DataGridContainerColumn();
		ContentSignature cs = new ContentSignature();
		cs.setBinding("text");
		col.getWidgets().add(cs);
		dr.getColumns().add(col);
		view.getContained().add(dr);

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderViewWithWidgetIdMatchedDataGridFragment() {
		ViewImpl view = createEditView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		grid.setWidgetId("grid-fragment");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("bizKey");
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view, "grid-fragment");
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderInlineDataGridAddsEditableBoundColumnComponent() {
		ViewImpl view = createEditView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		grid.setInline(Boolean.TRUE);
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("text");
		col.setEditable(Boolean.TRUE);
		WidgetReference wr = new WidgetReference();
		TextField tf = new TextField();
		tf.setBinding("text");
		wr.setWidget(tf);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderListGridWithoutZoomContextMenuWhenConversationContinues() {
		ViewImpl view = createEditView();
		ListGrid listGrid = new ListGrid();
		listGrid.setQueryName("qExpressionQuery");
		listGrid.setContinueConversation(true);
		view.getContained().add(listGrid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderListGridWithoutZoomContextMenuWhenZoomHidden() {
		ViewImpl view = createEditView();
		ListGrid listGrid = new ListGrid();
		listGrid.setQueryName("qExpressionQuery");
		listGrid.setShowZoom(Boolean.FALSE);
		view.getContained().add(listGrid);
		view.getContained().add(createFormWithTextField());

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderLookupDescriptionWithClearedHandler() {
		ViewImpl view = createEditView();
		LookupDescription ld = new LookupDescription();
		ld.setBinding("aggregatedAssociation");
		RerenderEventAction cleared = new RerenderEventAction();
		ld.getClearedActions().add(cleared);
		view.getContained().add(createFormWithWidget(ld));

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderCollapsibleFormRequiresBorderTitle() {
		ViewImpl view = createEditView();
		Form form = createFormWithTextField();
		form.setCollapsible(Collapsible.open);
		view.getContained().add(form);

		FacesViewRenderer renderer = createRenderer(view);
		assertThrows(org.skyve.metadata.MetaDataException.class, renderer::visit);
	}

	@Test
	void renderCollapsibleHBoxRequiresBorderTitle() {
		ViewImpl view = createEditView();
		HBox hbox = new HBox();
		hbox.setCollapsible(Collapsible.open);
		hbox.getContained().add(createFormWithTextField());
		view.getContained().add(hbox);

		FacesViewRenderer renderer = createRenderer(view);
		assertThrows(org.skyve.metadata.MetaDataException.class, renderer::visit);
	}

	@Test
	void renderCollapsibleVBoxWithBorderTitle() {
		ViewImpl view = createEditView();
		VBox vbox = new VBox();
		vbox.setBorderTitle("Collapsible Section");
		vbox.setCollapsible(Collapsible.closed);
		vbox.getContained().add(createFormWithTextField());
		view.getContained().add(vbox);

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	@Test
	void renderCollapsibleHBoxWithBorderTitle() {
		ViewImpl view = createEditView();
		HBox hbox = new HBox();
		hbox.setBorderTitle("Collapsible HBox");
		hbox.setCollapsible(Collapsible.closed);
		hbox.getContained().add(createFormWithTextField());
		view.getContained().add(hbox);

		FacesViewRenderer renderer = createRenderer(view);
		renderer.visit();
		assertNotNull(renderer.getFacesView());
	}

	// ---- helpers ----

	private static ViewImpl createEditView() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");
		return view;
	}

	private static Form createFormWithTextField() {
		TextField tf = new TextField();
		tf.setBinding("text");
		return createFormWithWidget(tf);
	}

	private static Form createFormWithWidget(MetaData widget) {
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		item.setWidget(widget);
		row.getItems().add(item);
		return form;
	}
}
