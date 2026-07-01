package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.generate.ViewRenderer;
import org.skyve.impl.metadata.controller.CustomisationsStaticSingleton;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.LoadingType;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.VerticalAlignment;
import org.skyve.impl.metadata.view.container.Collapsible;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.event.EventSource;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.Chart.ChartType;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.input.CompleteType;
import org.skyve.impl.metadata.view.widget.bound.input.ContentCapture;
import org.skyve.impl.metadata.view.widget.bound.input.ContentDisplay;
import org.skyve.impl.metadata.view.widget.bound.input.ContentUpload;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryInputType;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.controller.Customisations;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.user.User;
import org.skyve.util.OWASP;

class SmartClientViewRendererCoverageTest {
	private static final String UNSAFE_TITLE = "<img src=x onerror=alert(1)> & \"quoted\" 'single'";

	private CustomerImpl customer;
	private User user;
	private ModuleImpl module;
	private DocumentImpl document;
	private ViewImpl view;
	private Customisations previousCustomisations;

	@BeforeEach
	void before() {
		previousCustomisations = CustomisationsStaticSingleton.get();
		Customisations customisations = mock(Customisations.class);
		CustomisationsStaticSingleton.set(customisations);

		customer = mock(CustomerImpl.class);
		user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);

		module = mock(ModuleImpl.class);
		when(module.getName()).thenReturn("admin");
		document = new DocumentImpl();
		document.setName("Contact");
		document.setOwningModuleName("admin");
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(customer.getModule("admin")).thenReturn(module);

		view = mock(ViewImpl.class);
		when(view.getTitle()).thenReturn("Contacts");
	}

	@AfterEach
	void after() {
		CustomisationsStaticSingleton.set(previousCustomisations);
	}

	@Test
	void renderListGridWithoutDocumentContextThrows() {
		when(view.getName()).thenReturn("edit");

		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, null, view, "desktop", false);
		ListGrid grid = new ListGrid();

		assertThrows(MetaDataException.class, () -> renderer.renderListGrid("Contacts", false, grid));
	}

	@Test
	void renderEditViewWithoutSidebarCreatesEditContainer() {
		when(view.getName()).thenReturn("edit");
		when(view.getSidebar()).thenReturn(null);

		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, document, view, "desktop", false);
		renderer.renderView(null, null);

		assertTrue(renderer.getCode().toString().contains("var edit=isc.BizContainer.create({"));
	}

	@Test
	void renderCreateViewWithoutSidebarCreatesCreateContainer() {
		when(view.getName()).thenReturn("create");
		when(view.getSidebar()).thenReturn(null);

		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, document, view, "desktop", false);
		renderer.renderView(null, null);

		assertTrue(renderer.getCode().toString().contains("var create=isc.BizContainer.create({"));
	}

	@Test
	void renderEditViewWithSidebarCreatesSidebarLayout() {
		when(view.getName()).thenReturn("edit");
		when(view.getSidebar()).thenReturn(new Sidebar());

		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, document, view, "desktop", false);
		renderer.renderView(null, null);

		assertTrue(renderer.getCode().toString().contains("var sidebarPane=isc.BizContainer.create({"));
		assertTrue(renderer.getCode().toString().contains("var viewPane=isc.BizContainer.create({"));
	}

	@Test
	void renderedViewWithNoCreateViewAddsEditContainer() {
		when(view.getName()).thenReturn("edit");
		when(view.getSidebar()).thenReturn(new Sidebar());

		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, document, view, "desktop", true);
		renderer.renderView(null, null);
		renderer.renderedView(null, null);

		assertTrue(renderer.getCode().toString().contains("view.addContained(edit);"));
	}

	@Test
	void renderedViewWithNoCreateAndNoSidebarAddsHiddenFormShim() {
		when(view.getName()).thenReturn("edit");
		when(view.getSidebar()).thenReturn(null);

		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, document, view, "desktop", true);
		renderer.renderView(null, null);
		renderer.renderedView(null, null);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("var v0=isc.DynamicForm.create({invisibleConditionName:'true'});"));
		assertTrue(code.contains("view._vm.addMember(v0);"));
		assertTrue(code.contains("view.addContained(v0);"));
	}

	@Test
	void renderVBoxWithPaddingAndAlignmentWritesBorderAndAlignment() {
		when(view.getName()).thenReturn("edit");
		when(view.getSidebar()).thenReturn(null);

		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, document, view, "desktop", false);
		renderer.renderView(null, null);

		VBox vbox = new VBox();
		vbox.setBorder(Boolean.TRUE);
		vbox.setBorderTitle("Outer");
		vbox.setPixelPadding(Integer.valueOf(7));
		vbox.setPixelMemberPadding(Integer.valueOf(8));
		vbox.setVerticalAlignment(VerticalAlignment.top);
		vbox.setHorizontalAlignment(HorizontalAlignment.centre);

		renderer.renderVBox("Outer", vbox);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("layoutMargin:7,"), code);
		assertTrue(code.contains("membersMargin:8,"));
		assertTrue(code.contains("align:'top',"));
		assertTrue(code.contains("defaultLayoutAlign:'center'"));
		assertTrue(code.contains("groupTitle:'&nbsp;&nbsp;Outer&nbsp;&nbsp;',"));
	}

	@Test
	void renderVBoxEscapesBorderTitleByDefaultAndWhenExplicitlyTrue() {
		SmartClientViewRenderer defaultRenderer = rendererWithViewContainer();
		VBox defaultVBox = new VBox();
		defaultVBox.setBorder(Boolean.TRUE);
		defaultRenderer.renderVBox(UNSAFE_TITLE, defaultVBox);
		assertTrue(defaultRenderer.getCode().toString().contains(escapedGroupTitle(UNSAFE_TITLE, null)));

		SmartClientViewRenderer explicitRenderer = rendererWithViewContainer();
		VBox explicitVBox = new VBox();
		explicitVBox.setBorder(Boolean.TRUE);
		explicitVBox.setEscapeBorderTitle(Boolean.TRUE);
		explicitRenderer.renderVBox(UNSAFE_TITLE, explicitVBox);
		assertTrue(explicitRenderer.getCode().toString().contains(escapedGroupTitle(UNSAFE_TITLE, Boolean.TRUE)));
	}

	@Test
	void renderVBoxLeavesTrustedBorderTitleMarkupWhenEscapeFalse() {
		SmartClientViewRenderer renderer = rendererWithViewContainer();
		VBox vbox = new VBox();
		vbox.setBorder(Boolean.TRUE);
		vbox.setEscapeBorderTitle(Boolean.FALSE);

		renderer.renderVBox(UNSAFE_TITLE, vbox);

		String code = renderer.getCode().toString();
		assertTrue(code.contains(escapedGroupTitle(UNSAFE_TITLE, Boolean.FALSE)), code);
		assertFalse(code.contains(OWASP.escapeHtml(UNSAFE_TITLE)), code);
	}

	@Test
	void renderHBoxWithCollapsibleDefaultsWritesWrapper() {
		when(view.getName()).thenReturn("edit");
		when(view.getSidebar()).thenReturn(null);

		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, document, view, "desktop", false);
		renderer.renderView(null, null);

		HBox hbox = new HBox();
		hbox.setCollapsible(Collapsible.closed);
		hbox.setHorizontalAlignment(HorizontalAlignment.right);
		hbox.setVerticalAlignment(VerticalAlignment.bottom);

		renderer.renderHBox("Collapsed", hbox);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("width:'100%',height:'100%',layoutMargin:10,"), code);
		assertTrue(code.contains("minimized:true,"));
		assertTrue(code.contains("align:'right',"));
		assertTrue(code.contains("defaultLayoutAlign:'bottom'"));
	}

	@Test
	void renderHBoxEscapesCollapsibleTitleByDefaultAndAllowsTrustedMarkupWhenFalse() {
		SmartClientViewRenderer defaultRenderer = rendererWithViewContainer();
		HBox defaultHBox = new HBox();
		defaultHBox.setCollapsible(Collapsible.closed);
		defaultRenderer.renderHBox(UNSAFE_TITLE, defaultHBox);
		assertTrue(defaultRenderer.getCode().toString().contains(escapedCollapsibleTitle(UNSAFE_TITLE, null)));

		SmartClientViewRenderer trustedRenderer = rendererWithViewContainer();
		HBox trustedHBox = new HBox();
		trustedHBox.setCollapsible(Collapsible.closed);
		trustedHBox.setEscapeBorderTitle(Boolean.FALSE);
		trustedRenderer.renderHBox(UNSAFE_TITLE, trustedHBox);
		String code = trustedRenderer.getCode().toString();
		assertTrue(code.contains("title:'<img src=x onerror=alert(1)>"), code);
		assertTrue(code.contains("&quot;quoted&quot;"), code);
		assertFalse(code.contains("&lt;img"), code);
	}

	@Test
	void renderTabPaneAndTabWithIconStyleWritesSelectedBindingAndTabMetadata() {
		when(view.getName()).thenReturn("edit");
		when(view.getSidebar()).thenReturn(null);

		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, document, view, "desktop", false);
		renderer.renderView(null, null);

		TabPane tabPane = new TabPane();
		tabPane.setSelectedTabIndexBinding("bean.currentTab");
		renderer.renderTabPane(tabPane);

		Tab tab = new Tab();
		tab.setIconStyleClass("fa-solid fa-star");
		tab.setDisabledConditionName("bean.tabDisabled");
		tab.setInvisibleConditionName("bean.tabHidden");

		renderer.renderTab("Overview", null, tab);
		renderer.renderedTab("Overview", null, tab);
		renderer.renderedTabPane(tabPane);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("selectedTabIndexBinding:'bean_currentTab'"), code);
		assertTrue(code.contains(".addBizTab({name:'0'"), code);
		assertTrue(code.contains("<i class=\"bizhubFontIcon fa-solid fa-star\"></i><span> &nbsp;</span>Overview"), code);
		assertTrue(code.contains("disabledConditionName:'bean.tabDisabled'"), code);
		assertTrue(code.contains("invisibleConditionName:'bean.tabHidden'"), code);
	}

	@Test
	void renderedTabEscapesTitleByDefaultAndWhenExplicitlyTrue() {
		SmartClientViewRenderer defaultRenderer = rendererWithOpenTabPane();
		Tab defaultTab = new Tab();
		defaultRenderer.renderTab(UNSAFE_TITLE, null, defaultTab);
		defaultRenderer.renderedTab(UNSAFE_TITLE, null, defaultTab);
		assertTrue(defaultRenderer.getCode().toString().contains(escapedTabTitle(UNSAFE_TITLE, null)));

		SmartClientViewRenderer explicitRenderer = rendererWithOpenTabPane();
		Tab explicitTab = new Tab();
		explicitTab.setEscapeTitle(Boolean.TRUE);
		explicitRenderer.renderTab(UNSAFE_TITLE, null, explicitTab);
		explicitRenderer.renderedTab(UNSAFE_TITLE, null, explicitTab);
		assertTrue(explicitRenderer.getCode().toString().contains(escapedTabTitle(UNSAFE_TITLE, Boolean.TRUE)));
	}

	@Test
	void renderedTabLeavesTrustedTitleMarkupWhenEscapeFalse() {
		SmartClientViewRenderer renderer = rendererWithOpenTabPane();
		Tab tab = new Tab();
		tab.setEscapeTitle(Boolean.FALSE);

		renderer.renderTab(UNSAFE_TITLE, null, tab);
		renderer.renderedTab(UNSAFE_TITLE, null, tab);

		String code = renderer.getCode().toString();
		assertTrue(code.contains(escapedTabTitle(UNSAFE_TITLE, Boolean.FALSE)), code);
		assertFalse(code.contains(OWASP.escapeHtml(UNSAFE_TITLE)), code);
	}

	@Test
	void renderedTabWithIconStyleEscapesOnlyMetadataTitle() {
		SmartClientViewRenderer renderer = rendererWithOpenTabPane();
		Tab tab = new Tab();
		tab.setIconStyleClass("fa-solid fa-star");

		renderer.renderTab(UNSAFE_TITLE, null, tab);
		renderer.renderedTab(UNSAFE_TITLE, null, tab);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("<i class=\"bizhubFontIcon fa-solid fa-star\"></i><span> &nbsp;</span>" +
									SmartClientViewRenderer.escapeSmartClientText(UNSAFE_TITLE, true)), code);
	}

	@Test
	void renderedTabWithIconUrlWritesRelativeIconPath() {
		when(view.getName()).thenReturn("edit");
		when(view.getSidebar()).thenReturn(null);

		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, document, view, "desktop", false);
		renderer.renderView(null, null);

		TabPane tabPane = new TabPane();
		renderer.renderTabPane(tabPane);

		Tab tab = new Tab();
		renderer.renderTab("Documents", "images/doc16.png", tab);
		renderer.renderedTab("Documents", "images/doc16.png", tab);
		renderer.renderedTabPane(tabPane);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("icon:'../images/doc16.png'"), code);
		assertTrue(code.contains("title:'Documents'"), code);
	}

	@Test
	void renderedTabWithoutIconUsesPlainEscapedTitlePath() {
		when(view.getName()).thenReturn("edit");
		when(view.getSidebar()).thenReturn(null);

		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, document, view, "desktop", false);
		renderer.renderView(null, null);

		TabPane tabPane = new TabPane();
		renderer.renderTabPane(tabPane);

		Tab tab = new Tab();
		renderer.renderTab("Plain ' title", null, tab);
		renderer.renderedTab("Plain ' title", null, tab);
		renderer.renderedTabPane(tabPane);

		String code = renderer.getCode().toString();
		assertTrue(code.contains(escapedTabTitle("Plain ' title", null)), code);
		assertFalse(code.contains("bizhubFontIcon"));
		assertFalse(code.contains("icon:'../"));
	}

	@Test
	void renderedTabIncrementsTabNumbersAcrossTabs() {
		when(view.getName()).thenReturn("edit");
		when(view.getSidebar()).thenReturn(null);

		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, document, view, "desktop", false);
		renderer.renderView(null, null);

		TabPane tabPane = new TabPane();
		renderer.renderTabPane(tabPane);

		Tab first = new Tab();
		renderer.renderTab("First", null, first);
		renderer.renderedTab("First", null, first);

		Tab second = new Tab();
		renderer.renderTab("Second", null, second);
		renderer.renderedTab("Second", null, second);

		renderer.renderedTabPane(tabPane);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("addBizTab({name:'0'"), code);
		assertTrue(code.contains("addBizTab({name:'1'"), code);
	}

	@Test
	void renderTabPaneWithDisabledAndInvisibleWritesBothConditions() {
		when(view.getName()).thenReturn("edit");
		when(view.getSidebar()).thenReturn(null);

		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, document, view, "desktop", false);
		renderer.renderView(null, null);

		TabPane tabPane = new TabPane();
		tabPane.setDisabledConditionName("bean.tabPaneDisabled");
		tabPane.setInvisibleConditionName("bean.tabPaneHidden");
		renderer.renderTabPane(tabPane);
		renderer.renderedTabPane(tabPane);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("disabledConditionName:'bean.tabPaneDisabled'"), code);
		assertTrue(code.contains("invisibleConditionName:'bean.tabPaneHidden'"), code);
	}

	@Test
	void renderFormColumnsRowsAndItemsWritesLayoutFragments() {
		when(view.getName()).thenReturn("edit");
		when(view.getSidebar()).thenReturn(null);

		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, document, view, "desktop", false);
		renderer.renderView(null, null);

		Form form = new Form();
		form.setLabelDefaultHorizontalAlignment(HorizontalAlignment.right);
		form.getColumns().add(new FormColumn());
		form.getColumns().add(new FormColumn());
		form.getColumns().add(new FormColumn());
		form.getColumns().add(new FormColumn());

		renderer.renderForm("Details", form);
		FormColumn pixel = new FormColumn();
		pixel.setPixelWidth(Integer.valueOf(120));
		renderer.renderFormColumn(pixel);
		FormColumn percentage = new FormColumn();
		percentage.setPercentageWidth(Integer.valueOf(25));
		renderer.renderFormColumn(percentage);
		FormColumn responsive = new FormColumn();
		responsive.setResponsiveWidth(Integer.valueOf(6));
		renderer.renderFormColumn(responsive);
		renderer.renderFormColumn(new FormColumn());

		FormRow row = new FormRow();
		renderer.renderFormRow(row);
		FormItem item = new FormItem();
		item.setRowspan(Integer.valueOf(2));
		item.setHorizontalAlignment(HorizontalAlignment.centre);
		item.setLabelHorizontalAlignment(HorizontalAlignment.left);
		renderer.renderFormItem("Status", null, null, true, 2, item);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("titleAlign:'right'"), code);
		assertTrue(code.contains("numCols:4,colWidths:[120,'25%','50%','*']"), code);
		assertTrue(code.contains("view._vm.addMember("), code);
		assertTrue(code.contains("showTitle:true,colSpan:2,rowSpan:2,align:'center',titleAlign:'left',"), code);
	}

	@Test
	void renderFormEscapesBorderTitleUsingFormFlag() {
		SmartClientViewRenderer renderer = rendererWithViewContainer();
		Form form = new Form();
		form.setBorder(Boolean.TRUE);
		form.setEscapeBorderTitle(Boolean.FALSE);

		renderer.renderForm(UNSAFE_TITLE, form);

		String code = renderer.getCode().toString();
		assertTrue(code.contains(escapedGroupTitle(UNSAFE_TITLE, Boolean.FALSE)), code);
		assertFalse(code.contains(OWASP.escapeHtml(UNSAFE_TITLE)), code);
	}

	@Test
	void renderMapAndChartAppendConfiguredWidgetsToCurrentContainer() {
		when(view.getName()).thenReturn("edit");
		when(view.getSidebar()).thenReturn(null);

		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, document, view, "desktop", false);
		renderer.renderView(null, null);

		MapDisplay map = new MapDisplay();
		map.setModelName("locations");
		map.setLoading(LoadingType.lazy);
		map.setRefreshTimeInSeconds(Integer.valueOf(30));
		map.setShowRefreshControls(Boolean.TRUE);
		renderer.renderMap(map);

		Chart chart = new Chart();
		chart.setType(ChartType.bar);
		chart.setModelName("salesByMonth");
		chart.setPixelWidth(Integer.valueOf(320));
		renderer.renderChart(chart);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("isc.BizMap.create({_view:view,loading:'lazy',refreshTime:30,showRefresh:true})"), code);
		assertTrue(code.contains(".setDataSource('locations');"), code);
		assertTrue(code.contains("isc.BizChart.create({_view:view,width:320,chartType:'bar'})"), code);
		assertTrue(code.contains(".setDataSource('salesByMonth');"), code);
	}

	@Test
	void renderSpacerStaticImageAndLabelAppendStandaloneComponents() {
		when(view.getName()).thenReturn("edit");
		when(view.getSidebar()).thenReturn(null);

		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, document, view, "desktop", false);
		renderer.renderView(null, null);

		Spacer spacer = new Spacer();
		spacer.setPixelWidth(Integer.valueOf(12));
		spacer.setPixelHeight(Integer.valueOf(8));
		spacer.setInvisibleConditionName("bean.hideSpacer");
		renderer.renderSpacer(spacer);

		StaticImage image = new StaticImage();
		image.setRelativeFile("images/contact.png");
		image.setPixelWidth(Integer.valueOf(64));
		renderer.renderStaticImage("images/contact.png", image);

		Label label = new Label();
		label.setTextAlignment(HorizontalAlignment.right);
		renderer.renderLabel("Contact summary", false, label);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("isc.LayoutSpacer.create({width:12,height:8,invisibleConditionName:'bean.hideSpacer'})"), code);
		assertTrue(code.contains("isc.BizImage.create({modoc:'admin.Contact',file:'images/contact.png',width:64})"), code);
		assertTrue(code.contains("isc.BizLabel.create({width:'100%',textAlign:'right',value:'Contact summary'});"), code);
	}

	@Test
	void renderBoundLabelUsesSanitisedBindingAndRejectsBoundStandaloneLabel() {
		when(view.getName()).thenReturn("edit");
		when(view.getSidebar()).thenReturn(null);

		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, document, view, "desktop", false);
		renderer.renderView(null, null);

		Label label = new Label();
		label.setBinding("contact.name");
		renderer.renderLabel("ignored", false, label);
		assertTrue(renderer.getCode().toString().contains("binding:'contact_name'"));

		Label invalid = new Label();
		invalid.setValue("{bizKey}");
		assertThrows(MetaDataException.class, () -> renderer.renderLabel("{bizKey}", true, invalid));
	}

	@Test
	void renderFormGeometryAndMapWriteDrawingToolsAndDefaultMapHeight() {
		addTextAttribute("area");
		addTextAttribute("mapArea");
		SmartClientViewRenderer renderer = rendererWithOpenForm();

		FormItem geometryItem = beginFormItem(renderer, "Area");
		Geometry geometry = new Geometry();
		geometry.setBinding("area");
		geometry.setType(GeometryInputType.polygon);
		geometry.setDisabledConditionName("areaDisabled");
		geometry.setInvisibleConditionName("areaHidden");
		renderer.visitGeometry(geometry, true, true);
		renderer.visitedGeometry(geometry, true, true);
		renderer.visitedFormItem(geometryItem, true, true);

		FormItem mapItem = beginFormItem(renderer, "Map Area");
		GeometryMap geometryMap = new GeometryMap();
		geometryMap.setBinding("mapArea");
		geometryMap.setType(GeometryInputType.line);
		renderer.visitGeometryMap(geometryMap, true, true);
		renderer.visitedGeometryMap(geometryMap, true, true);
		renderer.visitedFormItem(mapItem, true, true);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("type:'geometry'"), code);
		assertTrue(code.contains("drawingTools:'polygon'"), code);
		assertTrue(code.contains("disabledConditionName:'areaDisabled'"), code);
		assertTrue(code.contains("invisibleConditionName:'areaHidden'"), code);
		assertTrue(code.contains("type:'geometryMap'"), code);
		assertTrue(code.contains("height:'100%'"), code);
		assertTrue(code.contains("minHeight:170"), code);
		assertTrue(code.contains("drawingTools:'line'"), code);
	}

	@Test
	void renderFormContentUploadWritesDisplayCaptureAndCompanionMetadata() {
		addTextAttribute("attachment");
		SmartClientViewRenderer renderer = rendererWithOpenForm();

		FormItem item = beginFormItem(renderer, "Attachment");
		ContentUpload content = new ContentUpload();
		content.setBinding("attachment");
		content.setCapture(ContentCapture.all);
		content.setShowMarkup(Boolean.TRUE);
		renderer.visitContent(content, true, true);
		renderer.visitedFormItem(item, true, true);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("v0.setItems([{showTitle:true"), code);
		assertTrue(code.contains(",name:'attachment'"), code);
		assertTrue(code.contains("type:'bizContent'"), code);
		assertTrue(code.contains("display:'auto'"), code);
		assertTrue(code.contains("capture:'all'"), code);
		assertTrue(code.contains("emptyText:'No content'"), code);
		assertTrue(code.contains("uploadNoun:'Content'"), code);
		assertTrue(code.contains("companion:'_attachment'"), code);
		assertTrue(code.contains("showMarkup:true"), code);
		assertFalse(code.contains("width:200"), code);
		assertFalse(code.contains("height:200"), code);
	}

	@Test
	void renderFormContentUploadAutoWritesExplicitImageDimensions() {
		addTextAttribute("attachment");
		SmartClientViewRenderer renderer = rendererWithOpenForm();

		FormItem item = beginFormItem(renderer, "Attachment");
		ContentUpload content = new ContentUpload();
		content.setBinding("attachment");
		content.setPixelWidth(Integer.valueOf(120));
		content.setPixelHeight(Integer.valueOf(80));
		renderer.visitContent(content, true, true);
		renderer.visitedFormItem(item, true, true);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("type:'bizContent'"), code);
		assertTrue(code.contains("display:'auto'"), code);
		assertTrue(code.contains("width:120"), code);
		assertTrue(code.contains("height:80"), code);
	}

	@Test
	void renderFormContentUploadVideoUsesFluidDimensionsWhenUnsized() {
		addTextAttribute("movie");
		SmartClientViewRenderer renderer = rendererWithOpenForm();

		FormItem item = beginFormItem(renderer, "Movie");
		ContentUpload content = new ContentUpload();
		content.setBinding("movie");
		content.setDisplay(ContentDisplay.video);
		renderer.visitContent(content, true, true);
		renderer.visitedFormItem(item, true, true);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("type:'bizContent'"), code);
		assertTrue(code.contains("display:'video'"), code);
		assertTrue(code.contains("emptyText:'No video'"), code);
		assertTrue(code.contains("uploadNoun:'Video'"), code);
		assertFalse(code.contains("width:320"), code);
		assertFalse(code.contains("height:180"), code);
	}

	@Test
	void renderFormContentUploadCameraCaptureWritesImageUploadNoun() {
		addTextAttribute("photo");
		SmartClientViewRenderer renderer = rendererWithOpenForm();

		FormItem item = beginFormItem(renderer, "Photo");
		ContentUpload content = new ContentUpload();
		content.setBinding("photo");
		content.setCapture(ContentCapture.camera);
		renderer.visitContent(content, true, true);
		renderer.visitedFormItem(item, true, true);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("type:'bizContent'"), code);
		assertTrue(code.contains("display:'auto'"), code);
		assertTrue(code.contains("capture:'camera'"), code);
		assertTrue(code.contains("uploadNoun:'Image'"), code);
	}

	@Test
	void renderFormContentUploadLinkWritesLinkDisplayMetadata() {
		addTextAttribute("attachment");
		SmartClientViewRenderer renderer = rendererWithOpenForm();

		FormItem item = beginFormItem(renderer, "Attachment");
		ContentUpload content = new ContentUpload();
		content.setBinding("attachment");
		content.setDisplay(ContentDisplay.link);
		content.setPixelWidth(Integer.valueOf(303));
		content.setPixelHeight(Integer.valueOf(303));
		renderer.visitContent(content, true, true);
		renderer.visitedFormItem(item, true, true);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("type:'bizContent'"), code);
		assertTrue(code.contains("display:'link'"), code);
		assertTrue(code.contains("emptyText:'No file'"), code);
		assertTrue(code.contains("uploadNoun:'Content'"), code);
		assertFalse(code.contains("companion:'_attachment'"), code);
		assertTrue(code.contains("width:303"), code);
		assertTrue(code.contains("height:303"), code);
	}

	@Test
	void contentUploadDataGridFieldWritesFormatterProperties() {
		addTextAttribute("attachment");
		ContentUpload content = new ContentUpload();
		content.setBinding("attachment");
		content.setCapture(ContentCapture.video);
		content.setShowMarkup(Boolean.TRUE);

		SmartClientDataGridFieldDefinition definition =
				new SmartClientDataGridFieldDefinition(user, customer, module, document, content, null, false, true, false, "desktop");
		String javascript = definition.toJavascript();

		assertTrue(javascript.contains("editorType:'bizContent'"), javascript);
		assertTrue(javascript.contains("display:'auto'"), javascript);
		assertTrue(javascript.contains("capture:'video'"), javascript);
		assertTrue(javascript.contains("emptyText:'No content'"), javascript);
		assertTrue(javascript.contains("companion:'_attachment'"), javascript);
		assertTrue(javascript.contains("k=(rec&&rec['_attachment'])||'link'"), javascript);
		assertFalse(javascript.contains("&_w=200&_h=200"), javascript);
		assertTrue(javascript.contains("style=\"width:100%;height:auto;aspect-ratio:1 / 1;object-fit:contain\""), javascript);
		assertTrue(javascript.contains("<video controls preload=\"metadata\""), javascript);
		assertTrue(javascript.contains("style=\"width:100%;height:auto;aspect-ratio:16 / 9;object-fit:contain\""), javascript);
	}

	@Test
	void renderBoundColumnContentUploadWritesDataGridFormatter() {
		addTextAttribute("attachment");
		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, document, view, "desktop", false);
		ContentUpload content = new ContentUpload();
		content.setBinding("attachment");
		DataGridBoundColumn column = new DataGridBoundColumn();

		renderer.renderBoundColumnContent(content);
		renderer.renderedDataGridBoundColumn("Attachment", column);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("editorType:'bizContent'"), code);
		assertTrue(code.contains("formatCellValue:function"), code);
	}

	@Test
	void renderContainerColumnContentUploadIsNoOp() {
		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, document, view, "desktop", false);
		ContentUpload content = new ContentUpload();
		content.setBinding("attachment");

		renderer.renderContainerColumnContent(content);

		assertTrue(renderer.getCode().isEmpty());
	}

	@Test
	void renderFormHtmlPasswordAndRadioWriteSpecialOptions() {
		addTextAttribute("markup");
		addTextAttribute("secret");
		addTextAttribute("choice");
		SmartClientViewRenderer renderer = rendererWithOpenForm();

		FormItem htmlItem = beginFormItem(renderer, "Markup");
		HTML html = new HTML();
		html.setBinding("markup");
		html.setMentionMarkers("@,#");
		renderer.visitHTML(html, true, true);
		renderer.visitedFormItem(htmlItem, true, true);

		FormItem passwordItem = beginFormItem(renderer, "Secret");
		Password password = new Password();
		password.setBinding("secret");
		renderer.visitPassword(password, true, true);
		renderer.visitedFormItem(passwordItem, true, true);

		FormItem radioItem = beginFormItem(renderer, "Choice");
		Radio radio = new Radio();
		radio.setBinding("choice");
		radio.setVertical(Boolean.FALSE);
		renderer.visitRadio(radio, true, true);
		renderer.visitedFormItem(radioItem, true, true);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("type:'bizHTML'"), code);
		assertTrue(code.contains("mentionMarkers:'@,#'"), code);
		assertTrue(code.contains("type:'password'"), code);
		assertTrue(code.contains("autoComplete:'none'"), code);
		assertTrue(code.contains("browserSpellCheck:false"), code);
		assertTrue(code.contains("type:'radioGroup'"), code);
		assertTrue(code.contains("vertical:false"), code);
	}

	@Test
	void renderFormInputEscapesLabelRequiredMessageAndHelpByDefault() {
		addTextAttribute("name");
		SmartClientViewRenderer renderer = rendererWithOpenForm();

		FormItem item = beginFormItem(renderer, UNSAFE_TITLE);
		item.setRequired(Boolean.TRUE);
		item.setRequiredMessage(UNSAFE_TITLE);
		item.setHelp(UNSAFE_TITLE);
		TextField textField = new TextField();
		textField.setBinding("name");
		renderer.visitTextField(textField, true, true);
		renderer.visitedFormItem(item, true, true);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("title:'" + SmartClientViewRenderer.escapeSmartClientText(UNSAFE_TITLE, true) + " *'"), code);
		assertTrue(code.contains("requiredMessage:'" + SmartClientViewRenderer.escapeSmartClientText(UNSAFE_TITLE, true) + "'"), code);
		assertTrue(code.contains("prompt:'" + SmartClientViewRenderer.escapeSmartClientText(UNSAFE_TITLE, true) + "'"), code);
	}

	@Test
	void renderFormInputHonoursFalseLabelRequiredMessageAndHelpFlags() {
		addTextAttribute("name");
		SmartClientViewRenderer renderer = rendererWithOpenForm();

		FormItem item = beginFormItem(renderer, UNSAFE_TITLE);
		item.setEscapeLabel(Boolean.FALSE);
		item.setRequired(Boolean.TRUE);
		item.setRequiredMessage(UNSAFE_TITLE);
		item.setEscapeRequiredMessage(Boolean.FALSE);
		item.setHelp(UNSAFE_TITLE);
		item.setEscapeHelp(Boolean.FALSE);
		TextField textField = new TextField();
		textField.setBinding("name");
		renderer.visitTextField(textField, true, true);
		renderer.visitedFormItem(item, true, true);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("title:'" + SmartClientViewRenderer.escapeSmartClientText(UNSAFE_TITLE, false) + " *'"), code);
		assertTrue(code.contains("requiredMessage:'" + SmartClientViewRenderer.escapeSmartClientText(UNSAFE_TITLE, false) + "'"), code);
		assertTrue(code.contains("prompt:'" + SmartClientViewRenderer.escapeSmartClientText(UNSAFE_TITLE, false) + "'"), code);
		assertFalse(code.contains(OWASP.escapeHtml(UNSAFE_TITLE)), code);
	}

	@Test
	void renderFormItemLabelBypassesHonourEscapeLabelFlag() {
		SmartClientViewRenderer renderer = rendererWithOpenForm();

		FormItem staticImageItem = beginFormItem(renderer, UNSAFE_TITLE);
		staticImageItem.setEscapeLabel(Boolean.FALSE);
		staticImageItem.setShowLabel(Boolean.TRUE);
		StaticImage image = new StaticImage();
		image.setRelativeFile("icons/test.png");
		renderer.visitStaticImage(image, true, true);
		renderer.visitedFormItem(staticImageItem, true, true);

		FormItem linkItem = beginFormItem(renderer, UNSAFE_TITLE);
		linkItem.setEscapeLabel(Boolean.FALSE);
		Link link = new Link();
		link.setValue("Open");
		renderer.visitLink(link, true, true);
		renderer.visitedFormItem(linkItem, true, true);

		FormItem injectItem = beginFormItem(renderer, UNSAFE_TITLE);
		injectItem.setEscapeLabel(Boolean.FALSE);
		injectItem.setRequired(Boolean.TRUE);
		Inject inject = new Inject();
		inject.setScript("name:'custom'");
		renderer.visitInject(inject, true, true);
		renderer.visitedFormItem(injectItem, true, true);

		String escapedRawTitle = SmartClientViewRenderer.escapeSmartClientText(UNSAFE_TITLE, false);
		String code = renderer.getCode().toString();
		assertTrue(code.contains("showTitle:true,title:\"" + escapedRawTitle + "\""), code);
		assertTrue(code.contains("title:'" + escapedRawTitle + "',type:'blurb'"), code);
		assertTrue(code.contains("title:'" + escapedRawTitle + "',required:true,name:'custom'"), code);
		assertFalse(code.contains(OWASP.escapeHtml(UNSAFE_TITLE)), code);
	}

	@Test
	void renderListMembershipEscapesHeadingsByDefaultAndWhenExplicitTrue() {
		addCollectionAttribute("roles");
		SmartClientViewRenderer defaultRenderer = rendererWithViewContainer();
		ListMembership defaultMembership = listMembership(Boolean.TRUE);
		defaultMembership.setEscapeCandidatesHeading(null);
		defaultMembership.setEscapeMembersHeading(null);

		defaultRenderer.visitListMembership(defaultMembership, true, true);
		String defaultCode = defaultRenderer.getCode().toString();
		assertTrue(defaultCode.contains("candidatesHeading:'" + SmartClientViewRenderer.escapeSmartClientText(UNSAFE_TITLE, true) + "'"), defaultCode);
		assertTrue(defaultCode.contains("membersHeading:'" + SmartClientViewRenderer.escapeSmartClientText(UNSAFE_TITLE, true) + "'"), defaultCode);

		SmartClientViewRenderer explicitRenderer = rendererWithViewContainer();
		ListMembership explicitMembership = listMembership(Boolean.TRUE);

		explicitRenderer.visitListMembership(explicitMembership, true, true);
		String explicitCode = explicitRenderer.getCode().toString();
		assertTrue(explicitCode.contains("candidatesHeading:'" + SmartClientViewRenderer.escapeSmartClientText(UNSAFE_TITLE, true) + "'"), explicitCode);
		assertTrue(explicitCode.contains("membersHeading:'" + SmartClientViewRenderer.escapeSmartClientText(UNSAFE_TITLE, true) + "'"), explicitCode);
	}

	@Test
	void renderListMembershipAllowsTrustedHeadingsWhenEscapingFalse() {
		addCollectionAttribute("roles");
		SmartClientViewRenderer renderer = rendererWithViewContainer();
		ListMembership membership = listMembership(Boolean.FALSE);

		renderer.visitListMembership(membership, true, true);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("candidatesHeading:'" + SmartClientViewRenderer.escapeSmartClientText(UNSAFE_TITLE, false) + "'"), code);
		assertTrue(code.contains("membersHeading:'" + SmartClientViewRenderer.escapeSmartClientText(UNSAFE_TITLE, false) + "'"), code);
		assertFalse(code.contains(OWASP.escapeHtml(UNSAFE_TITLE)), code);
	}

	@Test
	void renderFormSliderSpinnerAndTextInputsWriteNumericAndCompletionOptions() {
		addTextAttribute("score");
		addTextAttribute("quantity");
		addTextAttribute("notes");
		addTextAttribute("name");
		SmartClientViewRenderer renderer = rendererWithOpenForm();

		FormItem sliderItem = beginFormItem(renderer, "Score");
		Slider slider = new Slider();
		slider.setBinding("score");
		slider.setMin(Double.valueOf(1.0));
		slider.setMax(Double.valueOf(5.0));
		slider.setNumberOfDiscreteValues(Integer.valueOf(4));
		slider.setRoundingPrecision(Integer.valueOf(2));
		slider.setVertical(Boolean.TRUE);
		renderer.visitSlider(slider, true, true);
		renderer.visitedFormItem(sliderItem, true, true);

		FormItem spinnerItem = beginFormItem(renderer, "Quantity");
		Spinner spinner = new Spinner();
		spinner.setBinding("quantity");
		spinner.setMin(Double.valueOf(0.0));
		spinner.setMax(Double.valueOf(10.0));
		spinner.setStep(Double.valueOf(0.5));
		renderer.visitSpinner(spinner, true, true);
		renderer.visitedFormItem(spinnerItem, true, true);

		FormItem textAreaItem = beginFormItem(renderer, "Notes");
		TextArea textArea = new TextArea();
		textArea.setBinding("notes");
		textArea.setEditable(Boolean.FALSE);
		renderer.visitTextArea(textArea, true, true);
		renderer.visitedFormItem(textAreaItem, true, true);

		FormItem textFieldItem = beginFormItem(renderer, "Name");
		TextField textField = new TextField();
		textField.setBinding("name");
		textField.setComplete(CompleteType.previous);
		textField.setEditable(Boolean.FALSE);
		renderer.visitTextField(textField, true, true);
		renderer.visitedFormItem(textFieldItem, true, true);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("type:'slider'"), code);
		assertTrue(code.contains("minValue:1.0"), code);
		assertTrue(code.contains("maxValue:5.0"), code);
		assertTrue(code.contains("numValues:5"), code);
		assertTrue(code.contains("roundValues:false,roundingPrecision:2"), code);
		assertTrue(code.contains("vertical:true"), code);
		assertTrue(code.contains("type:'spinner'"), code);
		assertTrue(code.contains("min:0.0"), code);
		assertTrue(code.contains("max:10.0"), code);
		assertTrue(code.contains("step:0.5"), code);
		assertTrue(code.contains("type:'textArea'"), code);
		assertTrue(code.contains("selectOnFocus:true"), code);
		assertTrue(code.contains("type:'bizComplete'"), code);
		assertTrue(code.contains("_a:'previous'"), code);
		assertTrue(code.contains("_b:'name'"), code);
	}

	@Test
	void renderActionsWritesPanelButtonsParametersAndSkipRules() {
		when(view.getName()).thenReturn("edit");
		when(view.getSidebar()).thenReturn(null);
		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, document, view, "desktop", false);
		renderer.renderView(null, null);

		ActionImpl custom = action("customAction");
		custom.setClientValidation(Boolean.FALSE);
		custom.setDisabledConditionName("customDisabled");
		custom.setInvisibleConditionName("customHidden");
		ParameterImpl literal = new ParameterImpl();
		literal.setName("literal.param");
		literal.setValue("fixed");
		ParameterImpl binding = new ParameterImpl();
		binding.setName("bound.param");
		binding.setValueBinding("contact.name");
		custom.getParameters().add(literal);
		custom.getParameters().add(binding);
		renderer.renderCustomAction("customAction", "Do It", null, "fa-solid fa-play", "Run it", "Sure?", custom);

		renderer.renderAddAction("addAction", "Add", null, null, null, null, action("addAction"));
		renderer.renderEditAction("editAction", "Edit", null, null, null, null, action("editAction"));
		renderer.renderRemoveAction("removeAction", "Remove", "icons/remove.png", null, null, null, action("removeAction"), false);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("view.addEditAction(isc.BizButton.create({validate:false,type:null,actionName:'customAction'"), code);
		assertTrue(code.contains("<i class=\"bizhubFontIcon fa-solid fa-play\"></i><span> &nbsp;</span>Do It"), code);
		assertTrue(code.contains("disabledConditionName:'customDisabled'"), code);
		assertTrue(code.contains("invisibleConditionName:'customHidden'"), code);
		assertTrue(code.contains("tooltip:'Run it'"), code);
		assertTrue(code.contains("confirm:'Sure?'"), code);
		assertTrue(code.contains("params:{'literal_param':'fixed','bound_param':'{contact.name}'}"), code);
		assertFalse(code.contains("actionName:'addAction'"), code);
		assertFalse(code.contains("actionName:'editAction'"), code);
		assertTrue(code.contains("type:'Remove',actionName:'removeAction'"), code);
		assertTrue(code.contains("icon:'../icons/remove.png'"), code);
		assertTrue(code.contains("_canDelete:false"), code);
	}

	@Test
	void renderActionEscapesDisplayNameTooltipAndConfirmUnlessExplicitlyTrusted() {
		when(view.getName()).thenReturn("edit");
		when(view.getSidebar()).thenReturn(null);
		doReturn(Boolean.TRUE).when(user).canExecuteAction(document, "unsafeAction");
		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, document, view, "desktop", false);
		renderer.renderView(null, null);

		ActionImpl action = action("unsafeAction");
		action.setDisplayName(UNSAFE_TITLE);
		action.setToolTip(UNSAFE_TITLE);
		action.setConfirmationText(UNSAFE_TITLE);
		renderer.visitCustomAction(action);

		String escaped = SmartClientViewRenderer.escapeSmartClientText(UNSAFE_TITLE, true);
		String code = renderer.getCode().toString();
		assertTrue(code.contains("displayName:'<span> &nbsp;</span>" + escaped + "'"), code);
		assertTrue(code.contains("tooltip:'" + escaped + "'"), code);
		assertTrue(code.contains("confirm:'" + escaped + "'"), code);

		SmartClientViewRenderer trustedRenderer = new SmartClientViewRenderer(user, module, document, view, "desktop", false);
		trustedRenderer.renderView(null, null);
		action.setEscapeDisplayName(Boolean.FALSE);
		action.setEscapeToolTip(Boolean.FALSE);
		action.setEscapeConfirm(Boolean.FALSE);
		trustedRenderer.visitCustomAction(action);

		String trusted = SmartClientViewRenderer.escapeSmartClientText(UNSAFE_TITLE, false);
		String trustedCode = trustedRenderer.getCode().toString();
		assertTrue(trustedCode.contains("displayName:'<span> &nbsp;</span>" + trusted + "'"), trustedCode);
		assertTrue(trustedCode.contains("tooltip:'" + trusted + "'"), trustedCode);
		assertTrue(trustedCode.contains("confirm:'" + trusted + "'"), trustedCode);
	}

	@Test
	void renderButtonUploadActionReferenceWritesCapture() {
		when(view.getName()).thenReturn("edit");
		when(view.getSidebar()).thenReturn(null);
		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, document, view, "desktop", false);
		renderer.renderView(null, null);

		ActionImpl upload = action("UploadBackup");
		upload.setImplicitName(ImplicitActionName.Upload);
		upload.getProperties().put("capture", ContentCapture.all.name());
		Button button = new Button();

		renderer.renderButton("UploadBackup", "Upload", null, null, null, null, upload, button);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("type:'Upload',actionName:'UploadBackup'"), code);
		assertTrue(code.contains("capture:'all'"), code);
	}

	@Test
	void eventHandlersWriteServerRerenderAndVisibilityActions() {
		when(view.getName()).thenReturn("edit");
		when(view.getSidebar()).thenReturn(null);
		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, document, view, "desktop", false);
		renderer.renderView(null, null);

		TextField text = new TextField();
		renderer.visitOnChangedEventHandler(text, true, true);
		renderer.visitedOnChangedEventHandler(text, true, true);
		renderer.visitOnFocusEventHandler(text, true, true);
		renderer.visitedOnFocusEventHandler(text, true, true);

		ActionImpl serverAction = action("serverAction");
		serverAction.setClientValidation(Boolean.FALSE);
		renderer.visitOnBlurEventHandler(text, true, true);
		renderer.visitServerSideActionEventAction(serverAction, new ServerSideActionEventAction());
		RerenderEventAction rerender = new RerenderEventAction();
		rerender.setClientValidation(Boolean.FALSE);
		EventSource source = mock(EventSource.class);
		when(source.getSource()).thenReturn("contact.name");
		renderer.visitRerenderEventAction(rerender, source, true, true);
		renderer.visitedOnBlurEventHandler(text, true, true);

		SetDisabledEventAction setDisabled = new SetDisabledEventAction();
		setDisabled.setBinding("contact.name");
		setDisabled.setDisabledConditionName("nameDisabled");
		renderer.visitSetDisabledEventAction(setDisabled, true, true);

		SetInvisibleEventAction setInvisible = new SetInvisibleEventAction();
		setInvisible.setBinding("contact.secret");
		setInvisible.setInvisibleConditionName("secretHidden");
		renderer.visitSetInvisibleEventAction(setInvisible, true, true);

		ToggleDisabledEventAction toggleDisabled = new ToggleDisabledEventAction();
		toggleDisabled.setBinding("contact.active");
		renderer.visitToggleDisabledEventAction(toggleDisabled, true, true);

		ToggleVisibilityEventAction toggleVisibility = new ToggleVisibilityEventAction();
		toggleVisibility.setBinding("contact.notes");
		renderer.visitToggleVisibilityEventAction(toggleVisibility, true, true);

		String code = renderer.getCode().toString();
		assertTrue(code.contains("changed:function(){var view=this._view;},"));
		assertTrue(code.contains("editorEnter:function(form,item,value){if((!isc.RPCManager.requestsArePending())&&item.validate()){var view=form._view;}},"));
		assertTrue(code.contains("blur:function(form,item){if(isc.RPCManager.requestsArePending())"), code);
		assertTrue(code.contains("view.doBlurryAction('serverAction',false);"), code);
		assertTrue(code.contains("view.rerenderBlurryAction(false,'contact.name');"), code);
		assertTrue(code.contains("view.setDisabled('contact_name','nameDisabled');"), code);
		assertTrue(code.contains("view.setInvisible('contact_secret','secretHidden');"), code);
		assertTrue(code.contains("view.toggleDisabled('contact_active');"), code);
		assertTrue(code.contains("view.toggleVisibility('contact_notes');"), code);
	}

	private SmartClientViewRenderer rendererWithOpenForm() {
		when(view.getName()).thenReturn("edit");
		when(view.getSidebar()).thenReturn(null);

		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, document, view, "desktop", false);
		renderer.renderView(null, null);
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		renderer.renderForm("Details", form);
		renderer.renderFormColumn(new FormColumn());
		renderer.renderFormRow(new FormRow());
		return renderer;
	}

	private SmartClientViewRenderer rendererWithViewContainer() {
		when(view.getName()).thenReturn("edit");
		when(view.getSidebar()).thenReturn(null);

		SmartClientViewRenderer renderer = new SmartClientViewRenderer(user, module, document, view, "desktop", false);
		renderer.renderView(null, null);
		return renderer;
	}

	private SmartClientViewRenderer rendererWithOpenTabPane() {
		SmartClientViewRenderer renderer = rendererWithViewContainer();
		renderer.renderTabPane(new TabPane());
		return renderer;
	}

	private static String escapedTabTitle(String title, Boolean escape) {
		return "title:'" + SmartClientViewRenderer.escapeSmartClientText(title, ViewRenderer.shouldEscape(escape)) + "'";
	}

	private static String escapedGroupTitle(String title, Boolean escape) {
		return "groupTitle:'&nbsp;&nbsp;" + SmartClientViewRenderer.escapeSmartClientText(title, ViewRenderer.shouldEscape(escape)) + "&nbsp;&nbsp;'";
	}

	private static String escapedCollapsibleTitle(String title, Boolean escape) {
		return "BizCollapsible.create({title:'" + SmartClientViewRenderer.escapeSmartClientText(title, ViewRenderer.shouldEscape(escape)) + "'";
	}

	private static FormItem beginFormItem(SmartClientViewRenderer renderer, String label) {
		FormItem item = new FormItem();
		item.setLabel(label);
		renderer.visitFormItem(item, true, true);
		return item;
	}

	private void addTextAttribute(String name) {
		Text text = new Text();
		text.setName(name);
		text.setDisplayName(name);
		text.setLength(100);
		document.putAttribute(text);
	}

	private void addCollectionAttribute(String name) {
		CollectionImpl collection = new CollectionImpl();
		collection.setName(name);
		collection.setDisplayName(name);
		collection.setDocumentName("Role");
		document.putAttribute(collection);
	}

	private static ListMembership listMembership(Boolean escape) {
		ListMembership membership = new ListMembership();
		membership.setBinding("roles");
		membership.setCandidatesHeading(UNSAFE_TITLE);
		membership.setMembersHeading(UNSAFE_TITLE);
		membership.setEscapeCandidatesHeading(escape);
		membership.setEscapeMembersHeading(escape);
		return membership;
	}

	private static ActionImpl action(String resourceName) {
		ActionImpl action = new ActionImpl();
		action.setResourceName(resourceName);
		return action;
	}
}
