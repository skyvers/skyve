package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.controller.CustomisationsStaticSingleton;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.LoadingType;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
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

class SmartClientViewRendererCoverageTest {
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
		assertTrue(code.contains("title:'Plain \\' title'"), code);
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
		assertTrue(code.contains("companion:'_attachment'"), code);
		assertTrue(code.contains("showMarkup:true"), code);
		assertTrue(code.contains("width:100"), code);
		assertTrue(code.contains("height:100"), code);
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
	void renderFormContentUploadVideoWritesStableDefaultDimensions() {
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
		assertTrue(code.contains("width:320"), code);
		assertTrue(code.contains("height:180"), code);
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
		assertTrue(javascript.contains("&_w=100&_h=100"), javascript);
		assertTrue(javascript.contains("style=\"width:100px;height:100px;object-fit:contain\""), javascript);
		assertTrue(javascript.contains("<video controls preload=\"metadata\""), javascript);
		assertTrue(javascript.contains("style=\"width:160px;height:90px;object-fit:contain\""), javascript);
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

	private static ActionImpl action(String resourceName) {
		ActionImpl action = new ActionImpl();
		action.setResourceName(resourceName);
		return action;
	}
}
