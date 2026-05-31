package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.VerticalAlignment;
import org.skyve.impl.metadata.view.container.Collapsible;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.user.User;

class SmartClientViewRendererCoverageTest {
	private CustomerImpl customer;
	private User user;
	private ModuleImpl module;
	private DocumentImpl document;
	private ViewImpl view;

	@BeforeEach
	void before() {
		customer = mock(CustomerImpl.class);
		user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);

		module = mock(ModuleImpl.class);
		document = new DocumentImpl();
		document.setName("Contact");

		view = mock(ViewImpl.class);
		when(view.getTitle()).thenReturn("Contacts");
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
}
