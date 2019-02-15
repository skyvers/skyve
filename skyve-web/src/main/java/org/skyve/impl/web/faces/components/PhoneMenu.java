package org.skyve.impl.web.faces.components;

import java.io.IOException;
import java.util.Stack;

import javax.faces.application.Application;
import javax.faces.component.FacesComponent;
import javax.faces.component.UIForm;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.component.html.HtmlPanelGroup;
import javax.faces.context.FacesContext;
import javax.servlet.http.HttpServletRequest;

import org.primefaces.component.button.Button;
import org.primefaces.mobile.component.content.Content;
import org.primefaces.mobile.component.header.Header;
import org.primefaces.mobile.component.page.Page;
import org.skyve.CORE;
import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.LinkItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.UserAgentType;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.menu.Menu;
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuRenderer;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.router.UxUiSelector;
import org.skyve.metadata.user.User;
import org.skyve.util.Binder;

@FacesComponent(PhoneMenu.COMPONENT_TYPE)
public class PhoneMenu extends HtmlPanelGroup {
	@SuppressWarnings("hiding")
	public static final String COMPONENT_TYPE = "org.skyve.impl.web.faces.components.PhoneMenu";

	@Override
	public void encodeBegin(FacesContext context) throws IOException {
		if (getChildCount() == 0) {
			new FacesAction<Void>() {
				@Override
				public Void callback() throws Exception {
					HttpServletRequest request = (HttpServletRequest) FacesContext.getCurrentInstance().getExternalContext().getRequest();
					UserAgentType userAgentType = UserAgent.getType(request);
					Router router = CORE.getRepository().getRouter();
					UxUi uxui = ((UxUiSelector) router.getUxuiSelector()).select(userAgentType, request);

					addMenuPage();
					addModulePages(uxui.getName());
					return null;
				}
			}.execute();
		}

		super.encodeBegin(context);
	}
	
	void addMenuPage() {
		UserImpl user = (UserImpl) CORE.getUser();
		Customer customer = user.getCustomer();

		FacesContext fc = FacesContext.getCurrentInstance();
		Application a = fc.getApplication();
		
		// <pm:page id="menu">
		Page page = (Page) a.createComponent(Page.COMPONENT_TYPE);
		page.setId("menu");
		// <pm:header title="Modules" fixed="true">
		//     <p:button styleClass="ui-btn-right ui-btn-inline" value="Logout" icon="ui-icon-gear" href="/loggedOut" />
		// </pm:header>
		Header header = (Header) a.createComponent(Header.COMPONENT_TYPE);
		header.setTitle("Modules");
		header.setFixed(true);
		Button logout = (Button) a.createComponent(Button.COMPONENT_TYPE);
		logout.setValue("Logout");
		logout.setIcon("ui-icon-gear");
		logout.setHref("/loggedOut");
		logout.setStyleClass("ui-btn-right ui-btn-inline");
		header.getChildren().add(logout);
		page.getChildren().add(header);

		// <pm:content>
		//     <h:form>
		//         <p:menu styleClass="ui-listview-inset ui-corner-all">
		//             <p:menuitem value="Admin" url="#admin?transition=slide" />
		//             <p:menuitem value="Northwind Demonstration" url="#northwind?transition=slide" />
		//         </p:menu>
		//     </h:form>
		// </pm:content>
		Content content = (Content) a.createComponent(Content.COMPONENT_TYPE);
		UIForm form = (UIForm) a.createComponent(UIForm.COMPONENT_TYPE);
		content.getChildren().add(form);

		HtmlOutputText menu = (HtmlOutputText) a.createComponent(HtmlOutputText.COMPONENT_TYPE);
		menu.setEscape(false);
		form.getChildren().add(menu);
		StringBuilder markup = new StringBuilder(512);
		markup.append("<ul data-role=\"listview\" data-inset=\"true\">");
		
		// render each module link
		for (Module module : customer.getModules()) {
			String moduleName = module.getName();
			org.skyve.metadata.module.menu.Menu moduleMenu = user.getModuleMenu(moduleName);
			if (! moduleMenu.getItems().isEmpty()) {
				markup.append("<li><a href=\"#").append(moduleName).append("?transition=slide\">");
				markup.append(module.getTitle()).append("</a></li>");
			}
		}
		markup.append("</ul>");
		menu.setValue(markup.toString());

		page.getChildren().add(content);
		PhoneMenu.this.getChildren().add(page);
	}

	void addModulePages(String uxui) {
		User user = CORE.getUser();
		new MenuRenderer(uxui, user.getLocale(), null) {
			private Stack<String> menuIds = new Stack<>();
			private Stack<HtmlOutputText> menus = new Stack<>();
			private Stack<StringBuilder> markups = new Stack<>();

			@Override
			public void renderModuleMenu(Menu menu, Module menuModule, boolean open) {
				menuIds.push(menuModule.getName());
				menus.push(renderMenuPage("#menu", menuModule.getTitle()));
				StringBuilder markup = new StringBuilder(256);
				markup.append("<ul data-role=\"listview\" data-inset=\"true\">");
				markups.push(markup);
			}
			
			@Override
			public void renderedModuleMenu(Menu menu, Module menuModule, boolean open) {
				menuIds.pop();
				StringBuilder markup = markups.pop();
				markup.append("</ul>");
				menus.pop().setValue(markup);
			}
			
			@Override
			public void renderMenuGroup(MenuGroup group, Module menuModule) {
				String groupName = group.getName();
				String parentMenuId = menuIds.peek();
				String newMenuId = Binder.toJavaInstanceIdentifier(parentMenuId + groupName);
				StringBuilder markup = markups.peek();
				markup.append("<li><a href=\"#").append(newMenuId).append("?transition=slide\">");
				markup.append(groupName).append("</a></li>");

				menuIds.push(newMenuId);
				menus.push(renderMenuPage("#" + parentMenuId, groupName));

				markup = new StringBuilder(256);
				markup.append("<ul data-role=\"listview\" data-inset=\"true\">");
				markups.push(markup);
			}
			
			@Override
			public void renderedMenuGroup(MenuGroup group, Module menuModule) {
				menuIds.pop();
				StringBuilder markup = markups.pop();
				markup.append("</ul>");
				menus.pop().setValue(markup);
			}
			
			@Override
			public void renderCalendarItem(CalendarItem item,
											Module menuModule,
											Module itemModule,
											Document itemDocument,
											String itemQueryName,
											String icon16,
											String iconStyleClass) {
				StringBuilder markup = markups.peek();
				String url = org.skyve.impl.web.faces.beans.Menu.createMenuItemUrl(menuModule, itemModule, item, itemQueryName);
				markup.append("<li><a href=\"").append(url).append("\">");
				markup.append(item.getName()).append("</a></li>");
			}
			
			@Override
			public void renderEditItem(EditItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String icon16,
										String iconStyleClass) {
				String url = org.skyve.impl.web.faces.beans.Menu.createMenuItemUrl(menuModule, itemModule, item, null);
				renderItem(url, item.getName());
			}
			
			@Override
			public void renderLinkItem(LinkItem item, Module menuModule, boolean relative, String absoluteHref) {
				renderItem(absoluteHref, item.getName());
			}
			
			@Override
			public void renderListItem(ListItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String itemQueryName,
										String icon16,
										String iconStyleClass) {
				String url = org.skyve.impl.web.faces.beans.Menu.createMenuItemUrl(menuModule, itemModule, item, itemQueryName);
				renderItem(url, item.getName());
			}
			
			@Override
			public void renderMapItem(MapItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String itemQueryName,
										String icon16,
										String iconStyleClass) {
				String url = org.skyve.impl.web.faces.beans.Menu.createMenuItemUrl(menuModule, itemModule, item, itemQueryName);
				renderItem(url, item.getName());
			}
			
			@Override
			public void renderTreeItem(TreeItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String itemQueryName,
										String icon16,
										String iconStyleClass) {
				String url = org.skyve.impl.web.faces.beans.Menu.createMenuItemUrl(menuModule, itemModule, item, itemQueryName);
				renderItem(url, item.getName());
			}
			
			private void renderItem(String url, String label) {
				StringBuilder markup = markups.peek();
				markup.append("<li><a href=\"").append(url).append("\">");
				markup.append(label).append("</a></li>");
			}
			
			private HtmlOutputText renderMenuPage(String parentMenuHref, String menuTitle) {
				FacesContext fc = FacesContext.getCurrentInstance();
				Application a = fc.getApplication();

				// <pm:page id="menu">
				Page page = (Page) a.createComponent(Page.COMPONENT_TYPE);
				page.setId(menuIds.peek());
				// <pm:header title="Modules" fixed="true">
				//     <p:button styleClass="ui-btn-left ui-btn-inline" value="Back" icon="ui-icon-back" href="#menu" />
				//     <p:button styleClass="ui-btn-right ui-btn-inline" value="Logout" icon="ui-icon-gear" href="/loggedOut" />
				// </pm:header>
				Header header = (Header) a.createComponent(Header.COMPONENT_TYPE);
				header.setTitle(menuTitle);
				header.setFixed(true);
				Button button = (Button) a.createComponent(Button.COMPONENT_TYPE);
				button.setValue("Back");
				button.setIcon("ui-icon-back");
				button.setHref(parentMenuHref + "?reverse=true&transition=slide");
				button.setStyleClass("ui-btn-left ui-btn-inline");
				header.getChildren().add(button);
				button = (Button) a.createComponent(Button.COMPONENT_TYPE);
				button.setValue("Logout");
				button.setIcon("ui-icon-gear");
				button.setHref("/loggedOut");
				button.setStyleClass("ui-btn-right ui-btn-inline");
				header.getChildren().add(button);
				page.getChildren().add(header);
				// <pm:content>
				//     <h:form>
				//         <ul data-role="listview" data-inset="true">
				//         </ul>
				//     </h:form>
				// </pm:content>
				Content content = (Content) a.createComponent(Content.COMPONENT_TYPE);
				UIForm form = (UIForm) a.createComponent(UIForm.COMPONENT_TYPE);
				content.getChildren().add(form);
				page.getChildren().add(content);

				HtmlOutputText result = (HtmlOutputText) a.createComponent(HtmlOutputText.COMPONENT_TYPE);
				result.setEscape(false);
				form.getChildren().add(result);

				PhoneMenu.this.getChildren().add(page);
				
				return result;
			}
		}.render(user);
	}
}
