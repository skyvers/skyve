package org.skyve.impl.web.faces.components;

import java.io.IOException;
import java.util.List;

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
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.beans.Menu;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.router.UxUiSelector;
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
					Router router = CORE.getRepository().getRouter();
					HttpServletRequest request = (HttpServletRequest) FacesContext.getCurrentInstance().getExternalContext().getRequest();
					UxUi uxui = ((UxUiSelector) router.getUxuiSelector()).select(request);

					addMenuPage();
					addModulePages(uxui.getName());
					return null;
				}
			}.execute();
		}

		super.encodeBegin(context);
	}
	
	void addMenuPage() throws MetaDataException {
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

	void addModulePages(String uxui) throws MetaDataException {
		UserImpl user = (UserImpl) CORE.getUser();
		Customer customer = user.getCustomer();

		for (Module module : customer.getModules()) {
			String moduleName = module.getName();
			org.skyve.metadata.module.menu.Menu moduleMenu = user.getModuleMenu(moduleName);
			if (moduleMenu.isApplicable(uxui)) {
				addMenuPage("#menu", moduleName, module.getTitle(), moduleMenu.getItems(), uxui, customer, module);
			}
		}
	}

	private void addMenuPage(String parentMenuHref,
								String menuId, 
								String menuTitle, 
								List<MenuItem> items,
								String uxui,
								Customer customer,
								Module module) throws MetaDataException {
		FacesContext fc = FacesContext.getCurrentInstance();
		Application a = fc.getApplication();

		// <pm:page id="menu">
		Page page = (Page) a.createComponent(Page.COMPONENT_TYPE);
		page.setId(menuId);
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
		for (MenuItem item : items) {
			if (item.isApplicable(uxui)) {
				if (item instanceof MenuGroup) {
					String newMenuId = Binder.toJavaInstanceIdentifier(menuId + item.getName());
					addMenuPage('#' + menuId, newMenuId, item.getName(), ((MenuGroup) item).getItems(), uxui, customer, module);
	
					markup.append("<li><a href=\"#").append(newMenuId).append("?transition=slide\">");
					markup.append(item.getName()).append("</a></li>");
				}
				else {
					String url = Menu.createMenuItemUrl(customer, module, item);
					markup.append("<li><a href=\"").append(url).append("\">");
					markup.append(item.getName()).append("</a></li>");
				}
			}
		}
		markup.append("</ul>");
		menu.setValue(markup.toString());

		page.getChildren().add(content);
		PhoneMenu.this.getChildren().add(page);
	}
}
