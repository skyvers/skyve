package org.skyve.impl.web.faces.views;

import java.util.ArrayDeque;
import java.util.Deque;

import org.primefaces.model.menu.DefaultMenuItem;
import org.primefaces.model.menu.DefaultMenuModel;
import org.primefaces.model.menu.DefaultSubMenu;
import org.primefaces.model.menu.MenuModel;
import org.primefaces.model.menu.Submenu;
import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.LinkItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.module.menu.MenuRenderer;
import org.skyve.metadata.router.UxUi;
import org.skyve.util.Util;
import org.skyve.web.WebAction;

import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Named;
import jakarta.servlet.http.HttpServletRequest;

/**
 * The menu is session scoped.
 * The expanded state is set based on the "m" parameter of first URL hit.
 * The menu is reset by home.xhtml <s:resetMenuState /> tag so that the / URL sets up the menu based on the defaults.
 * The menus respond to the cookie/localStorage in the browser before adhering to the server side state in this menu model.
 * This means the menu model expanded state is really only used once WebUtil.clearMenuCookies() has been called.
 */
@Named("menu")
@SessionScoped
public class MenuView extends HarnessView {
	private static final long serialVersionUID = -7523306130675202901L;

	// The modules menu on the LHS
	// Note: MenuModel is never mutated once established, only the reference is dropped
	private transient volatile MenuModel menu;
	
	public MenuModel getMenu() {
		// double-checked locking
		if (menu == null) {
			synchronized (this) {
				if (menu == null) {
					setState();
				}
			}
		}
		return menu;
	}
	
	public void resetState() {
		menu = null;
	}

	private void setState() {
		new FacesAction<Void>() {
			@Override
			public Void callback() throws Exception {
				FacesContext fc = FacesContext.getCurrentInstance();
				if (! fc.isPostback()) {
					HttpServletRequest request = (HttpServletRequest) fc.getExternalContext().getRequest();
					setBizModuleParameter(request.getParameter("m"));
					initialise(); // check m parameter and set to default if DNE
					
					UxUi uxui = UserAgent.getUxUi(request);
					menu = createMenuModel(getBizModuleParameter(), uxui.getName());
				}
				
				return null;
			}
		}.execute();
	}

	private transient int menuItemId; 

	private MenuModel createMenuModel(String bizModule, String uxui) {
		MenuModel result = new DefaultMenuModel();

		menuItemId = 1; // reset IDs for this run
		
		// render each module menu
		new MenuRenderer(uxui, bizModule) {
			private Deque<Submenu> subs = new ArrayDeque<>(16); // non-null elements
			
			@Override
			public void renderModuleMenu(org.skyve.metadata.module.menu.Menu moduleMenu,
											Module menuModule,
											boolean open) {
				DefaultSubMenu moduleSub = DefaultSubMenu.builder().id(String.valueOf(menuItemId++)).label(menuModule.getLocalisedTitle()).build();
				result.getElements().add(moduleSub);
				moduleSub.setExpanded(open);
				subs.push(moduleSub);
			}

			@Override
			public void renderMenuGroup(MenuGroup group, Module menuModule) {
				DefaultSubMenu sub = DefaultSubMenu.builder().id(String.valueOf(menuItemId++)).label(group.getLocalisedName()).build();
				subs.peek().getElements().add(sub);
				subs.push(sub);
			}

			@Override
			public void renderCalendarItem(CalendarItem item,
											Module menuModule,
											Module itemModule,
											Document itemDocument,
											String itemQueryName, 
											String icon16,
											String iconStyleClass) {
				subs.peek().getElements().add(createMenuItem(item, iconStyleClass, menuModule, itemModule, itemQueryName, null));
			}
			
			@Override
			public void renderEditItem(EditItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String icon16,
										String iconStyleClass) {
				subs.peek().getElements().add(createMenuItem(item, iconStyleClass, menuModule, itemModule, null, null));
			}
			
			@Override
			public void renderLinkItem(LinkItem item,
										Module menuModule,
										boolean relative,
										String absoluteHref) {
				subs.peek().getElements().add(createMenuItem(item, null, menuModule, null, null, absoluteHref));
			}
			
			@Override
			public void renderListItem(ListItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String itemQueryName,
										String icon16,
										String iconStyleClass) {
				subs.peek().getElements().add(createMenuItem(item, iconStyleClass, menuModule, itemModule, itemQueryName, null));
			}

			@Override
			public void renderMapItem(MapItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String itemQueryName,
										String icon16,
										String iconStyleClass) {
				subs.peek().getElements().add(createMenuItem(item, iconStyleClass, menuModule, itemModule, itemQueryName, null));
			}
			
			@Override
			public void renderTreeItem(TreeItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String itemQueryName,
										String icon16,
										String iconStyleClass) {
				subs.peek().getElements().add(createMenuItem(item, iconStyleClass, menuModule, itemModule, itemQueryName, null));
			}

			@Override
			public void renderedMenuGroup(MenuGroup group, Module menuModule) {
				subs.pop();
			}
			
			@Override
			public void renderedModuleMenu(org.skyve.metadata.module.menu.Menu moduleMenu, Module menuModule, boolean open) {
				subs.pop();
			}
		}.render(getUser());
		
		return result;
	}

	private org.primefaces.model.menu.MenuItem createMenuItem(MenuItem item,
																String iconStyleClass,
																Module menuModule,
																Module itemModule,
																String itemQueryName,
																String itemAbsoluteHref) {
		DefaultMenuItem result = DefaultMenuItem.builder().id(String.valueOf(menuItemId++)).value(item.getLocalisedName()).icon(iconStyleClass).build();
		result.setHref(createMenuHref(menuModule, itemModule, item, itemQueryName, itemAbsoluteHref));
		return result;
	}

	public static String createMenuHref(Module menuModule,
											Module itemModule,
											MenuItem item,
											String itemQueryName,
											String itemAbsoluteHref) {
		StringBuilder result = new StringBuilder(128);
		result.append("javascript:SKYVE.PF.startHistory('");
		
		if (itemAbsoluteHref != null) {
			result.append(itemAbsoluteHref.replace("'", "\\'"));
		}
		else if (item instanceof ListItem listItem) {
			result.append(Util.getSkyveContextUrl());
			result.append("/?a=").append(WebAction.l.toString()).append("&m=").append(menuModule.getName());
			String modelName = listItem.getModelName();
			if (modelName != null) {
				result.append("&d=").append(listItem.getDocumentName());
				result.append("&q=").append(listItem.getModelName());
			}
			else {
				result.append("&q=").append(itemQueryName);
			}
		}
		else if (item instanceof EditItem editItem) {
			result.append(Util.getSkyveContextUrl());
			result.append("/?a=").append(WebAction.e.toString()).append("&m=").append(itemModule.getName());
			result.append("&d=").append(editItem.getDocumentName());
		}
		else if (item instanceof CalendarItem calendarItem) {
    		result.append(Util.getSkyveContextUrl());
            result.append("/?a=").append(WebAction.c.toString()).append("&m=").append(menuModule.getName());
			String modelName = calendarItem.getModelName();
			if (modelName != null) {
				result.append("&d=").append(calendarItem.getDocumentName());
				result.append("&q=").append(calendarItem.getModelName());
			}
			else {
				result.append("&q=").append(itemQueryName);
			}
        }
        else if (item instanceof TreeItem treeItem) {
    		result.append(Util.getSkyveContextUrl());
    		result.append("/?a=").append(WebAction.t.toString()).append("&m=").append(menuModule.getName());
			String modelName = treeItem.getModelName();
			if (modelName != null) {
				result.append("&d=").append(treeItem.getDocumentName());
				result.append("&q=").append(treeItem.getModelName());
			}
			else {
				result.append("&q=").append(itemQueryName);
			}
        }
        else if (item instanceof MapItem mapItem) {
    		result.append(Util.getSkyveContextUrl());
            result.append("/?a=").append(WebAction.m.toString()).append("&m=").append(menuModule.getName());
            String modelName = mapItem.getModelName();
			if (modelName != null) {
				result.append("&d=").append(mapItem.getDocumentName());
				result.append("&q=").append(mapItem.getModelName());
			}
			else {
				result.append("&q=").append(itemQueryName);
			}
			result.append("&b=").append(mapItem.getGeometryBinding());
        }
		
		result.append("')");
		return result.toString();
	}
}
