package org.skyve.impl.web.faces.beans;

import java.util.Map;
import java.util.Map.Entry;
import java.util.Stack;
import java.util.TreeMap;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import javax.faces.context.FacesContext;
import javax.servlet.http.HttpServletRequest;

import org.primefaces.model.menu.DefaultMenuItem;
import org.primefaces.model.menu.DefaultMenuModel;
import org.primefaces.model.menu.DefaultSubMenu;
import org.primefaces.model.menu.MenuModel;
import org.primefaces.model.menu.Submenu;
import org.skyve.CORE;
import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.LinkItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.module.menu.MenuRenderer;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.router.UxUiSelector;
import org.skyve.util.Util;
import org.skyve.web.UserAgentType;
import org.skyve.web.WebAction;

@ManagedBean
@SessionScoped
public class Menu extends Harness {
	private static final long serialVersionUID = -7523306130675202901L;

	// The modules menu on the LHS
	private MenuModel menu;
	// The map of module name to sub menu nodes in the model
	private Map<String, DefaultSubMenu> moduleSubMenus = new TreeMap<>();
	
	public MenuModel getMenu() {
		if (menu == null) {
			preRender();
		}
		setExpandedModule();
		return menu;
	}

	public void preRender() {
		new FacesAction<Void>() {
			@Override
			@SuppressWarnings("synthetic-access")
			public Void callback() throws Exception {
				FacesContext fc = FacesContext.getCurrentInstance();
				if (! fc.isPostback()) {
					AbstractPersistence persistence = AbstractPersistence.get();
					UserImpl internalUser = (UserImpl) persistence.getUser();
					Customer customer = internalUser.getCustomer();

					initialise(customer, internalUser, fc.getExternalContext().getRequestLocale());
					
					HttpServletRequest request = (HttpServletRequest) fc.getExternalContext().getRequest();
					UserAgentType userAgentType = UserAgent.getType(request);
					Router router = CORE.getRepository().getRouter();
					UxUi uxui = ((UxUiSelector) router.getUxuiSelector()).select(userAgentType, request);

					menu = createMenuModel(getBizModuleParameter(), uxui.getName());
				}
				
				return null;
			}
		}.execute();
	}
	
	private void setExpandedModule() {
		if (menu != null) {
			Map<String, String> params = FacesContext.getCurrentInstance().getExternalContext().getRequestParameterMap();
			String moduleName = params.get("m");
			if (moduleName != null) {
				for (Entry<String, DefaultSubMenu> e : moduleSubMenus.entrySet()) {
					e.getValue().setExpanded(moduleName.equals(e.getKey()));
				}
			}
		}
	}
	
	private MenuModel createMenuModel(String bizModule, String uxui) {
		MenuModel result = new DefaultMenuModel();

		// render each module menu
		new MenuRenderer(uxui, getLocale(), bizModule) {
			private Stack<Submenu> subs = new Stack<>();
			
			@Override
			@SuppressWarnings("synthetic-access")
			public void renderModuleMenu(org.skyve.metadata.module.menu.Menu moduleMenu,
											Module menuModule,
											boolean open) {
				DefaultSubMenu moduleSub = new DefaultSubMenu(menuModule.getTitle());
				moduleSubMenus.put(menuModule.getName(), moduleSub);
				result.addElement(moduleSub);
				moduleSub.setExpanded(open);
				subs.push(moduleSub);
			}

			@Override
			public void renderMenuGroup(MenuGroup group, Module menuModule) {
				DefaultSubMenu sub = new DefaultSubMenu(group.getName());
				sub.setExpanded(true);
				subs.peek().getElements().add(sub);
				subs.push(sub);
			}

			@Override
			@SuppressWarnings("synthetic-access")
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
			@SuppressWarnings("synthetic-access")
			public void renderEditItem(EditItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String icon16,
										String iconStyleClass) {
				subs.peek().getElements().add(createMenuItem(item, iconStyleClass, menuModule, itemModule, null, null));
			}
			
			@Override
			@SuppressWarnings("synthetic-access")
			public void renderLinkItem(LinkItem item,
										Module menuModule,
										boolean relative,
										String absoluteHref) {
				subs.peek().getElements().add(createMenuItem(item, null, menuModule, null, null, absoluteHref));
			}
			
			@Override
			@SuppressWarnings("synthetic-access")
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
			@SuppressWarnings("synthetic-access")
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
			@SuppressWarnings("synthetic-access")
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

	private static org.primefaces.model.menu.MenuItem createMenuItem(MenuItem item,
																		String iconStyleClass,
																		Module menuModule,
																		Module itemModule,
																		String itemQueryName,
																		String itemAbsoluteHref) {
		DefaultMenuItem result = new DefaultMenuItem(item.getName(), iconStyleClass);
		result.setAjax(false);
		result.setHref("#");
		result.setOnclick(createMenuItemOnClick(menuModule, itemModule, item, itemQueryName, itemAbsoluteHref));
		return result;
	}

	public static String createMenuItemOnClick(Module menuModule,
												Module itemModule,
												MenuItem item,
												String itemQueryName,
												String itemAbsoluteHref) {
		StringBuilder result = new StringBuilder(128);
		result.append("SKYVE.PF.startHistory('");
		
		if (itemAbsoluteHref != null) {
			result.append(itemAbsoluteHref.replace("'", "\\'"));
		}
		else if (item instanceof ListItem) {
			ListItem listItem = (ListItem) item;
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
		else if (item instanceof EditItem) {
			result.append(Util.getSkyveContextUrl());
			result.append("/?a=").append(WebAction.e.toString()).append("&m=").append(itemModule.getName());
			result.append("&d=").append(((EditItem) item).getDocumentName());
		}
		else if (item instanceof CalendarItem) {
			CalendarItem calendarItem = (CalendarItem) item;
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
        else if (item instanceof TreeItem) {
        	TreeItem treeItem = (TreeItem) item;
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
        else if (item instanceof MapItem) {
            MapItem mapItem = (MapItem) item;
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
		
		result.append("');return false");
		return result.toString();
	}
}
