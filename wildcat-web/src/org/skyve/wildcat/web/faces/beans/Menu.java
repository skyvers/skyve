package org.skyve.wildcat.web.faces.beans;

import java.util.List;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;
import javax.faces.context.FacesContext;
import javax.servlet.http.HttpServletRequest;

import org.primefaces.model.menu.DefaultMenuItem;
import org.primefaces.model.menu.DefaultMenuModel;
import org.primefaces.model.menu.DefaultSubMenu;
import org.primefaces.model.menu.MenuModel;
import org.primefaces.model.menu.Submenu;
import org.skyve.CORE;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.router.UxUiSelector;
import org.skyve.util.Util;
import org.skyve.web.WebAction;
import org.skyve.wildcat.metadata.module.menu.CalendarItem;
import org.skyve.wildcat.metadata.module.menu.EditItem;
import org.skyve.wildcat.metadata.module.menu.GridItem;
import org.skyve.wildcat.metadata.module.menu.MapItem;
import org.skyve.wildcat.metadata.module.menu.TreeItem;
import org.skyve.wildcat.metadata.repository.router.Router;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.web.faces.FacesAction;

@ManagedBean
@ViewScoped
public class Menu extends Harness {
	private static final long serialVersionUID = -7523306130675202901L;

	// The modules menu on the LHS
	private MenuModel menu;
	public MenuModel getMenu() {
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
					org.skyve.wildcat.metadata.user.User internalUser = (org.skyve.wildcat.metadata.user.User) persistence.getUser();
					Customer customer = internalUser.getCustomer();

					initialise(customer, internalUser);
					
					Router router = CORE.getRepository().getRouter();
					String uxui = ((UxUiSelector) router.getUxuiSelector()).select((HttpServletRequest) fc.getExternalContext().getRequest());

					menu = createMenuModel(getBizModuleParameter(), uxui);
				}
				
				return null;
			}
		}.execute();
	}
	
	private static MenuModel createMenuModel(String bizModule, String uxui) throws MetaDataException {
		MenuModel result = new DefaultMenuModel();

		org.skyve.wildcat.metadata.user.User user = (org.skyve.wildcat.metadata.user.User) CORE.getUser();
		Customer customer = user.getCustomer();

		// determine if the first menu should be open - ie no default
		org.skyve.metadata.module.menu.Menu moduleMenu = user.getModuleMenu(bizModule);
		boolean setFirstModuleOpen = (moduleMenu == null) || moduleMenu.getItems().isEmpty();

		// render each module menu
		List<Module> modules = customer.getModules();
		for (int i = 0, l = modules.size(); i < l; i++) {
			Module thisModule = modules.get(i);
			String thisModuleName = thisModule.getName();

			moduleMenu = user.getModuleMenu(thisModuleName);
			if (moduleMenu.isApplicable(uxui)) {
				Submenu moduleSub = new DefaultSubMenu(thisModule.getTitle());
				result.addElement(moduleSub);

				for (MenuItem item : moduleMenu.getItems()) {
					if (item.isApplicable(uxui)) {
						processItem(item, moduleSub, customer, thisModule);
					}
				}
/*				
				if (setFirstModuleOpen) {
					result.append(",open:");
					result.append(i == 0);
				} 
				else {
					result.append(",open:");
					result.append(thisModuleName.equals(moduleName));
				}
*/
			}
		}
		
		return result;
	}

	private static void processItem(MenuItem item,
										Submenu menu,
										Customer customer,
										Module module)
	throws MetaDataException {
		if (item instanceof MenuGroup) {
			menu.getElements().add(createSubMenu((MenuGroup) item, customer, module));
		}
		else {
			menu.getElements().add(createMenuItem(item, customer, module));
		}
	}

	private static org.primefaces.model.menu.MenuItem createMenuItem(MenuItem item,
																		Customer customer,
																		Module module)
	throws MetaDataException {
		String url = createMenuItemUrl(customer, module, item);
		DefaultMenuItem result = new DefaultMenuItem(item.getName(), null, url);
		result.setAjax(false);
		result.setHref(url);
		result.setTarget("content");
		return result;
	}

	private static Submenu createSubMenu(MenuGroup group,
											Customer customer, 
											Module module)
	throws MetaDataException {
		Submenu result = new DefaultSubMenu(group.getName());
		for (MenuItem subItem : group.getItems()) {
			processItem(subItem, result, customer, module);
		}

		return result;
	}
	
	public static String createMenuItemUrl(Customer customer, Module module, MenuItem item) throws MetaDataException {
		StringBuilder url = new StringBuilder(64);
		url.append(Util.getWildcatContextUrl());
		if (item instanceof GridItem) {
			GridItem gridItem = (GridItem) item;
			url.append("/?a=").append(WebAction.g.toString()).append("&m=").append(module.getName());
			url.append("&q=").append(Harness.deriveQueryName(customer,
																module,
																item,
																gridItem.getQueryName(),
																gridItem.getDocumentName()));
		}
		else if (item instanceof EditItem) {
			url.append("/?a=").append(WebAction.e.toString()).append("&m=").append(module.getName());
			url.append("&d=").append(((EditItem) item).getDocumentName());
		}
		else if (item instanceof CalendarItem) {
            CalendarItem calendarItem = (CalendarItem) item;
            url.append("/?a=").append(WebAction.c.toString()).append("&m=").append(module.getName());
			url.append("&q=").append(Harness.deriveQueryName(customer,
																module,
																item,
																calendarItem.getQueryName(),
																calendarItem.getDocumentName()));
        }
        else if (item instanceof TreeItem) {
    		TreeItem treeItem = (TreeItem) item;
    		url.append("/?a=").append(WebAction.t.toString()).append("&m=").append(module.getName());
			url.append("&q=").append(Harness.deriveQueryName(customer,
																module,
																item,
																treeItem.getQueryName(),
																treeItem.getDocumentName()));
        }
        else if (item instanceof MapItem) {
            MapItem mapItem = (MapItem) item;
            url.append("/?a=").append(WebAction.m.toString()).append("&m=").append(module.getName());
			url.append("&q=").append(Harness.deriveQueryName(customer,
																module,
																item,
																mapItem.getQueryName(),
																mapItem.getDocumentName()));
			url.append("&b=").append(mapItem.getGeometryBinding());
        }

		return url.toString();
	}
}
