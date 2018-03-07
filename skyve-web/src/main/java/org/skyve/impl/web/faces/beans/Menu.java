package org.skyve.impl.web.faces.beans;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

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
import org.skyve.impl.metadata.module.menu.AbstractDocumentMenuItem;
import org.skyve.impl.metadata.module.menu.AbstractDocumentOrQueryOrModelMenuItem;
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
import org.skyve.impl.web.UserAgentType;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.module.query.DocumentQueryDefinition;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.router.UxUiSelector;
import org.skyve.util.Util;
import org.skyve.web.WebAction;

@ManagedBean
@SessionScoped
public class Menu extends Harness {
	private static final long serialVersionUID = -7523306130675202901L;

	// The modules menu on the LHS
	private MenuModel menu;
	public MenuModel getMenu() {
		if (menu == null) {
			preRender();
		}
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
	
	private static MenuModel createMenuModel(String bizModule, String uxui) {
		MenuModel result = new DefaultMenuModel();

		UserImpl user = (UserImpl) CORE.getUser();
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
					processItem(item, moduleSub, customer, thisModule, uxui);
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
										Module module,
										String uxui) {
		if (item.isApplicable(uxui)) {
			if (item instanceof MenuGroup) {
				menu.getElements().add(createSubMenu((MenuGroup) item, customer, module, uxui));
			}
			else if (item instanceof AbstractDocumentMenuItem) {
				String documentName = ((AbstractDocumentMenuItem) item).getDocumentName();
				// determine the document name from the query if it is not provided
				if (documentName == null) {
					if (item instanceof AbstractDocumentOrQueryOrModelMenuItem) {
						AbstractDocumentOrQueryOrModelMenuItem aggregateItem = (AbstractDocumentOrQueryOrModelMenuItem) item;
						DocumentQueryDefinition query = deriveDocumentQuery(customer, 
																				module, 
																				aggregateItem, 
																				aggregateItem.getQueryName(), 
																				null);
						if (query != null) {
							documentName = query.getDocumentName();
						}
					}
				}
				// if we have a document name, use it's icon for the menu item	
				if (documentName != null) {
					String iconStyleClass = module.getDocument(customer, documentName).getIconStyleClass();
					menu.getElements().add(createMenuItem(item, iconStyleClass, customer, module));
				}
			}
			else {
				createMenuItem(item, null, customer, module);
			}
		}
	}

	private static org.primefaces.model.menu.MenuItem createMenuItem(MenuItem item,
																		String iconStyleClass,
																		Customer customer,
																		Module module) {
		String url = createMenuItemUrl(customer, module, item);
		DefaultMenuItem result = new DefaultMenuItem(item.getName(), iconStyleClass, url);
		result.setAjax(false);
		result.setHref(url);
		return result;
	}

	private static Submenu createSubMenu(MenuGroup group,
											Customer customer, 
											Module module,
											String uxui) {
		Submenu result = new DefaultSubMenu(group.getName());
		for (MenuItem subItem : group.getItems()) {
			processItem(subItem, result, customer, module, uxui);
		}

		return result;
	}
	
	public static String createMenuItemUrl(Customer customer, Module module, MenuItem item) {
		StringBuilder url = new StringBuilder(64);
		if (item instanceof ListItem) {
			ListItem gridItem = (ListItem) item;
			url.append(Util.getSkyveContextUrl());
			url.append("/?a=").append(WebAction.l.toString()).append("&m=").append(module.getName());
			url.append("&q=").append(Harness.deriveDocumentQuery(customer,
																	module,
																	item,
																	gridItem.getQueryName(),
																	gridItem.getDocumentName()).getName());
		}
		else if (item instanceof EditItem) {
			url.append(Util.getSkyveContextUrl());
			url.append("/?a=").append(WebAction.e.toString()).append("&m=").append(module.getName());
			url.append("&d=").append(((EditItem) item).getDocumentName());
		}
		else if (item instanceof CalendarItem) {
            CalendarItem calendarItem = (CalendarItem) item;
    		url.append(Util.getSkyveContextUrl());
            url.append("/?a=").append(WebAction.c.toString()).append("&m=").append(module.getName());
			url.append("&q=").append(Harness.deriveDocumentQuery(customer,
																	module,
																	item,
																	calendarItem.getQueryName(),
																	calendarItem.getDocumentName()).getName());
        }
        else if (item instanceof TreeItem) {
    		TreeItem treeItem = (TreeItem) item;
    		url.append(Util.getSkyveContextUrl());
    		url.append("/?a=").append(WebAction.t.toString()).append("&m=").append(module.getName());
			url.append("&q=").append(Harness.deriveDocumentQuery(customer,
																	module,
																	item,
																	treeItem.getQueryName(),
																	treeItem.getDocumentName()).getName());
        }
        else if (item instanceof MapItem) {
            MapItem mapItem = (MapItem) item;
    		url.append(Util.getSkyveContextUrl());
            url.append("/?a=").append(WebAction.m.toString()).append("&m=").append(module.getName());
			url.append("&q=").append(Harness.deriveDocumentQuery(customer,
																	module,
																	item,
																	mapItem.getQueryName(),
																	mapItem.getDocumentName()).getName());
			url.append("&b=").append(mapItem.getGeometryBinding());
        }
        else if (item instanceof LinkItem) {
        	String href = ((LinkItem) item).getHref();
        	try {
				if (new URI(href).isAbsolute()) {
					return href;
				}
			} catch (URISyntaxException e) {
				// do nothing here if its not know to be absolute
			}
        	
    		url.append(Util.getSkyveContextUrl());
    		if (href.charAt(0) != '/') {
    			url.append('/');
    		}
    		url.append(href);
        }

		return url.toString();
	}
}
