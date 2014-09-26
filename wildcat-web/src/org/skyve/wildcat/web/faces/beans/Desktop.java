package org.skyve.wildcat.web.faces.beans;

import java.io.IOException;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.RequestScoped;
import javax.faces.context.FacesContext;
import javax.servlet.http.HttpServletRequest;

import org.skyve.CORE;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.menu.Menu;
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.module.query.Query;
import org.skyve.metadata.router.UxUiSelector;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.wildcat.generate.SmartClientGenerateUtils;
import org.skyve.wildcat.metadata.module.menu.CalendarItem;
import org.skyve.wildcat.metadata.module.menu.EditItem;
import org.skyve.wildcat.metadata.module.menu.GridItem;
import org.skyve.wildcat.metadata.module.menu.MapItem;
import org.skyve.wildcat.metadata.module.menu.TreeItem;
import org.skyve.wildcat.metadata.repository.router.Router;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.web.faces.FacesAction;

@ManagedBean
@RequestScoped
public class Desktop extends Harness {
	private static final long serialVersionUID = 913239189728613263L;

	private String script;
	public String getScript() {
		return script;
	}
	
	@SuppressWarnings("static-method")
	public String getSmartClientDir() {
		return UtilImpl.SMART_CLIENT_DIR;
	}

	public void preRender() {
        final FacesContext fc = FacesContext.getCurrentInstance();
        if (! fc.isPostback()) {
			script = new FacesAction<String>() {
				@Override
				@SuppressWarnings("synthetic-access")
				public String callback() throws Exception {
					AbstractPersistence persistence = AbstractPersistence.get();
			    	org.skyve.wildcat.metadata.user.User user = (org.skyve.wildcat.metadata.user.User) persistence.getUser();
			    	Customer customer = user.getCustomer();
			    	
			    	initialise(customer, user);
	
			    	String bizModule = getBizModuleParameter();
			    	String bizDocument = getBizDocumentParameter();
			    	String bizId = getBizIdParameter();
			    	
					StringBuilder result = new StringBuilder(8192);

					Router router = CORE.getRepository().getRouter();
					String uxui = ((UxUiSelector) router.getUxuiSelector()).select((HttpServletRequest) fc.getExternalContext().getRequest());

					constructMenu(customer, user, bizModule, uxui, result);
					listDataSources(customer, user, result);
	
					result.append("BizUtil.customer='").append(customer.getName()).append("';");
					if (ViewType.list.equals(getViewType())) { // we have a home ref that is a list view
						result.append("details.showMember(ListView.contents);");
						// TODO should cater for map, tree, calendar etc
						Module homeModule = customer.getModule(bizModule);
						Query query = homeModule.getDocumentDefaultQuery(customer, bizDocument);
						result.append("ListView.setGridDataSource('").append(bizModule).append('_').append(query.getName()).append("');");
					} 
					else {
						result.append("BizUtil.getEditView('").append(bizModule).append("','");
						result.append(bizDocument).append("',function(view){");
						result.append("details.addMember(view);BizUtil._currentView=view;");
						if (bizId == null) {
						    result.append("view.newInstance();});");
						}
						else {
						    result.append("view.editInstance('").append(bizId).append("');});");
						}
					}
					
					return result.toString();
				}
			}.execute();
        }
	}

	@SuppressWarnings("static-method")
	public String getHeaderTemplate() {
		StringBuilder result = new StringBuilder(128);
		
		result.append("<div id=\"formHeader\">");
		result.append("<div>");
    	result.append("<table style=\"");
		result.append("width:100%;background:url(images/bg-body.gif) repeat-x 0 0;");
    	result.append("\"><tr height=\"46px\"><td><strong>{title}</strong>{link}</td>");
    	result.append("<td width=\"10%\" align=\"right\">");
    	result.append("<img src=\"images/bizhub_menu_logo.gif\" alt=\"Get Organized\"/></td>");
    	result.append("<td width=\"1%\" align=\"right\"><a href=\"javascript:BizUtil.showHelp();\" class=\"dhtmlPageButton\" title=\"Dashboard\"><img src=\"images/help.png\"/></a></td>");
    	result.append("<td width=\"1%\" align=\"right\"><a href=\"javascript:BizUtil.showPortal();\" class=\"dhtmlPageButton\" title=\"Dashboard\"><img src=\"images/home.png\"/></a></td>");
    	result.append("<td width=\"1%\" align=\"right\"><a href=\"javascript:BizUtil.popupSearch();\" class=\"dhtmlPageButton\" title=\"Text Search\"><img src=\"images/textSearch.gif\"/></a></td>");
		result.append("<td width=\"1%\" align=\"right\"><a href=\"loggedOut\" class=\"dhtmlPageButton\" title=\"Logout\"><img src=\"images/logout.gif\"/></a></td>");
    	result.append("</tr></table>");
    	result.append("</div>");
    	
    	return result.toString();
	}

	private void constructMenu(Customer customer, 
								org.skyve.wildcat.metadata.user.User user, 
								String moduleName,
								String uxui,
								StringBuilder result)
	throws IOException, MetaDataException {
		result.append("BizUtil.init('").append(getHeaderTemplate());
		result.append("','../").append(getLogoRelativeFileNameUrl());
		result.append("',[");

		// process each menu

		// determine if the first menu should be open - ie no default
		Menu menu = user.getModuleMenu(moduleName);
		boolean setFirstModuleOpen = (menu == null) || menu.getItems().isEmpty();

		// render each module menu
		List<Module> modules = customer.getModules();
		for (int i = 0, l = modules.size(); i < l; i++) {
			Module thisModule = modules.get(i);
			String thisModuleName = thisModule.getName();

			menu = user.getModuleMenu(thisModuleName);
			if (menu.isApplicable(uxui)) {
				result.append("{name:'");
				result.append(thisModuleName);
				result.append("',");
				result.append("title:'");
				result.append(SmartClientGenerateUtils.processString(thisModule.getTitle()));
				result.append("',");

				renderMenuStructure(customer, thisModule, menu.getItems(), uxui, result);

				if (setFirstModuleOpen) {
					result.append(",open:");
					result.append(i == 0);
				} 
				else {
					result.append(",open:");
					result.append(thisModuleName.equals(moduleName));
				}
				// close the definition
				result.append("},");
			}
		}

		// finish up menu defs
		result.setLength(result.length() - 1); // remove last comma from menu
		// defs
		result.append(']');
	}

	private static void listDataSources(Customer customer, org.skyve.wildcat.metadata.user.User user, StringBuilder result) 
	throws MetaDataException {
		StringBuilder dataSources = new StringBuilder(1024);

		result.append(",[");
		for (Module module : customer.getModules()) {
			Set<String> visitedQueryNames = new TreeSet<>();
			
			if (ViewType.list.equals(module.getHomeRef())) {
				String homeDocumentName = module.getHomeDocumentName();
				if (homeDocumentName != null) {
					Query query = module.getDocumentDefaultQuery(customer, homeDocumentName);
					SmartClientGenerateUtils.appendDataSourceDefinition(user, customer, query, null, null, true, dataSources, visitedQueryNames);
				}
			}
			
			String moduleName = module.getName();
			Menu menu = user.getModuleMenu(moduleName);
			listDataSourcesForMenuItems(user, customer, moduleName, module, menu.getItems(), dataSources, visitedQueryNames);
		}
		if (dataSources.length() > 0) { // we have appended some data sources
			dataSources.setLength(dataSources.length() - 2); // remove the last data source comma
			result.append(dataSources);
		}
		result.append("]);");
	}

	private static void listDataSourcesForMenuItems(org.skyve.wildcat.metadata.user.User user,
														Customer customer, 
														String moduleName, 
														Module module, 
														List<MenuItem> items,
														StringBuilder dataSources,
														Set<String> visitedQueryNames)
	throws MetaDataException {
		for (MenuItem item : items) {
			if (item instanceof MenuGroup) {
				listDataSourcesForMenuItems(user, customer, moduleName, module, ((MenuGroup) item).getItems(), dataSources, visitedQueryNames);
			} 
			else if (item instanceof GridItem) {
				GridItem grid = (GridItem) item;
				
				Query query = null;
				String target = grid.getQueryName();
				if (target != null) {
					query = module.getQuery(target);
				}
				else {
					target = grid.getDocumentName();
					query = module.getDocumentDefaultQuery(customer, target);
				}

				SmartClientGenerateUtils.appendDataSourceDefinition(user, customer, query, null, null, true, dataSources, visitedQueryNames);
			}
		}
	}

	private static void renderMenuStructure(Customer customer,
												Module module, 
												List<MenuItem> items, 
												String uxui,
												StringBuilder result) 
	throws MetaDataException, IOException {
		result.append("root:{name:'");
		result.append(module.getName());
		result.append("',sub:[");
		renderMenuItems(customer, module, items, uxui, result);
		result.append("]}");
	}

	private static void renderMenuItems(Customer customer,
											Module module, 
											List<MenuItem> items, 
											String uxui,
											StringBuilder result)
	throws MetaDataException, IOException {
		for (int i = 0, l = items.size(); i < l; i++) {
			MenuItem item = items.get(i);
			if (item.isApplicable(uxui)) {
				if (item instanceof MenuGroup) {
					MenuGroup group = (MenuGroup) item;
					result.append("{desc:'");
					result.append(group.getName());
					result.append("', sub:[");
					renderMenuItems(customer, module, group.getItems(), uxui, result);
					// print a comma if not the last module being processed
					result.append((i < (l - 1)) ? "]}," : "]}");
				}
				else {
					result.append("{name:'");
					String ref = null;
					if (item instanceof GridItem) {
						GridItem gridItem = (GridItem) item;
						result.append(deriveQueryName(customer,
						                                module,
						                                item,
						                                gridItem.getQueryName(),
						                                gridItem.getDocumentName()));
						ref = "grid";
					}
					else if (item instanceof CalendarItem) {
	                    CalendarItem calendarItem = (CalendarItem) item;
	                    result.append(deriveQueryName(customer,
	                                                    module,
	                                                    item,
	                                                    calendarItem.getQueryName(),
	                                                    calendarItem.getDocumentName()));
	                    ref = "cal";
	                }
	                else if (item instanceof TreeItem) {
	                    TreeItem treeItem = (TreeItem) item;
	                    result.append(deriveQueryName(customer,
	                                                    module,
	                                                    item,
	                                                    treeItem.getQueryName(),
	                                                    treeItem.getDocumentName()));
	                    ref = "tree";
	                }
	                else if (item instanceof MapItem) {
	                    MapItem mapItem = (MapItem) item;
	                    result.append(deriveQueryName(customer,
	                                                    module,
	                                                    item,
	                                                    mapItem.getQueryName(),
	                                                    mapItem.getDocumentName()));
	                    result.append('_').append(mapItem.getGeometryBinding());
	                    ref = "map";
	                }
					else if (item instanceof EditItem) {
						result.append(((EditItem) item).getDocumentName());
						ref = "edit";
					}
					result.append("',desc:'");
					result.append(SmartClientGenerateUtils.processString(item.getName()));
					result.append("',ref:'");
					result.append(ref);
					// ,icon:'../images/pooHead.png' - use a document icon here or a  menu/action icon?
					// print a comma if not the last module being processed
					result.append((i < (l - 1)) ? "'},\n" : "'}\n");
				}
			}
		}
	}
}
