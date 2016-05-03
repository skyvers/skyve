package org.skyve.impl.web.faces.beans;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.TreeSet;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.RequestScoped;
import javax.faces.context.FacesContext;
import javax.servlet.http.HttpServletRequest;

import org.skyve.CORE;
import org.skyve.impl.generate.SmartClientGenerateUtils;
import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.LinkItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.menu.Menu;
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.module.query.DocumentQueryDefinition;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.router.UxUiSelector;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Util;

@ManagedBean
@RequestScoped
public class Desktop extends Harness {
	private static final long serialVersionUID = 913239189728613263L;

	private String script;
	public String getScript() {
		return script;
	}
	
	private String localeScript;
	public String getLocaleScript() {
		return localeScript;
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
			    	UserImpl user = (UserImpl) persistence.getUser();
			    	Customer customer = user.getCustomer();
			    	
			    	initialise(customer, user, fc.getExternalContext().getRequestLocale());

					createLocaleScriptIfRequired();

			    	String bizModule = getBizModuleParameter();
			    	String bizDocument = getBizDocumentParameter();
			    	String bizId = getBizIdParameter();
			    	
					StringBuilder result = new StringBuilder(8192);

					Router router = CORE.getRepository().getRouter();
					String uxui = ((UxUiSelector) router.getUxuiSelector()).select((HttpServletRequest) fc.getExternalContext().getRequest());

					constructMenu(customer, user, bizModule, uxui, result);
					listDataSources(customer, user, result);
	
					result.append("BizUtil.customer='").append(customer.getName()).append("';");
					result.append("BizUtil.version='").append(UtilImpl.JAVASCRIPT_FILE_VERSION).append("';");
					if (ViewType.list.equals(getViewType())) { // we have a home ref that is a list view
						result.append("details.showMember(ListView.contents);");
						// TODO should cater for map, tree, calendar etc
						Module homeModule = customer.getModule(bizModule);
						QueryDefinition query = homeModule.getDocumentDefaultQuery(customer, bizDocument);
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

	public String getHeaderTemplate() {
		StringBuilder result = new StringBuilder(128);
		
		result.append("<div id=\"formHeader\">");
		result.append("<div>");
    	result.append("<table style=\"");
		result.append("width:100%;background:url(images/skyve_bar.png) repeat-x 0 0;");
    	result.append("\"><tr height=\"46px\"><td width=\"1%\"><img style=\"width:32px;height:32px\" src=\"resources?_doc={modoc}&_n={icon}&v=").append(getJavascriptFileVersion());
    	result.append("\"/></td><td><div class=\"titleBar\">{title}</div></td>");
    	result.append("<td width=\"10%\" align=\"right\">");
    	result.append("<img src=\"images/skyve_inv.png\" alt=\"Skyve\"/></td>");
    	result.append("<td width=\"1%\" align=\"right\"><div class=\"skyveDocumentLink\">{link}</div></td>");
    	result.append("<td width=\"1%\" align=\"right\"><a href=\"javascript:BizUtil.popupSearch();\" class=\"dhtmlPageButton\" title=\"Search\"><img src=\"images/menu_search.png\"/></a></td>");
    	result.append("<td width=\"1%\" align=\"right\"><a href=\"javascript:BizUtil.showHelp();\" class=\"dhtmlPageButton\" title=\"Help\"><img src=\"images/menu_help.png\"/></a></td>");
    	result.append("<td width=\"1%\" align=\"right\"><a href=\"javascript:BizUtil.showPortal();\" class=\"dhtmlPageButton\" title=\"Dashboard\"><img src=\"images/menu_home.png\"/></a></td>");
		result.append("<td width=\"1%\" align=\"right\"><a href=\"loggedOut\" class=\"dhtmlPageButton\" title=\"Sign-out\"><img src=\"images/menu_logout.png\"/></a></td>");
    	result.append("</tr></table>");
    	result.append("</div>");
    	
    	return result.toString();
	}

	private void createLocaleScriptIfRequired() {
		Locale locale = getLocale();
		String language = locale.getLanguage();
		String country = locale.getCountry();

		Locale hu_HU = new Locale("hu", "HU");
		Locale nb_NO = new Locale("nb", "NO");
		Locale pl_PL = new Locale("pl", "PL");
		Locale pt_BR = new Locale("pt", "BR");
		Locale ro_RO = new Locale("ro", "RO");
		Locale ru_RU = new Locale("ru", "RU");
		Locale sr_Latn = new Locale("sr", "SR", "Latn");
		Locale sv_SE = new Locale("sv", "SE");
		Locale uk_UA = new Locale("uk", "UA");
		Locale zh_CN = new Locale("zh", "CN");
		Locale zh_TW = new Locale("zh", "TW");
		
		if (new Locale("ar").getLanguage().equals(language)) {
			localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_ar.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
		}
		else if (new Locale("ba").getLanguage().equals(language)) {
			localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_ba.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
		}
		else if (new Locale("cr").getLanguage().equals(language)) {
			localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_cr.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
		}
		else if (new Locale("cs").getLanguage().equals(language)) {
			localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_cs.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
		}
		else if (new Locale("de").getLanguage().equals(language)) {
			localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_de.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
		}
		else if (new Locale("el").getLanguage().equals(language)) {
			localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_el.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
		}
		else if (new Locale("es").getLanguage().equals(language)) {
			localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_es.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
		}
		else if (new Locale("fi").getLanguage().equals(language)) {
			localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_fi.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
		}
		else if (Locale.FRANCE.getLanguage().equals(language)) {
			if (Locale.FRANCE.getCountry().equals(country)) {
				localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_fr_FR.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
			}
			else {
				localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_fr.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
			}
		}
		else if (new Locale("hr").getLanguage().equals(language)) {
			localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_hr.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
		}
		else if (hu_HU.getLanguage().equals(language)) {
			if (hu_HU.getCountry().equals(country)) {
				localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_hu_HU.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
			}
		}
		else if (new Locale("id").getLanguage().equals(language)) {
			localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_id.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
		}
		else if (new Locale("it").getLanguage().equals(language)) {
			localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_it.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
		}
		else if (new Locale("ja").getLanguage().equals(language)) {
			localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_ja.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
		}
		else if (new Locale("ko").getLanguage().equals(language)) {
			localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_ko.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
		}
		else if (nb_NO.getLanguage().equals(language)) {
			if (nb_NO.getCountry().equals(country)) {
				localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_.nb_NOproperties\"></script>", UtilImpl.SMART_CLIENT_DIR);
			}
		}
		else if (new Locale("nl").getLanguage().equals(language)) {
			localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_nl.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
		}
		else if (pl_PL.getLanguage().equals(language)) {
			if (pl_PL.getCountry().equals(country)) {
				localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_pl_PL.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
			}
			else {
				localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_pl.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
			}
		}
		else if (pt_BR.getLanguage().equals(language)) {
			if (pt_BR.getCountry().equals(country)) {
				localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_pt_BR.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
			}
			else {
				localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_pt.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
			}
		}
		else if (ro_RO.getLanguage().equals(language)) {
			if (ro_RO.getCountry().equals(country)) {
				localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_ro_RO.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
			}
		}
		else if (ru_RU.getLanguage().equals(language)) {
			if (ru_RU.getCountry().equals(country)) {
				localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_ru_RU.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
			}
			else {
				localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_ru.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
			}
		}
		else if (new Locale("sk").getLanguage().equals(language)) {
			localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_sk.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
		}
		else if (sr_Latn.getLanguage().equals(language)) {
			if (sr_Latn.getVariant().equals(locale.getVariant())) {
				localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_sr_Latn.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
			}
			else {
				localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_sr.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
			}
		}
		else if (sv_SE.getLanguage().equals(language)) {
			if (sv_SE.getCountry().equals(country)) {
				localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_sv_SE.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
			}
		}
		else if (uk_UA.getLanguage().equals(language)) {
			if (uk_UA.getCountry().equals(country)) {
				localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_uk_UA.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
			}
		}
		else if (zh_CN.getLanguage().equals(language)) {
			if (zh_CN.getCountry().equals(country)) {
				localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_zh_CN.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
			}
			else if (zh_TW.getCountry().equals(country)) {
				localeScript = String.format("<script type=\"text/javascript\" src=\"%s/locales/frameworkMessages_zh_TW.properties\"></script>", UtilImpl.SMART_CLIENT_DIR);
			}
		}
	}
	
	private void constructMenu(Customer customer, 
								UserImpl user, 
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
				result.append(SmartClientGenerateUtils.processString(Util.i18n(thisModule.getTitle(), getLocale())));
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

	private static void listDataSources(Customer customer, UserImpl user, StringBuilder result) 
	throws MetaDataException {
		StringBuilder dataSources = new StringBuilder(1024);

		result.append(",[");
		for (Module module : customer.getModules()) {
			Set<String> visitedQueryNames = new TreeSet<>();
			
			if (ViewType.list.equals(module.getHomeRef())) {
				String homeDocumentName = module.getHomeDocumentName();
				if (homeDocumentName != null) {
					DocumentQueryDefinition query = module.getDocumentDefaultQuery(customer, homeDocumentName);
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

	private static void listDataSourcesForMenuItems(UserImpl user,
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
			else if ((item instanceof ListItem) || (item instanceof TreeItem)) {
				ListItem grid = (ListItem) item;
				
				DocumentQueryDefinition query = null;
				String queryName = grid.getQueryName();
				String modelName = grid.getModelName();
				String documentName = grid.getDocumentName();
				
				if (queryName != null) { // its a query
					query = module.getDocumentQuery(queryName);
					SmartClientGenerateUtils.appendDataSourceDefinition(user, customer, query, null, null, true, dataSources, visitedQueryNames);
				}
				else {
					if (modelName != null) { // its a model
						Document document = module.getDocument(customer, documentName);
						SmartClientGenerateUtils.appendDataSourceDefinition(user, 
																				customer, 
																				module, 
																				document,
																				modelName,
																				true,
																				dataSources, 
																				visitedQueryNames);
					}
					else {
						query = module.getDocumentDefaultQuery(customer, documentName);
						SmartClientGenerateUtils.appendDataSourceDefinition(user, customer, query, null, null, true, dataSources, visitedQueryNames);
					}
				}
			}
		}
	}

	private void renderMenuStructure(Customer customer,
										Module module, 
										List<MenuItem> items, 
										String uxui,
										StringBuilder result) 
	throws MetaDataException, IOException {
		result.append("root:{name:'");
		result.append(SmartClientGenerateUtils.processString(Util.i18n(module.getName(), getLocale())));
		result.append("',sub:[");
		renderMenuItems(customer, module, items, uxui, result);
		result.append("]}");
	}

	private void renderMenuItems(Customer customer,
									Module module, 
									List<MenuItem> items, 
									String uxui,
									StringBuilder result)
	throws MetaDataException, IOException {
		Locale locale = getLocale();
		
		for (int i = 0, l = items.size(); i < l; i++) {
			MenuItem item = items.get(i);
			if (item.isApplicable(uxui)) {
				if (item instanceof MenuGroup) {
					MenuGroup group = (MenuGroup) item;
					result.append("{desc:'");
					result.append(SmartClientGenerateUtils.processString(Util.i18n(group.getName(), locale)));
					result.append("', sub:[");
					renderMenuItems(customer, module, group.getItems(), uxui, result);
					// print a comma if not the last module being processed
					result.append((i < (l - 1)) ? "]}," : "]}");
				}
				else {
					result.append("{name:'");
					String ref = null;
					String icon16 = null;
					Module itemModule = null;
					String itemDocumentName = null;
	                if (item instanceof TreeItem) {
	                    TreeItem treeItem = (TreeItem) item;
	                    itemDocumentName = treeItem.getDocumentName();
						String itemQueryName = treeItem.getQueryName();
						String modelName = treeItem.getModelName();
						if (modelName != null) {
							itemModule = customer.getModule(module.getDocument(customer, itemDocumentName).getOwningModuleName());
							result.append(itemDocumentName).append("__").append(modelName);
						}
						else {
		                    DocumentQueryDefinition query = deriveDocumentQuery(customer,
													                                module,
													                                item,
													                                itemQueryName,
													                                itemDocumentName);
							itemDocumentName = query.getDocumentName();
							result.append(query.getName());
							itemModule = query.getDocumentModule(customer);
						}
						icon16 = itemModule.getDocument(customer, itemDocumentName).getIcon16x16RelativeFileName();
	                    ref = "tree";
	                }
	                else if (item instanceof ListItem) {
						ListItem gridItem = (ListItem) item;
						itemDocumentName = gridItem.getDocumentName();
						String itemQueryName = gridItem.getQueryName();
						String modelName = gridItem.getModelName();
						if (modelName != null) {
							itemModule = customer.getModule(module.getDocument(customer, itemDocumentName).getOwningModuleName());
							result.append(itemDocumentName).append("__").append(modelName);
						}
						else {
							DocumentQueryDefinition query = deriveDocumentQuery(customer,
													                                module,
													                                item,
													                                itemQueryName,
													                                itemDocumentName);
							itemDocumentName = query.getDocumentName();
							result.append(query.getName());
							itemModule = query.getDocumentModule(customer);
						}
						icon16 = itemModule.getDocument(customer, itemDocumentName).getIcon16x16RelativeFileName();
						ref = "grid";
					}
					else if (item instanceof CalendarItem) {
	                    CalendarItem calendarItem = (CalendarItem) item;
	                    itemDocumentName = calendarItem.getDocumentName();
						DocumentQueryDefinition query = deriveDocumentQuery(customer,
												                                module,
												                                item,
												                                calendarItem.getQueryName(),
												                                itemDocumentName);
						itemDocumentName = query.getDocumentName();
						result.append(query.getName());
						itemModule = query.getDocumentModule(customer);
						icon16 = itemModule.getDocument(customer, itemDocumentName).getIcon16x16RelativeFileName();
	                    ref = "cal";
	                }
	                else if (item instanceof MapItem) {
	                    MapItem mapItem = (MapItem) item;
	                    itemDocumentName = mapItem.getDocumentName();
						String itemQueryName = mapItem.getQueryName();
						String modelName = mapItem.getModelName();
						if (modelName != null) {
							itemModule = customer.getModule(module.getDocument(customer, itemDocumentName).getOwningModuleName());
							result.append(itemDocumentName).append("__").append(modelName);
		                    result.append('_').append(mapItem.getGeometryBinding());
						}
						else {
							DocumentQueryDefinition query = deriveDocumentQuery(customer,
													                                module,
													                                item,
													                                itemQueryName,
													                                itemDocumentName);
							result.append(query.getName());
		                    result.append('_').append(mapItem.getGeometryBinding());
							itemModule = query.getDocumentModule(customer);
						}
						icon16 = itemModule.getDocument(customer, itemDocumentName).getIcon16x16RelativeFileName();
	                    ref = "map";
	                }
					else if (item instanceof EditItem) {
						itemDocumentName = ((EditItem) item).getDocumentName();
						result.append(itemDocumentName);
						itemModule = module;
						icon16 = module.getDocument(customer, itemDocumentName).getIcon16x16RelativeFileName();
						ref = "edit";
					}
					else if (item instanceof LinkItem) {
						ref = "link";
						String href = ((LinkItem) item).getHref();
						boolean relative = true;
						try {
							if (new URI(href).isAbsolute()) {
								result.append(href);
								relative = false;
							}
						} catch (URISyntaxException e) {
							// do nothing here if its not know to be absolute
						}
			        	
						if (relative) {
							result.append(Util.getSkyveContextUrl());
				    		if (href.charAt(0) != '/') {
				    			result.append('/');
				    		}
				    		result.append(href);
						}
					}
					result.append("',desc:'");
					result.append(SmartClientGenerateUtils.processString(Util.i18n(item.getName(), locale)));
					result.append("',ref:'");
					result.append(ref);
					if (icon16 != null) {
						result.append("',icon:'../resources?");
						if ((itemModule != null) && (itemDocumentName != null)) { // NB link items have no document
							result.append("_doc=").append(itemModule.getName()).append('.').append(itemDocumentName).append('&');
						}
						result.append("_n=").append(icon16);
					}
					// print a comma if not the last module being processed
					result.append((i < (l - 1)) ? "'},\n" : "'}\n");
				}
			}
		}
	}
}
