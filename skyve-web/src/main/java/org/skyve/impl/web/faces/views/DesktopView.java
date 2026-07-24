package org.skyve.impl.web.faces.views;

import java.net.MalformedURLException;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.TreeSet;

import org.skyve.CORE;
import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.LinkItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.actions.ActionUtil;
import org.skyve.impl.web.service.smartclient.SmartClientViewRenderer;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.menu.Menu;
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.module.menu.MenuRenderer;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Icons;
import org.skyve.util.OWASP;
import org.skyve.web.WebAction;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.enterprise.context.RequestScoped;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Named;
import jakarta.servlet.http.HttpServletRequest;

/**
 * Models a view interaction and binds it to the active Skyve web context.
 */
@RequestScoped
@Named("desktop")
@SuppressWarnings("java:S1192") // Repeated literals are deliberate desktop-view markup fragments.
public class DesktopView extends HarnessView {
	private static final long serialVersionUID = 913239189728613263L;

	private static final String DESKTOP_SCRIPT_PATH = "desktop/";
	private static final String FRAMEWORK_MESSAGES_DIR = UtilImpl.SMART_CLIENT_DIR + "/locales/";
	private static final String FRAMEWORK_MESSAGES_FILE = "frameworkMessages";
	private static final String JS_EXTENSION = ".js";
	private static final String LOCALE_SEPARATOR = "_";
	private static final String PROPERTIES_EXTENSION = ".properties";
	private static final String SKYVE_MESSAGES_FILE = "skyveMessages";
	private static final String DEFAULT_SKYVE_MESSAGES_SCRIPT = DESKTOP_SCRIPT_PATH + SKYVE_MESSAGES_FILE + JS_EXTENSION;
	@SuppressWarnings("jabva:S1075") // Constant file path is deliberate for web
	private static final String DESKTOP_RESOURCE_PATH = "/" + DESKTOP_SCRIPT_PATH;
	private static final String SCRIPT_CLOSE = "\"></script>";
	private static final String SCRIPT_OPEN = "<script type=\"text/javascript\" src=\"";
	
	private String localeScript;
	
	/**
	 * Returns locale bootstrap script markup for SmartClient internationalization.
	 *
	 * @return locale bootstrap script markup, or {@code null}
	 */
	public String getLocaleScript() {
		return localeScript;
	}
	
	private String menuScript;

	/**
	 * Returns generated desktop menu bootstrap script.
	 *
	 * @return generated desktop menu bootstrap script, or {@code null}
	 */
	public String getMenuScript() {
		return menuScript;
	}
	
	private String dataSourceScript;

	/**
	 * Returns generated SmartClient datasource bootstrap script.
	 *
	 * @return generated SmartClient datasource bootstrap script, or {@code null}
	 */
	public String getDataSourceScript() {
		return dataSourceScript;
	}
	
	private String uiScript;

	/**
	 * Returns generated initial UI action script.
	 *
	 * @return generated initial UI action script, or {@code null}
	 */
	public String getUiScript() {
		return uiScript;
	}
	
	private String bannerScript;

	/**
	 * Returns generated environment banner script.
	 *
	 * @return generated environment banner script, or {@code null}
	 */
	public String getBannerScript() {
		return bannerScript;
	}

	/**
	 * Returns SmartClient base directory configured for this runtime.
	 *
	 * @return SmartClient base directory
	 */
	@SuppressWarnings("static-method")
	public String getSmartClientDir() {
		return UtilImpl.SMART_CLIENT_DIR;
	}

	private String skin;

	/**
	 * Returns the SmartClient skin selected for the active UX/UI profile.
	 *
	 * @return SmartClient skin name, or {@code null}
	 */
	public String getSkin() {
		return skin;
	}
	
	/**
	 * Prepares desktop scripts (menu, datasource, UI, and banner) for initial non-postback rendering.
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	public void preRender() {
        final FacesContext fc = FacesContext.getCurrentInstance();
        if (! fc.isPostback()) {
        	new FacesAction<Void>() {
				@Override
				public Void callback() throws Exception {
			    	UserImpl user = (UserImpl) CORE.getUser();
			    	Customer customer = user.getCustomer();
			    	
			    	initialise();
					createLocaleScripts();

			    	String bizModule = getBizModuleParameter();
			    	String bizDocument = getBizDocumentParameter();
			    	String bizId = getBizIdParameter();
			    	
					HttpServletRequest request = (HttpServletRequest) fc.getExternalContext().getRequest();
					UxUi uxui = UserAgent.getSelection(request).getUxUi();
					skin = uxui.getScSkin();
					
					StringBuilder sb = new StringBuilder(8192);
					sb.append("isc.BizUtil.headerTemplate='").append(getHeaderTemplate()).append("';");
					sb.append("isc.BizUtil.userName='").append(getUserName()).append("';");
					sb.append("isc.BizUtil.userContactName='").append(getUserContactName()).append("';");
					sb.append("isc.BizUtil.userContactInitials='").append(getUserContactInitials()).append("';");
					final String url = getUserContactImageUrl();
					if (url != null) {
						sb.append("isc.BizUtil.userContactImageUrl='").append(url).append("';");
					}
					sb.append("isc.BizUtil.canSwitchMode=").append(isCanSwitchMode()).append(";\n");

					constructMenu(bizModule, uxui.getName(), sb);
					menuScript = sb.toString();
					sb.setLength(0); 
					listDataSources(customer, user, uxui.getName(), sb);
					dataSourceScript = sb.toString();
					sb.setLength(0);
	
					// Set the first view to show
					WebAction a = DesktopView.this.getWebActionParameter();
					if (WebAction.e.equals(a)) { // edit
						sb.append("isc.BizUtil.getEditView('").append(bizModule).append("','");
						sb.append(bizDocument).append("',function(view){");
						sb.append("details.addMember(view);isc.BizUtil._currentView=view;");
						if (bizId == null) {
						    sb.append("view.newInstance();});");
						}
						else {
						    sb.append("view.editInstance('").append(bizId).append("');});");
						}
					}
					else {
						sb.append("details.showMember(isc.ListView.contents);");
						sb.append("isc.ListView.set");
						if (WebAction.l.equals(a)) {
							sb.append("Grid");
						}
						else if (WebAction.t.equals(a)) {
							sb.append("Tree");
						}
						else if (WebAction.m.equals(a)) {
							sb.append("Map");
						}
						else if (WebAction.c.equals(a)) {
							sb.append("Calendar");
						}
						
						if (bizDocument != null) {
							sb.append("DataSource('").append(bizModule).append('_').append(bizDocument).append("__").append(getQueryNameParameter()).append("');");
						}
						else {
							QueryDefinition query = ActionUtil.getMetaDataQuery(bizModule, getQueryNameParameter());
							sb.append("DataSource('").append(bizModule).append('_').append(query.getName()).append("');");
						}
					}
					uiScript = sb.toString();
					sb.setLength(0);
					
					if (UtilImpl.ENVIRONMENT_IDENTIFIER != null) {
						sb.append("$('body').append('<div class=\"skyveEnvBanner skyveTopEnvBanner\">");
						sb.append(UtilImpl.ENVIRONMENT_IDENTIFIER).append("</div>');");
						sb.append("$('body').append('<div class=\"skyveEnvBanner skyveBottomEnvBanner\">");
						sb.append(UtilImpl.ENVIRONMENT_IDENTIFIER).append("</div>');");
					}
					bannerScript = sb.toString();
					
					return null;
				}
			}.execute();
        }
	}

	/**
	 * Builds the desktop header HTML template injected into SmartClient runtime state.
	 *
	 * @return header template HTML
	 */
	public String getHeaderTemplate() {
		StringBuilder result = new StringBuilder(256);
		result.append("<table class=\"skyveHeaderTable\">");
		result.append("<tr>");
		result.append("<td width=\"1%\">{icon}</td>");
		result.append("<td><div class=\"titleBar\">{title}</div></td>");
		result.append("<td width=\"10%\" align=\"right\"><img src=\"images/skyve-thick-grey.png\" style=\"max-height: 28px; height: auto;\" alt=\"Skyve\"/></td>");
		result.append("<td width=\"1%\" align=\"right\"><div class=\"skyveDocumentLink\">{link}</div></td>");
		if (isCanTextSearch()) {
			result.append("<td width=\"1%\" align=\"right\"><a href=\"javascript:isc.BizUtil.popupSearch();\" class=\"dhtmlPageButton\" title=\"Search\"><i class=\"");
			result.append(Icons.FONT_SEARCH);
			result.append(" fa-2x\"></i></a></td>");
		}
		result.append("<td width=\"1%\" align=\"right\"><a href=\"javascript:isc.BizUtil.showHelp({help});\" class=\"dhtmlPageButton\" title=\"Help\"><i class=\"");
		result.append(Icons.FONT_HELP);
		result.append(" fa-2x\"></i></a></td>");
		result.append("<td width=\"1%\" align=\"right\"><a href=\"javascript:isc.BizUtil.showPortal();\" class=\"dhtmlPageButton\" title=\"Dashboard\"><i class=\"");
		result.append(Icons.FONT_DASHBOARD);
		result.append(" fa-2x\"></i></a></td>");
		result.append("<td width=\"1%\" align=\"right\"></td></tr></table>");

		return result.toString();
	}

	/**
	 * Creates locale-specific script include markup for SmartClient and Skyve messages.
	 */
	private void createLocaleScripts() {
		createLocaleScripts(CORE.getUser().getLocale());
	}
	
	/**
	 * Creates locale-specific script include markup for the supplied locale.
	 *
	 * <p>Side effects: updates {@link #getLocaleScript()} with script tags for the
	 * SmartClient framework messages and the Skyve desktop messages. The SmartClient
	 * framework bundle falls back to the default bundle when no more specific mapping
	 * exists; the Skyve desktop bundle falls back to the default bundle when the
	 * localized web resource is absent.
	 *
	 * @param locale locale to use when selecting message bundles; must not be {@code null}
	 */
	void createLocaleScripts(@Nonnull Locale locale) {
		@Nonnull StringBuilder result = new StringBuilder(192);
		appendScriptTag(result, resolveFrameworkMessagesScript(locale));
		result.append('\n');
		appendScriptTag(result, resolveSkyveMessagesScript(locale));
		localeScript = result.toString();
	}
	
	/**
	 * Appends one script tag for a pre-resolved source URL.
	 *
	 * @param result buffer receiving the script tag; must not be {@code null}
	 * @param src script source URL; must not be {@code null}
	 */
	private static void appendScriptTag(@Nonnull StringBuilder result, @Nonnull String src) {
		result.append(SCRIPT_OPEN).append(src).append(SCRIPT_CLOSE);
	}
	
	/**
	 * Resolves the SmartClient framework message bundle script for a locale.
	 *
	 * <p>Returns the default SmartClient framework bundle when the locale has no
	 * mapped localized bundle.
	 *
	 * @param locale locale to resolve; must not be {@code null}
	 * @return script source URL for the framework message bundle; never {@code null}
	 */
	@SuppressWarnings("java:S3776") // Locale mapping mirrors the available SmartClient framework bundles.
	private static @Nonnull String resolveFrameworkMessagesScript(@Nonnull Locale locale) {
		@Nonnull String language = locale.getLanguage();
		@Nonnull String country = locale.getCountry();
		@Nonnull String variant = locale.getVariant();
		@Nullable String bundleName = null;

		if ("ar".equals(language)) {
			bundleName = "ar";
		}
		else if ("ba".equals(language)) {
			bundleName = "ba";
		}
		else if ("bg".equals(language) && "BG".equals(country)) {
			bundleName = "bg_BG";
		}
		else if ("cr".equals(language)) {
			bundleName = "cr";
		}
		else if ("cs".equals(language)) {
			bundleName = "cs";
		}
		else if ("da".equals(language)) {
			bundleName = "da";
		}
		else if ("de".equals(language)) {
			bundleName = "de";
		}
		else if ("el".equals(language)) {
			bundleName = "el";
		}
		else if ("es".equals(language)) {
			bundleName = "es";
		}
		else if ("fi".equals(language)) {
			bundleName = "fi";
		}
		else if ("fr".equals(language) && "FR".equals(country)) {
			bundleName = "fr_FR";
		}
		else if ("hr".equals(language)) {
			bundleName = "hr";
		}
		else if ("hu".equals(language) && "HU".equals(country)) {
			bundleName = "hu_HU";
		}
		else if ("id".equals(language)) {
			bundleName = "id";
		}
		else if ("it".equals(language)) {
			bundleName = "it";
		}
		else if ("ja".equals(language)) {
			bundleName = "ja";
		}
		else if ("ko".equals(language)) {
			bundleName = "ko";
		}
		else if ("nb".equals(language) && "NO".equals(country)) {
			bundleName = "nb_NO";
		}
		else if ("nl".equals(language)) {
			bundleName = "nl";
		}
		else if ("pl".equals(language)) {
			bundleName = "PL".equals(country) ? "pl_PL" : "pl";
		}
		else if ("pt".equals(language)) {
			bundleName = "BR".equals(country) ? "pt_BR" : "pt";
		}
		else if ("ro".equals(language) && "RO".equals(country)) {
			bundleName = "ro_RO";
		}
		else if ("ru".equals(language)) {
			bundleName = "RU".equals(country) ? "ru_RU" : "ru";
		}
		else if ("sk".equals(language)) {
			bundleName = "sk";
		}
		else if ("sr".equals(language)) {
			bundleName = "Latn".equals(variant) ? "sr_Latn" : "sr";
		}
		else if ("sv".equals(language) && "SE".equals(country)) {
			bundleName = "sv_SE";
		}
		else if ("tr".equals(language) && "TR".equals(country)) {
			bundleName = "tr_TR";
		}
		else if ("uk".equals(language) && "UA".equals(country)) {
			bundleName = "uk_UA";
		}
		else if ("zh".equals(language)) {
			if ("CN".equals(country)) {
				bundleName = "zh_CN";
			}
			else if ("TW".equals(country)) {
				bundleName = "zh_TW";
			}
		}
		
		if (bundleName == null) {
			StringBuilder result = new StringBuilder(FRAMEWORK_MESSAGES_DIR.length() + 
														FRAMEWORK_MESSAGES_FILE.length() +
														PROPERTIES_EXTENSION.length());
			return result.append(FRAMEWORK_MESSAGES_DIR)
							.append(FRAMEWORK_MESSAGES_FILE)
							.append(PROPERTIES_EXTENSION)
							.toString();
		}
			
		StringBuilder result = new StringBuilder(FRAMEWORK_MESSAGES_DIR.length() +
													FRAMEWORK_MESSAGES_FILE.length() +
													LOCALE_SEPARATOR.length() +
													bundleName.length() +
													PROPERTIES_EXTENSION.length());
		return result.append(FRAMEWORK_MESSAGES_DIR)
						.append(FRAMEWORK_MESSAGES_FILE)
						.append(LOCALE_SEPARATOR)
						.append(bundleName)
						.append(PROPERTIES_EXTENSION)
						.toString();
	}

	/**
	 * Resolves the Skyve desktop message bundle script for a locale.
	 *
	 * <p>Returns the default Skyve desktop message bundle when the localized bundle
	 * for the locale language is not present as a web resource.
	 *
	 * @param locale locale to resolve; must not be {@code null}
	 * @return script source URL for the Skyve desktop message bundle; never {@code null}
	 */
	private @Nonnull String resolveSkyveMessagesScript(@Nonnull Locale locale) {
		@Nonnull String language = locale.getLanguage();
		if (! language.isEmpty()) {
			@Nonnull String fileName = SKYVE_MESSAGES_FILE + LOCALE_SEPARATOR + language + JS_EXTENSION;
			@Nonnull String resourcePath = DESKTOP_RESOURCE_PATH + fileName;
			if (webResourceExists(resourcePath)) {
				return DESKTOP_SCRIPT_PATH + fileName;
			}
		}
		return DEFAULT_SKYVE_MESSAGES_SCRIPT;
	}
		
	/**
	 * Tests whether a web resource path can be resolved by the active Faces context.
	 *
	 * @param resourcePath context-relative resource path; must not be {@code null}
	 * @return {@code true} when the resource exists, otherwise {@code false}
	 */
	@SuppressWarnings("static-method") // Instance test seam for servlet-container resource resolution.
	boolean webResourceExists(@Nonnull String resourcePath) {
		try {
			return FacesContext.getCurrentInstance().getExternalContext().getResource(resourcePath) != null;
		}
		catch (@SuppressWarnings("unused") MalformedURLException e) {
			return false;
		}
	}
	
	/**
	 * Builds menu bootstrap script for all visible module menu entries.
	 *
	 * @param chosenModuleName module name currently being rendered
	 * @param uxui UX/UI name used to resolve the menu structure
	 * @param result buffer that receives the generated script
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	private void constructMenu(String chosenModuleName,
								String uxui,
								StringBuilder result) {
		result.append("isc.BizUtil.init('../").append(getLogoRelativeFileNameUrl());
		result.append("',[");

		// render each module menu
		new MenuRenderer(uxui, chosenModuleName) {
			/**
			 * Starts rendering a module menu descriptor.
			 */
			@Override
			public void renderModuleMenu(Menu menu, Module menuModule, boolean open) {
				result.append("{name:'");
				result.append(menuModule.getName());
				result.append("',");
				result.append("title:'");
				result.append(OWASP.escapeJsString(menuModule.getLocalisedTitle()));
				result.append("',");
			}
			
			/**
			 * Starts rendering the root menu node for a module.
			 */
			@Override
			public void renderMenuRoot(Menu menu, Module menuModule) {
				result.append("root:{name:'");
				result.append(OWASP.escapeJsString(menuModule.getName()));
				result.append("',sub:[");
			}
			
			/**
			 * Starts rendering a menu-group node.
			 */
			@Override
			public void renderMenuGroup(MenuGroup group, Module menuModule) {
				result.append("{desc:'");
				result.append(OWASP.escapeJsString(group.getLocalisedName()));
				result.append("', sub:[");
			}
			
			@Override
			public void renderCalendarItem(CalendarItem item,
											Module menuModule,
											Module itemModule,
											Document itemDocument,
											String itemQueryName,
											String icon16,
											String iconStyleClass) {
				result.append("{name:'").append(itemQueryName);
				renderItem(item.getLocalisedName(), icon16, iconStyleClass, null, "cal", itemModule, itemDocument);
			}
			
			@Override
			public void renderEditItem(EditItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String icon16,
										String iconStyleClass) {
				result.append("{name:'").append(itemDocument.getName());
				String menuModuleName = menuModule.getName();
				String itemModuleName = itemModule.getName();
				if ((menuModuleName != null) && (! menuModuleName.equals(itemModuleName))) {
					result.append("',module:'").append(itemModuleName);
				}
				renderItem(item.getLocalisedName(), icon16, iconStyleClass, null, "edit", itemModule, itemDocument);
			}
			
			@Override
			public void renderLinkItem(LinkItem item,
										Module menuModule,
										boolean relative,
										String absoluteHref) {
				result.append("{name:'").append(absoluteHref);
				renderItem(item.getLocalisedName(), null, null, null, "link", null, null);
			}
			
			@Override
			public void renderListItem(ListItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String itemQueryName,
										String icon16,
										String iconStyleClass) {
				result.append("{name:'");
				String modelName = item.getModelName();
				if (modelName != null) {
					result.append(itemDocument.getName()).append("__").append(modelName);
				}
				else {
					result.append(itemQueryName);
				}
				renderItem(item.getLocalisedName(),
							icon16,
							iconStyleClass,
							item.isAutoPopulate() ? "{}" : "{autoPopulate:false}",
							"grid",
							itemModule,
							itemDocument);
			}
			
			@Override
			public void renderMapItem(MapItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String itemQueryName, 
										String icon16,
										String iconStyleClass) {
				result.append("{name:'");
				String modelName = item.getModelName();
				if (modelName != null) {
					result.append(itemDocument.getName()).append("__").append(modelName);
                    result.append('_').append(item.getGeometryBinding());
				}
				else {
					result.append(itemQueryName).append('_').append(item.getGeometryBinding());
				}
				renderItem(item.getLocalisedName(), icon16, iconStyleClass, null, "map", itemModule, itemDocument);
			}
			
			@Override
			public void renderTreeItem(TreeItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String itemQueryName,
										String icon16,
										String iconStyleClass) {
				result.append("{name:'");
				String modelName = item.getModelName();
				if (modelName != null) {
					result.append(itemDocument.getName()).append("__").append(modelName);
				}
				else {
					result.append(itemQueryName);
				}
				renderItem(item.getLocalisedName(),
							icon16,
							iconStyleClass, 
							item.isAutoPopulate() ? "{}" : "{autoPopulate:false}",
							"tree",
							itemModule,
							itemDocument);
			}
			
			private void renderItem(String name,
										String icon16,
										String iconStyleClass,
										String config,
										String ref,
										Module itemModule,
										Document itemDocument) {
				result.append("',desc:'");
				if (iconStyleClass != null) {
					result.append("<i class=\"bizhubFontIcon ").append(iconStyleClass).append("\"></i>");
				}
				// Leave some space between the icon and its label
				if ((icon16 != null) || (iconStyleClass != null)) {
					result.append("<span> &nbsp;</span>");
				}
				result.append(OWASP.escapeJsString(name)).append('\'');
				if (config != null) {
					result.append(",config:").append(config);
				}
				result.append(",ref:'").append(OWASP.escapeJsString(ref));
				if ((iconStyleClass == null) && (icon16 != null)) {
					result.append("',icon:'../resources?");
					if ((itemModule != null) && (itemDocument != null)) { // NB link items have no document
						result.append("_doc=").append(OWASP.escapeJsString(itemModule.getName())).append('.')
								.append(OWASP.escapeJsString(itemDocument.getName())).append('&');
					}
					result.append("_n=").append(OWASP.escapeJsString(icon16));
				}
				result.append("'},");
			}
			
			/**
			 * Finalizes a rendered menu-group node.
			 */
			@Override
			public void renderedMenuGroup(MenuGroup group, Module menuModule) {
				result.setLength(result.length() - 1);
				result.append("]},");
			}
			
			/**
			 * Finalizes the rendered root menu node.
			 */
			@Override
			public void renderedMenuRoot(Menu menu, Module menuModule) {
				result.setLength(result.length() -1); // remove the last comma
				result.append("]}");
			}

			/**
			 * Finalizes a rendered module menu descriptor.
			 */
			@Override
			public void renderedModuleMenu(Menu menu, Module menuModule, boolean open) {
				result.append(",open:").append(open).append("},");
			}
		}.render(requireUser());

		// finish up menu defs
		result.setLength(result.length() - 1); // remove last comma from menu
		// defs
		result.append(']');
	}
	
	private @Nonnull User requireUser() {
		@Nullable User result = getUser();
		if (result == null) {
			throw new IllegalStateException("Cannot render the desktop menu without a session user.");
		}
		return result;
	}

	/**
	 * Appends SmartClient datasource definitions for accessible module menu items.
	 *
	 * @param customer customer whose modules are being rendered
	 * @param user current user whose permissions determine accessible menus
	 * @param uxui UX/UI name used to resolve the menu structure
	 * @param result buffer that receives the generated script
	 */
	private static void listDataSources(Customer customer, UserImpl user, String uxui, StringBuilder result) {
		StringBuilder dataSources = new StringBuilder(1024);

		result.append(",[");
		for (Module module : customer.getModules()) {
			Set<String> visitedQueryNames = new TreeSet<>();
			
			if (ViewType.list.equals(module.getHomeRef())) {
				String homeDocumentName = module.getHomeDocumentName();
				if (homeDocumentName != null) {
					MetaDataQueryDefinition query = module.getDocumentDefaultQuery(customer, homeDocumentName);
					SmartClientViewRenderer.appendDataSourceDefinition(user, customer, query, null, null, uxui, true, dataSources, visitedQueryNames);
				}
			}
			
			String moduleName = module.getName();
			Menu menu = user.getModuleMenu(moduleName);
			listDataSourcesForMenuItems(user, customer, moduleName, module, menu.getItems(), uxui, dataSources, visitedQueryNames);
		}
		if (! dataSources.isEmpty()) { // we have appended some data sources
			dataSources.setLength(dataSources.length() - 2); // remove the last data source comma
			result.append(dataSources);
		}
		result.append("]);");
	}

	/**
	 * Recursively appends datasource definitions for nested menu items.
	 *
	 * @param user current user whose permissions determine accessible items
	 * @param customer customer whose modules are being rendered
	 * @param moduleName module name containing the menu items
	 * @param module module containing the menu items
	 * @param items menu items to traverse
	 * @param uxui UX/UI name used to resolve the menu structure
	 * @param dataSources buffer that receives generated datasource definitions
	 * @param visitedQueryNames set used to avoid duplicate query definitions
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	private static void listDataSourcesForMenuItems(UserImpl user,
														Customer customer, 
														String moduleName, 
														Module module, 
														List<MenuItem> items,
														String uxui,
														StringBuilder dataSources,
														Set<String> visitedQueryNames) {
		for (MenuItem item : items) {
			if (item instanceof MenuGroup group) {
				listDataSourcesForMenuItems(user, customer, moduleName, module, group.getItems(), uxui, dataSources, visitedQueryNames);
			} 
			else if (item instanceof ListItem listOrTree) {
				MetaDataQueryDefinition query = null;
				String queryName = listOrTree.getQueryName();
				String modelName = listOrTree.getModelName();
				String documentName = listOrTree.getDocumentName();
				
				if (queryName != null) { // its a query
					query = module.getMetaDataQuery(queryName);
					SmartClientViewRenderer.appendDataSourceDefinition(user, customer, query, null, null, uxui, true, dataSources, visitedQueryNames);
				}
				else {
					if (modelName != null) { // its a model
						Document document = module.getDocument(customer, documentName);
						SmartClientViewRenderer.appendDataSourceDefinition(user, 
																			customer, 
																			module, 
																			document,
																			modelName,
																			uxui,
																			true,
																			dataSources, 
																			visitedQueryNames);
					}
					else {
						query = module.getDocumentDefaultQuery(customer, documentName);
						SmartClientViewRenderer.appendDataSourceDefinition(user, customer, query, null, null, uxui, true, dataSources, visitedQueryNames);
					}
				}
			}
		}
	}
}
