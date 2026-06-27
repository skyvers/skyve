package org.skyve.impl.web.faces.views;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Objects;

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
import org.skyve.web.UserAgentType;
import org.skyve.web.WebAction;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
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
@SuppressWarnings("java:S1192") // Repeated literals are deliberate menu URL rendering fragments.
public class MenuView extends HarnessView {
	private static final long serialVersionUID = -7523306130675202901L;

	// The modules menu on the LHS
	// Note: MenuModel is never mutated once established, only the reference is dropped
	@SuppressWarnings("java:S3077") // Session-scoped access pattern uses effectively immutable menu instances.
	private transient volatile MenuModel menu;

	@SuppressWarnings("java:S3077") // Access follows the same session-scoped pattern as menu.
	private transient volatile UserAgentType menuEmulatedUserAgentType;

	public @Nullable MenuModel getMenu() {
		UserAgentType emulatedUserAgentType = getCurrentEmulatedUserAgentType();
		// double-checked locking
		if ((menu == null) || (menuEmulatedUserAgentType != emulatedUserAgentType)) {
			synchronized (this) {
				if ((menu == null) || (menuEmulatedUserAgentType != emulatedUserAgentType)) {
					setState();
				}
			}
		}
		return menu;
	}
	
	public void resetState() {
		menu = null;
		menuEmulatedUserAgentType = null;
	}

	private void setState() {
		new FacesAction<Void>() {
			@Override
			public @Nullable Void callback() throws Exception {
				@Nonnull FacesContext fc = FacesContext.getCurrentInstance();
				if (! fc.isPostback()) {
					@Nonnull HttpServletRequest request = (HttpServletRequest) fc.getExternalContext().getRequest();
					setBizModuleParameter(request.getParameter("m"));
					initialise(); // check m parameter and set to default if DNE
					
					UxUi uxui = UserAgent.getUxUi(request);
					UserAgentType emulatedUserAgentType = UserAgent.isEmulated(request) ? UserAgent.getType(request) : null;
					menu = createMenuModel(getBizModuleParameter(), uxui.getName(), emulatedUserAgentType);
					menuEmulatedUserAgentType = emulatedUserAgentType;
				}
				
				return null;
			}
		}.execute();
	}

	private transient int menuItemId; 

	/**
	 * Builds the PrimeFaces menu model for the selected module and UX/UI mode.
	 *
	 * @param bizModule the currently selected module
	 * @param uxui the active UX/UI name
	 * @param emulatedUserAgentType the emulated user-agent type, when device preview is active
	 * @return the populated menu model
	 */
	private @Nonnull MenuModel createMenuModel(@Nullable String bizModule, @Nullable String uxui, @Nullable UserAgentType emulatedUserAgentType) {
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
				subs.peek().getElements().add(createMenuItem(item, iconStyleClass, menuModule, itemModule, itemQueryName, null, emulatedUserAgentType));
			}
			
			@Override
			public void renderEditItem(EditItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String icon16,
										String iconStyleClass) {
				subs.peek().getElements().add(createMenuItem(item, iconStyleClass, menuModule, itemModule, null, null, emulatedUserAgentType));
			}
			
			@Override
			public void renderLinkItem(LinkItem item,
										Module menuModule,
										boolean relative,
										String absoluteHref) {
				subs.peek().getElements().add(createMenuItem(item, null, menuModule, null, null, absoluteHref, emulatedUserAgentType));
			}
			
			@Override
			public void renderListItem(ListItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String itemQueryName,
										String icon16,
										String iconStyleClass) {
				subs.peek().getElements().add(createMenuItem(item, iconStyleClass, menuModule, itemModule, itemQueryName, null, emulatedUserAgentType));
			}

			@Override
			public void renderMapItem(MapItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String itemQueryName,
										String icon16,
										String iconStyleClass) {
				subs.peek().getElements().add(createMenuItem(item, iconStyleClass, menuModule, itemModule, itemQueryName, null, emulatedUserAgentType));
			}
			
			@Override
			public void renderTreeItem(TreeItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String itemQueryName,
										String icon16,
										String iconStyleClass) {
				subs.peek().getElements().add(createMenuItem(item, iconStyleClass, menuModule, itemModule, itemQueryName, null, emulatedUserAgentType));
			}

			@Override
			public void renderedMenuGroup(MenuGroup group, Module menuModule) {
				subs.pop();
			}
			
			@Override
			public void renderedModuleMenu(org.skyve.metadata.module.menu.Menu moduleMenu, Module menuModule, boolean open) {
				subs.pop();
			}
		}.render(Objects.requireNonNull(getUser(), "user"));
		
		return result;
	}

	/**
	 * Creates a PrimeFaces menu item from Skyve menu metadata.
	 *
	 * @param item the Skyve menu item metadata
	 * @param iconStyleClass the icon style class
	 * @param menuModule the current menu module
	 * @param itemModule the module for the target item
	 * @param itemQueryName the resolved query name
	 * @param itemAbsoluteHref an explicit absolute href, when supplied
	 * @param emulatedUserAgentType the emulated user-agent type, when device preview is active
	 * @return the configured PrimeFaces menu item
	 */
	private @Nonnull org.primefaces.model.menu.MenuItem createMenuItem(@Nonnull MenuItem item,
																		@Nullable String iconStyleClass,
																		@Nonnull Module menuModule,
																		@Nullable Module itemModule,
																		@Nullable String itemQueryName,
																		@Nullable String itemAbsoluteHref,
																		@Nullable UserAgentType emulatedUserAgentType) {
		DefaultMenuItem result = DefaultMenuItem.builder().id(String.valueOf(menuItemId++)).value(item.getLocalisedName()).icon(iconStyleClass).build();
		result.setHref(createMenuHref(menuModule, itemModule, item, itemQueryName, itemAbsoluteHref, emulatedUserAgentType));
		return result;
	}

	private static @Nullable UserAgentType getCurrentEmulatedUserAgentType() {
		FacesContext fc = FacesContext.getCurrentInstance();
		@Nonnull HttpServletRequest request = (HttpServletRequest) fc.getExternalContext().getRequest();
		return UserAgent.isEmulated(request) ? UserAgent.getType(request) : null;
	}

	/**
	 * Builds the JavaScript history URL for a rendered menu item.
	 *
	 * @param menuModule the module containing the menu definition
	 * @param itemModule the module targeted by the menu item
	 * @param item the Skyve menu item metadata
	 * @param itemQueryName the resolved query name
	 * @param itemAbsoluteHref an explicit absolute href, when supplied
	 * @return the JavaScript menu href
	 */
	@SuppressWarnings("java:S3776") // Legacy URL assembly branching retained during Javadoc remediation.
	public static @Nonnull String createMenuHref(@Nonnull Module menuModule,
													@Nullable Module itemModule,
													@Nonnull MenuItem item,
													@Nullable String itemQueryName,
													@Nullable String itemAbsoluteHref) {
		return createMenuHref(menuModule, itemModule, item, itemQueryName, itemAbsoluteHref, null);
	}

	/**
	 * Builds the JavaScript history URL for a rendered menu item.
	 *
	 * @param menuModule the module containing the menu definition
	 * @param itemModule the module targeted by the menu item
	 * @param item the Skyve menu item metadata
	 * @param itemQueryName the resolved query name
	 * @param itemAbsoluteHref an explicit absolute href, when supplied
	 * @param emulatedUserAgentType the emulated user-agent type, when device preview is active
	 * @return the JavaScript menu href
	 */
	@SuppressWarnings("java:S3776") // Legacy URL assembly branching retained during Javadoc remediation.
	public static @Nonnull String createMenuHref(@Nonnull Module menuModule,
													@Nullable Module itemModule,
													@Nonnull MenuItem item,
													@Nullable String itemQueryName,
													@Nullable String itemAbsoluteHref,
													@Nullable UserAgentType emulatedUserAgentType) {
		@Nonnull StringBuilder result = new StringBuilder(128);
		result.append("javascript:SKYVE.PF.startHistory('");
		
		if (itemAbsoluteHref != null) {
			result.append(itemAbsoluteHref.replace("'", "\\'"));
		}
		else if (item instanceof ListItem listItem) {
			appendSkyveNavigationPrefix(result, emulatedUserAgentType);
			result.append("a=").append(WebAction.l.toString()).append("&m=").append(menuModule.getName());
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
			appendSkyveNavigationPrefix(result, emulatedUserAgentType);
			@Nonnull Module editModule = Objects.requireNonNull(itemModule, "itemModule");
			result.append("a=").append(WebAction.e.toString()).append("&m=").append(editModule.getName());
			result.append("&d=").append(editItem.getDocumentName());
		}
		else if (item instanceof CalendarItem calendarItem) {
			appendSkyveNavigationPrefix(result, emulatedUserAgentType);
			result.append("a=").append(WebAction.c.toString()).append("&m=").append(menuModule.getName());
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
			appendSkyveNavigationPrefix(result, emulatedUserAgentType);
			result.append("a=").append(WebAction.t.toString()).append("&m=").append(menuModule.getName());
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
			appendSkyveNavigationPrefix(result, emulatedUserAgentType);
			result.append("a=").append(WebAction.m.toString()).append("&m=").append(menuModule.getName());
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

	/**
	 * Appends the common Skyve navigation URL prefix for menu items, taking into account device
	 * preview mode when an emulated user-agent type is provided.

	 * @param result
	 * @param emulatedUserAgentType
	 */
	private static void appendSkyveNavigationPrefix(@Nonnull StringBuilder result, @Nullable UserAgentType emulatedUserAgentType) {
		result.append(Util.getSkyveContextUrl());
		if (emulatedUserAgentType == null) {
			result.append("/?");
		}
		else {
			result.append("/device.jsp?ua=").append(emulatedUserAgentType).append('&');
		}
	}
}
