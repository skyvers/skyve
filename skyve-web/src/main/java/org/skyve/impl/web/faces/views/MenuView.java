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
import org.skyve.util.OWASP;
import org.skyve.util.Util;
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
	private transient volatile String menuUxUiName;

	public @Nullable MenuModel getMenu() {
		@Nonnull FacesContext fc = FacesContext.getCurrentInstance();
		@Nonnull HttpServletRequest request = (HttpServletRequest) fc.getExternalContext().getRequest();
		String uxuiName = UserAgent.getSelection(request).getUxUi().getName();
		// double-checked locking
		if ((menu == null) || (! Objects.equals(menuUxUiName, uxuiName))) {
			synchronized (this) {
				if ((menu == null) || (! Objects.equals(menuUxUiName, uxuiName))) {
					setState(fc, request, uxuiName);
				}
			}
		}
		return menu;
	}
	
	public void resetState() {
		menu = null;
		menuUxUiName = null;
	}

	private void setState(@Nonnull FacesContext fc,
							@Nonnull HttpServletRequest request,
							@Nullable String uxuiName) {
		new FacesAction<Void>() {
			@Override
			public @Nullable Void callback() throws Exception {
				if (! fc.isPostback()) {
					setBizModuleParameter(request.getParameter("m"));
					initialise(); // check m parameter and set to default if DNE
				}

				menu = createMenuModel(getBizModuleParameter(), uxuiName);
				menuUxUiName = uxuiName;
				
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
	 * @return the populated menu model
	 */
	@Nonnull MenuModel createMenuModel(@Nullable String bizModule, @Nullable String uxui) {
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
	 * @return the configured PrimeFaces menu item
	 */
	private @Nonnull org.primefaces.model.menu.MenuItem createMenuItem(@Nonnull MenuItem item,
																		@Nullable String iconStyleClass,
																		@Nonnull Module menuModule,
																		@Nullable Module itemModule,
																		@Nullable String itemQueryName,
																		@Nullable String itemAbsoluteHref) {
		DefaultMenuItem result = DefaultMenuItem.builder().id(String.valueOf(menuItemId++)).value(item.getLocalisedName()).icon(iconStyleClass).build();
		result.setHref(createMenuHref(menuModule, itemModule, item, itemQueryName, itemAbsoluteHref));
		return result;
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
		@Nonnull StringBuilder destination = new StringBuilder(128);
		
		if (itemAbsoluteHref != null) {
			destination.append(itemAbsoluteHref);
		}
		else if (item instanceof ListItem listItem) {
			appendSkyveNavigationPrefix(destination);
			destination.append("a=").append(WebAction.l.toString()).append("&m=").append(menuModule.getName());
			String modelName = listItem.getModelName();
			if (modelName != null) {
				destination.append("&d=").append(listItem.getDocumentName());
				destination.append("&q=").append(listItem.getModelName());
			}
			else {
				destination.append("&q=").append(itemQueryName);
			}
		}
		else if (item instanceof EditItem editItem) {
			appendSkyveNavigationPrefix(destination);
			@Nonnull Module editModule = Objects.requireNonNull(itemModule, "itemModule");
			destination.append("a=").append(WebAction.e.toString()).append("&m=").append(editModule.getName());
			destination.append("&d=").append(editItem.getDocumentName());
		}
		else if (item instanceof CalendarItem calendarItem) {
			appendSkyveNavigationPrefix(destination);
			destination.append("a=").append(WebAction.c.toString()).append("&m=").append(menuModule.getName());
			String modelName = calendarItem.getModelName();
			if (modelName != null) {
				destination.append("&d=").append(calendarItem.getDocumentName());
				destination.append("&q=").append(calendarItem.getModelName());
			}
			else {
				destination.append("&q=").append(itemQueryName);
			}
		}
		else if (item instanceof TreeItem treeItem) {
			appendSkyveNavigationPrefix(destination);
			destination.append("a=").append(WebAction.t.toString()).append("&m=").append(menuModule.getName());
			String modelName = treeItem.getModelName();
			if (modelName != null) {
				destination.append("&d=").append(treeItem.getDocumentName());
				destination.append("&q=").append(treeItem.getModelName());
			}
			else {
				destination.append("&q=").append(itemQueryName);
			}
		}
		else if (item instanceof MapItem mapItem) {
			appendSkyveNavigationPrefix(destination);
			destination.append("a=").append(WebAction.m.toString()).append("&m=").append(menuModule.getName());
			String modelName = mapItem.getModelName();
			if (modelName != null) {
				destination.append("&d=").append(mapItem.getDocumentName());
				destination.append("&q=").append(mapItem.getModelName());
			}
			else {
				destination.append("&q=").append(itemQueryName);
			}
			destination.append("&b=").append(mapItem.getGeometryBinding());
		}

		return "javascript:SKYVE.PF.startHistory('" + OWASP.escapeJsString(destination.toString()) + "')";
	}

	/**
	 * Appends the common Skyve navigation URL prefix for menu items.

	 * @param result
	 */
	private static void appendSkyveNavigationPrefix(@Nonnull StringBuilder result) {
		result.append(Util.getSkyveContextUrl()).append("/?");
	}
}
