package org.skyve.metadata.module.menu;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.LinkItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;

/**
 * Base class for rendering a Skyve module menu tree for a specific UX/UI variant.
 *
 * <p>{@code MenuRenderer} drives a depth-first traversal of the full module navigation
 * hierarchy, dispatching to typed callback methods that subclasses override to produce
 * concrete output (HTML, JSON, navigation models, etc.).
 *
 * <p>The rendering lifecycle for each module menu follows this sequence:
 * <ol>
 *   <li>{@link #renderModuleMenu} — called when a module container opens</li>
 *   <li>{@link #renderMenuRoot} — called before the module's top-level items</li>
 *   <li>Items are traversed recursively:
 *     <ul>
 *       <li>{@link MenuGroup}: {@link #renderMenuGroup} &rarr; recurse children &rarr; {@link #renderedMenuGroup}</li>
 *       <li>List, tree, calendar, map, edit, and link items: their respective typed render methods</li>
 *     </ul>
 *   </li>
 *   <li>{@link #renderedMenuRoot} — called after the module's top-level items</li>
 *   <li>{@link #renderedModuleMenu} — called when a module container closes</li>
 * </ol>
 *
 * <p>All render methods in this base class are no-ops. Subclasses override the methods
 * relevant to the target output format; unrecognised item types are silently skipped.
 *
 * <p>UX/UI filtering is applied automatically: only items and menus for which
 * {@link MenuItem#isApplicable(String)} returns {@code true} are visited.
 *
 * <p>Not thread-safe. Create a new instance per render request.
 *
 * @see Menu
 * @see MenuItem
 * @see MenuGroup
 */
public class MenuRenderer {
	/** The UX/UI variant name to filter by, or {@code null} for no filtering. */
	protected String uxui;
	/** The name of the module whose menu should be rendered in the open/selected state. */
	protected String selectedModuleName;

	/**
	 * Constructs a renderer for the given UX/UI variant and selected module.
	 *
	 * @param uxui                the UX/UI variant to filter by; {@code null} means no filtering
	 * @param selectedModuleName  the name of the module to render as selected/open;
	 *                            {@code null} causes the first module with non-empty items to be opened
	 */
	protected MenuRenderer(String uxui, String selectedModuleName) {
		this.uxui = uxui;
		this.selectedModuleName = selectedModuleName;
	}
	
	/**
	 * Called when a module menu container is opened during traversal.
	 *
	 * <p>Invoked once per module before any items within the module are rendered.
	 * The base implementation is a no-op.
	 *
	 * @param menu        the menu being opened; never {@code null}
	 * @param menuModule  the module that owns this menu; never {@code null}
	 * @param open        {@code true} if this module's menu should be initially expanded
	 */
	public void renderModuleMenu(@SuppressWarnings("unused") Menu menu,
									@SuppressWarnings("unused") Module menuModule,
									@SuppressWarnings("unused") boolean open) {
		// nothing to do
	}
	
	/**
	 * Called immediately before the top-level items of a module menu are rendered.
	 *
	 * <p>Paired with {@link #renderedMenuRoot}; together they bracket all item callbacks
	 * for the module. The base implementation is a no-op.
	 *
	 * @param menu        the menu whose root is being rendered; never {@code null}
	 * @param menuModule  the module that owns this menu; never {@code null}
	 */
	public void renderMenuRoot(@SuppressWarnings("unused") Menu menu,
								@SuppressWarnings("unused") Module menuModule) {
		// nothing to do
	}
	
	/**
	 * Called when a {@link MenuGroup} composite node is opened during traversal.
	 *
	 * <p>After this callback the renderer recurses into the group's children, then calls
	 * {@link #renderedMenuGroup} to close the group. The base implementation is a no-op.
	 *
	 * @param group       the group being opened; never {@code null}
	 * @param menuModule  the module that owns this menu; never {@code null}
	 */
	public void renderMenuGroup(@SuppressWarnings("unused") MenuGroup group,
									@SuppressWarnings("unused") Module menuModule) {
		// nothing to do
	}
	
	/**
	 * Called to render a tree-view menu item.
	 *
	 * <p>A tree item navigates to a hierarchical tree representation of a document.
	 * The renderer receives the fully resolved module, document, and query context
	 * so it can generate the appropriate navigation target. The base implementation
	 * is a no-op.
	 *
	 * @param item            the tree menu item; never {@code null}
	 * @param menuModule      the module that owns the containing menu; never {@code null}
	 * @param itemModule      the module that owns the target document; never {@code null}
	 * @param itemDocument    the target document; never {@code null}
	 * @param itemQueryName   the resolved query name driving the tree, or {@code null}
	 * @param icon16          the document's 16-pixel icon path, or {@code null}
	 * @param iconStyleClass  the document's icon CSS class, or {@code null}
	 */
	public void renderTreeItem(@SuppressWarnings("unused") TreeItem item,
								@SuppressWarnings("unused") Module menuModule,
								@SuppressWarnings("unused") Module itemModule,
								@SuppressWarnings("unused") Document itemDocument,
								@SuppressWarnings("unused") String itemQueryName,
								@SuppressWarnings("unused") String icon16,
								@SuppressWarnings("unused") String iconStyleClass) {
		// nothing to do
	}
	
	/**
	 * Called to render a list-view menu item.
	 *
	 * <p>A list item navigates to a tabular list of document instances. The renderer
	 * receives the fully resolved module, document, and query context. The base
	 * implementation is a no-op.
	 *
	 * @param item            the list menu item; never {@code null}
	 * @param menuModule      the module that owns the containing menu; never {@code null}
	 * @param itemModule      the module that owns the target document; never {@code null}
	 * @param itemDocument    the target document; never {@code null}
	 * @param itemQueryName   the resolved query name driving the list, or {@code null}
	 * @param icon16          the document's 16-pixel icon path, or {@code null}
	 * @param iconStyleClass  the document's icon CSS class, or {@code null}
	 */
	public void renderListItem(@SuppressWarnings("unused") ListItem item,
								@SuppressWarnings("unused") Module menuModule,
								@SuppressWarnings("unused") Module itemModule,
								@SuppressWarnings("unused") Document itemDocument,
								@SuppressWarnings("unused") String itemQueryName,
								@SuppressWarnings("unused") String icon16,
								@SuppressWarnings("unused") String iconStyleClass) {
		// nothing to do
	}
	
	/**
	 * Called to render a calendar-view menu item.
	 *
	 * <p>A calendar item navigates to a temporal calendar display of document instances.
	 * The renderer receives the fully resolved module, document, and query context.
	 * The base implementation is a no-op.
	 *
	 * @param item            the calendar menu item; never {@code null}
	 * @param menuModule      the module that owns the containing menu; never {@code null}
	 * @param itemModule      the module that owns the target document; never {@code null}
	 * @param itemDocument    the target document; never {@code null}
	 * @param itemQueryName   the resolved query name driving the calendar, or {@code null}
	 * @param icon16          the document's 16-pixel icon path, or {@code null}
	 * @param iconStyleClass  the document's icon CSS class, or {@code null}
	 */
	public void renderCalendarItem(@SuppressWarnings("unused") CalendarItem item,
									@SuppressWarnings("unused") Module menuModule,
									@SuppressWarnings("unused") Module itemModule,
									@SuppressWarnings("unused") Document itemDocument,
									@SuppressWarnings("unused") String itemQueryName,
									@SuppressWarnings("unused") String icon16,
									@SuppressWarnings("unused") String iconStyleClass) {
		// nothing to do
	}
	
	/**
	 * Called to render a map-view menu item.
	 *
	 * <p>A map item navigates to a geospatial map display of document instances.
	 * The renderer receives the fully resolved module, document, and query context.
	 * The base implementation is a no-op.
	 *
	 * @param item            the map menu item; never {@code null}
	 * @param menuModule      the module that owns the containing menu; never {@code null}
	 * @param itemModule      the module that owns the target document; never {@code null}
	 * @param itemDocument    the target document; never {@code null}
	 * @param itemQueryName   the resolved query name driving the map, or {@code null}
	 * @param icon16          the document's 16-pixel icon path, or {@code null}
	 * @param iconStyleClass  the document's icon CSS class, or {@code null}
	 */
	public void renderMapItem(@SuppressWarnings("unused") MapItem item,
								@SuppressWarnings("unused") Module menuModule,
								@SuppressWarnings("unused") Module itemModule,
								@SuppressWarnings("unused") Document itemDocument,
								@SuppressWarnings("unused") String itemQueryName,
								@SuppressWarnings("unused") String icon16,
								@SuppressWarnings("unused") String iconStyleClass) {
		// nothing to do
	}
	
	/**
	 * Called to render an edit-view menu item.
	 *
	 * <p>An edit item navigates directly to the edit view of a specific document (often
	 * a singleton or well-known instance). No query is involved. The base implementation
	 * is a no-op.
	 *
	 * @param item            the edit menu item; never {@code null}
	 * @param menuModule      the module that owns the containing menu; never {@code null}
	 * @param itemModule      the module that owns the target document; never {@code null}
	 * @param itemDocument    the target document; never {@code null}
	 * @param icon16          the document's 16-pixel icon path, or {@code null}
	 * @param iconStyleClass  the document's icon CSS class, or {@code null}
	 */
	public void renderEditItem(@SuppressWarnings("unused") EditItem item,
								@SuppressWarnings("unused") Module menuModule,
								@SuppressWarnings("unused") Module itemModule,
								@SuppressWarnings("unused") Document itemDocument,
								@SuppressWarnings("unused") String icon16,
								@SuppressWarnings("unused") String iconStyleClass) {
		// nothing to do
	}
	
	/**
	 * Called to render an external or context-relative link menu item.
	 *
	 * <p>The {@code relative} flag indicates whether {@code absoluteHref} was originally
	 * a context-relative URL (which has been resolved to an absolute context path) or
	 * a fully external URI. The base implementation is a no-op.
	 *
	 * @param item          the link menu item; never {@code null}
	 * @param menuModule    the module that owns the containing menu; never {@code null}
	 * @param relative      {@code true} if the link is context-relative, {@code false} if absolute
	 * @param absoluteHref  the fully resolved href to use in navigation; never {@code null}
	 */
	public void renderLinkItem(@SuppressWarnings("unused") LinkItem item,
								@SuppressWarnings("unused") Module menuModule,
								@SuppressWarnings("unused") boolean relative,
								@SuppressWarnings("unused") String absoluteHref) {
		// nothing to do
	}

	/**
	 * Called after all children of a {@link MenuGroup} have been rendered.
	 *
	 * <p>Post-hook counterpart to {@link #renderMenuGroup}. Subclasses override this
	 * to emit closing markup or finalise group state. The base implementation is a no-op.
	 *
	 * @param group       the group that was just rendered; never {@code null}
	 * @param menuModule  the module that owns this menu; never {@code null}
	 */
	public void renderedMenuGroup(@SuppressWarnings("unused") MenuGroup group,
									@SuppressWarnings("unused") Module menuModule) {
		// nothing to do
	}
	
	/**
	 * Called after all top-level items of a module menu have been rendered.
	 *
	 * <p>Post-hook counterpart to {@link #renderMenuRoot}. The base implementation
	 * is a no-op.
	 *
	 * @param menu        the menu whose root was just rendered; never {@code null}
	 * @param menuModule  the module that owns this menu; never {@code null}
	 */
	public void renderedMenuRoot(@SuppressWarnings("unused") Menu menu,
									@SuppressWarnings("unused") Module menuModule) {
		// nothing to do
	}
	
	/**
	 * Called after a module menu container has been fully rendered.
	 *
	 * <p>Post-hook counterpart to {@link #renderModuleMenu}. The base implementation
	 * is a no-op.
	 *
	 * @param menu        the menu that was just rendered; never {@code null}
	 * @param menuModule  the module that owns this menu; never {@code null}
	 * @param open        {@code true} if this module's menu was rendered in the open/expanded state
	 */
	public void renderedModuleMenu(@SuppressWarnings("unused") Menu menu,
									@SuppressWarnings("unused") Module menuModule,
									@SuppressWarnings("unused") boolean open) {
		// nothing to do
	}

	/**
	 * Renders all module menus for a customer, bypassing user role filtering.
	 *
	 * <p>Every module and every menu item that is applicable for the configured UX/UI
	 * variant is rendered, regardless of role restrictions. Use this overload to render
	 * full navigation for administrative or tooling purposes.
	 *
	 * @param customer  the tenant to render menus for; never {@code null}
	 */
	public void render(Customer customer) {
		render(customer, null);
	}
	
	/**
	 * Renders all module menus accessible to the given user.
	 *
	 * <p>Menu items are filtered by the user's assigned roles in addition to the
	 * configured UX/UI variant. Only items whose role set intersects the user's roles
	 * (or that are unrestricted) are visited.
	 *
	 * @param user  the authenticated user; never {@code null}
	 */
	public void render(User user) {
		render(user.getCustomer(), user);
	}
	
	private void render(Customer customer, User user) {
		// determine if the first menu should be open - ie no default
		Menu chosenMenu = (selectedModuleName == null) ? 
							null : 
							((user == null) ? 
								customer.getModule(selectedModuleName).getMenu() :
								((UserImpl) user).getModuleMenu(selectedModuleName));
		final boolean setFirstModuleOpen = (chosenMenu == null) || chosenMenu.getItems().isEmpty();
		final AtomicBoolean first = new AtomicBoolean(true);

		// render each module menu
		for (Module menuModule : customer.getModules()) {
			String menuModuleName = menuModule.getName();

			Menu menu = (user == null) ?
							customer.getModule(menuModuleName).getMenu() :
							((UserImpl) user).getModuleMenu(menuModuleName);
			if ((uxui == null) || menu.isApplicable(uxui)) {
				boolean open = false;
				if (setFirstModuleOpen) {
					open = first.get();
					first.set(false);
				} 
				else {
					open = menuModuleName.equals(selectedModuleName);
				}

				renderModuleMenu(menu, menuModule, open);
				renderMenuRoot(menu, menuModule);
				renderMenuItems(customer, menuModule, menu.getItems());
				renderedMenuRoot(menu, menuModule);
				renderedModuleMenu(menu, menuModule, open);
			}
		}
	}

	@SuppressWarnings("java:S3776") // Complexity OK
	private void renderMenuItems(Customer customer,
									Module menuModule, 
									List<MenuItem> items) {
		for (MenuItem item : items) {
			if ((uxui == null) || item.isApplicable(uxui)) {
				if (item instanceof MenuGroup group) {
					renderMenuGroup(group, menuModule);
					renderMenuItems(customer, menuModule, group.getItems());
					renderedMenuGroup(group, menuModule);
				}
				else {
					String icon16 = null;
					String iconStyleClass = null;
					Module itemModule = null;
					String itemDocumentName = null;
	                if (item instanceof TreeItem treeItem) {
	                    itemDocumentName = treeItem.getDocumentName();
						String itemQueryName = treeItem.getQueryName();
						String modelName = treeItem.getModelName();
						if (modelName != null) {
							itemModule = customer.getModule(menuModule.getDocument(customer, itemDocumentName).getOwningModuleName());
						}
						else {
		                    MetaDataQueryDefinition query = deriveDocumentQuery(customer,
													                                menuModule,
													                                item,
													                                itemQueryName,
													                                itemDocumentName);
							itemDocumentName = query.getDocumentName();
							itemQueryName = query.getName();
							itemModule = query.getDocumentModule(customer);
						}
						Document itemDocument = itemModule.getDocument(customer, itemDocumentName);
						icon16 = itemDocument.getIcon16x16RelativeFileName();
						iconStyleClass = itemDocument.getIconStyleClass();
	                    renderTreeItem(treeItem, menuModule, itemModule, itemDocument, itemQueryName, icon16, iconStyleClass);
	                }
	                else if (item instanceof ListItem listItem) {
						itemDocumentName = listItem.getDocumentName();
						String itemQueryName = listItem.getQueryName();
						String modelName = listItem.getModelName();
						if (modelName != null) {
							itemModule = customer.getModule(menuModule.getDocument(customer, itemDocumentName).getOwningModuleName());
						}
						else {
							MetaDataQueryDefinition query = deriveDocumentQuery(customer,
													                                menuModule,
													                                item,
													                                itemQueryName,
													                                itemDocumentName);
							itemDocumentName = query.getDocumentName();
							itemQueryName = query.getName();
							itemModule = query.getDocumentModule(customer);
						}
						Document itemDocument = itemModule.getDocument(customer, itemDocumentName);
						icon16 = itemDocument.getIcon16x16RelativeFileName();
						iconStyleClass = itemDocument.getIconStyleClass();
						renderListItem(listItem, menuModule, itemModule, itemDocument, itemQueryName, icon16, iconStyleClass);
					}
					else if (item instanceof CalendarItem calendarItem) {
	                    itemDocumentName = calendarItem.getDocumentName();
						MetaDataQueryDefinition query = deriveDocumentQuery(customer,
												                                menuModule,
												                                item,
												                                calendarItem.getQueryName(),
												                                itemDocumentName);
						itemDocumentName = query.getDocumentName();
						String itemQueryName = query.getName();
						itemModule = query.getDocumentModule(customer);
						Document itemDocument = itemModule.getDocument(customer, itemDocumentName);
						icon16 = itemDocument.getIcon16x16RelativeFileName();
						iconStyleClass = itemDocument.getIconStyleClass();
						renderCalendarItem(calendarItem, menuModule, itemModule, itemDocument, itemQueryName, icon16, iconStyleClass);
	                }
	                else if (item instanceof MapItem mapItem) {
	                    itemDocumentName = mapItem.getDocumentName();
						String itemQueryName = mapItem.getQueryName();
						String modelName = mapItem.getModelName();
						if (modelName != null) {
							itemModule = customer.getModule(menuModule.getDocument(customer, itemDocumentName).getOwningModuleName());
						}
						else {
							MetaDataQueryDefinition query = deriveDocumentQuery(customer,
													                                menuModule,
													                                item,
													                                itemQueryName,
													                                itemDocumentName);
							itemDocumentName = query.getDocumentName();
							itemQueryName = query.getName();
							itemModule = query.getDocumentModule(customer);
						}
						Document itemDocument = itemModule.getDocument(customer, itemDocumentName);
						icon16 = itemDocument.getIcon16x16RelativeFileName();
						iconStyleClass = itemDocument.getIconStyleClass();
						renderMapItem(mapItem, menuModule, itemModule, itemDocument, itemQueryName, icon16, iconStyleClass);
	                }
					else if (item instanceof EditItem editItem) {
						itemDocumentName = editItem.getDocumentName();
						Document itemDocument = menuModule.getDocument(customer, itemDocumentName);
						itemModule = customer.getModule(itemDocument.getOwningModuleName());
						icon16 = itemDocument.getIcon16x16RelativeFileName();
						iconStyleClass = itemDocument.getIconStyleClass();
						renderEditItem(editItem, menuModule, itemModule, itemDocument, icon16, iconStyleClass);
					}
					else if (item instanceof LinkItem linkItem) {
						String href = linkItem.getHref();
						boolean relative = true;
						try {
							if (new URI(href).isAbsolute()) {
								relative = false;
							}
						}
						catch (@SuppressWarnings("unused") URISyntaxException e) {
							// do nothing here if its not know to be absolute
						}

						if (relative) {
				    		if (href.charAt(0) != '/') {
				    			href = String.format("%s/%s", Util.getSkyveContextUrl(), href);
				    		}
				    		else {
				    			href = String.format("%s%s", Util.getSkyveContextUrl(), href);
				    		}
						}

						renderLinkItem(linkItem, menuModule, relative, href);
					}
				}
			}
		}
	}
	
	/**
	 * Resolves the {@link MetaDataQueryDefinition} for a menu item given an optional
	 * explicit query name and a document name.
	 *
	 * <p>If {@code queryName} is non-{@code null}, that named query is looked up in the
	 * module and returned. If {@code queryName} is {@code null}, the document's default
	 * query is returned via {@link Module#getDocumentDefaultQuery}.
	 *
	 * @param customer      the customer context for document resolution; never {@code null}
	 * @param module        the module to look up queries in; never {@code null}
	 * @param item          the menu item requesting the query (used in error messages); never {@code null}
	 * @param queryName     the explicit query name, or {@code null} to use the document default
	 * @param documentName  the document name to fall back to if no explicit query is given; never {@code null}
	 * @return the resolved query definition; never {@code null}
	 * @throws MetaDataException if the named query does not exist or the document has no default query
	 */
	public static MetaDataQueryDefinition deriveDocumentQuery(Customer customer,
																Module module,
																MenuItem item,
																String queryName,
																String documentName) {
        MetaDataQueryDefinition query = null;
		if (queryName != null) {
            query = module.getMetaDataQuery(queryName);
            if ((query == null) || (query.getName() == null)) {
            	throw new MetaDataException("The target query " + queryName + " for menu action " +
                								item.getName() + " is invalid in module " + module.getName());
            }
        }
        else {
        	try {
        		query = module.getDocumentDefaultQuery(customer, documentName);
        	}
        	catch (MetaDataException e) {
        		throw new MetaDataException("The target document " + documentName + " for menu action " +
                								item.getName() + " has no default query in module " + module.getName(), e);
            }
        }

        return query;
	}
}
