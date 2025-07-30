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

public class MenuRenderer {
	protected String uxui;
	protected String selectedModuleName;
	
	protected MenuRenderer(String uxui, String selectedModuleName) {
		this.uxui = uxui;
		this.selectedModuleName = selectedModuleName;
	}
	
	public void renderModuleMenu(@SuppressWarnings("unused") Menu menu,
									@SuppressWarnings("unused") Module menuModule,
									@SuppressWarnings("unused") boolean open) {
		// nothing to do
	}
	
	public void renderMenuRoot(@SuppressWarnings("unused") Menu menu,
								@SuppressWarnings("unused") Module menuModule) {
		// nothing to do
	}
	
	public void renderMenuGroup(@SuppressWarnings("unused") MenuGroup group,
									@SuppressWarnings("unused") Module menuModule) {
		// nothing to do
	}
	
	public void renderTreeItem(@SuppressWarnings("unused") TreeItem item,
								@SuppressWarnings("unused") Module menuModule,
								@SuppressWarnings("unused") Module itemModule,
								@SuppressWarnings("unused") Document itemDocument,
								@SuppressWarnings("unused") String itemQueryName,
								@SuppressWarnings("unused") String icon16,
								@SuppressWarnings("unused") String iconStyleClass) {
		// nothing to do
	}
	
	public void renderListItem(@SuppressWarnings("unused") ListItem item,
								@SuppressWarnings("unused") Module menuModule,
								@SuppressWarnings("unused") Module itemModule,
								@SuppressWarnings("unused") Document itemDocument,
								@SuppressWarnings("unused") String itemQueryName,
								@SuppressWarnings("unused") String icon16,
								@SuppressWarnings("unused") String iconStyleClass) {
		// nothing to do
	}
	
	public void renderCalendarItem(@SuppressWarnings("unused") CalendarItem item,
									@SuppressWarnings("unused") Module menuModule,
									@SuppressWarnings("unused") Module itemModule,
									@SuppressWarnings("unused") Document itemDocument,
									@SuppressWarnings("unused") String itemQueryName,
									@SuppressWarnings("unused") String icon16,
									@SuppressWarnings("unused") String iconStyleClass) {
		// nothing to do
	}
	
	public void renderMapItem(@SuppressWarnings("unused") MapItem item,
								@SuppressWarnings("unused") Module menuModule,
								@SuppressWarnings("unused") Module itemModule,
								@SuppressWarnings("unused") Document itemDocument,
								@SuppressWarnings("unused") String itemQueryName,
								@SuppressWarnings("unused") String icon16,
								@SuppressWarnings("unused") String iconStyleClass) {
		// nothing to do
	}
	
	public void renderEditItem(@SuppressWarnings("unused") EditItem item,
								@SuppressWarnings("unused") Module menuModule,
								@SuppressWarnings("unused") Module itemModule,
								@SuppressWarnings("unused") Document itemDocument,
								@SuppressWarnings("unused") String icon16,
								@SuppressWarnings("unused") String iconStyleClass) {
		// nothing to do
	}
	
	public void renderLinkItem(@SuppressWarnings("unused") LinkItem item,
								@SuppressWarnings("unused") Module menuModule,
								@SuppressWarnings("unused") boolean relative,
								@SuppressWarnings("unused") String absoluteHref) {
		// nothing to do
	}

	public void renderedMenuGroup(@SuppressWarnings("unused") MenuGroup group,
									@SuppressWarnings("unused") Module menuModule) {
		// nothing to do
	}
	
	public void renderedMenuRoot(@SuppressWarnings("unused") Menu menu,
									@SuppressWarnings("unused") Module menuModule) {
		// nothing to do
	}
	
	public void renderedModuleMenu(@SuppressWarnings("unused") Menu menu,
									@SuppressWarnings("unused") Module menuModule,
									@SuppressWarnings("unused") boolean open) {
		// nothing to do
	}

	public void render(Customer customer) {
		render(customer, null);
	}
	
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

	private void renderMenuItems(Customer customer,
									Module menuModule, 
									List<MenuItem> items) {
		for (MenuItem item : items) {
			if ((uxui == null) || item.isApplicable(uxui)) {
				if (item instanceof MenuGroup) {
					MenuGroup group = (MenuGroup) item;
					renderMenuGroup(group, menuModule);
					renderMenuItems(customer, menuModule, group.getItems());
					renderedMenuGroup(group, menuModule);
				}
				else {
					String icon16 = null;
					String iconStyleClass = null;
					Module itemModule = null;
					String itemDocumentName = null;
	                if (item instanceof TreeItem) {
	                    TreeItem treeItem = (TreeItem) item;
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
	                else if (item instanceof ListItem) {
						ListItem listItem = (ListItem) item;
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
					else if (item instanceof CalendarItem) {
	                    CalendarItem calendarItem = (CalendarItem) item;
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
	                else if (item instanceof MapItem) {
	                    MapItem mapItem = (MapItem) item;
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
					else if (item instanceof EditItem) {
						EditItem editItem = (EditItem) item;
						itemDocumentName = editItem.getDocumentName();
						Document itemDocument = menuModule.getDocument(customer, itemDocumentName);
						itemModule = customer.getModule(itemDocument.getOwningModuleName());
						icon16 = itemDocument.getIcon16x16RelativeFileName();
						iconStyleClass = itemDocument.getIconStyleClass();
						renderEditItem(editItem, menuModule, itemModule, itemDocument, icon16, iconStyleClass);
					}
					else if (item instanceof LinkItem) {
						LinkItem linkItem = (LinkItem) item;
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
