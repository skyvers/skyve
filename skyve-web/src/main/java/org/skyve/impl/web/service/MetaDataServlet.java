package org.skyve.impl.web.service;

import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.security.Principal;
import java.util.Set;
import java.util.TreeSet;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.metadata.module.menu.AbstractDocumentOrQueryOrModelMenuItem;
import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.LinkItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.WebUtil;
import org.skyve.impl.web.service.smartclient.SmartClientQueryColumnDefinition;
import org.skyve.impl.web.service.smartclient.SmartClientViewRenderer;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.menu.Menu;
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuRenderer;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.util.OWASP;
import org.skyve.util.Util;

public class MetaDataServlet extends HttpServlet {
	private static final long serialVersionUID = -2160904569807647301L;

	@Override
	protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		doGet(req, resp);
	}

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
    	response.setContentType(MimeType.json.toString());
        response.setCharacterEncoding(Util.UTF8);
		response.addHeader("Cache-control", "private,no-cache,no-store"); // never
		response.addDateHeader("Expires", 0); // never
		
		try (PrintWriter pw = response.getWriter()) {
			AbstractPersistence persistence = AbstractPersistence.get();
			try {
				try {
					persistence.begin();
			    	Principal userPrincipal = request.getUserPrincipal();
			    	User user = WebUtil.processUserPrincipalForRequest(request, (userPrincipal == null) ? null : userPrincipal.getName(), true);
					if (user == null) {
						throw new SessionEndedException(request.getLocale());
					}
					persistence.setUser(user);

					UxUi uxui = UserAgent.getUxUi(request);
					String chosenModuleName = Util.processStringValue(request.getParameter(AbstractWebContext.MODULE_NAME));
			
					StringBuilder menus = new StringBuilder(2048);
					StringBuilder dataSources = new StringBuilder(2048);
					processModules(uxui.getName(), user,  chosenModuleName, menus, dataSources);
					pw.append("{\"menus\":").append(menus).append(",\n");
					pw.append("\"dataSources\":").append(dataSources).append('}');
				}
				catch (InvocationTargetException e) {
					throw e.getTargetException();
				}
			}
			catch (Throwable t) {
				t.printStackTrace();
				persistence.rollback();
				pw.print(emptyResponse());
			}
			finally {
				if (persistence != null) {
					persistence.commit(true);
				}
			}
		}
	}
	
	private static void processModules(final String uxui, 
										final User user,
										final String chosenModuleName,
										final StringBuilder menuJson,
										final StringBuilder dataSourceJson) {
		final Customer customer = user.getCustomer();
		
		menuJson.append("[");
		dataSourceJson.append("[");
		
		new MenuRenderer(uxui, chosenModuleName) {
			@Override
			public void renderModuleMenu(Menu menu, Module menuModule, boolean open) {
				menuJson.append("{\"module\":\"").append(OWASP.escapeJsonString(menuModule.getName()));
				menuJson.append("\",\"title\":\"").append(OWASP.escapeJsonString(menuModule.getLocalisedTitle()));
				menuJson.append("\",\"open\":").append(open).append(",\"menu\":[");
			}

			@Override
			public void renderMenuGroup(MenuGroup group, Module menuModule) {
				menuJson.append("{\"group\":\"").append(OWASP.escapeJsonString(group.getLocalisedName())).append("\",\"items\":[");
			}
			
			@Override
			public void renderCalendarItem(CalendarItem item,
											Module menuModule,
											Module itemModule,
											Document itemDocument,
											String itemQueryName,
											String icon16,
											String iconStyleClass) {
				menuJson.append("{\"calendar\":\"").append(OWASP.escapeJsonString(item.getLocalisedName()));
				if (iconStyleClass != null) {
					menuJson.append("\",\"fontIcon\":\"").append(iconStyleClass);
				}
				else if (icon16 != null) {
					menuJson.append("\",\"fontIcon\":\"").append(iconStyleClass);
				}
				String modelName = item.getModelName();
				if (modelName != null) {
					menuJson.append("\",\"document\":\"").append(itemDocument.getName());
					menuJson.append("\",\"model\":\"").append(modelName);
				}
				else {
					menuJson.append("\",\"query\":\"").append(itemQueryName);					
				}
				menuJson.append("\",\"startBinding\":\"").append(item.getStartBinding());
				menuJson.append("\",\"endBinding\":\"").append(item.getEndBinding());
				menuJson.append("\"},");
				
				addDataSource(menuModule, itemDocument, item);
			}
			
			@Override
			public void renderEditItem(EditItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String icon16,
										String iconStyleClass) {
				menuJson.append("{\"edit\":\"").append(OWASP.escapeJsonString(item.getLocalisedName()));
				if (iconStyleClass != null) {
					menuJson.append("\",\"fontIcon\":\"").append(iconStyleClass);
				}
				else if (icon16 != null) {
					menuJson.append("\",\"fontIcon\":\"").append(iconStyleClass);
				}
				menuJson.append("\",\"document\":\"").append(itemDocument.getName()).append("\"},");
			}

			@Override
			public void renderLinkItem(LinkItem item, Module menuModule, boolean relative, String absoluteHref) {
				menuJson.append("{\"link\":\"").append(OWASP.escapeJsonString(item.getLocalisedName()));
				menuJson.append("\",\"href\":\"").append(absoluteHref).append("\"},");
			}
			
			@Override
			public void renderListItem(ListItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String itemQueryName,
										String icon16,
										String iconStyleClass) {
				menuJson.append("{\"list\":\"").append(OWASP.escapeJsonString(item.getLocalisedName()));
				if (iconStyleClass != null) {
					menuJson.append("\",\"fontIcon\":\"").append(iconStyleClass);
				}
				else if (icon16 != null) {
					menuJson.append("\",\"fontIcon\":\"").append(iconStyleClass);
				}
				String modelName = item.getModelName();
				if (modelName != null) {
					menuJson.append("\",\"document\":\"").append(itemDocument.getName());
					menuJson.append("\",\"model\":\"").append(modelName);
				}
				else {
					menuJson.append("\",\"query\":\"").append(itemQueryName);					
				}
				menuJson.append("\"},");
				
				addDataSource(menuModule, itemDocument, item);
			}
			
			@Override
			public void renderMapItem(MapItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String itemQueryName,
										String icon16,
										String iconStyleClass) {
				menuJson.append("{\"map\":\"").append(OWASP.escapeJsonString(item.getLocalisedName()));
				if (iconStyleClass != null) {
					menuJson.append("\",\"fontIcon\":\"").append(iconStyleClass);
				}
				else if (icon16 != null) {
					menuJson.append("\",\"fontIcon\":\"").append(iconStyleClass);
				}
				String modelName = item.getModelName();
				if (modelName != null) {
					menuJson.append("\",\"document\":\"").append(itemDocument.getName());
					menuJson.append("\",\"model\":\"").append(modelName);
				}
				else {
					menuJson.append("\",\"query\":\"").append(itemQueryName);					
				}
				menuJson.append("\",\"geometryBinding\":\"").append(item.getGeometryBinding());
				menuJson.append("\"},");
				
				addDataSource(menuModule, itemDocument, item);
			}
			
			@Override
			public void renderTreeItem(TreeItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String itemQueryName,
										String icon16,
										String iconStyleClass) {
				menuJson.append("{\"tree\":\"").append(OWASP.escapeJsonString(item.getLocalisedName()));
				if (iconStyleClass != null) {
					menuJson.append("\",\"fontIcon\":\"").append(iconStyleClass);
				}
				else if (icon16 != null) {
					menuJson.append("\",\"fontIcon\":\"").append(iconStyleClass);
				}
				String modelName = item.getModelName();
				if (modelName != null) {
					menuJson.append("\",\"document\":\"").append(itemDocument.getName());
					menuJson.append("\",\"model\":\"").append(modelName);
				}
				else {
					menuJson.append("\",\"query\":\"").append(itemQueryName);					
				}
				menuJson.append("\"},");
				
				addDataSource(menuModule, itemDocument, item);
			}
			
			@Override
			public void renderedMenuGroup(MenuGroup group, Module menuModule) {
				menuJson.setLength(menuJson.length() - 1); // remove trailing comma
				menuJson.append("]},");
			}
			
			@Override
			public void renderedModuleMenu(Menu menu, Module menuModule, boolean open) {
				menuJson.setLength(menuJson.length() - 1); // remove trailing comma
				menuJson.append("]},");
			}

			private Set<String> visitedDataSourceNames = new TreeSet<>();
			
			private void addDataSource(Module menuModule,
										Document itemDocument,
										AbstractDocumentOrQueryOrModelMenuItem item) {
				String queryName = item.getQueryName();
				String modelName = item.getModelName();
				String documentName = item.getDocumentName();
				
				if (queryName != null) { // its a query
					MetaDataQueryDefinition query = menuModule.getMetaDataQuery(queryName);
					addQueryDataSource(query);
				}
				else {
					if (modelName != null) { // its a model
						SmartClientViewRenderer.appendDataSourceDefinition(user, 
																			customer, 
																			menuModule, 
																			itemDocument,
																			modelName,
																			true,
																			dataSourceJson, 
																			visitedDataSourceNames);
					}
					else {
						MetaDataQueryDefinition query = menuModule.getDocumentDefaultQuery(customer, documentName);
						addQueryDataSource(query);
					}
				}
			}
			
			private void addQueryDataSource(MetaDataQueryDefinition query) {
				String documentName = query.getDocumentName();
				Module documentModule = query.getDocumentModule(customer);
				Module owningModule = query.getOwningModule();
				Document drivingDocument = documentModule.getDocument(customer, documentName);
				String drivingDocumentName = drivingDocument.getName();
				String drivingDocumentModuleName = drivingDocument.getOwningModuleName();
				Module drivingDocumentModule = customer.getModule(drivingDocumentModuleName);
				
				String dataSourceName = new StringBuilder(32).append(owningModule.getName()).append('_').append(query.getName()).toString();
				if (visitedDataSourceNames.contains(dataSourceName)) {
					return;
				}

				dataSourceJson.append("{\"name\":\"").append(dataSourceName);
				dataSourceJson.append("\",\"module\":\"").append(drivingDocumentModuleName);
				dataSourceJson.append("\",\"document\":\"").append(drivingDocumentName);

				String icon = drivingDocument.getIconStyleClass();
				if (icon != null) {
					dataSourceJson.append("\",\"fontIcon\":\"").append(icon);
				}
				else {
					String icon32 = drivingDocument.getIcon32x32RelativeFileName();
					if (icon32 != null) {
						dataSourceJson.append("\",\"icon\":\"").append(icon32);
					}
				}
				dataSourceJson.append("\",\"aggregate\":").append(query.isAggregate());
				dataSourceJson.append(",\"canCreate\":").append(user.canCreateDocument(drivingDocument));
				dataSourceJson.append(",\"canUpdate\":").append(user.canUpdateDocument(drivingDocument));
				dataSourceJson.append(",\"canDelete\":").append(user.canDeleteDocument(drivingDocument));
				dataSourceJson.append(",\"title\":\"");
				dataSourceJson.append(OWASP.escapeJsonString(query.getLocalisedDescription()));
				dataSourceJson.append("\",\"fields\":[");

				if (drivingDocumentName.equals(drivingDocument.getParentDocumentName())) { // hierarchical
					dataSourceJson.append("{\"name\":\"bizParentId\",\"title\":\"Parent ID\",\"type\":\"text\",\"hidden\":true},");
				}
				
				int cellHeight = 0; // fixed cell height of list grid (defined in data source)
				
				for (MetaDataQueryColumn column : query.getColumns()) {
					if ((column instanceof MetaDataQueryProjectedColumn) && 
							(! ((MetaDataQueryProjectedColumn) column).isProjected())) {
						continue;
					}

					SmartClientQueryColumnDefinition def = SmartClientViewRenderer.getQueryColumn(user, customer, drivingDocumentModule, drivingDocument, column, true);
					dataSourceJson.append("{\"name\":\"").append(def.getName());
					dataSourceJson.append("\",\"title\":\"").append(OWASP.escapeJsonString(def.getTitle()));
					dataSourceJson.append("\",\"type\":\"").append(def.getType());
					HorizontalAlignment align = def.getAlign();
					if (align != null) {
						dataSourceJson.append("\",\"align\":\"").append(align);
					}
					Integer length = def.getLength();
					if (length != null) {
						dataSourceJson.append("\",\"length\":").append(length);
					}
					else {
						dataSourceJson.append('"');
					}
// TODO These cannot be single quotes in JSON.
//					String valueMap = def.getValueMap();
//					if (valueMap != null) {
//						dataSourceJson.append(",\"values\":").append(valueMap);
//					}
					dataSourceJson.append("},");

					// define the minimum fixed cell height for the grid (dataSource) based on any content image columns
					Integer pixelHeight = def.getPixelHeight();
					if (pixelHeight != null) {
						int h = pixelHeight.intValue();
						if (h > cellHeight) {
							cellHeight = h;
						}
					}
/*
					SmartClientLookupDefinition lookup = def.getLookup();
					if (lookup != null) {
						// Add lookup description data source field
						toAppendTo.append("{name:'");
						boolean bindingToDataGrid = lookup.isBindingToDataGrid();
			        	if (! bindingToDataGrid) {
			        		toAppendTo.append(def.getName()).append('_');
			        	}
			        	toAppendTo.append(lookup.getDisplayField()).append("',type:'text',hidden:'true'},");

						StringBuilder childDataSourceDefinition = new StringBuilder(512);
						String childDataSourceId = appendDataSourceDefinition(user,
																				customer,
																				lookup.getQuery(),
																				lookup.getOptionDataSource(),
																				null,
																				config,
																				childDataSourceDefinition,
																				visitedQueryNames);
						childDataSources.put(childDataSourceId, childDataSourceDefinition.toString());
					}
*/
				}

				dataSourceJson.setLength(dataSourceJson.length() - 1); // remove the last field comma
				dataSourceJson.append("]");
				
				// Add cellHeight if applicable
				if (cellHeight > 0) {
					dataSourceJson.append(",\"rowHeight\":").append(cellHeight);
				}
				
				dataSourceJson.append("},\n");
				visitedDataSourceNames.add(dataSourceName);
			}

			private void addModelDataSource(Document itemDocument, String modelName) {
				ListModel<Bean> model = itemDocument.getListModel(customer, modelName, true);
				// Note we cannot set the bean on the model here as we are only generating out the UI.
				Document drivingDocument = model.getDrivingDocument();
				if (drivingDocument == null) {
					throw new MetaDataException("List Model" + model + " has no driving document defined and JSON clients can not support dynamic/late list grid generation");
				}
				String drivingDocumentModuleName = drivingDocument.getOwningModuleName();
				Module drivingDocumentModule = customer.getModule(drivingDocumentModuleName);

				String dataSourceName = new StringBuilder(32).append(drivingDocumentModuleName).append('_').append(itemDocument.getName()).append("__").append(modelName).toString();
			}
/*			
			private void addDataSource() {
				String documentName = query.getDocumentName();
				Module documentModule = query.getDocumentModule(customer);
				Module owningModule = query.getOwningModule();
				Document drivingDocument = documentModule.getDocument(customer, documentName);
				String drivingDocumentName = drivingDocument.getName();
				String drivingDocumentModuleName = drivingDocument.getOwningModuleName();
				Module drivingDocumentModule = customer.getModule(drivingDocumentModuleName);
				
				String dataSourceName = new StringBuilder(32).append(owningModule.getName()).append('_').append(query.getName()).toString();
				if (visitedDataSourceNames.contains(dataSourceName)) {
					return;
				}
				visitedDataSourceNames.add(dataSourceName);

				dataSourceJson.append("{\"name\":\"").append(dataSourceName);
				dataSourceJson.append("\",\"module\":\"").append(drivingDocumentModuleName);
				dataSourceJson.append("\",\"document\":\"").append(drivingDocumentName);

				String icon = drivingDocument.getIconStyleClass();
				if (icon != null) {
					dataSourceJson.append("\",\"fontIcon\":\"").append(icon);
				}
				else {
					String icon32 = drivingDocument.getIcon32x32RelativeFileName();
					if (icon32 != null) {
						dataSourceJson.append("\",\"icon\":\"").append(icon32);
					}
				}
				dataSourceJson.append("\",\"aggregate\":").append(query.isAggregate());
				dataSourceJson.append(",\"canCreate\":").append(user.canCreateDocument(drivingDocument));
				dataSourceJson.append(",\"canUpdate\":").append(user.canUpdateDocument(drivingDocument));
				dataSourceJson.append(",\"canDelete\":").append(user.canDeleteDocument(drivingDocument));
				dataSourceJson.append(",\"title\":\"");
				dataSourceJson.append(OWASP.escapeJsonString(query.getLocalisedDescription()));
				dataSourceJson.append("\",\"fields\":[");

				if (drivingDocumentName.equals(drivingDocument.getParentDocumentName())) { // hierarchical
					dataSourceJson.append("{\"name\":\"bizParentId\",\"title\":\"Parent ID\",\"type\":\"text\",\"hidden\":true");
				}
				
				int cellHeight = 0; // fixed cell height of list grid (defined in data source)
				
				for (MetaDataQueryColumn column : query.getColumns()) {
					if ((column instanceof MetaDataQueryProjectedColumn) && 
							(! ((MetaDataQueryProjectedColumn) column).isProjected())) {
						continue;
					}

					SmartClientQueryColumnDefinition def = SmartClientViewRenderer.getQueryColumn(user, customer, drivingDocumentModule, drivingDocument, column, true);
					dataSourceJson.append("{\"name\":\"").append(def.getName());
					dataSourceJson.append("\",\"title\":\"").append(def.getTitle());
					dataSourceJson.append("\",\"type\":\"").append(def.getType());
					HorizontalAlignment align = def.getAlign();
					if (align != null) {
						dataSourceJson.append("\",\"align\":\"").append(align);
					}
					Integer length = def.getLength();
					if (length != null) {
						dataSourceJson.append("\",\"length\":").append(length);
					}
					else {
						dataSourceJson.append('"');
					}
					String valueMap = def.getValueMap();
					if (valueMap != null) {
						dataSourceJson.append(",\"values\":").append(valueMap);
					}
					dataSourceJson.append("},");

					// define the minimum fixed cell height for the grid (dataSource) based on any content image columns
					Integer pixelHeight = def.getPixelHeight();
					if (pixelHeight != null) {
						int h = pixelHeight.intValue();
						if (h > cellHeight) {
							cellHeight = h;
						}
					}
*/
/*
					SmartClientLookupDefinition lookup = def.getLookup();
					if (lookup != null) {
						// Add lookup description data source field
						toAppendTo.append("{name:'");
						boolean bindingToDataGrid = lookup.isBindingToDataGrid();
			        	if (! bindingToDataGrid) {
			        		toAppendTo.append(def.getName()).append('_');
			        	}
			        	toAppendTo.append(lookup.getDisplayField()).append("',type:'text',hidden:'true'},");

						StringBuilder childDataSourceDefinition = new StringBuilder(512);
						String childDataSourceId = appendDataSourceDefinition(user,
																				customer,
																				lookup.getQuery(),
																				lookup.getOptionDataSource(),
																				null,
																				config,
																				childDataSourceDefinition,
																				visitedQueryNames);
						childDataSources.put(childDataSourceId, childDataSourceDefinition.toString());
					}
*/
/*
				}

				dataSourceJson.setLength(dataSourceJson.length() - 1); // remove the last field comma
				dataSourceJson.append("]");
				
				// Add cellHeight if applicable
				if (cellHeight > 0) {
					dataSourceJson.append(",rowHeight:").append(cellHeight);
				}
				
				dataSourceJson.append("},\n");
			}
*/
		}.render(user);

        menuJson.setLength(menuJson.length() - 1);
        menuJson.append("]");
        dataSourceJson.setLength(dataSourceJson.length() - 2); // ,\n
        dataSourceJson.append("]");
	}
	
	private static String emptyResponse() {
		return "{\"menus\":[],\"dataSources\":[]}";
	}
}
