package org.skyve.impl.web.service;

import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.security.Principal;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.generate.ViewRenderer;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.module.menu.AbstractDocumentOrQueryOrModelMenuItem;
import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.LinkItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.WidgetReference;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.event.Addable;
import org.skyve.impl.metadata.view.event.Changeable;
import org.skyve.impl.metadata.view.event.Editable;
import org.skyve.impl.metadata.view.event.EventSource;
import org.skyve.impl.metadata.view.event.Focusable;
import org.skyve.impl.metadata.view.event.Removable;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.Selectable;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.Comparison;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
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
import org.skyve.metadata.module.query.MetaDataQueryContentColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewParameter;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.util.OWASP;
import org.skyve.util.Util;

public class MetaDataServlet extends HttpServlet {
	private static final long serialVersionUID = -2160904569807647301L;

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

					String uxui = UserAgent.getUxUi(request).getName();
					String moduleName = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter(AbstractWebContext.MODULE_NAME)));
					String documentName = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter(AbstractWebContext.DOCUMENT_NAME)));
					if (documentName != null) {
						pw.append(view(user, uxui, moduleName, documentName));
					}
					else {
						metadata(user, uxui, moduleName, pw);
					}
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
	
	@Override
	protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		doGet(req, resp);
	}

	private static void metadata(User user, String uxui, String chosenModuleName, PrintWriter pw) {
		StringBuilder menus = new StringBuilder(2048);
		StringBuilder dataSources = new StringBuilder(2048);
		processModules(uxui, user, chosenModuleName, menus, dataSources);
		pw.append("{\"menus\":").append(menus).append(",\n");
		pw.append("\"dataSources\":").append(dataSources).append('}');
	}
	
	private static void processModules(final String uxui, 
										final User user,
										final String chosenModuleName,
										final StringBuilder menuJson,
										final StringBuilder dataSourceJson) {
		final Customer customer = user.getCustomer();
		
		menuJson.append("[");
		dataSourceJson.append("{");
		
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

				dataSourceJson.append('"').append(dataSourceName);
				dataSourceJson.append("\":{\"module\":\"").append(drivingDocumentModuleName);
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
        dataSourceJson.append("}");
	}
	
	private static StringBuilder view(User user,
										String uxui,
										String moduleName,
										String documentName) {
		final StringBuilder result = new StringBuilder(5120);
		Customer c = user.getCustomer();
		Module module = c.getModule(moduleName);
		Document document = module.getDocument(c, documentName);
		View editView = document.getView(uxui, c, ViewType.edit.toString());
		
		new ViewRenderer(user, module, document, editView, uxui, false) {
			@Override
			public void renderView(String icon16x16Url, String icon32x32Url) {
				result.append("{\"type\":\"view\",\"name\":\"");
				result.append(view.getName()).append("\",\"title\":\"").append(view.getTitle()).append('"');
				String value = view.getIconStyleClass();
				if (value != null) {
					result.append(",\"iconStyleClass\":\"").append(value).append('"');
				}
				if (icon32x32Url != null) {
					result.append(",\"icon32x32Url\":\"").append(icon32x32Url).append('"');
				}
				value = view.getHelpRelativeFileName();
				if (value != null) {
					result.append(",\"helpRelativeFileName\":\"").append(value).append('"');
				}
				value = view.getHelpURL();
				if (value != null) {
					result.append(",\"helpURL\":\"").append(value).append('"');
				}
				Integer integer = view.getRefreshTimeInSeconds();
				if (integer != null) {
					result.append(",\"helpURL\":").append(integer);
				}
				value = view.getRefreshConditionName();
				if (value != null) {
					result.append(",\"refreshConditionName\":\"").append(value).append('"');
				}
				value = view.getRefreshActionName();
				if (value != null) {
					result.append(",\"refreshActionName\":\"").append(value).append('"');
				}
				
				List<ViewParameter> parameters = view.getParameters();
				if ((parameters != null) && (! parameters.isEmpty())) {
					result.append(",\"parameters\":[");
					for (ViewParameter parameter : parameters) {
						result.append("{\"fromBinding\":\"").append(parameter.getFromBinding());
						result.append("\",\"boundTo\":\"").append(parameter.getBoundTo()).append("\"},");
					}
					result.setLength(result.length() - 1); // remove last comma
					result.append(']');
				}

				processProperties(view.getProperties());

				Collection<Action> actions = view.getActions();
				if ((actions != null) && (! actions.isEmpty())) {
					result.append(",\"actions\":[");
					for (Action action : actions) {
// TODO what do I do here
					}
					result.append(']');
				}
				
				processContainer(view);
			}

			@Override
			public void renderedView(String icon16x16Url, String icon32x32Url) {
				processedContainer(view);
				result.setLength(result.length() - 1); // remove last comma
				result.append('}');
			}
			
			@Override
			public void renderVBox(String borderTitle, VBox vbox) {
				result.append("{\"type\":\"vbox\"");
				processContainer(vbox);
			}
			
			@Override
			public void renderedVBox(String borderTitle, VBox vbox) {
				processedContainer(vbox);
				result.setLength(result.length() - 1); // remove last comma
				result.append("},");
			}
			
			@Override
			public void renderHBox(String borderTitle, HBox hbox) {
				result.append("{\"type\":\"hbox\"");
				processContainer(hbox);
			}
			
			@Override
			public void renderedHBox(String title, HBox hbox) {
				processedContainer(hbox);
				result.setLength(result.length() - 1); // remove last comma
				result.append("},");
			}
			
			@Override
			public void renderTabPane(TabPane tabPane) {
				result.append("{\"type\":\"tabPane\",\"tabs\"[");
			}
			
			@Override
			public void renderedTabPane(TabPane tabPane) {
				result.setLength(result.length() - 1); // remove last comma
				result.append("],");
			}
			
			@Override
			public void renderTab(String title, String icon16x16Url, Tab tab) {
				result.append("{\"type\":\"tab\"");
				processContainer(tab);
			}
			
			@Override
			public void renderedTab(String title, String icon16x16Url, Tab tab) {
				processedContainer(tab);
				result.setLength(result.length() - 1); // remove last comma
				result.append("},");
			}
			
			@Override
			public void renderForm(String borderTitle, Form form) {
				result.append("{\"type\":\"form\",\"columns\":[");
				for (FormColumn column : form.getColumns()) {
					result.append("{\"type\":\"column\"");
					processWidths(column.getPixelWidth(),
									column.getResponsiveWidth(),
									column.getPercentageWidth(),
									column.getSm(),
									column.getMd(),
									column.getLg(),
									column.getXl());
					result.append("},");
				}
				result.setLength(result.length() - 1); // remove last comma
				result.append("],\"rows\":[");
			}
			
			@Override
			public void renderedForm(String borderTitle, Form form) {
				result.setLength(result.length() - 1); // remove last comma
				result.append("]},");
			}
			
			@Override
			public void renderFormColumn(FormColumn column) {
				// handled in renderForm()
			}
			
			@Override
			public void renderFormRow(FormRow row) {
				result.append("{\"type\":\"row\",\"items\":[");
			}
			
			@Override
			public void renderedFormRow(FormRow row) {
				result.setLength(result.length() - 1); // remove last comma
				result.append("]},");
			}
			
			@Override
			public void renderFormItem(String label,
										boolean required,
										String help,
										boolean showsLabel,
										int colspan,
										FormItem item) {
				result.append("{\"type\":\"item\",\"widget\":");
			}
			
			@Override
			public void renderedFormItem(String label,
											boolean required,
											String help,
											boolean showLabel,
											int colspan,
											FormItem item) {
				result.append("},");
			}
			
			@Override
			public void renderFormTextField(TextField text) {
				result.append("{\"type\":\"textField\"}");
			}
			
			@Override
			public void renderedFormTextField(TextField text) {
				// nothing to see here
			}
			
			@Override
			public void renderFormTextArea(TextArea text) {
				result.append("{\"type\":\"textArea\"}");
			}
			
			@Override
			public void renderedFormTextArea(TextArea text) {
				// nothing to see here
			}
			
			@Override
			public void renderFormZoomIn(String label,
											String iconUrl,
											String iconStyleClass,
											String toolTip,
											ZoomIn zoomIn) {
				result.append("{\"type\":\"zoomIn\"}");
			}
			
			@Override
			public void renderFormStaticImage(String fileUrl, StaticImage image) {
				result.append("{\"type\":\"staticImage\"}");
			}
			
			@Override
			public void renderFormSpinner(Spinner spinner) {
				result.append("{\"type\":\"spinner\"}");
			}
			
			@Override
			public void renderedFormSpinner(Spinner spinner) {
				// nothing to see here
			}
			
			@Override
			public void renderFormSpacer(Spacer spacer) {
				result.append("{\"type\":\"spacer\"}");
			}
			
			@Override
			public void renderFormSlider(Slider slider) {
				result.append("{\"type\":\"slider\"}");
			}
			
			@Override
			public void renderedFormSlider(Slider slider) {
				// nothing to see here
			}
			
			@Override
			public void renderFormRichText(RichText text) {
				result.append("{\"type\":\"richText\"}");
			}
			
			@Override
			public void renderedFormRichText(RichText text) {
				// nothing to see here
			}
			
			@Override
			public void renderFormRadio(Radio radio) {
				result.append("{\"type\":\"richText\"}");
			}
			
			@Override
			public void renderedFormRadio(Radio radio) {
				// nothing to see here
			}
			
			@Override
			public void renderFormProgressBar(ProgressBar progressBar) {
				result.append("{\"type\":\"progressBar\"}");
			}
			
			@Override
			public void renderFormPassword(Password password) {
				result.append("{\"type\":\"password\"}");
			}
			
			@Override
			public void renderedFormPassword(Password password) {
				// nothing to see here
			}
			
			@Override
			public void renderFormLookupDescription(MetaDataQueryDefinition query,
														boolean canCreate,
														boolean canUpdate,
														String descriptionBinding,
														LookupDescription lookup) {
				result.append("{\"type\":\"lookupDescription\"}");
			}
			
			@Override
			public void renderedFormLookupDescription(MetaDataQueryDefinition query,
														boolean canCreate,
														boolean canUpdate,
														String descriptionBinding,
														LookupDescription lookup) {
				// nothing to see here
			}
			
			@Override
			public void renderFormLink(String value, Link link) {
				result.append("{\"type\":\"link\"}");
			}
			
			@Override
			public void renderFormLabel(String value, Label label) {
				result.append("{\"type\":\"label\"}");
			}
			
			@Override
			public void renderFormInject(Inject inject) {
				result.append("{\"type\":\"inject\"}");
			}
			
			@Override
			public void renderFormHTML(HTML html) {
				result.append("{\"type\":\"html\"}");
			}
			
			@Override
			public void renderFormGeometryMap(GeometryMap geometry) {
				result.append("{\"type\":\"geometryMap\"}");
			}
			
			@Override
			public void renderedFormGeometryMap(GeometryMap geometry) {
				// nothing to see here
			}
			
			@Override
			public void renderFormGeometry(Geometry geometry) {
				result.append("{\"type\":\"geometry\"}");
			}
			
			@Override
			public void renderedFormGeometry(Geometry geometry) {
				// nothing to see here
			}

			@Override
			public void renderFormDialogButton(String label, DialogButton button) {
				result.append("{\"type\":\"dialogButton\"}");
			}
			
			@Override
			public void renderFormContentSignature(ContentSignature signature) {
				result.append("{\"type\":\"contentSignature\"}");
			}
			
			@Override
			public void renderFormContentLink(String value, ContentLink link) {
				result.append("{\"type\":\"contentLink\"}");
			}
			
			@Override
			public void renderFormContentImage(ContentImage image) {
				result.append("{\"type\":\"contentImage\"}");
			}
			
			@Override
			public void renderFormCombo(Combo combo) {
				result.append("{\"type\":\"combo\"}");
			}
			
			@Override
			public void renderedFormCombo(Combo combo) {
				// nothing to see here
			}
			
			@Override
			public void renderFormColourPicker(ColourPicker colour) {
				result.append("{\"type\":\"colour\"}");
			}
			
			@Override
			public void renderedFormColourPicker(ColourPicker colour) {
				// nothing to see here
			}
			
			@Override
			public void renderFormCheckBox(CheckBox checkBox) {
				result.append("{\"type\":\"checkBox\"}");
			}
			
			@Override
			public void renderedFormCheckBox(CheckBox checkBox) {
				// nothing to see here
			}
			
			@Override
			public void renderFormButton(Action action,
											String label,
											String iconUrl,
											String iconStyleClass,
											String toolTip,
											String confirmationText,
											char type,
											Button button) {
				result.append("{\"type\":\"button\"}");
			}
			
			@Override
			public void renderFormBlurb(String markup, Blurb blurb) {
				result.append("{\"type\":\"blurb\"}");
			}
			
			@Override
			public void renderDataGrid(String title, DataGrid grid) {
				result.append("{\"type\":\"dataGrid\",\"columns\":[");
			}

			@Override
			public void renderDataGridContainerColumn(String title, DataGridContainerColumn column) {
				result.append("{\"type\":\"containerColumn\",\"widgets\":[");
			}
			
			@Override
			public void renderContainerColumnStaticImage(String fileUrl, StaticImage image) {
				result.append("{\"type\":\"staticImge\"},");
			}
			
			@Override
			public void renderContainerColumnLink(String value, Link link) {
				result.append("{\"type\":\"link\"},");
			}
			
			@Override
			public void renderContainerColumnLabel(String value, Label label) {
				result.append("{\"type\":\"label\"},");
			}
			
			@Override
			public void renderContainerColumnDynamicImage(DynamicImage image) {
				result.append("{\"type\":\"dynamicImge\"},");
			}
			
			@Override
			public void renderContainerColumnContentImage(ContentImage image) {
				result.append("{\"type\":\"contentImge\"},");
			}
			
			@Override
			public void renderContainerColumnBlurb(String markup, Blurb blurb) {
				result.append("{\"type\":\"blurb\"},");
			}
			
			@Override
			public void renderedDataGridContainerColumn(String title, DataGridContainerColumn column) {
				result.setLength(result.length() - 1); // remove last comma
				result.append("]},");
			}
			
			@Override
			public void renderDataGridBoundColumn(String title, DataGridBoundColumn column) {
				result.append("{\"type\":\"boundColumn\"");
				WidgetReference input = column.getInputWidget();
				if (input != null) {
					result.append(",\"input\":");
				}
				result.append("},");
			}
			
			@Override
			public void renderBoundColumnTextField(TextField text) {
				result.append("{\"type\":\"textField\"}");
			}
			
			@Override
			public void renderedBoundColumnTextField(TextField text) {
				// nothing to see here
			}
			
			@Override
			public void renderBoundColumnTextArea(TextArea text) {
				result.append("{\"type\":\"textArea\"}");
			}
			
			@Override
			public void renderedBoundColumnTextArea(TextArea text) {
				// nothing to see here
			}
			
			@Override
			public void renderBoundColumnSpinner(Spinner spinner) {
				result.append("{\"type\":\"spinner\"}");
			}
			
			@Override
			public void renderedBoundColumnSpinner(Spinner spinner) {
				// nothing to see here
			}
			
			@Override
			public void renderBoundColumnSlider(Slider slider) {
				result.append("{\"type\":\"slider\"}");
			}
			
			@Override
			public void renderedBoundColumnSlider(Slider slider) {
				// nothing to see here
			}
			
			@Override
			public void renderBoundColumnRichText(RichText text) {
				result.append("{\"type\":\"richText\"}");
			}
			
			@Override
			public void renderedBoundColumnRichText(RichText text) {
				// nothing to see here
			}
			
			@Override
			public void renderBoundColumnRadio(Radio radio) {
				result.append("{\"type\":\"radio\"}");
			}
			
			@Override
			public void renderedBoundColumnRadio(Radio radio) {
				// nothing to see here
			}
			
			@Override
			public void renderBoundColumnPassword(Password password) {
				result.append("{\"type\":\"password\"}");
			}
			
			@Override
			public void renderedBoundColumnPassword(Password password) {
				// nothing to see here
			}
			
			@Override
			public void renderBoundColumnLookupDescription(MetaDataQueryDefinition query,
															boolean canCreate,
															boolean canUpdate,
															String descriptionBinding,
															LookupDescription lookup) {
				result.append("{\"type\":\"lookupDescription\"}");
			}
			
			@Override
			public void renderedBoundColumnLookupDescription(MetaDataQueryDefinition query,
																	boolean canCreate,
																	boolean canUpdate,
																	String descriptionBinding,
																	LookupDescription lookup) {
				// nothing to see here
			}
			
			@Override
			public void renderBoundColumnHTML(HTML html) {
				result.append("{\"type\":\"html\"}");
			}
			
			@Override
			public void renderBoundColumnGeometry(Geometry geometry) {
				result.append("{\"type\":\"geometry\"}");
			}
			
			@Override
			public void renderedBoundColumnGeometry(Geometry geometry) {
				// nothing to see here
			}
			
			@Override
			public void renderBoundColumnContentLink(String value, ContentLink link) {
				result.append("{\"type\":\"contentLink\"}");
			}
			
			@Override
			public void renderBoundColumnContentImage(ContentImage image) {
				result.append("{\"type\":\"contentImage\"}");
			}
			
			@Override
			public void renderBoundColumnCombo(Combo combo) {
				result.append("{\"type\":\"combo\"}");
			}
			
			@Override
			public void renderedBoundColumnCombo(Combo combo) {
				// nothing to see here
			}
			
			@Override
			public void renderBoundColumnColourPicker(ColourPicker colour) {
				result.append("{\"type\":\"colourPicker\"}");
			}
			
			@Override
			public void renderedBoundColumnColourPicker(ColourPicker colour) {
				// nothing to see here
			}
			
			@Override
			public void renderBoundColumnCheckBox(CheckBox checkBox) {
				result.append("{\"type\":\"checkBox\"}");
			}

			@Override
			public void renderedBoundColumnCheckBox(CheckBox checkBox) {
				// nothing to see here
			}

			@Override
			public void renderedDataGridBoundColumn(String title, DataGridBoundColumn column) {
				// nothing to see here
			}
			
			@Override
			public void renderedDataGrid(String title, DataGrid grid) {
				result.setLength(result.length() - 1); // remove last comma
				result.append("]},");
			}
			
			@Override
			public void renderDataRepeater(String title, DataRepeater repeater) {
				result.append("{\"type\":\"dataRepeater\",\"columns\":[");
			}
			
			@Override
			public void renderDataRepeaterContainerColumn(String title, DataGridContainerColumn column) {
				result.append("{\"type\":\"containerColumn\"},");
			}
			
			@Override
			public void renderedDataRepeaterContainerColumn(String title, DataGridContainerColumn column) {
				// nothing to see here
			}

			@Override
			public void renderDataRepeaterBoundColumn(String title, DataGridBoundColumn column) {
				result.append("{\"type\":\"boundColumn\"},");
			}
			
			@Override
			public void renderedDataRepeaterBoundColumn(String title, DataGridBoundColumn column) {
				// nothing to see here
			}
			
			@Override
			public void renderedDataRepeater(String title, DataRepeater repeater) {
				result.setLength(result.length() - 1); // remove last comma
				result.append("]},");
			}
			
			@Override
			public void renderListGrid(String title, boolean aggregateQuery, ListGrid grid) {
				result.append("{\"type\":\"listGrid\",\"columns\":[");
			}
			
			@Override
			public void renderListGridProjectedColumn(MetaDataQueryProjectedColumn column) {
				result.append("{\"type\":\"column\"},");
			}
			
			@Override
			public void renderListGridContentColumn(MetaDataQueryContentColumn column) {
				result.append("{\"type\":\"contentColumn\"},");
			}
			
			@Override
			public void renderedListGrid(String title, boolean aggregateQuery, ListGrid grid) {
				result.setLength(result.length() - 1); // remove last comma
				result.append("]},");
			}
			
			@Override
			public void renderListRepeater(String title, ListRepeater repeater) {
				result.append("{\"type\":\"listRepeater\",\"columns\":[");
			}
			
			@Override
			public void renderListRepeaterProjectedColumn(MetaDataQueryProjectedColumn column) {
				result.append("{\"type\":\"column\"},");
			}
			
			@Override
			public void renderListRepeaterContentColumn(MetaDataQueryContentColumn column) {
				result.append("{\"type\":\"contentColumn\"},");
			}
			
			@Override
			public void renderedListRepeater(String title, ListRepeater repeater) {
				result.setLength(result.length() - 1); // remove last comma
				result.append("]},");
			}
			
			@Override
			public void renderTreeGrid(String title, TreeGrid grid) {
				result.append("{\"type\":\"treeGrid\",\"columns\":[");
			}
			
			@Override
			public void renderTreeGridProjectedColumn(MetaDataQueryProjectedColumn column) {
				result.append("{\"type\":\"column\"},");
			}
			
			@Override
			public void renderTreeGridContentColumn(MetaDataQueryContentColumn column) {
				result.append("{\"type\":\"contentColumn\"},");
			}
			
			@Override
			public void renderedTreeGrid(String title, TreeGrid grid) {
				result.setLength(result.length() - 1); // remove last comma
				result.append("]},");
			}
			
			@Override
			public void renderListMembership(String candidatesHeading, String membersHeading, ListMembership membership) {
				result.append("{\"type\":\"listMembership\"},");
			}
			
			@Override
			public void renderedListMembership(String candidatesHeading, String membersHeading, ListMembership membership) {
				// nothing to see here
			}
			
			@Override
			public void renderCheckMembership(CheckMembership membership) {
				result.append("{\"type\":\"checkMembership\"},");
			}
			
			@Override
			public void renderedCheckMembership(CheckMembership membership) {
				// nothing to see here
			}
			
			@Override
			public void renderStaticImage(String fileUrl, StaticImage image) {
				result.append("{\"type\":\"staticImage\"},");
			}
			
			@Override
			public void renderSpacer(Spacer spacer) {
				result.append("{\"type\":\"spacer\"},");
			}
			
			@Override
			public void renderZoomIn(String label, String iconUrl, String iconStyleClass, String toolTip, ZoomIn zoomIn) {
				result.append("{\"type\":\"zoomIn\"},");
			}
						
			@Override
			public void renderMap(MapDisplay map) {
				result.append("{\"type\":\"map\"},");
			}
			
			@Override
			public void renderLink(String value, Link link) {
				result.append("{\"type\":\"link\"},");
			}
			
			@Override
			public void renderLabel(String value, Label label) {
				result.append("{\"type\":\"label\"},");
			}
			
			@Override
			public void renderInject(Inject inject) {
				result.append("{\"type\":\"inject\"},");
			}

			@Override
			public void renderDynamicImage(DynamicImage image) {
				result.append("{\"type\":\"dynamicImage\"},");
			}
			
			@Override
			public void renderDialogButton(String label, DialogButton button) {
				result.append("{\"type\":\"dialogButton\"},");
			}
			
			@Override
			public void renderComparison(Comparison comparison) {
				result.append("{\"type\":\"comparison\"},");
			}
			
			@Override
			public void renderChart(Chart chart) {
				result.append("{\"type\":\"chart\"},");
			}
			
			@Override
			public void renderButton(Action action,
										String label,
										String iconUrl,
										String iconStyleClass,
										String toolTip,
										String confirmationText,
										char type,
										Button button) {
				result.append("{\"type\":\"button\"},");
			}
			
			@Override
			public void renderBlurb(String markup, Blurb blurb) {
				result.append("{\"type\":\"blurb\"},");
			}
			
			@Override
			public void renderZoomOutAction(String label,
												String iconUrl,
												String iconStyleClass,
												String toolTip,
												String confirmationText,
												char type,
												ActionImpl action) {
//TODO				result.append("{\"type\":\"zoomOutAction\"},");
			}
			
			@Override
			public void renderUploadAction(String label,
											String iconUrl,
											String iconStyleClass,
											String toolTip,
											String confirmationText,
											char type,
											ActionImpl action) {
//TODO				result.append("{\"type\":\"uploadAction\"},");
			}
			
			@Override
			public void renderSaveAction(String label,
											String iconUrl,
											String iconStyleClass,
											String toolTip,
											String confirmationText,
											char type,
											ActionImpl action) {
//TODO				result.append("{\"type\":\"saveAction\"},");
			}
			
			@Override
			public void renderReportAction(String label,
											String iconUrl,
											String iconStyleClass,
											String toolTip,
											String confirmationText,
											char type,
											ActionImpl action) {
//TODO				result.append("{\"type\":\"reportAction\"},");
			}
			
			@Override
			public void renderRemoveAction(String label,
											String iconUrl,
											String iconStyleClass,
											String toolTip,
											String confirmationText,
											char type,
											ActionImpl action,
											boolean canDelete) {
//TODO				result.append("{\"type\":\"removeAction\"},");
			}
			
			@Override
			public void renderPrintAction(String label,
											String iconUrl,
											String iconStyleClass,
											String toolTip,
											String confirmationText,
											char type,
											ActionImpl action) {
//TODO				result.append("{\"type\":\"printAction\"},");
			}
			
			@Override
			public void renderOKAction(String label,
										String iconUrl,
										String iconStyleClass,
										String toolTip,
										String confirmationText,
										char type,
										ActionImpl action) {
//TODO				result.append("{\"type\":\"okAction\"},");
			}
			
			@Override
			public void renderNewAction(String label,
											String iconUrl,
											String iconStyleClass,
											String toolTip,
											String confirmationText,
											char type,
											ActionImpl action) {
//TODO				result.append("{\"type\":\"newAction\"},");
			}
			
			@Override
			public void renderNavigateAction(String label,
												String iconUrl,
												String iconStyleClass,
												String toolTip,
												String confirmationText,
												char type,
												ActionImpl action) {
//TODO				result.append("{\"type\":\"navigateAction\"},");
			}
			
			@Override
			public void renderEditAction(String label,
											String iconUrl,
											String iconStyleClass,
											String toolTip,
											String confirmationText,
											char type,
											ActionImpl action) {
//TODO				result.append("{\"type\":\"editAction\"},");
			}
			
			@Override
			public void renderDownloadAction(String label,
												String iconUrl,
												String iconStyleClass,
												String toolTip,
												String confirmationText,
												char type,
												ActionImpl action) {
//TODO				result.append("{\"type\":\"downloadAction\"},");
			}
			
			@Override
			public void renderDeleteAction(String label,
											String iconUrl,
											String iconStyleClass,
											String toolTip,
											String confirmationText,
											char type,
											ActionImpl action) {
//TODO				result.append("{\"type\":\"deleteAction\"},");
			}
			
			@Override
			public void renderCustomAction(String label,
											String iconUrl,
											String iconStyleClass,
											String toolTip,
											String confirmationText,
											char type,
											ActionImpl action) {
//TODO				result.append("{\"type\":\"serverAction\"},");
			}
			
			@Override
			public void renderCancelAction(String label,
											String iconUrl,
											String iconStyleClass,
											String toolTip,
											String confirmationText,
											char type,
											ActionImpl action) {
//TODO				result.append("{\"type\":\"cancelAction\"},");
			}
			
			@Override
			public void renderBizImportAction(String label,
												String iconUrl,
												String iconStyleClass,
												String toolTip,
												String confirmationText,
												char type,
												ActionImpl action) {
//TODO				result.append("{\"type\":\"importAction\"},");
			}
			
			@Override
			public void renderBizExportAction(String label,
												String iconUrl,
												String iconStyleClass,
												String toolTip,
												String confirmationText,
												char type,
												ActionImpl action) {
//TODO				result.append("{\"type\":\"exportAction\"},");
			}
			
			@Override
			public void renderAddAction(String label,
											String iconUrl,
											String iconStyleClass,
											String toolTip,
											String confirmationText,
											char type,
											ActionImpl action) {
//TODO				result.append("{\"type\":\"addAction\"},");
			}
			
			@Override
			public void visitParameter(Parameter parameter, boolean parentVisible, boolean parentEnabled) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visitFilterParameter(FilterParameter parameter, boolean parentVisible, boolean parentEnabled) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visitedOnSelectedEventHandler(Selectable selectable, boolean parentVisible, boolean parentEnabled) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visitedOnRemovedEventHandler(Removable removable, boolean parentVisible, boolean parentEnabled) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visitedOnPickedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visitedOnFocusEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visitedOnEditedEventHandler(Editable editable, boolean parentVisible, boolean parentEnabled) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visitedOnClearedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visitedOnChangedEventHandler(Changeable changeable, boolean parentVisible, boolean parentEnabled) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visitedOnBlurEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visitedOnAddedEventHandler(Addable addable, boolean parentVisible, boolean parentEnabled) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visitToggleVisibilityEventAction(ToggleVisibilityEventAction toggleVisibility, boolean parentVisible,
					boolean parentEnabled) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visitToggleDisabledEventAction(ToggleDisabledEventAction toggleDisabled, boolean parentVisible,
					boolean parentEnabled) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visitSetInvisibleEventAction(SetInvisibleEventAction setInvisible, boolean parentVisible,
					boolean parentEnabled) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visitSetDisabledEventAction(SetDisabledEventAction setDisabled, boolean parentVisible,
					boolean parentEnabled) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visitRerenderEventAction(RerenderEventAction rerender, EventSource source, boolean parentVisible,
					boolean parentEnabled) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visitOnSelectedEventHandler(Selectable selectable, boolean parentVisible, boolean parentEnabled) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visitOnRemovedEventHandler(Removable removable, boolean parentVisible, boolean parentEnabled) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visitOnPickedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visitOnFocusEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visitOnEditedEventHandler(Editable editable, boolean parentVisible, boolean parentEnabled) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visitOnClearedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visitOnChangedEventHandler(Changeable changeable, boolean parentVisible, boolean parentEnabled) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visitOnBlurEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visitOnAddedEventHandler(Addable addable, boolean parentVisible, boolean parentEnabled) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visitServerSideActionEventAction(Action action, ServerSideActionEventAction server) {
				// TODO Auto-generated method stub
				
			}
			
			private void processContainer(Container container) {
				if (! container.getContained().isEmpty()) {
					result.append(",\"contained\":[");
				}
			}

			private void processedContainer(Container container) {
				if (! container.getContained().isEmpty()) {
					result.setLength(result.length() - 1); // remove last comma
					result.append("],");
				}
			}
			
			private void processProperties(Map<String, String> properties) {
				if ((properties != null) && (! properties.isEmpty())) {
					result.append(",\"properties\":{");
					for (Entry<String, String> entry : properties.entrySet()) {
						result.append('"').append(entry.getKey()).append("\":\"").append(entry.getValue()).append("\",");
					}
					result.setLength(result.length() - 1); // remove comma
					result.append('}');
				}
			}
			
			private void processWidths(Integer pixelWidth,
										Integer responsiveWidth,
										Integer percentageWidth,
										Integer sm,
										Integer md,
										Integer lg,
										Integer xl) {
				if (pixelWidth != null) {
					result.append(",\"pixelWidth\":").append(pixelWidth);
				}
				if (responsiveWidth != null) {
					result.append(",\"responsiveWidth\":").append(responsiveWidth);
				}
				if (percentageWidth != null) {
					result.append(",\"percentageWidth\":").append(percentageWidth);
				}
				if (sm != null) {
					result.append(",\"sm\":").append(sm);
				}
				if (md != null) {
					result.append(",\"md\":").append(md);
				}
				if (lg != null) {
					result.append(",\"lg\":").append(lg);
				}
				if (xl != null) {
					result.append(",\"xl\":").append(xl);
				}
			}
		}.visit();
		
		return result;
	}
	
	private static String emptyResponse() {
		return "{\"menus\":[],\"dataSources\":[]}";
	}
}
