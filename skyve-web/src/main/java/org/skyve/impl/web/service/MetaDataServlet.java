package org.skyve.impl.web.service;

import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.security.Principal;
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
import org.skyve.impl.metadata.repository.module.MetaDataQueryContentColumnMetaData.DisplayType;
import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.metadata.view.AbsoluteWidth;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.Bordered;
import org.skyve.impl.metadata.view.ConstrainableHeight;
import org.skyve.impl.metadata.view.ConstrainableSize;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.Identifiable;
import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.LoadingType;
import org.skyve.impl.metadata.view.MinimumHeight;
import org.skyve.impl.metadata.view.RelativeSize;
import org.skyve.impl.metadata.view.ShrinkWrap;
import org.skyve.impl.metadata.view.ShrinkWrapper;
import org.skyve.impl.metadata.view.VerticalAlignment;
import org.skyve.impl.metadata.view.WidgetReference;
import org.skyve.impl.metadata.view.container.Box;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.event.Addable;
import org.skyve.impl.metadata.view.event.Changeable;
import org.skyve.impl.metadata.view.event.Editable;
import org.skyve.impl.metadata.view.event.EventAction;
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
import org.skyve.impl.metadata.view.model.chart.ChartBuilderMetaData;
import org.skyve.impl.metadata.view.model.chart.ChartBuilderOrderMetaData;
import org.skyve.impl.metadata.view.model.chart.ChartBuilderTopMetaData;
import org.skyve.impl.metadata.view.model.chart.NoBucketMetaData;
import org.skyve.impl.metadata.view.model.chart.NumericMultipleBucketMetaData;
import org.skyve.impl.metadata.view.model.chart.NumericRangeBucketMetaData;
import org.skyve.impl.metadata.view.model.chart.NumericRangeMetaData;
import org.skyve.impl.metadata.view.model.chart.TemporalBucketMetaData;
import org.skyve.impl.metadata.view.model.chart.TextLengthBucketMetaData;
import org.skyve.impl.metadata.view.model.chart.TextStartsWithBucketMetaData;
import org.skyve.impl.metadata.view.reference.ActionReference;
import org.skyve.impl.metadata.view.reference.ContentReference;
import org.skyve.impl.metadata.view.reference.DefaultListViewReference;
import org.skyve.impl.metadata.view.reference.EditViewReference;
import org.skyve.impl.metadata.view.reference.ExternalReference;
import org.skyve.impl.metadata.view.reference.ImplicitActionReference;
import org.skyve.impl.metadata.view.reference.QueryListViewReference;
import org.skyve.impl.metadata.view.reference.Reference;
import org.skyve.impl.metadata.view.reference.ReferenceProcessor;
import org.skyve.impl.metadata.view.reference.ReferenceTarget;
import org.skyve.impl.metadata.view.reference.ReportReference;
import org.skyve.impl.metadata.view.reference.ResourceReference;
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
import org.skyve.impl.metadata.view.widget.bound.input.CompleteType;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryInputType;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;
import org.skyve.impl.metadata.view.widget.bound.input.KeyboardType;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescriptionColumn;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.AbstractListWidget;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.DisableableCRUDGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.WebUtil;
import org.skyve.impl.web.service.smartclient.SmartClientQueryColumnDefinition;
import org.skyve.impl.web.service.smartclient.SmartClientViewRenderer;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.SortDirection;
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
import org.skyve.metadata.view.Action.ActionShow;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Filterable;
import org.skyve.metadata.view.Invisible;
import org.skyve.metadata.view.Parameterizable;
import org.skyve.metadata.view.TextOutput;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewParameter;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.chart.Bucket;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Bound;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.report.ReportFormat;
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
					menuJson.append("\",\"icon16\":\"").append(icon16);
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
					menuJson.append("\",\"icon16\":\"").append(icon16);
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
					menuJson.append("\",\"icon16\":\"").append(icon16);
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
					menuJson.append("\",\"icon16\":\"").append(icon16);
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
					menuJson.append("\",\"icon16\":\"").append(icon16);
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
		final StringBuilder actionsJSON = new StringBuilder(2048);
		Customer c = user.getCustomer();
		Module module = c.getModule(moduleName);
		Document document = module.getDocument(c, documentName);
		View editView = document.getView(uxui, c, ViewType.edit.toString());
		
		new ViewRenderer(user, module, document, editView, uxui, false) {
			@Override
			public void renderView(String icon16x16Url, String icon32x32Url) {
				result.append("{\"type\":\"view\",\"name\":\"");
				result.append(view.getName()).append("\",\"title\":\"").append(view.getLocalisedTitle()).append('"');
				String value = view.getIconStyleClass();
				if (value != null) {
					result.append(",\"iconStyleClass\":\"").append(value).append('"');
				}
				if (icon16x16Url != null) {
					result.append(",\"icon16x16Url\":\"").append(OWASP.escapeJsonString(icon32x32Url)).append('"');
				}
				if (icon32x32Url != null) {
					result.append(",\"icon32x32Url\":\"").append(OWASP.escapeJsonString(icon32x32Url)).append('"');
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

				processDecorated(view);
				processContainer(view);
			}

			@Override
			public void renderedView(String icon16x16Url, String icon32x32Url) {
				processedContainer(view);
				result.setLength(result.length() - 1); // remove last comma

				if (actionsJSON.length() > 0) {
					actionsJSON.setLength(actionsJSON.length() - 1); // remove last comma
					result.append(",\"actions\":[").append(actionsJSON).append(']');
				}

				result.append('}');
			}
			
			@Override
			public void renderVBox(String borderTitle, VBox vbox) {
				result.append("{\"type\":\"vbox\"");
				processIdentifiable(vbox);
				processBorder(vbox, borderTitle);
				processSize(vbox);
				processBox(vbox);
				HorizontalAlignment horizontal = vbox.getHorizontalAlignment();
				if (horizontal != null) {
					result.append(",\"horizontalAlignment\":\"").append(horizontal).append('"');
				}
				VerticalAlignment vertical = vbox.getVerticalAlignment();
				if (vertical != null) {
					result.append(",\"verticalAlignment\":\"").append(vertical).append('"');
				}
				processInvisible(vbox);
				processDecorated(vbox);
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
				processIdentifiable(hbox);
				processBorder(hbox, borderTitle);
				processSize(hbox);
				processBox(hbox);
				HorizontalAlignment horizontal = hbox.getHorizontalAlignment();
				if (horizontal != null) {
					result.append(",\"horizontalAlignment\":\"").append(horizontal).append('"');
				}
				VerticalAlignment vertical = hbox.getVerticalAlignment();
				if (vertical != null) {
					result.append(",\"verticalAlignment\":\"").append(vertical).append('"');
				}
				processInvisible(hbox);
				processDecorated(hbox);
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
				result.append("{\"type\":\"tabPane\"");
				processIdentifiable(tabPane);
				String binding = tabPane.getSelectedTabIndexBinding();
				if (binding != null) {
					result.append(",\"selectedTabIndexBinding\":\"").append(binding).append('"');
				}
				processSize(tabPane);
				processDisableable(tabPane);
				processInvisible(tabPane);
				processDecorated(tabPane);
				result.append(",\"tabs\"[");
			}
			
			@Override
			public void renderedTabPane(TabPane tabPane) {
				result.setLength(result.length() - 1); // remove last comma
				result.append("],");
			}
			
			@Override
			public void renderTab(String title, String icon16x16Url, Tab tab) {
				result.append("{\"type\":\"tab\",\"title\":\"");
				result.append(OWASP.escapeJsonString(title)).append('"');
				if (icon16x16Url != null) {
					result.append(",\"icon16x16Url\":\"").append(OWASP.escapeJsonString(icon16x16Url)).append('"');
				}
				String icon = tab.getIconStyleClass();
				if (icon != null) {
					result.append(",\"iconStyleClass\":\"").append(OWASP.escapeJsonString(icon)).append('"');
				}
				processDisableable(tab);
				processInvisible(tab);
				processDecorated(tab);
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
				result.append("{\"type\":\"form\"");
				processIdentifiable(form);
				processBorder(form, borderTitle);
				HorizontalAlignment align = form.getLabelDefaultHorizontalAlignment();
				if (align != null) {
					result.append(",\"labelDefaultHorizontalAlignment\":\"").append(align).append('"');
				}
				FormLabelLayout layout = form.getLabelLayout();
				if (layout != null) {
					result.append(",\"labelLayout\":\"").append(layout).append('"');
				}
				processSize(form);
				processDisableable(form);
				processInvisible(form);
				result.append(",\"columns\":[");
				
				for (FormColumn column : form.getColumns()) {
					result.append("{\"type\":\"column\"");
					Integer value = column.getPixelWidth();
					if (value != null) {
						result.append(",\"pixelWidth\":").append(value);
					}
					value = column.getPercentageWidth();
					if (value != null) {
						result.append(",\"percentageWidth\":").append(value);
					}
					value = column.getResponsiveWidth();
					if (value != null) {
						result.append(",\"responsiveWidth\":").append(value);
					}
					value = column.getSm();
					if (value != null) {
						result.append(",\"sm\":").append(value);
					}
					value = column.getMd();
					if (value != null) {
						result.append(",\"md\":").append(value);
					}
					value = column.getLg();
					if (value != null) {
						result.append(",\"lg\":").append(value);
					}
					value = column.getXl();
					if (value != null) {
						result.append(",\"xl\":").append(value);
					}
					processDecorated(column);
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
				result.append("{\"type\":\"row\"");
				processDecorated(row);
				result.append(",\"items\":[");
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
				result.append("{\"type\":\"item\"");
				if (colspan > 1) {
					result.append(",\"colspan\":").append(colspan);
				}
				Integer span = item.getRowspan();
				if (span != null) {
					result.append(",\"rowspan\":").append(span);
				}
				HorizontalAlignment align = item.getHorizontalAlignment();
				if (align != null) {
					result.append(",\"horizontalAlignment\":\"").append(align).append('"');
				}
				if (label != null) {
					result.append(",\"label\":\"").append(OWASP.escapeJsonString(label)).append('"');
				}
				result.append(",\"showsLabel\":").append(showsLabel);
				align = item.getLabelHorizontalAlignment();
				if (align != null) {
					result.append(",\"labelHorizontalAlignment\":\"").append(align).append('"');
				}
				Boolean bool = item.getShowHelp();
				if (bool != null) {
					result.append(",\"showHelp\":").append(bool);
				}
				if (help != null) {
					result.append(",\"help\":\"").append(OWASP.escapeJsonString(help)).append('"');
				}
				result.append(",\"required\":").append(required);
				processDecorated(item);
				result.append(",\"widget\":");
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
				result.append("{\"type\":\"textField\"");
				renderTextFieldGuts(text);
				result.append('}');
			}

			private void renderTextFieldGuts(TextField text) {
				processInputWidget(text);
				processEditable(text);
				KeyboardType keyboardType = text.getKeyboardType();
				if (keyboardType != null) {
					result.append(",\"keyboardType\":\"").append(keyboardType).append('"');
				}
				CompleteType complete = text.getComplete();
				if (complete != null) {
					result.append(",\"complete\":\"").append(complete).append('"');
				}
				processSize(text);
				processDecorated(text);
			}

			@Override
			public void renderedFormTextField(TextField text) {
				// nothing to see here
			}
			
			@Override
			public void renderFormTextArea(TextArea text) {
				result.append("{\"type\":\"textArea\"}");
				processInputWidget(text);
				processEditable(text);
				KeyboardType keyboardType = text.getKeyboardType();
				if (keyboardType != null) {
					result.append(",\"keyboardType\":\"").append(keyboardType).append('"');
				}
				Boolean wordWrap = text.getWordWrap();
				if (wordWrap != null) {
					result.append(",\"wordWrap\":\"").append(wordWrap).append('"');
					
				}
				processSize(text);
				processDecorated(text);
				result.append('}');
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
				result.append("{\"type\":\"zoomIn\"");
				if (label != null) {
					result.append(",\"label\":\"").append(OWASP.escapeJsonString(label)).append('"');
				}
				processBound(zoomIn);
				if (iconStyleClass != null) {
					result.append(",\"fontIcon\":\"").append(OWASP.escapeJsonString(iconStyleClass)).append('"');
				}
				if (iconUrl != null) {
					result.append(",\"iconUrl\":\"").append(OWASP.escapeJsonString(iconUrl)).append('"');
				}
				ActionShow show = zoomIn.getShow();
				if (show != null) {
					result.append(",\"show\":\"").append(show).append('"');
				}
				if (toolTip != null) {
					result.append(",\"toolTip\":\"").append(OWASP.escapeJsonString(toolTip)).append('"');
				}
				processSize(zoomIn);
				processDisableable(zoomIn);
				processInvisible(zoomIn);
				processDecorated(zoomIn);
				result.append('}');
			}
			
			@Override
			public void renderFormStaticImage(String fileUrl, StaticImage image) {
				result.append("{\"type\":\"staticImage\",\"fileUrl\":\"");
				result.append(OWASP.escapeJsonString(fileUrl)).append('"');
				processSize(image);
				processInvisible(image);
				processDecorated(image);
				result.append('}');
			}
			
			@Override
			public void renderFormSpinner(Spinner spinner) {
				result.append("{\"type\":\"spinner\"");
				Double value = spinner.getMin();
				if (value != null) {
					result.append(",\"min\":").append(value);
				}
				value = spinner.getMax();
				if (value != null) {
					result.append(",\"max\":").append(value);
				}
				value = spinner.getStep();
				if (value != null) {
					result.append(",\"step\":").append(value);
				}
				renderTextFieldGuts(spinner);
				result.append('}');
			}
			
			@Override
			public void renderedFormSpinner(Spinner spinner) {
				// nothing to see here
			}
			
			@Override
			public void renderFormSpacer(Spacer spacer) {
				result.append("{\"type\":\"spacer\"");
				processSize(spacer);
				processInvisible(spacer);
				processDecorated(spacer);
				result.append('}');
			}
			
			@Override
			public void renderFormSlider(Slider slider) {
				result.append("{\"type\":\"slider\"");
				processInputWidget(slider);
				Double dubs = slider.getMin();
				if (dubs != null) {
					result.append(",\"min\":\"").append(dubs);
				}
				dubs = slider.getMax();
				if (dubs != null) {
					result.append(",\"max\":\"").append(dubs);
				}
				Integer integer = slider.getNumberOfDiscreteValues();
				if (integer != null) {
					result.append(",\"numberOfDiscreteValues\":\"").append(integer);
				}
				integer = slider.getRoundingPrecision();
				if (integer != null) {
					result.append(",\"roundingPrecision\":\"").append(integer);
				}
				Boolean vertical = slider.getVertical();
				if (vertical != null) {
					result.append(",\"vertical\":").append(vertical);
				}
				processSize(slider);
				processDecorated(slider);
				result.append('}');
			}
			
			@Override
			public void renderedFormSlider(Slider slider) {
				// nothing to see here
			}
			
			@Override
			public void renderFormRichText(RichText text) {
				result.append("{\"type\":\"richText\"");
				processInputWidget(text);
				Sanitisation sanitise = text.getSanitise();
				if (sanitise != null) {
					result.append(",\"sanitise\":\"").append(sanitise).append('"');
				}
				processSize(text);
				processDecorated(text);
				result.append('}');
			}
			
			@Override
			public void renderedFormRichText(RichText text) {
				// nothing to see here
			}
			
			@Override
			public void renderFormRadio(Radio radio) {
				result.append("{\"type\":\"radio\"");
				processInputWidget(radio);
				Boolean vertical = radio.getVertical();
				if (vertical != null) {
					result.append(",\"vertical\":\"").append(vertical).append('"');
				}
				processSize(radio);
				processDecorated(radio);
				result.append('}');
			}
			
			@Override
			public void renderedFormRadio(Radio radio) {
				// nothing to see here
			}
			
			@Override
			public void renderFormProgressBar(ProgressBar progressBar) {
				result.append("{\"type\":\"progressBar\"");
				processBound(progressBar);
				processSize(progressBar);
				processInvisible(progressBar);
				processDecorated(progressBar);
				result.append('}');
			}
			
			@Override
			public void renderFormPassword(Password password) {
				result.append("{\"type\":\"password\"");
				processInputWidget(password);
				processSize(password);
				processDecorated(password);
				result.append('}');
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
				result.append("{\"type\":\"lookupDescription\"");
				processInputWidget(lookup);
				result.append(",\"descriptionBinding\":\"").append(descriptionBinding).append('"');
				result.append("\"dataSource\":\"").append(query.getOwningModule().getName()).append('_');
				result.append(query.getDocumentName()).append('"');
				result.append(",\"canCreate\":").append(canCreate);
				result.append(",\"canUpdate\":").append(canUpdate);
				processEditable(lookup);
				String name = lookup.getDisableEditConditionName();
				if (name != null) {
					result.append(",\"disableEditConditionName\":\"").append(name).append('"');
				}
				name = lookup.getDisableAddConditionName();
				if (name != null) {
					result.append(",\"disableAddConditionName\":\"").append(name).append('"');
				}
				name = lookup.getDisableClearConditionName();
				if (name != null) {
					result.append(",\"disableClearConditionName\":\"").append(name).append('"');
				}
				name = lookup.getDisablePickConditionName();
				if (name != null) {
					result.append(",\"disablePickConditionName\":\"").append(name).append('"');
				}
				processSize(lookup);
				List<LookupDescriptionColumn> dropDownColumns = lookup.getDropDownColumns();
				if ((dropDownColumns != null) && (! dropDownColumns.isEmpty())) {
					result.append(",\"dropDownColumns\":[");
					for (LookupDescriptionColumn column : dropDownColumns) {
						result.append("{\"name\":\"").append(column.getName());
						Boolean filterable = column.getFilterable();
						if (filterable != null) {
							result.append(",\"filterable\":").append(filterable);
						}
						result.append("},");
					}
					result.setLength(result.length() - 1); // remove last comma
					result.append(']');
				}
				processFilterable(lookup);
				processParameterizable(lookup);
				processDecorated(lookup);
				result.append('}');
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
				result.append("{\"type\":\"link\"");
				if (value != null) {
					result.append(",\"value\":\"").append(OWASP.escapeJsonString(value)).append('"');
				}
				Reference linkReference = link.getReference();
				if (linkReference != null) {
					result.append(",\"reference\":{");
					new ReferenceProcessor() {
						@Override
						public void processResourceReference(ResourceReference reference) {
							result.append("\"type\":\"resourceRef\",\"relativeFile\":\"");
							result.append(OWASP.escapeJsonString(reference.getRelativeFile())).append('"');
						}
						
						@Override
						public void processReportReference(ReportReference reference) {
							result.append("\"type\":\"reportRef\"");
							String name = reference.getModuleName();
							if (name != null) {
								result.append(",\"moduleName\":\"").append(name).append('"');
							}
							name = reference.getDocumentName();
							if (name != null) {
								result.append(",\"documentName\":\"").append(name).append('"');
							}
							name = reference.getReportName();
							if (name != null) {
								result.append(",\"reportName\":\"").append(name).append('"');
							}
							ReportFormat format = reference.getFormat();
							if (format != null) {
								result.append(",\"format\":\"").append(format).append('"');
							}
							MetaDataServlet.processParameterizable(reference, result);
						}
						
						@Override
						public void processQueryListViewReference(QueryListViewReference reference) {
							result.append("\"type\":\"queryListViewRef\",\"queryName\":\"");
							result.append(reference.getQueryName()).append('"');
						}
						
						@Override
						public void processImplicitActionReference(ImplicitActionReference reference) {
							result.append("\"type\":\"implicitActionRef\",\"implicitActionname\":\"");
							result.append(reference.getImplicitActionName()).append('"');
						}
						
						@Override
						public void processExternalReference(ExternalReference reference) {
							result.append("\"type\":\"externalRef\",\"href\":\"");
							result.append(reference.getHref()).append('"');
						}
						
						@Override
						public void processEditViewReference(EditViewReference reference) {
							result.append("\"type\":\"editViewRef\"");
							String string = reference.getModuleName();
							if (string != null) {
								result.append(",\"moduleName\":\"").append(string).append('"');
							}
							string = reference.getDocumentName();
							if (string != null) {
								result.append(",\"documentName\":\"").append(string).append('"');
							}
							string = reference.getBinding();
							if (string != null) {
								result.append(",\"binding\":\"").append(string).append('"');
							}
						}
						
						@Override
						public void processDefaultListViewReference(DefaultListViewReference reference) {
							result.append("\"type\":\"listViewRef\",\"moduleName\":\"");
							result.append(reference.getModuleName()).append("\",\"documentName\":\"");
							result.append(reference.getDocumentName()).append('"');
						}
						
						@Override
						public void processContentReference(ContentReference reference) {
							result.append("\"type\":\"contentRef\",\"binding\":\"");
							result.append(reference.getBinding()).append('"');
						}
						
						@Override
						public void processActionReference(ActionReference reference) {
							result.append("\"type\":\"actionRef\",\"actionName\":\"");
							result.append(reference.getActionName()).append('"');
						}
					}.process(linkReference);
				}
				ReferenceTarget target = link.getTarget();
				if (target != null) {
					result.append(",\"target\":\"").append(target).append('"');
				}
				processSize(link);
				processInvisible(link);
				processDecorated(link);
				result.append('}');
			}
			
			@Override
			public void renderFormLabel(String value, Label label) {
				result.append("{\"type\":\"label\"");
				processBound(label);
				if (value != null) {
					result.append(",\"value\":\"").append(OWASP.escapeJsonString(value)).append('"');
				}
				Boolean bool = label.getFormatted();
				if (bool != null) {
					result.append(",\"formatted\":").append(bool);
				}
				HorizontalAlignment textAlignment = label.getTextAlignment();
				if (textAlignment != null) {
					result.append(",\"textAlignment\":\"").append(textAlignment).append('"');
				}
				processTextOutput(label);
				processSize(label);
				processInvisible(label);
				processDecorated(label);
				result.append('}');
			}
			
			@Override
			public void renderFormInject(Inject inject) {
				// Unused so just stub
				result.append("{\"type\":\"inject\"}");
			}
			
			@Override
			public void renderFormHTML(HTML html) {
				result.append("{\"type\":\"html\"");
				processInputWidget(html);
				Sanitisation sanitise = html.getSanitise();
				if (sanitise != null) {
					result.append(",\"sanitise\":\"").append(sanitise).append('"');
				}
				processSize(html);
				processDecorated(html);
				result.append('}');
			}
			
			@Override
			public void renderFormGeometryMap(GeometryMap geometry) {
				result.append("{\"type\":\"geometryMap\"");
				processInputWidget(geometry);
				processSize(geometry);
				GeometryInputType type = geometry.getType();
				if (type != null) {
					result.append(",\"type\":\"").append(type).append('"');
				}
				processDecorated(geometry);
				result.append('}');
			}
			
			@Override
			public void renderedFormGeometryMap(GeometryMap geometry) {
				// nothing to see here
			}
			
			@Override
			public void renderFormGeometry(Geometry geometry) {
				result.append("{\"type\":\"geometry\"");
				processInputWidget(geometry);
				processSize(geometry);
				GeometryInputType type = geometry.getType();
				if (type != null) {
					result.append(",\"type\":\"").append(type).append('"');
				}
				processDecorated(geometry);
				result.append('}');
			}
			
			@Override
			public void renderedFormGeometry(Geometry geometry) {
				// nothing to see here
			}

			@Override
			public void renderFormDialogButton(String label, DialogButton button) {
				result.append("{\"type\":\"dialogButton\"");
				if (label != null) {
					result.append(",\"label\":\"").append(OWASP.escapeJsonString(label)).append('"');
				}
				String string = button.getDialogName();
				if (string != null) {
					result.append(",\"dialogName\":\"").append(string).append('"');
				}
				string = button.getCommand();
				if (string != null) {
					result.append(",\"command\":\"").append(string).append('"');
				}
				result.append(",\"modalDialog\":").append(button.isModalDialog());
				result.append(",\"dialogWidth\":").append(button.getDialogWidth());
				result.append(",\"dialogHeight\":").append(button.getDialogHeight());
				processDisableable(button);
				processInvisible(button);
				processParameterizable(button);
				processDecorated(button);
				result.append('}');
			}
			
			@Override
			public void renderFormContentSignature(ContentSignature signature) {
				result.append("{\"type\":\"contentSignature\"");
				processInputWidget(signature);
				String colour = signature.getRgbHexBackgroundColour();
				if (colour != null) {
					result.append(",\"rgbHexBackgroundColour\":\"").append(colour).append('"');
				}
				colour = signature.getRgbHexForegroundColour();
				if (colour != null) {
					result.append(",\"rgbHexForegroundColour\":\"").append(colour).append('"');
				}
				processSize(signature);
				processDecorated(signature);
				result.append('}');
			}
			
			@Override
			public void renderFormContentLink(String value, ContentLink link) {
				result.append("{\"type\":\"contentLink\"");
				if (value != null) {
					result.append(",\"value\":\"").append(value).append('"');
				}
				processInputWidget(link);
				processEditable(link);
				processSize(link);
				processParameterizable(link);
				processDecorated(link);
				result.append('}');
			}
			
			@Override
			public void renderFormContentImage(ContentImage image) {
				result.append("{\"type\":\"contentImage\"");
				processInputWidget(image);
				processEditable(image);
				processSize(image);
				processDecorated(image);
				result.append('}');
			}
			
			@Override
			public void renderFormCombo(Combo combo) {
				result.append("{\"type\":\"combo\"");
				processInputWidget(combo);
				processSize(combo);
				processDecorated(combo);
				result.append('}');
			}
			
			@Override
			public void renderedFormCombo(Combo combo) {
				// nothing to see here
			}
			
			@Override
			public void renderFormColourPicker(ColourPicker colour) {
				result.append("{\"type\":\"colour\"");
				processInputWidget(colour);
				processSize(colour);
				processDecorated(colour);
				result.append('}');
			}
			
			@Override
			public void renderedFormColourPicker(ColourPicker colour) {
				// nothing to see here
			}
			
			@Override
			public void renderFormCheckBox(CheckBox checkBox) {
				result.append("{\"type\":\"checkBox\"");
				processInputWidget(checkBox);
				Boolean triState = checkBox.getTriState();
				if (triState != null) {
					result.append(",\"triState\":").append(triState);
				}
				processSize(checkBox);
				processDecorated(checkBox);
				result.append('}');
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
				result.append("{\"type\":\"button\",\"actionType\":\"").append(type);
				if (action != null) {
					result.append(",\"actionName\":\"").append(action.getName()).append('"');
				}
				if (label != null) {
					result.append(",\"label\":\"").append(OWASP.escapeJsonString(label)).append('"');
				}
				if (iconStyleClass != null) {
					result.append(",\"fontIcon\":\"").append(OWASP.escapeJsonString(iconStyleClass)).append('"');
				}
				if (iconUrl != null) {
					result.append(",\"iconUrl\":\"").append(OWASP.escapeJsonString(iconUrl)).append('"');
				}
				if (toolTip != null) {
					result.append(",\"toolTip\":\"").append(OWASP.escapeJsonString(toolTip)).append('"');
				}
				if (confirmationText != null) {
					result.append(",\"confirmationText\":\"").append(OWASP.escapeJsonString(confirmationText)).append('"');
				}
				ActionShow show = button.getShow();
				if (show != null) {
					result.append(",\"show\":\"").append(show).append('"');
				}
				processSize(button);
				processDecorated(button);
				result.append('}');
			}
			
			@Override
			public void renderFormBlurb(String markup, Blurb blurb) {
				result.append("{\"type\":\"blurb\"");
				if (markup != null) {
					result.append(",\"markup\":\"").append(OWASP.escapeJsonString(markup)).append('"');
				}
				HorizontalAlignment textAlignment = blurb.getTextAlignment();
				if (textAlignment != null) {
					result.append(",\"textAlignment\":\"").append(textAlignment).append('"');
				}
				processSize(blurb);
				processInvisible(blurb);
				processTextOutput(blurb);
				processDecorated(blurb);
				result.append('}');
			}
			
			@Override
			public void renderDataGrid(String title, DataGrid grid) {
				result.append("{\"type\":\"dataGrid\"");
				if (title != null) {
					result.append("\"title\":\"").append(OWASP.escapeJsonString(title)).append('"');
				}
				processBound(grid);
				processIdentifiable(grid);
				processSize(grid);
				processInvisible(grid);
				processDisableable(grid);
				processEditable(grid);
				processDisableableCRUDGrid(grid);
				Boolean bool = grid.getShowAdd();
				if (bool != null) {
					result.append(",\"showAdd\":").append(bool);
				}
				bool = grid.getShowZoom();
				if (bool != null) {
					result.append(",\"showZoom\":").append(bool);
				}
				bool = grid.getShowEdit();
				if (bool != null) {
					result.append(",\"showEdit\":").append(bool);
				}
				bool = grid.getShowRemove();
				if (bool != null) {
					result.append(",\"showRemove\":").append(bool);
				}
				bool = grid.getShowDeselect();
				if (bool != null) {
					result.append(",\"showDeselect\":").append(bool);
				}
				bool = grid.getInline();
				if (bool != null) {
					result.append(",\"inline\":").append(bool);
				}
				bool = grid.getWordWrap();
				if (bool != null) {
					result.append(",\"wordWrap\":").append(bool);
				}
				processDecorated(grid);
				result.append(",\"columns\":[");
			}

			@Override
			public void renderDataGridContainerColumn(String title, DataGridContainerColumn column) {
				result.append("{\"type\":\"containerColumn\"");
				if (title != null) {
					result.append("\"title\":\"").append(OWASP.escapeJsonString(title)).append('"');
				}
				HorizontalAlignment alignment = column.getAlignment();
				if (alignment != null) {
					result.append("\"alignment\":\"").append(alignment).append('"');
				}
				processSize(column);
				processDecorated(column);
				result.append(",\"widgets\":[");
			}
			
			@Override
			public void renderContainerColumnStaticImage(String fileUrl, StaticImage image) {
				renderStaticImage(fileUrl, image);
			}
			
			@Override
			public void renderContainerColumnLink(String value, Link link) {
				renderLink(value, link);
			}
			
			@Override
			public void renderContainerColumnLabel(String value, Label label) {
				renderLabel(value, label);
			}
			
			@Override
			public void renderContainerColumnDynamicImage(DynamicImage image) {
				renderDynamicImage(image);
			}
			
			@Override
			public void renderContainerColumnContentImage(ContentImage image) {
				renderFormContentImage(image);
				result.append(',');
			}
			
			@Override
			public void renderContainerColumnBlurb(String markup, Blurb blurb) {
				renderBlurb(markup, blurb);
			}
			
			@Override
			public void renderedDataGridContainerColumn(String title, DataGridContainerColumn column) {
				result.setLength(result.length() - 1); // remove last comma
				result.append("]},");
			}
			
			@Override
			public void renderDataGridBoundColumn(String title, DataGridBoundColumn column) {
				result.append("{\"type\":\"boundColumn\"");
				if (title != null) {
					result.append("\"title\":\"").append(OWASP.escapeJsonString(title)).append('"');
				}
				HorizontalAlignment alignment = column.getAlignment();
				if (alignment != null) {
					result.append("\"alignment\":\"").append(alignment).append('"');
				}
				processSize(column);
				processDecorated(column);
				WidgetReference input = column.getInputWidget();
				if (input != null) {
					result.append(",\"input\":");
				}
			}
			
			@Override
			public void renderBoundColumnTextField(TextField text) {
				renderFormTextField(text);
			}
			
			@Override
			public void renderedBoundColumnTextField(TextField text) {
				renderedFormTextField(text);
			}
			
			@Override
			public void renderBoundColumnTextArea(TextArea text) {
				renderFormTextArea(text);
			}
			
			@Override
			public void renderedBoundColumnTextArea(TextArea text) {
				renderedFormTextArea(text);
			}
			
			@Override
			public void renderBoundColumnSpinner(Spinner spinner) {
				renderFormSpinner(spinner);
			}
			
			@Override
			public void renderedBoundColumnSpinner(Spinner spinner) {
				renderedFormSpinner(spinner);
			}
			
			@Override
			public void renderBoundColumnSlider(Slider slider) {
				renderFormSlider(slider);
			}
			
			@Override
			public void renderedBoundColumnSlider(Slider slider) {
				renderedFormSlider(slider);
			}
			
			@Override
			public void renderBoundColumnRichText(RichText text) {
				renderFormRichText(text);
			}
			
			@Override
			public void renderedBoundColumnRichText(RichText text) {
				renderedFormRichText(text);
			}
			
			@Override
			public void renderBoundColumnRadio(Radio radio) {
				renderFormRadio(radio);
			}
			
			@Override
			public void renderedBoundColumnRadio(Radio radio) {
				renderedFormRadio(radio);
			}
			
			@Override
			public void renderBoundColumnPassword(Password password) {
				renderFormPassword(password);
			}
			
			@Override
			public void renderedBoundColumnPassword(Password password) {
				renderedFormPassword(password);
			}
			
			@Override
			public void renderBoundColumnLookupDescription(MetaDataQueryDefinition query,
															boolean canCreate,
															boolean canUpdate,
															String descriptionBinding,
															LookupDescription lookup) {
				renderFormLookupDescription(query, canCreate, canUpdate, descriptionBinding, lookup);
			}
			
			@Override
			public void renderedBoundColumnLookupDescription(MetaDataQueryDefinition query,
																boolean canCreate,
																boolean canUpdate,
																String descriptionBinding,
																LookupDescription lookup) {
				renderedFormLookupDescription(query, canCreate, canUpdate, descriptionBinding, lookup);
			}
			
			@Override
			public void renderBoundColumnHTML(HTML html) {
				renderFormHTML(html);
			}
			
			@Override
			public void renderBoundColumnGeometry(Geometry geometry) {
				renderFormGeometry(geometry);
			}
			
			@Override
			public void renderedBoundColumnGeometry(Geometry geometry) {
				renderedFormGeometry(geometry);
			}
			
			@Override
			public void renderBoundColumnContentLink(String value, ContentLink link) {
				renderFormContentLink(value, link);
			}
			
			@Override
			public void renderBoundColumnContentImage(ContentImage image) {
				renderFormContentImage(image);
			}
			
			@Override
			public void renderBoundColumnCombo(Combo combo) {
				renderFormCombo(combo);
			}
			
			@Override
			public void renderedBoundColumnCombo(Combo combo) {
				renderedFormCombo(combo);
			}
			
			@Override
			public void renderBoundColumnColourPicker(ColourPicker colour) {
				renderFormColourPicker(colour);
			}
			
			@Override
			public void renderedBoundColumnColourPicker(ColourPicker colour) {
				renderedFormColourPicker(colour);
			}
			
			@Override
			public void renderBoundColumnCheckBox(CheckBox checkBox) {
				renderFormCheckBox(checkBox);
			}

			@Override
			public void renderedBoundColumnCheckBox(CheckBox checkBox) {
				renderedFormCheckBox(checkBox);
			}

			@Override
			public void renderedDataGridBoundColumn(String title, DataGridBoundColumn column) {
				result.append("},");
			}
			
			@Override
			public void renderedDataGrid(String title, DataGrid grid) {
				result.setLength(result.length() - 1); // remove last comma
				result.append("]},");
			}
			
			@Override
			public void renderDataRepeater(String title, DataRepeater repeater) {
				result.append("{\"type\":\"dataRepeater\"");
				if (title != null) {
					result.append("\"title\":\"").append(OWASP.escapeJsonString(title)).append('"');
				}
				processBound(repeater);
				processIdentifiable(repeater);
				processSize(repeater);
				processInvisible(repeater);
				Boolean bool = repeater.getShowColumnHeaders();
				if (bool != null) {
					result.append(",\"showColumnHeaders\":").append(bool);
				}
				bool = repeater.getShowGrid();
				if (bool != null) {
					result.append(",\"showGrid\":").append(bool);
				}
				processDecorated(repeater);
				result.append(",\"columns\":[");
			}
			
			@Override
			public void renderDataRepeaterContainerColumn(String title, DataGridContainerColumn column) {
				renderDataGridContainerColumn(title, column);
			}
			
			@Override
			public void renderedDataRepeaterContainerColumn(String title, DataGridContainerColumn column) {
				renderedDataGridContainerColumn(title, column);
			}

			@Override
			public void renderDataRepeaterBoundColumn(String title, DataGridBoundColumn column) {
				renderDataGridBoundColumn(title, column);
			}
			
			@Override
			public void renderedDataRepeaterBoundColumn(String title, DataGridBoundColumn column) {
				renderedDataGridBoundColumn(title, column);
			}
			
			@Override
			public void renderedDataRepeater(String title, DataRepeater repeater) {
				result.setLength(result.length() - 1); // remove last comma
				result.append("]},");
			}
			
			@Override
			public void renderListGrid(String title, boolean aggregateQuery, ListGrid grid) {
				result.append("{\"type\":\"listGrid\"");
				renderListGridGuts(title, aggregateQuery, grid);
				result.append(",\"columns\":[");
			}
			
			private void renderListGridGuts(String title, boolean aggregateQuery, ListGrid grid) {
				if (title != null) {
					result.append(",\"title\":\"").append(OWASP.escapeJsonString(title)).append('"');
				}
				result.append(",\"aggregateQuery\":").append(aggregateQuery);
				result.append(",\"continueConversation\":").append(grid.getContinueConversation());
				Boolean bool = grid.getAutoPopulate();
				if (bool != null) {
					result.append(",\"autoPopulate\":").append(bool);
				}

				processAbstractListWidget(grid);
				processDisableable(grid);
				processDisableableCRUDGrid(grid);
				
				bool = grid.getShowAdd();
				if (bool != null) {
					result.append(",\"showAdd\":").append(bool);
				}
				bool = grid.getShowZoom();
				if (bool != null) {
					result.append(",\"showZoom\":").append(bool);
				}
				bool = grid.getShowEdit();
				if (bool != null) {
					result.append(",\"showEdit\":").append(bool);
				}
				bool = grid.getShowRemove();
				if (bool != null) {
					result.append(",\"showRemove\":").append(bool);
				}
				bool = grid.getShowDeselect();
				if (bool != null) {
					result.append(",\"showDeselect\":").append(bool);
				}
				bool = grid.getShowExport();
				if (bool != null) {
					result.append(",\"showExport\":").append(bool);
				}
				bool = grid.getShowChart();
				if (bool != null) {
					result.append(",\"showChart\":").append(bool);
				}
				bool = grid.getShowFilter();
				if (bool != null) {
					result.append(",\"showFilter\":").append(bool);
				}
				bool = grid.getShowSummary();
				if (bool != null) {
					result.append(",\"showSummary\":").append(bool);
				}
				bool = grid.getShowSnap();
				if (bool != null) {
					result.append(",\"showSnap\":").append(bool);
				}
				bool = grid.getShowTag();
				if (bool != null) {
					result.append(",\"showTag\":").append(bool);
				}

				processDecorated(grid);
			}
			
			@Override
			public void renderListGridProjectedColumn(MetaDataQueryProjectedColumn column) {
				result.append("{\"type\":\"column\"");
				processMetaDataQueryColumn(column);
				String expression = column.getExpression();
				if (expression != null) {
					result.append(",\"expression\":\"").append(OWASP.escapeJsonString(expression)).append('"');
				}
				result.append(",\"projected\":").append(column.isProjected());
				result.append(",\"sortable\":").append(column.isSortable());
				result.append(",\"filterable\":").append(column.isFilterable());
				result.append(",\"editable\":").append(column.isEditable());
				result.append("},");
			}
			
			@Override
			public void renderListGridContentColumn(MetaDataQueryContentColumn column) {
				result.append("{\"type\":\"contentColumn\"");
				processMetaDataQueryColumn(column);
				DisplayType display = column.getDisplay();
				if (display != null) {
					result.append(",\"display\":\"").append(display).append('"');
				}
				Integer pixelHeight = column.getPixelHeight();
				if (pixelHeight != null) {
					result.append(",\"pixelHeight\":").append(pixelHeight);
				}
				String emptyThumbnailRelativeFile = column.getEmptyThumbnailRelativeFile();
				if (emptyThumbnailRelativeFile != null) {
					result.append(",\"display\":\"").append(OWASP.escapeJsonString(emptyThumbnailRelativeFile)).append('"');
				}
				result.append("},");
			}
			
			@Override
			public void renderedListGrid(String title, boolean aggregateQuery, ListGrid grid) {
				result.setLength(result.length() - 1); // remove last comma
				result.append("]},");
			}
			
			@Override
			public void renderListRepeater(String title, ListRepeater repeater) {
				result.append("{\"type\":\"listRepeater\"");
				if (title != null) {
					result.append(",\"title\":\"").append(OWASP.escapeJsonString(title)).append('"');
				}
				processAbstractListWidget(repeater);
				Boolean bool = repeater.getShowColumnHeaders();
				if (bool != null) {
					result.append(",\"showColumnHeaders\":").append(bool);
				}
				bool = repeater.getShowGrid();
				if (bool != null) {
					result.append(",\"showGrid\":").append(bool);
				}
				processDecorated(repeater);
				result.append(",\"columns\":[");
			}
			
			@Override
			public void renderListRepeaterProjectedColumn(MetaDataQueryProjectedColumn column) {
				renderListGridProjectedColumn(column);
			}
			
			@Override
			public void renderListRepeaterContentColumn(MetaDataQueryContentColumn column) {
				renderListGridContentColumn(column);
			}
			
			@Override
			public void renderedListRepeater(String title, ListRepeater repeater) {
				result.setLength(result.length() - 1); // remove last comma
				result.append("]},");
			}
			
			@Override
			public void renderTreeGrid(String title, TreeGrid grid) {
				result.append("{\"type\":\"treeGrid\"");
				renderListGridGuts(title, false, grid);
				String rootIdBinding = grid.getRootIdBinding();
				if (rootIdBinding != null) {
					result.append(",\"rootIdBinding\":\"").append(rootIdBinding).append('"');
				}
				result.append(",\"columns\":[");
			}
			
			@Override
			public void renderTreeGridProjectedColumn(MetaDataQueryProjectedColumn column) {
				renderListGridProjectedColumn(column);
			}
			
			@Override
			public void renderTreeGridContentColumn(MetaDataQueryContentColumn column) {
				renderListGridContentColumn(column);
			}
			
			@Override
			public void renderedTreeGrid(String title, TreeGrid grid) {
				result.setLength(result.length() - 1); // remove last comma
				result.append("]},");
			}
			
			@Override
			public void renderListMembership(String candidatesHeading, String membersHeading, ListMembership membership) {
				result.append("{\"type\":\"listMembership\"");
				processInputWidget(membership);
				if (candidatesHeading != null) {
					result.append(",\"candidatesHeading\":\"").append(OWASP.escapeJsonString(candidatesHeading)).append('"');
				}
				if (membersHeading != null) {
					result.append(",\"membersHeading\":\"").append(OWASP.escapeJsonString(membersHeading)).append('"');
				}
				processSize(membership);
				processDecorated(membership);
				result.append("},");
			}
			
			@Override
			public void renderedListMembership(String candidatesHeading, String membersHeading, ListMembership membership) {
				// nothing to see here
			}
			
			@Override
			public void renderCheckMembership(CheckMembership membership) {
				result.append("{\"type\":\"checkMembership\"");
				processInputWidget(membership);
				processDecorated(membership);
				result.append("},");
			}
			
			@Override
			public void renderedCheckMembership(CheckMembership membership) {
				// nothing to see here
			}
			
			@Override
			public void renderStaticImage(String fileUrl, StaticImage image) {
				renderFormStaticImage(fileUrl, image);
				result.append(',');
			}
			
			@Override
			public void renderSpacer(Spacer spacer) {
				renderFormSpacer(spacer);
				result.append(',');
			}
			
			@Override
			public void renderZoomIn(String label, String iconUrl, String iconStyleClass, String toolTip, ZoomIn zoomIn) {
				renderFormZoomIn(label, iconUrl, iconStyleClass, toolTip, zoomIn);
				result.append(',');
			}
						
			@Override
			public void renderMap(MapDisplay map) {
				result.append("{\"type\":\"map\"");
				String modelName = map.getModelName();
				if (modelName != null) {
					result.append(",\"modelName\":\"").append(modelName).append('"');
				}
				LoadingType loading = map.getLoading();
				if (loading != null) {
					result.append(",\"loading\":\"").append(loading).append('"');
				}
				Integer refreshTimeInSeconds = map.getRefreshTimeInSeconds();
				if (refreshTimeInSeconds != null) {
					result.append(",\"refreshTimeInSeconds\":").append(refreshTimeInSeconds);
				}
				Boolean showRefreshControls = map.getShowRefreshControls();
				if (showRefreshControls != null) {
					result.append(",\"showRefreshControls\":").append(showRefreshControls);
				}				
				processSize(map);
				processInvisible(map);
				processDecorated(map);
				result.append("},");
			}
			
			@Override
			public void renderLink(String value, Link link) {
				renderFormLink(value, link);
				result.append(',');
			}
			
			@Override
			public void renderLabel(String value, Label label) {
				renderFormLabel(value, label);
				result.append(',');
			}
			
			@Override
			public void renderInject(Inject inject) {
				renderFormInject(inject);
				result.append(',');
			}

			@Override
			public void renderDynamicImage(DynamicImage image) {
				result.append("{\"type\":\"dynamicImage\"");
				String name = image.getName();
				if (name != null) {
					result.append(",\"name\":\"").append(name).append('"');
				}
				Integer pixels = image.getImageInitialPixelWidth();
				if (pixels != null) {
					result.append(",\"imageInitialPixelWidth\":").append(pixels);
				}
				pixels = image.getImageInitialPixelHeight();
				if (pixels != null) {
					result.append(",\"imageInitialPixelHeight\":").append(pixels);
				}
				processSize(image);
				processInvisible(image);
				processParameterizable(image);
				processDecorated(image);
				result.append("},");
			}
			
			@Override
			public void renderDialogButton(String label, DialogButton button) {
				renderFormDialogButton(label, button);
				result.append(',');
			}
			
			@Override
			public void renderComparison(Comparison comparison) {
				result.append("{\"type\":\"comparison\"");
				String modelName = comparison.getModelName();
				if (modelName != null) {
					result.append(",\"modelName\":\"").append(modelName).append('"');
				}
				processEditable(comparison);
				processSize(comparison);
				processDecorated(comparison);
				result.append("},");
			}
			
			@Override
			public void renderChart(Chart chart) {
				result.append("{\"type\":\"chart\",\"chartType\":\"").append(chart.getType()).append('"');
				String modelName = chart.getModelName();
				if (modelName != null) {
					result.append(",\"modelName\":\"").append(modelName).append('"');
				}
				else {
					ChartBuilderMetaData model = chart.getModel();
					result.append(",\"modelName\":\"").append(model.getModelName()).append('"');
					String string = model.getTitle();
					if (string != null) {
						result.append(",\"title\":\"").append(string).append('"');
					}
					string = model.getLabel();
					if (string != null) {
						result.append(",\"label\":\"").append(string).append('"');
					}
					string = model.getModuleName();
					if (string != null) {
						result.append(",\"moduleName\":\"").append(string).append('"');
					}
					string = model.getDocumentName();
					if (string != null) {
						result.append(",\"documentName\":\"").append(string).append('"');
					}
					string = model.getQueryName();
					if (string != null) {
						result.append(",\"queryName\":\"").append(string).append('"');
					}
					string = model.getCategoryBinding();
					if (string != null) {
						result.append(",\"categoryBinding\":\"").append(string).append('"');
					}
					Bucket bucket = model.getCategoryBucket();
					if (bucket != null) {
						result.append(",\"categoryBucket\":");
						if (bucket instanceof NoBucketMetaData) {
							result.append("null");
						}
						else if (bucket instanceof NumericMultipleBucketMetaData) {
							result.append("{\"type\":\"numericMultipleBucket\",\"multiple\":");
							result.append(((NumericMultipleBucketMetaData) bucket).getMultiple());
							result.append('}');
						}
						else if (bucket instanceof NumericRangeBucketMetaData) {
							result.append("{\"type\":\"numericRangeBucket\",\"range\":[");
							for (NumericRangeMetaData rangeInt : ((NumericRangeBucketMetaData) bucket).getRanges()) {
								result.append(rangeInt.getRange()).append(',');
							}
							result.setLength(result.length() - 1);
							result.append("]}");
						}
						else if (bucket instanceof TemporalBucketMetaData) {
							result.append("{\"type\":\"temporalBucket\",\"temporalType\":\"");
							result.append(((TemporalBucketMetaData) bucket).getType()).append("\"}");
						}
						else if (bucket instanceof TextLengthBucketMetaData) {
							result.append("{\"type\":\"textLengthBucket\"}");
						}
						else if (bucket instanceof TextStartsWithBucketMetaData) {
							result.append("{\"type\":\"textStartsWithBucket\",\"textStartsWithBucket\":\"length\":");
							TextStartsWithBucketMetaData startsWith = (TextStartsWithBucketMetaData) bucket;
							result.append(startsWith.getLength());
							result.append("\"caseSensitive\":").append(startsWith.isCaseSensitive());
							result.append('}');
						}
						else {
							throw new IllegalStateException(bucket + " is not catered for");
						}
					}
					string = model.getValueBinding();
					if (string != null) {
						result.append(",\"valueBinding\":\"").append(string).append('"');
					}
					AggregateFunction valueFunction = model.getValueFunction();
					if (valueFunction != null) {
						result.append(",\"valueFunction\":\"").append(valueFunction).append('"');
					}
					ChartBuilderTopMetaData top = model.getTop();
					if (top != null) {
						result.append(",\"top\":{");
						processChartBuilderOrderMetaData(top);
						result.append(",\"top\":").append(top.getTop());
						result.append(",\"includeOthers\":").append(top.isIncludeOthers());
						result.append('}');
					}
					ChartBuilderOrderMetaData order = model.getOrder();
					if (order != null) {
						result.append(",\"order\":{");
						processChartBuilderOrderMetaData(top);
						result.append('}');
					}
					string = model.getJFreeChartPostProcessorClassName();
					if (string != null) {
						result.append("\"jFreeChartPostProcessorClassName\":\"").append(string).append('"');
					}
					string = model.getPrimeFacesChartPostProcessorClassName();
					if (string != null) {
						result.append("\"primeFacesChartPostProcessorClassName\":\"").append(string).append('"');
					}
				}
				processSize(chart);
				processInvisible(chart);
				processDecorated(chart);
				result.append("},");
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
				renderFormButton(action, label, iconUrl, iconStyleClass, toolTip, confirmationText, type, button);
				result.append(',');
			}
			
			@Override
			public void renderBlurb(String markup, Blurb blurb) {
				renderFormBlurb(markup, blurb);
				result.append(',');
			}
			
			@Override
			public void renderZoomOutAction(String label,
												String iconUrl,
												String iconStyleClass,
												String toolTip,
												String confirmationText,
												char type,
												ActionImpl action) {
				actionsJSON.append("{\"type\":\"zoomOutAction\"");
				processAction(label, iconUrl, iconStyleClass, toolTip, confirmationText, type, null, action);
				actionsJSON.append("},");
			}
			
			@Override
			public void renderUploadAction(String label,
											String iconUrl,
											String iconStyleClass,
											String toolTip,
											String confirmationText,
											char type,
											ActionImpl action) {
				actionsJSON.append("{\"type\":\"uploadAction\"");
				processAction(label, iconUrl, iconStyleClass, toolTip, confirmationText, type, null, action);
				actionsJSON.append("},");
			}
			
			@Override
			public void renderSaveAction(String label,
											String iconUrl,
											String iconStyleClass,
											String toolTip,
											String confirmationText,
											char type,
											ActionImpl action) {
				actionsJSON.append("{\"type\":\"saveAction\"");
				processAction(label, iconUrl, iconStyleClass, toolTip, confirmationText, type, null, action);
				actionsJSON.append("},");
			}
			
			@Override
			public void renderReportAction(String label,
											String iconUrl,
											String iconStyleClass,
											String toolTip,
											String confirmationText,
											char type,
											ActionImpl action) {
				actionsJSON.append("{\"type\":\"reportAction\"");
				processAction(label, iconUrl, iconStyleClass, toolTip, confirmationText, type, null, action);
				actionsJSON.append("},");
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
				actionsJSON.append("{\"type\":\"removeAction\"");
				processAction(label, iconUrl, iconStyleClass, toolTip, confirmationText, type, null, action);
				actionsJSON.append("},");
			}
			
			@Override
			public void renderPrintAction(String label,
											String iconUrl,
											String iconStyleClass,
											String toolTip,
											String confirmationText,
											char type,
											ActionImpl action) {
				actionsJSON.append("{\"type\":\"printAction\"");
				processAction(label, iconUrl, iconStyleClass, toolTip, confirmationText, type, null, action);
				actionsJSON.append("},");
			}
			
			@Override
			public void renderOKAction(String label,
										String iconUrl,
										String iconStyleClass,
										String toolTip,
										String confirmationText,
										char type,
										ActionImpl action) {
				actionsJSON.append("{\"type\":\"okAction\"");
				processAction(label, iconUrl, iconStyleClass, toolTip, confirmationText, type, null, action);
				actionsJSON.append("},");
			}
			
			@Override
			public void renderNewAction(String label,
											String iconUrl,
											String iconStyleClass,
											String toolTip,
											String confirmationText,
											char type,
											ActionImpl action) {
				actionsJSON.append("{\"type\":\"newAction\"");
				processAction(label, iconUrl, iconStyleClass, toolTip, confirmationText, type, null, action);
				actionsJSON.append("},");
			}
			
			@Override
			public void renderNavigateAction(String label,
												String iconUrl,
												String iconStyleClass,
												String toolTip,
												String confirmationText,
												char type,
												ActionImpl action) {
				actionsJSON.append("{\"type\":\"navigateAction\"");
				processAction(label, iconUrl, iconStyleClass, toolTip, confirmationText, type, null, action);
				actionsJSON.append("},");
			}
			
			@Override
			public void renderEditAction(String label,
											String iconUrl,
											String iconStyleClass,
											String toolTip,
											String confirmationText,
											char type,
											ActionImpl action) {
				actionsJSON.append("{\"type\":\"editAction\"");
				processAction(label, iconUrl, iconStyleClass, toolTip, confirmationText, type, null, action);
				actionsJSON.append("},");
			}
			
			@Override
			public void renderDownloadAction(String label,
												String iconUrl,
												String iconStyleClass,
												String toolTip,
												String confirmationText,
												char type,
												ActionImpl action) {
				actionsJSON.append("{\"type\":\"downloadAction\"");
				processAction(label, iconUrl, iconStyleClass, toolTip, confirmationText, type, null, action);
				actionsJSON.append("},");
			}
			
			@Override
			public void renderDeleteAction(String label,
											String iconUrl,
											String iconStyleClass,
											String toolTip,
											String confirmationText,
											char type,
											ActionImpl action) {
				actionsJSON.append("{\"type\":\"deleteAction\"");
				processAction(label, iconUrl, iconStyleClass, toolTip, confirmationText, type, null, action);
				actionsJSON.append("},");
			}
			
			@Override
			public void renderCustomAction(String label,
											String iconUrl,
											String iconStyleClass,
											String toolTip,
											String confirmationText,
											char type,
											ActionImpl action) {
				actionsJSON.append("{\"type\":\"serverAction\"");
				processAction(label, iconUrl, iconStyleClass, toolTip, confirmationText, type, action.getResourceName(), action);
				actionsJSON.append("},");
			}
			
			@Override
			public void renderCancelAction(String label,
											String iconUrl,
											String iconStyleClass,
											String toolTip,
											String confirmationText,
											char type,
											ActionImpl action) {
				actionsJSON.append("{\"type\":\"cancelAction\"");
				processAction(label, iconUrl, iconStyleClass, toolTip, confirmationText, type, null, action);
				actionsJSON.append("},");
			}
			
			@Override
			public void renderBizImportAction(String label,
												String iconUrl,
												String iconStyleClass,
												String toolTip,
												String confirmationText,
												char type,
												ActionImpl action) {
				actionsJSON.append("{\"type\":\"importAction\"");
				processAction(label, iconUrl, iconStyleClass, toolTip, confirmationText, type, null, action);
				actionsJSON.append("},");
			}
			
			@Override
			public void renderBizExportAction(String label,
												String iconUrl,
												String iconStyleClass,
												String toolTip,
												String confirmationText,
												char type,
												ActionImpl action) {
				actionsJSON.append("{\"type\":\"exportAction\"");
				processAction(label, iconUrl, iconStyleClass, toolTip, confirmationText, type, null, action);
				actionsJSON.append("},");
			}
			
			@Override
			public void renderAddAction(String label,
											String iconUrl,
											String iconStyleClass,
											String toolTip,
											String confirmationText,
											char type,
											ActionImpl action) {
				actionsJSON.append("{\"type\":\"addAction\"");
				processAction(label, iconUrl, iconStyleClass, toolTip, confirmationText, type, null, action);
				actionsJSON.append("},");
			}
			
			@Override
			public void visitParameter(Parameter parameter, boolean parentVisible, boolean parentEnabled) {
				// handled in processParameterizable()
			}
			
			@Override
			public void visitFilterParameter(FilterParameter parameter, boolean parentVisible, boolean parentEnabled) {
				// handled in processFilterable()
			}
			
			@Override
			public void visitOnSelectedEventHandler(Selectable selectable, boolean parentVisible, boolean parentEnabled) {
				String selectedIdBinding = selectable.getSelectedIdBinding();
				if (selectedIdBinding != null) {
					result.append(",\"selectedIdBinding\":\"").append(selectedIdBinding).append('"');
				}
				List<EventAction> actions = selectable.getSelectedActions();
				if ((actions != null) && (! actions.isEmpty())) {
					result.append(",\"selectedActions\":[");
				}
			}
			
			@Override
			public void visitedOnSelectedEventHandler(Selectable selectable, boolean parentVisible, boolean parentEnabled) {
				List<EventAction> actions = selectable.getSelectedActions();
				if ((actions != null) && (! actions.isEmpty())) {
					result.setLength(result.length() - 1); // remove last comma
					result.append("]");
				}
			}

			@Override
			public void visitOnRemovedEventHandler(Removable removable, boolean parentVisible, boolean parentEnabled) {
				List<EventAction> actions = removable.getRemovedActions();
				if ((actions != null) && (! actions.isEmpty())) {
					result.append(",\"removedActions\":[");
				}
			}
			
			@Override
			public void visitedOnRemovedEventHandler(Removable removable, boolean parentVisible, boolean parentEnabled) {
				List<EventAction> actions = removable.getRemovedActions();
				if ((actions != null) && (! actions.isEmpty())) {
					result.setLength(result.length() - 1); // remove last comma
					result.append("]");
				}
			}
			
			@Override
			public void visitOnFocusEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
				List<EventAction> actions = blurable.getFocusActions();
				if ((actions != null) && (! actions.isEmpty())) {
					result.append(",\"focusActions\":[");
				}
			}
			
			@Override
			public void visitedOnFocusEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
				List<EventAction> actions = blurable.getFocusActions();
				if ((actions != null) && (! actions.isEmpty())) {
					result.setLength(result.length() - 1); // remove last comma
					result.append("]");
				}
			}
			
			@Override
			public void visitOnBlurEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
				List<EventAction> actions = blurable.getBlurActions();
				if ((actions != null) && (! actions.isEmpty())) {
					result.append(",\"blurActions\":[");
				}
			}
			
			@Override
			public void visitedOnBlurEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
				List<EventAction> actions = blurable.getBlurActions();
				if ((actions != null) && (! actions.isEmpty())) {
					result.setLength(result.length() - 1); // remove last comma
					result.append("]");
				}
			}
			
			@Override
			public void visitOnEditedEventHandler(Editable editable, boolean parentVisible, boolean parentEnabled) {
				List<EventAction> actions = editable.getEditedActions();
				if ((actions != null) && (! actions.isEmpty())) {
					result.append(",\"editedActions\":[");
				}
			}
			
			@Override
			public void visitedOnEditedEventHandler(Editable editable, boolean parentVisible, boolean parentEnabled) {
				List<EventAction> actions = editable.getEditedActions();
				if ((actions != null) && (! actions.isEmpty())) {
					result.setLength(result.length() - 1); // remove last comma
					result.append("]");
				}
			}
			
			@Override
			public void visitOnChangedEventHandler(Changeable changeable, boolean parentVisible, boolean parentEnabled) {
				List<EventAction> actions = changeable.getChangedActions();
				if ((actions != null) && (! actions.isEmpty())) {
					result.append(",\"changedActions\":[");
				}
			}
			
			@Override
			public void visitedOnChangedEventHandler(Changeable changeable, boolean parentVisible, boolean parentEnabled) {
				List<EventAction> actions = changeable.getChangedActions();
				if ((actions != null) && (! actions.isEmpty())) {
					result.setLength(result.length() - 1); // remove last comma
					result.append("]");
				}
			}
			
			@Override
			public void visitOnAddedEventHandler(Addable addable, boolean parentVisible, boolean parentEnabled) {
				List<EventAction> actions = addable.getAddedActions();
				if ((actions != null) && (! actions.isEmpty())) {
					result.append(",\"addedActions\":[");
				}
			}
			
			@Override
			public void visitedOnAddedEventHandler(Addable addable, boolean parentVisible, boolean parentEnabled) {
				List<EventAction> actions = addable.getAddedActions();
				if ((actions != null) && (! actions.isEmpty())) {
					result.setLength(result.length() - 1); // remove last comma
					result.append("]");
				}
			}

			@Override
			public void visitOnPickedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
				List<EventAction> actions = lookup.getPickedActions();
				if ((actions != null) && (! actions.isEmpty())) {
					result.append(",\"pickedActions\":[");
				}
			}
			
			@Override
			public void visitedOnPickedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
				List<EventAction> actions = lookup.getPickedActions();
				if ((actions != null) && (! actions.isEmpty())) {
					result.setLength(result.length() - 1); // remove last comma
					result.append("]");
				}
			}
			
			@Override
			public void visitOnClearedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
				List<EventAction> actions = lookup.getClearedActions();
				if ((actions != null) && (! actions.isEmpty())) {
					result.append(",\"clearedActions\":[");
				}
			}
			
			@Override
			public void visitedOnClearedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
				List<EventAction> actions = lookup.getClearedActions();
				if ((actions != null) && (! actions.isEmpty())) {
					result.setLength(result.length() - 1); // remove last comma
					result.append("]");
				}
			}
			
			@Override
			public void visitToggleVisibilityEventAction(ToggleVisibilityEventAction toggleVisibility,
															boolean parentVisible,
															boolean parentEnabled) {
				result.append("{\"type\":\"toggleVisibility\"}");
			}
			
			@Override
			public void visitToggleDisabledEventAction(ToggleDisabledEventAction toggleDisabled,
														boolean parentVisible,
														boolean parentEnabled) {
				result.append("{\"type\":\"toggleDisabled\"}");
			}
			
			@Override
			public void visitSetInvisibleEventAction(SetInvisibleEventAction setInvisible,
														boolean parentVisible,
														boolean parentEnabled) {
				result.append("{\"type\":\"setInvisible\"}");
			}
			
			@Override
			public void visitSetDisabledEventAction(SetDisabledEventAction setDisabled,
														boolean parentVisible,
														boolean parentEnabled) {
				result.append("{\"type\":\"setDisabled\"}");
			}
			
			@Override
			public void visitRerenderEventAction(RerenderEventAction rerender,
													EventSource source,
													boolean parentVisible,
													boolean parentEnabled) {
				result.append("{\"type\":\"rerender\"}");
			}
			
			
			@Override
			public void visitServerSideActionEventAction(Action action, ServerSideActionEventAction server) {
				result.append("{\"type\":\"server\"}");
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
			
			private void processSize(AbsoluteWidth size) {
				Integer value = size.getPixelWidth();
				if (value != null) {
					result.append(",\"pixelWidth\":").append(value);
				}
				if (size instanceof AbsoluteSize) {
					value = ((AbsoluteSize) size).getPixelHeight();
					if (value != null) {
						result.append(",\"pixelHeight\":").append(value);
					}
					if (size instanceof RelativeSize) {
						RelativeSize relative = (RelativeSize) size;
						value = relative.getPercentageWidth();
						if (value != null) {
							result.append(",\"percentageWidth\":").append(value);
						}
						value = relative.getResponsiveWidth();
						if (value != null) {
							result.append(",\"responsiveWidth\":").append(value);
						}
						value = relative.getSm();
						if (value != null) {
							result.append(",\"sm\":").append(value);
						}
						value = relative.getMd();
						if (value != null) {
							result.append(",\"md\":").append(value);
						}
						value = relative.getLg();
						if (value != null) {
							result.append(",\"lg\":").append(value);
						}
						value = relative.getXl();
						if (value != null) {
							result.append(",\"xl\":").append(value);
						}
						value = relative.getPercentageHeight();
						if (value != null) {
							result.append(",\"percentageHeight\":").append(value);
						}
						
						if (size instanceof ShrinkWrapper) {
							ShrinkWrap wrap = ((ShrinkWrapper) size).getShrinkWrap();
							if (wrap != null) {
								result.append(",\"shrinkWrap\":\"").append(wrap).append('"');
							}
						}
					}
				}
				if (size instanceof MinimumHeight) {
					value = ((MinimumHeight) size).getMinPixelHeight();
					if (value != null) {
						result.append(",\"minPixelHeight\":").append(value);
					}
					if (size instanceof ConstrainableHeight) {
						value = ((ConstrainableHeight) size).getMaxPixelHeight();
						if (value != null) {
							result.append(",\"maxPixelHeight\":").append(value);
						}
						if (size instanceof ConstrainableSize) {
							value = ((ConstrainableSize) size).getMinPixelWidth();
							if (value != null) {
								result.append(",\"minPixelWidth\":").append(value);
							}
							value = ((ConstrainableSize) size).getMaxPixelWidth();
							if (value != null) {
								result.append(",\"maxPixelWidth\":").append(value);
							}
						}
					}
				}
			}
			
			private void processBorder(Bordered bordered, String borderTitle) {
				Boolean border = bordered.getBorder();
				if (Boolean.TRUE.equals(border)) {
					result.append(",\"border\":true");
					if (borderTitle != null) {
						result.append(",\"borderTitle\":\"").append(OWASP.escapeJsonString(borderTitle)).append('"');
					}
				}
			}
			
			private void processBox(Box box) {
				Integer value = box.getPixelPadding();
				if (value != null) {
					result.append(",\"pixelPadding\":").append(value);
				}
				value = box.getPixelMemberPadding();
				if (value != null) {
					result.append(",\"pixelMemberPadding\":").append(value);
				}
			}
			
			private void processIdentifiable(Identifiable identifiable) {
				String widgetId = identifiable.getWidgetId();
				if (widgetId != null) {
					result.append(",\"widgetId\":\"").append(widgetId).append('"');
				}
			}

			private void processInvisible(Invisible invisible) {
				MetaDataServlet.processInvisible(invisible, result);
			}
			
			private void processDisableable(Disableable disableable) {
				MetaDataServlet.processDisableable(disableable, result);
			}
			
			private void processDecorated(DecoratedMetaData decorated) {
				MetaDataServlet.processDecorated(decorated, result);
			}
			
			private void processInputWidget(InputWidget widget) {
				processBound(widget);
				processDisableable(widget);
				processInvisible(widget);
			}

			private void processBound(Bound bound) {
				result.append(",\"bound\":\"").append(bound.getBinding()).append('"');
			}

			private void processEditable(org.skyve.metadata.view.Editable editable) {
				Boolean value = editable.getEditable();
				if (value != null) {
					result.append(",\"editable\":").append(value);
				}
			}
			
			private void processFilterable(Filterable filterable) {
				processParameterizable(filterable);
				List<FilterParameter> filters = filterable.getFilterParameters();
				if ((filters != null) && (! filters.isEmpty())) {
					result.append(",\"filterParameters\":[");
					for (FilterParameter filter : filters) {
						result.append("{\"filterBinding\":\"").append(filter.getFilterBinding());
						result.append(",\"operator\":\"").append(filter.getOperator()).append('"');
						String string = filter.getValue();
						if (string != null) {
							result.append(",\"value\":\"").append(OWASP.escapeJsonString(string)).append('"');
						}
						string = filter.getValueBinding();
						if (string != null) {
							result.append(",\"valueBinding\":\"").append(OWASP.escapeJsonString(string)).append('"');
						}
						result.append("},");
					}
					result.setLength(result.length() - 1); // remove last comma
					result.append(']');
				}
			}
			
			private void processParameterizable(Parameterizable parameterizable) {
				MetaDataServlet.processParameterizable(parameterizable, result);
			}
			
			private void processTextOutput(TextOutput output) {
				Boolean escape = output.getEscape();
				if (escape != null) {
					result.append(",\"escape\":").append(escape);
				}
				Sanitisation sanitise = output.getSanitise();
				if (sanitise != null) {
					result.append(",\"sanitise\":\"").append(sanitise).append('"');
				}
			}
			
			private void processDisableableCRUDGrid(DisableableCRUDGrid grid) {
				String condish = grid.getDisableAddConditionName();
				if (condish != null) {
					result.append(",\"disableAddConditionName\":\"").append(condish).append('"');
				}
				condish = grid.getDisableEditConditionName();
				if (condish != null) {
					result.append(",\"disableEditConditionName\":\"").append(condish).append('"');
				}
				condish = grid.getDisableRemoveConditionName();
				if (condish != null) {
					result.append(",\"disableRemoveConditionName\":\"").append(condish).append('"');
				}
				condish = grid.getDisableZoomConditionName();
				if (condish != null) {
					result.append(",\"disableZoomConditionName\":\"").append(condish).append('"');
				}
			}
			
			private void processAbstractListWidget(AbstractListWidget list) {
				String string = list.getQueryName();
				if (string != null) {
					result.append(",\"queryName\":\"").append(string).append('"');
				}
				string = list.getModelName();
				if (string != null) {
					result.append(",\"modelName\":\"").append(string).append('"');
				}
				string = list.getPostRefreshConditionName();
				if (string != null) {
					result.append(",\"postRefreshConditionName\":\"").append(string).append('"');
				}
				processSize(list);
				processInvisible(list);
				processFilterable(list);
			}
			
			private void processMetaDataQueryColumn(MetaDataQueryColumn column) {
				String string = column.getLocalisedDisplayName();
				if (string != null) {
					result.append(",\"lobel\":\"").append(string).append('"');
				}
				string = column.getName();
				if (string != null) {
					result.append(",\"name\":\"").append(string).append('"');
				}
				string = column.getBinding();
				if (string != null) {
					result.append(",\"binding\":\"").append(string).append('"');
				}
				FilterOperator filterOperator = column.getFilterOperator();
				if (filterOperator != null) {
					result.append(",\"filterOperator\":\"").append(filterOperator).append('"');
				}
				string = column.getFilterExpression();
				if (string != null) {
					result.append(",\"filterExpression\":\"").append(OWASP.escapeJsonString(string)).append('"');
				}
				SortDirection sortOrder = column.getSortOrder();
				if (sortOrder != null) {
					result.append(",\"sortOrder\":\"").append(sortOrder).append('"');
				}
				result.append(",\"hidden\":").append(column.isHidden());
				Integer pixelWidth = column.getPixelWidth();
				if (pixelWidth != null) {
					result.append(",\"pixelWidth\":").append(pixelWidth);
				}
				HorizontalAlignment alignment = column.getAlignment();
				if (alignment != null) {
					result.append(",\"alignment\":\"").append(alignment).append('"');
				}
			}
			
			private void processChartBuilderOrderMetaData(ChartBuilderOrderMetaData order) {
				result.append("\"by\":\"").append(order.getBy());
				result.append(",\"sort\":\"").append(order.getSort()).append('"');
			}
			
			private void processAction(String label,
										String iconUrl,
										String iconStyleClass,
										String toolTip,
										String confirmationText,
										char type,
										String className,
										ActionImpl action) {
				actionsJSON.append(",\"actionType\":\"").append(type).append('"');
				String string = action.getName();
				if (string != null) {
					actionsJSON.append(",\"name\":\"").append(string).append('"');
				}
				if (className != null) {
					actionsJSON.append(",\"className\":\"").append(className).append('"');
				}
				string = action.getResourceName();
				if (string != null) {
					actionsJSON.append(",\"resourceName\":\"").append(string).append('"');
				}
				if (label != null) {
					actionsJSON.append(",\"label\":\"").append(OWASP.escapeJsonString(label)).append('"');
				}
				if (iconStyleClass != null) {
					actionsJSON.append(",\"fontIcon\":\"").append(iconStyleClass).append('"');
				}
				if (iconUrl != null) {
					actionsJSON.append(",\"iconUrl\":\"").append(OWASP.escapeJsonString(iconUrl)).append('"');
				}
				actionsJSON.append(",\"clientValidation\":").append(! Boolean.FALSE.equals(action.getClientValidation()));
				actionsJSON.append(",\"inActionPanel\":").append(! Boolean.FALSE.equals(action.getInActionPanel()));
				ActionShow show = action.getShow();
				if (show != null) {
					actionsJSON.append(",\"show\":\"").append(show).append('"');
				}
				if (confirmationText != null) {
					actionsJSON.append(",\"confirm\":\"").append(OWASP.escapeJsonString(confirmationText)).append('"');
				}
				if (toolTip != null) {
					actionsJSON.append(",\"toolTip\":\"").append(OWASP.escapeJsonString(toolTip)).append('"');
				}
				MetaDataServlet.processDisableable(action, actionsJSON);
				MetaDataServlet.processInvisible(action, actionsJSON);
				MetaDataServlet.processParameterizable(action, actionsJSON);
				MetaDataServlet.processDecorated(action, actionsJSON);
			}
		}.visit();
		
		return result;
	}
	
	private static void processParameterizable(Parameterizable parameterizable, StringBuilder json) {
		List<Parameter> parameters = parameterizable.getParameters();
		if ((parameters != null) && (! parameters.isEmpty())) {
			json.append(",\"parameters\":[");
			for (Parameter parameter : parameters) {
				json.append("{\"name\":\"").append(parameter.getName()).append('"');
				String string = parameter.getValue();
				if (string != null) {
					json.append(",\"value\":\"").append(OWASP.escapeJsonString(string)).append('"');
				}
				string = parameter.getValueBinding();
				if (string != null) {
					json.append(",\"valueBinding\":\"").append(OWASP.escapeJsonString(string)).append('"');
				}
				json.append("},");
			}
			json.setLength(json.length() - 1); // remove last comma
			json.append(']');
		}
	}
	
	private static void processDecorated(DecoratedMetaData decorated, StringBuilder json) {
		Map<String, String> properties = decorated.getProperties();
		if ((properties != null) && (! properties.isEmpty())) {
			json.append(",\"properties\":{");
			for (Entry<String, String> entry : properties.entrySet()) {
				json.append('"').append(entry.getKey()).append("\":\"").append(entry.getValue()).append("\",");
			}
			json.setLength(json.length() - 1); // remove comma
			json.append('}');
		}
	}

	private static void processInvisible(Invisible invisible, StringBuilder json) {
		String name = invisible.getInvisibleConditionName();
		if (name != null) {
			json.append(",\"invisibleConditionName\":\"").append(name).append('"');
		}
	}
	
	private static void processDisableable(Disableable disableable, StringBuilder json) {
		String name = disableable.getDisabledConditionName();
		if (name != null) {
			json.append(",\"disabledConditionName\":\"").append(name).append('"');
		}
	}
	
	private static String emptyResponse() {
		return "{\"menus\":[],\"dataSources\":[]}";
	}
}
