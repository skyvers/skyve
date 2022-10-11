package org.skyve.impl.metadata.view;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.component.Component;
import org.skyve.impl.metadata.view.model.ModelMetaData;
import org.skyve.impl.metadata.view.model.chart.ChartBuilderMetaData;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CompleteType;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.user.UserAccess;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.util.Binder.TargetMetaData;

public class ViewImpl extends Container implements View {
	private static final long serialVersionUID = -2621201277538515637L;

	private String name;
	private long lastModifiedMillis = Long.MAX_VALUE;
	private String icon32x32RelativeFileName;
	private String iconStyleClass;
	private String helpRelativeFileName;
	private String helpURL;
	private String title;
	private String actionsWidgetId;
	private LinkedHashMap<String, Action> actions = new LinkedHashMap<>();
	private Integer refreshTimeInSeconds;
	private String refreshConditionName;
	private String refreshActionName;
	private List<ViewParameter> parameters = new ArrayList<>();
	private String documentation;
	// map of modelId -> model metadata used to instantiate models on the server-side
	private List<ModelMetaData> inlineModels = new ArrayList<>();
	private String overriddenCustomerName;
	private String overriddenUxUiName;
	private Map<String, String> properties = new TreeMap<>();
	
	// components clone its target view by Serialization - accesses are not required
	private transient Set<UserAccess> accesses;
	
	@Override
	public String getRefreshConditionName() {
		return refreshConditionName;
	}

	public void setRefreshConditionName(String refreshConditionName) {
		this.refreshConditionName = refreshConditionName;
	}

	@Override
	public Integer getRefreshTimeInSeconds() {
		return refreshTimeInSeconds;
	}

	public void setRefreshTimeInSeconds(Integer refreshTimeInSeconds) {
		this.refreshTimeInSeconds = refreshTimeInSeconds;
	}

	@Override
	public String getRefreshActionName() {
		return refreshActionName;
	}

	public void setRefreshActionName(String refreshActionName) {
		this.refreshActionName = refreshActionName;
	}

	@Override
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	@Override
	public long getLastModifiedMillis() {
		return lastModifiedMillis;
	}
	
	public void setLastModifiedMillis(long lastModifiedMillis) {
		this.lastModifiedMillis = lastModifiedMillis;
	}
	
	@Override
	public String getIcon32x32RelativeFileName() {
		return icon32x32RelativeFileName;
	}

	public void setIcon32x32RelativeFileName(String icon32x32RelativeFileName) {
		this.icon32x32RelativeFileName = icon32x32RelativeFileName;
	}

	@Override
	public String getIconStyleClass() {
		return iconStyleClass;
	}

	public void setIconStyleClass(String iconStyleClass) {
		this.iconStyleClass = iconStyleClass;
	}

	@Override
	public String getHelpRelativeFileName() {
		return helpRelativeFileName;
	}

	public void setHelpRelativeFileName(String helpRelativeFileName) {
		this.helpRelativeFileName = helpRelativeFileName;
	}

	@Override
	public String getHelpURL() {
		return helpURL;
	}

	public void setHelpURL(String helpURL) {
		this.helpURL = helpURL;
	}

	@Override
	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	@Override
	public String getActionsWidgetId() {
		return actionsWidgetId;
	}

	public void setActionsWidgetId(String actionsWidgetId) {
		this.actionsWidgetId = actionsWidgetId;
	}

	@Override
	public Action getAction(String actionName) {
		return actions.get(actionName);
	}

	public void putAction(Action action) {
		actions.put(action.getName(), action);
	}

	@Override
	public Collection<Action> getActions() {
		return actions.values();
	}

	/**
	 * These represent parameters that are allowed to be populated when creating a new record.
	 */
	@Override
	public List<ViewParameter> getParameters() {
		return parameters;
	}

	@Override
	public String getDocumentation() {
		return documentation;
	}

	public void setDocumentation(String documentation) {
		this.documentation = documentation;
	}
	
	/**
	 * Get an implicit inlined model - ie a chart model from a given modelId.
	 * @param modelIndex	The index of the model to get
	 * @return	The model.
	 */
	@Override
	public ModelMetaData getInlineModel(int modelIndex) {
		return inlineModels.get(modelIndex);
	}
	
	@Override
	public String getOverriddenCustomerName() {
		return overriddenCustomerName;
	}

	public void setOverriddenCustomerName(String overriddenCustomerName) {
		this.overriddenCustomerName = overriddenCustomerName;
	}

	@Override
	public String getOverriddenUxUiName() {
		return overriddenUxUiName;
	}

	public void setOverriddenUxUiName(String overriddenUxUiName) {
		this.overriddenUxUiName = overriddenUxUiName;
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
	
	/**
	 * Ensure that any component loaded matches the given UX/UI.
	 * Ensure that inlined metadata model definitions are added to the implicit models map.
	 * If accesses is not defined {through metadata convert()} then determine them.
	 */
	public void resolve(String uxui, Customer customer, Document document) {
		// First clone all components referenced
		// NB The 2 visitors below cannot be combined as cloning the components and visiting them
		// causes a stack overflow.

		final String moduleName = document.getOwningModuleName();
		final Module module = customer.getModule(moduleName);

		new NoOpViewVisitor((CustomerImpl) customer, (ModuleImpl) module, (DocumentImpl) document, this) {
			// Overrides visitComponent standard behaviour to load the component
			@Override
			public void visitComponent(Component component, boolean parentVisible, boolean parentEnabled) {
				component.setContained(uxui, customer, module, document, name);
			}
			
			@Override
			public void visitChart(Chart chart, boolean parentVisible, boolean parentEnabled) {
				ChartBuilderMetaData model = chart.getModel();
				if (model != null) {
					inlineModels.add(model);
					model.setModelIndex(inlineModels.size() - 1);
				}
			}
		}.visit();
		
		// If there are no accesses defined in view metadata, then determine them
		boolean determineAccesses = (accesses == null);
		if (determineAccesses) {
			accesses = new TreeSet<>();

			final String documentName = document.getName();
			new NoOpViewVisitor((CustomerImpl) customer, (ModuleImpl) module, (DocumentImpl) document, this) {
				@Override
				public void visitChart(Chart chart, boolean parentVisible, boolean parentEnabled) {
					ChartBuilderMetaData metaDataModel = chart.getModel();
					if (metaDataModel != null) {
						String modelModuleName = metaDataModel.getModuleName();
						String modelQueryName = metaDataModel.getQueryName();
						if (modelQueryName != null) {
							accesses.add(UserAccess.queryAggregate(modelModuleName, modelQueryName));
						}
						else {
							Module modelModule = customer.getModule(modelModuleName);
							String moduleDocumentName = metaDataModel.getDocumentName();
							DocumentRef ref = modelModule.getDocumentRefs().get(documentName);
							modelQueryName = ref.getDefaultQueryName();
							if (modelQueryName != null) {
								accesses.add(UserAccess.queryAggregate(modelModuleName, modelQueryName));
							}
							else {
								accesses.add(UserAccess.documentAggregate(modelModuleName, moduleDocumentName));
							}
						}
					}
					else {
						String modelName = chart.getModelName();
						accesses.add(UserAccess.modelAggregate(moduleName, documentName, modelName));
					}
				}
	
				private String dataGridBinding = null;
				
				@Override
				public void visitDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
					if (! (Boolean.FALSE.equals(grid.getShowAdd()) && Boolean.FALSE.equals(grid.getShowZoom()))) {
						dataGridBinding = grid.getBinding();
						accessThroughBinding(dataGridBinding);
					}
				}
	
				@Override
				public void visitedDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
					dataGridBinding = null;
				}
				
				// NB DataRepeater cannot zoom in
	
				@Override
				public void visitListGrid(ListGrid grid, boolean parentVisible, boolean parentEnabled) {
					String modelName = grid.getModelName();
					String queryName = grid.getQueryName();
					if (modelName != null) {
						accesses.add(UserAccess.modelAggregate(moduleName, documentName, modelName));
					}
					else {
						accesses.add(UserAccess.queryAggregate(moduleName, queryName));
					}
	
					if (! (Boolean.FALSE.equals(grid.getShowAdd()) && Boolean.FALSE.equals(grid.getShowZoom()))) {
						Document drivingDocument = null;
						String drivingModuleName = null;
						String drivingDocumentName = null;
						
						if (modelName != null) {
							ListModel<Bean> model = document.getListModel(customer, modelName, true);
							drivingDocument = model.getDrivingDocument();
							drivingModuleName = drivingDocument.getOwningModuleName();
							drivingDocumentName = drivingDocument.getName();
						}
						else {
							MetaDataQueryDefinition query = module.getMetaDataQuery(queryName);
							drivingDocumentName = query.getDocumentName();
							Module drivingModule = query.getDocumentModule(customer);
							drivingModuleName = drivingModule.getName();
							drivingDocument = drivingModule.getDocument(customer, drivingDocumentName);
						}
	
						accesses.add(UserAccess.singular(drivingModuleName, drivingDocumentName));
					}
				}
				
				@Override
				public void visitListRepeater(ListRepeater repeater, boolean parentVisible, boolean parentEnabled) {
					String modelName = repeater.getModelName();
					if (modelName != null) {
						accesses.add(UserAccess.modelAggregate(moduleName, documentName, modelName));
					}
					else {
						String queryName = repeater.getQueryName();
						accesses.add(UserAccess.queryAggregate(moduleName, queryName));
					}
					// NB ListRepeater cannot zoom in
				}
				
				@Override
				public void visitLookupDescription(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
					String binding = lookup.getBinding();
					if (dataGridBinding != null) {
						if (binding == null) { // binding can be null when placed in a data grid
							binding = dataGridBinding;
						}
						else {
							StringBuilder sb = new StringBuilder(dataGridBinding.length() + 1 + binding.length());
							sb.append(dataGridBinding).append('.').append(binding);
							binding = sb.toString();
						}
					}
					
					TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
					Reference targetReference = (Reference) target.getAttribute();
					String targetDocumentName = targetReference.getDocumentName();
	
					// Check lookup query
					String queryName = lookup.getQuery();
					// Maybe the reference has a query
					if (queryName == null) {
						queryName = targetReference.getQueryName();
					}
					// Look for the default query
					if (queryName == null) {
						queryName = module.getDocumentDefaultQuery(customer, targetDocumentName).getName();
					}
					// Otherwise use the document name
					queryName = targetDocumentName;
					accesses.add(UserAccess.queryAggregate(moduleName, queryName));
	
					if (! Boolean.FALSE.equals(lookup.getEditable())) {
						Document targetDocument = module.getDocument(customer, targetDocumentName);
						String targetModuleName = targetDocument.getOwningModuleName();
						accesses.add(UserAccess.singular(targetModuleName, targetDocumentName));
					}
				}
				
				@Override
				public void visitMap(MapDisplay map, boolean parentVisible, boolean parentEnabled) {
					if (! determineAccesses) {
						return;
					}
					
					String modelName = map.getModelName();
					accesses.add(UserAccess.modelAggregate(moduleName, documentName, modelName));
	
					// NB Can't work out what the map can navigate to - needs to be added to the router manually.
				}
				
				@Override
				public void visitTextField(TextField text, boolean parentVisible, boolean parentEnabled) {
					if (! determineAccesses) {
						return;
					}
	
					CompleteType type = text.getComplete();
					if (type == CompleteType.previous) {
						String binding = text.getBinding();
						if (dataGridBinding != null) {
							StringBuilder sb = new StringBuilder(dataGridBinding.length() + 1 + binding.length());
							sb.append(dataGridBinding).append('.').append(binding);
							binding = sb.toString();
						}
						accesses.add(UserAccess.previousComplete(moduleName, documentName, binding));
					}
				}
				
				@Override
				public void visitTreeGrid(TreeGrid grid, boolean parentVisible, boolean parentEnabled) {
					visitListGrid(grid, parentVisible, parentEnabled);
				}
				
				@Override
				public void visitZoomIn(ZoomIn zoomIn, boolean parentVisible, boolean parentEnabled) {
					if (! determineAccesses) {
						return;
					}
	
					accessThroughBinding(zoomIn.getBinding());
				}
	
				private void accessThroughBinding(String binding) {
					TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
	
					Document relatedDocument = null;
					if (ChildBean.PARENT_NAME.equals(binding) || binding.endsWith(ChildBean.CHILD_PARENT_NAME_SUFFIX)) {
						relatedDocument = target.getDocument().getParentDocument(customer);
					}
					else {
						Relation targetRelation = (Relation) target.getAttribute();
						String relatedDocumentName = targetRelation.getDocumentName();
						relatedDocument = module.getDocument(customer, relatedDocumentName);
					}
					@SuppressWarnings("null")
					String relatedModuleName = relatedDocument.getOwningModuleName();
					accesses.add(UserAccess.singular(relatedModuleName, relatedDocument.getName()));
				}
			}.visit();
		}
	}

	@Override
	public Set<UserAccess> getAccesses() {
		return accesses;
	}
}
