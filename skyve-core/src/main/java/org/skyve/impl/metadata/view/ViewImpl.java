package org.skyve.impl.metadata.view;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
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
import org.skyve.impl.metadata.repository.view.access.ViewUserAccessMetaData;
import org.skyve.impl.metadata.repository.view.access.ViewUserAccessUxUiMetadata;
import org.skyve.impl.metadata.repository.view.access.ViewUserAccessesMetaData;
import org.skyve.impl.metadata.view.component.Component;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.model.ModelMetaData;
import org.skyve.impl.metadata.view.model.chart.ChartBuilderMetaData;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CompleteType;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.MetaDataException;
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
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.util.Binder.TargetMetaData;

/**
 * Runtime implementation of the {@link View} metadata interface.
 *
 * <p>Holds the fully resolved view structure: widget tree ({@link Container}),
 * action list, sidebar, access-control entries, auto-refresh settings, and
 * layout parameters.  Instances are produced by converting a
 * {@link org.skyve.impl.metadata.repository.view.ViewMetaData} descriptor
 * during repository bootstrap.
 *
 * <p>Threading: not thread-safe.  Instances are read-only once placed in
 * the repository cache.
 *
 * @see View
 * @see org.skyve.impl.metadata.repository.view.ViewMetaData
 */
public class ViewImpl extends Container implements View {
	private static final long serialVersionUID = -2621201277538515637L;
	private static final String IN_VIEW = " in view";

	private String name;
	private long lastModifiedMillis = Long.MAX_VALUE;
	private long lastCheckedMillis = System.currentTimeMillis();
	private String icon32x32RelativeFileName;
	private String iconStyleClass;
	private String helpRelativeFileName;
	private String helpURL;
	private String title;
	private String actionsWidgetId;
	private Sidebar sidebar;
	private LinkedHashMap<String, Action> actions = new LinkedHashMap<>();
	private Integer refreshTimeInSeconds;
	private String refreshConditionName;
	private String refreshActionName;
	private List<ViewParameter> parameters = new ArrayList<>();
	private String documentation;
	private String overriddenCustomerName;
	private String overriddenUxUiName;
	private Map<String, String> properties = new TreeMap<>();

	// map of modelName -> model metadata used to instantiate models on the server-side
	private transient Map<String, ModelMetaData> inlineModels = new TreeMap<>();

	// Cloned view structures or parts of structures that are used by components in other views
	// that reference this view.
	// Note that these are not required to be Serialized as ViewImpls are cloned by serialization to populate this
	private transient ComponentFragments fragments = new ComponentFragments(this);
	
	// All components in this view (one level deep)
	// This is populated in resolve() and is used to recursively determine accesses in getAccesses().
	private transient Set<Component> components = new HashSet<>();
	
	// Accesses for this view only (not component fragments)
	private transient Set<UserAccess> accesses = null;
	
	/**
	 * Returns the condition name controlling auto-refresh, or {@code null} if auto-refresh is unconditional.
	 *
	 * @return the condition name, or {@code null} if auto-refresh fires unconditionally
	 */
	@Override
	public String getRefreshConditionName() {
		return refreshConditionName;
	}

	/**
	 * Sets the condition name controlling auto-refresh.
	 *
	 * @param refreshConditionName the condition name to set
	 */
	public void setRefreshConditionName(String refreshConditionName) {
		this.refreshConditionName = refreshConditionName;
	}

	/**
	 * Returns the auto-refresh interval in seconds, or {@code null} if auto-refresh is disabled.
	 *
	 * @return the interval in seconds, or {@code null} if auto-refresh is disabled
	 */
	@Override
	public Integer getRefreshTimeInSeconds() {
		return refreshTimeInSeconds;
	}

	/**
	 * Sets the auto-refresh interval in seconds.
	 *
	 * @param refreshTimeInSeconds the interval in seconds, or {@code null} to disable auto-refresh
	 */
	public void setRefreshTimeInSeconds(Integer refreshTimeInSeconds) {
		this.refreshTimeInSeconds = refreshTimeInSeconds;
	}

	/**
	 * Returns the action name executed on each auto-refresh, or {@code null} if none is configured.
	 *
	 * @return the action name, or {@code null} if none is configured
	 */
	@Override
	public String getRefreshActionName() {
		return refreshActionName;
	}

	/**
	 * Sets the action name executed on each auto-refresh.
	 *
	 * @param refreshActionName the action name to set
	 */
	public void setRefreshActionName(String refreshActionName) {
		this.refreshActionName = refreshActionName;
	}

	/**
	 * Returns the logical name of this view (e.g. {@code edit}, {@code list}).
	 *
	 * @return the logical view name
	 */
	@Override
	public String getName() {
		return name;
	}

	/**
	 * Sets the logical name of this view.
	 *
	 * @param name the logical name to assign
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * Returns the last-modified timestamp of the view source file in epoch milliseconds.
	 * Defaults to {@link Long#MAX_VALUE} when no file-system timestamp has been recorded.
	 *
	 * @return the last-modified timestamp in epoch milliseconds
	 */
	@Override
	public long getLastModifiedMillis() {
		return lastModifiedMillis;
	}
	
	/**
	 * Sets the last-modified timestamp of the view source file in epoch milliseconds.
	 *
	 * @param lastModifiedMillis the last-modified timestamp in epoch milliseconds
	 */
	public void setLastModifiedMillis(long lastModifiedMillis) {
		this.lastModifiedMillis = lastModifiedMillis;
	}
	
	/**
	 * Returns the timestamp at which the view source was last polled for changes, in epoch milliseconds.
	 * Initialised to {@link System#currentTimeMillis()} at construction.
	 *
	 * @return the last-polled timestamp in epoch milliseconds
	 */
	@Override
	public long getLastCheckedMillis() {
		return lastCheckedMillis;
	}

	/**
	 * Sets the timestamp at which the view source was last polled for changes.
	 *
	 * @param lastCheckedMillis the last-polled timestamp in epoch milliseconds
	 */
	@Override
	public void setLastCheckedMillis(long lastCheckedMillis) {
		this.lastCheckedMillis = lastCheckedMillis;
	}

	/**
	 * Returns the project-relative file name of the 32x32 icon for this view, or {@code null} if none.
	 *
	 * @return the project-relative icon file name, or {@code null} if none
	 */
	@Override
	public String getIcon32x32RelativeFileName() {
		return icon32x32RelativeFileName;
	}

	/**
	 * Sets the project-relative file name of the 32x32 icon for this view.
	 *
	 * @param icon32x32RelativeFileName the project-relative icon file name
	 */
	public void setIcon32x32RelativeFileName(String icon32x32RelativeFileName) {
		this.icon32x32RelativeFileName = icon32x32RelativeFileName;
	}

	/**
	 * Returns the CSS style class for the view icon, or {@code null} if none.
	 *
	 * @return the CSS style class, or {@code null} if none
	 */
	@Override
	public String getIconStyleClass() {
		return iconStyleClass;
	}

	/**
	 * Sets the CSS style class for the view icon.
	 *
	 * @param iconStyleClass the CSS style class
	 */
	public void setIconStyleClass(String iconStyleClass) {
		this.iconStyleClass = iconStyleClass;
	}

	/**
	 * Returns the project-relative file name of the help document, or {@code null} if none.
	 *
	 * @return the project-relative help document file name, or {@code null} if none
	 */
	@Override
	public String getHelpRelativeFileName() {
		return helpRelativeFileName;
	}

	/**
	 * Sets the project-relative file name of the help document.
	 *
	 * @param helpRelativeFileName the project-relative help document file name
	 */
	public void setHelpRelativeFileName(String helpRelativeFileName) {
		this.helpRelativeFileName = helpRelativeFileName;
	}

	/**
	 * Returns the URL of the external help page, or {@code null} if none.
	 *
	 * @return the external help page URL, or {@code null} if none
	 */
	@Override
	public String getHelpURL() {
		return helpURL;
	}

	/**
	 * Sets the URL of the external help page.
	 *
	 * @param helpURL the external help page URL
	 */
	public void setHelpURL(String helpURL) {
		this.helpURL = helpURL;
	}
	
	/**
	 * Returns the display title of this view, or {@code null} if not explicitly set.
	 *
	 * @return the display title, or {@code null} if not explicitly set
	 */
	@Override
	public String getTitle() {
		return title;
	}

	/**
	 * Sets the display title of this view.
	 *
	 * @param title the display title
	 */
	public void setTitle(String title) {
		this.title = title;
	}

	/**
	 * Returns the widget ID used to locate the actions area, or {@code null} for default positioning.
	 *
	 * @return the widget ID, or {@code null} for default positioning
	 */
	@Override
	public String getActionsWidgetId() {
		return actionsWidgetId;
	}

	/**
	 * Sets the widget ID of the actions area.
	 *
	 * @param actionsWidgetId the widget ID of the actions area
	 */
	public void setActionsWidgetId(String actionsWidgetId) {
		this.actionsWidgetId = actionsWidgetId;
	}

	/**
	 * Returns the action registered under {@code actionName}, or {@code null} if not found.
	 *
	 * @param actionName the key to look up
	 * @return the matching action, or {@code null} if not found
	 */
	@Override
	public Action getAction(String actionName) {
		return actions.get(actionName);
	}

	/**
	 * Adds or replaces an action keyed by its resolved action name.
	 *
	 * <p>Side effects: mutates the internal action map for this view. If another action
	 * already uses the same name, the existing entry is replaced.
	 *
	 * @param action the action metadata to register
	 */
	public void putAction(Action action) {
		actions.put(action.getName(), action);
	}

	/**
	 * Returns an ordered collection of all actions defined for this view.
	 *
	 * @return ordered collection of all actions; never {@code null}
	 */
	@Override
	public Collection<Action> getActions() {
		return actions.values();
	}

	/**
	 * Returns the sidebar container, or {@code null} if this view has no sidebar.
	 *
	 * @return the sidebar container, or {@code null} if this view has no sidebar
	 */
	@Override
	public Sidebar getSidebar() {
		return sidebar;
	}

	/**
	 * Sets the sidebar container for this view.
	 *
	 * @param sidebar the sidebar container, or {@code null} to remove it
	 */
	public void setSidebar(Sidebar sidebar) {
		this.sidebar = sidebar;
	}

	/**
	 * Returns parameters that may be populated when creating a new record via URL or action navigation.
	 *
	 * @return the list of view parameters; never {@code null}
	 */
	@Override
	public List<ViewParameter> getParameters() {
		return parameters;
	}

	/**
	 * Returns the documentation string for this view, or {@code null} if none.
	 *
	 * @return the documentation string, or {@code null} if none
	 */
	@Override
	public String getDocumentation() {
		return documentation;
	}

	/**
	 * Sets the documentation string for this view.
	 *
	 * @param documentation the documentation string
	 */
	public void setDocumentation(String documentation) {
		this.documentation = documentation;
	}
	
	/**
	 * Returns the inline model metadata registered under {@code modelName},
	 * or {@code null} if the model is defined externally.
	 *
	 * @param modelName the key under which the model was registered
	 * @return the inline model metadata, or {@code null} if the model is defined externally
	 */
	@Override
	public ModelMetaData getInlineModel(String modelName) {
		return inlineModels.get(modelName);
	}
	
	/**
	 * Returns the customer name this view overrides, or {@code null} if it is not a customer-specific override.
	 *
	 * @return the customer name, or {@code null} if this is not a customer-specific override
	 */
	@Override
	public String getOverriddenCustomerName() {
		return overriddenCustomerName;
	}

	/**
	 * Sets the customer name this view overrides.
	 *
	 * @param overriddenCustomerName the customer name
	 */
	public void setOverriddenCustomerName(String overriddenCustomerName) {
		this.overriddenCustomerName = overriddenCustomerName;
	}

	/**
	 * Returns the UX/UI name this view overrides, or {@code null} if it is not a UX/UI-specific override.
	 *
	 * @return the UX/UI name, or {@code null} if this is not a UX/UI-specific override
	 */
	@Override
	public String getOverriddenUxUiName() {
		return overriddenUxUiName;
	}

	/**
	 * Sets the UX/UI name this view overrides.
	 *
	 * @param overriddenUxUiName the UX/UI name
	 */
	public void setOverriddenUxUiName(String overriddenUxUiName) {
		this.overriddenUxUiName = overriddenUxUiName;
	}

	/**
	 * Returns the mutable property map for this view, sorted by key.
	 *
	 * @return the mutable, key-sorted property map; never {@code null}
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}

	/**
	 * Returns the effective access set for this view, including linked component fragments.
	 *
	 * <p>When component fragments are present, this method translates component-local access
	 * entries into the owning document context (module/document rebinding and binding-prefix
	 * adjustments) before merging into the view-level set.
	 *
	 * <p>Side effects: may append translated access entries to the cached access set.
	 *
	 * @param customer the resolved customer used to load component fragments
	 * @param document the owning document context for translation of component accesses
	 * @param uxui the active UX/UI profile used to resolve component variants
	 * @return the effective access set for this view and its component fragments,
	 *         or {@code null} if access control has not been enabled and accesses have not been generated
	 */
	public Set<UserAccess> getAccesses(CustomerImpl customer, Document document, String uxui) {
		Set<UserAccess> result = accesses;
		if (result != null) { // accesses were generated
			for (Component component : components) {
				String componentBinding = component.getBinding();
				ViewImpl fragment = component.getFragment(customer, uxui);
				Set<UserAccess> componentAccesses = fragment.getAccesses(customer, document, uxui);
				for (UserAccess componentAccess : componentAccesses) {
					// Change the module name for query and document aggregates as the query or document 
					// is imported into the referencing module.
					if (componentAccess.isQueryAggregate()) {
						if (! document.getOwningModuleName().equals(componentAccess.getModuleName())) {
							result.add(UserAccess.queryAggregate(document.getOwningModuleName(), componentAccess.getComponent()));
							continue;
						}
					}
					else if (componentAccess.isDocumentAggregate()) {
						if (! document.getOwningModuleName().equals(componentAccess.getModuleName())) {
							result.add(UserAccess.documentAggregate(document.getOwningModuleName(), componentAccess.getComponent()));
							continue;
						}
					}
					// Change the module and document name for model aggregates as these are required
					// to be defined on each document they are referenced from.
					else if (componentAccess.isModelAggregate()) {
						if (! (document.getOwningModuleName().equals(componentAccess.getModuleName()) &&
								document.getName().equals(componentAccess.getDocumentName()))) {
							result.add(UserAccess.modelAggregate(document.getOwningModuleName(), document.getName(), componentAccess.getComponent()));
							continue;
						}
					}
					// Change the module and document name and place the binding prefix on the binding, if a bound component
					else if (componentAccess.isContent()) {
						if (componentBinding != null) {
							result.add(UserAccess.content(document.getOwningModuleName(),
															document.getName(),
															componentBinding + '.' + componentAccess.getComponent()));
							continue;
						}
					}
					// Change the module and document name and place the binding prefix on the binding, if a bound component
					else if (componentAccess.isPreviousComplete()) {
						if (componentBinding != null) {
							result.add(UserAccess.previousComplete(document.getOwningModuleName(),
																	document.getName(),
																	componentBinding + '.' + componentAccess.getComponent()));
							continue;
						}
					}
					// Change the module and document name for dynamic images as these are required
					// to be defined on each document they are referenced from.
					else if (componentAccess.isDynamicImage()) {
						if (! (document.getOwningModuleName().equals(componentAccess.getModuleName()) &&
								document.getName().equals(componentAccess.getDocumentName()))) {
							result.add(UserAccess.dynamicImage(document.getOwningModuleName(), document.getName(), componentAccess.getComponent()));
							continue;
						}
					}

					// NB UserAccess.report have their own module and document in the definition and so require no translation
					// NB UserAccess.singular don't change either
					result.add(componentAccess);
				}
			}
		}

		return result;
	}

	/**
	 * Converts repository user-access metadata into this view's runtime access set.
	 *
	 * <p>Validates duplicate entries and UX/UI bindings before materialising
	 * {@link UserAccess} instances.
	 *
	 * @param module the owning module
	 * @param documentName the owning document name
	 * @param metaDataName source metadata name for diagnostics
	 * @param accessesMetaData repository access definitions, or {@code null}
	 */
	public void convertAccesses(Module module,
									String documentName,
									String metaDataName,
									ViewUserAccessesMetaData accessesMetaData) {
		final String moduleName = module.getName();
		
		// Populate User Accesses
		if (accessesMetaData != null) {
			accesses = new TreeSet<>();

			for (ViewUserAccessMetaData accessMetaData : accessesMetaData.getAccesses()) {
				// Validate access metadata
				accessMetaData.validate(metaDataName, module);

				// Validate and add ux/uis
				Set<String> uxuis = null;
				List<ViewUserAccessUxUiMetadata> uxuisMetaData = accessMetaData.getUxuis();
				if (uxuisMetaData.isEmpty()) {
					uxuis = UserAccess.ALL_UX_UIS;
				}
				else {
					uxuis = new TreeSet<>();
					for (ViewUserAccessUxUiMetadata uxuiMetaData : uxuisMetaData) {
						String uxuiName = uxuiMetaData.getName();
						if (uxuiName == null) {
							throw new MetaDataException(metaDataName + " : [name] is required for UX/UI in user access " + accessMetaData.toUserAccess(moduleName, documentName).toString() + IN_VIEW);
						}
						if (! uxuis.add(uxuiMetaData.getName())) {
							throw new MetaDataException(metaDataName + " : Duplicate UX/UI of " + uxuiMetaData.getName() + " in user access " + accessMetaData.toUserAccess(moduleName, documentName).toString() + IN_VIEW);
						}
					}
				}

				// Put into accesses
				if (! accesses.add(accessMetaData.toUserAccess(moduleName, documentName))) {
					throw new MetaDataException(metaDataName + " : Duplicate user access " + accessMetaData.toUserAccess(moduleName, documentName).toString() + IN_VIEW);
				}
			}
		}
	}
	
	/**
	 * Resolves this view for a specific UX/UI and document context.
	 *
	 * <p>Resolution links component references, captures inline model definitions,
	 * and either validates declared accesses or derives them by walking the view tree.
	 *
	 * <p>Side effects: mutates transient caches ({@code components}, {@code inlineModels},
	 * and optionally {@code accesses}) based on the supplied context.
	 *
	 * @param uxui the target UX/UI profile
	 * @param customer the active customer
	 * @param module the owning module
	 * @param document the owning document
	 * @param generate whether missing accesses should be generated when access control is enabled
	 */
	public void resolve(String uxui, Customer customer, Module module, Document document, boolean generate) {
		final String moduleName = module.getName();
		final String documentName = document.getName();

		// If there are no accesses defined in view metadata, or the view says to generate them, and access control is turned on, determine them
		boolean determineAccesses = UtilImpl.ACCESS_CONTROL && generate;
		if (determineAccesses && (accesses == null)) {
			accesses = new TreeSet<>();
		}

		new NoOpViewVisitor((CustomerImpl) customer, (ModuleImpl) module, (DocumentImpl) document, this, uxui) {
			@Override
			public void visitComponent(Component component, boolean parentVisible, boolean parentEnabled) {
				// Overrides visitComponent standard behaviour to link to the component
				component.link(currentUxUi, customer, module, document, name);
				components.add(component);

				// stop recursing through the component as we only want immediate (not recursive) accesses for this view
			}
			
			@Override
			public void visitContentImage(ContentImage image, boolean parentVisible, boolean parentEnabled) {
				if (determineAccesses) {
					String binding = image.getBinding();
					if (dataGridBinding != null) {
						StringBuilder sb = new StringBuilder(dataGridBinding.length() + 1 + binding.length());
						sb.append(dataGridBinding).append('.').append(binding);
						binding = sb.toString();
					}
					accesses.add(UserAccess.content(moduleName, documentName, binding));
				}
			}
			
			@Override
			public void visitContentLink(ContentLink link, boolean parentVisible, boolean parentEnabled) {
				if (determineAccesses) {
					String binding = link.getBinding();
					if (dataGridBinding != null) {
						StringBuilder sb = new StringBuilder(dataGridBinding.length() + 1 + binding.length());
						sb.append(dataGridBinding).append('.').append(binding);
						binding = sb.toString();
					}
					accesses.add(UserAccess.content(moduleName, documentName, binding));
				}
			}
			
			@Override
			public void visitContentSignature(ContentSignature signature, boolean parentVisible, boolean parentEnabled) {
				if (determineAccesses) {
					String binding = signature.getBinding();
					if (dataGridBinding != null) {
						StringBuilder sb = new StringBuilder(dataGridBinding.length() + 1 + binding.length());
						sb.append(dataGridBinding).append('.').append(binding);
						binding = sb.toString();
					}
					accesses.add(UserAccess.content(moduleName, documentName, binding));
				}
			}
			
			@Override
			public void visitChart(Chart chart, boolean parentVisible, boolean parentEnabled) {
				String modelName = chart.getModelName();

				// Add any inlined models to the view
				ChartBuilderMetaData metaDataModel = chart.getModel();
				if (metaDataModel != null) {
					modelName = metaDataModel.getModelName();
					inlineModels.put(modelName, metaDataModel);
				}
				else { // not inlined
					 // determine access if required
					 if (determineAccesses) {
						accesses.add(UserAccess.modelAggregate(moduleName, documentName, modelName));
					}
				}
			}

			private String dataGridBinding = null;
			
			@Override
			public void visitDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
				if (determineAccesses) {
					dataGridBinding = grid.getBinding();
					if (! (Boolean.FALSE.equals(grid.getShowAdd()) && Boolean.FALSE.equals(grid.getShowZoom()))) {
						accessThroughBinding(dataGridBinding);
					}
				}
			}

			@Override
			public void visitedDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
				if (determineAccesses) {
					dataGridBinding = null;
				}
			}
			
			// NB DataRepeater cannot zoom in
			
			@Override
			public void visitDynamicImage(DynamicImage image, boolean parentVisible, boolean parentEnabled) {
				if (determineAccesses) {
					String imageName = image.getName();
					accesses.add(UserAccess.dynamicImage(moduleName, documentName, imageName));
				}
			}

			@Override
			public void visitListGrid(ListGrid grid, boolean parentVisible, boolean parentEnabled) {
				if (determineAccesses) {
					String modelName = grid.getModelName();
					String queryName = grid.getQueryName();
					if (modelName != null) {
						accesses.add(UserAccess.modelAggregate(moduleName, documentName, modelName));
					}
					else {
						accesses.add(UserAccess.queryAggregate(moduleName, queryName));
					}
	
					if (! (Boolean.FALSE.equals(grid.getShowAdd()) && Boolean.FALSE.equals(grid.getShowZoom()))) {
						if (modelName != null) {
							ListModel<Bean> model = document.getListModel(customer, modelName, false);
							Document drivingDocument = model.getDrivingDocument();
							if (drivingDocument != null) {
								String drivingModuleName = drivingDocument.getOwningModuleName();
								String drivingDocumentName = drivingDocument.getName();
								accesses.add(UserAccess.singular(drivingModuleName, drivingDocumentName));
							}
						}
						else {
							MetaDataQueryDefinition query = module.getNullSafeMetaDataQuery(queryName);
							String drivingDocumentName = query.getDocumentName();
							Module drivingModule = query.getDocumentModule(customer);
							String drivingModuleName = drivingModule.getName();
							accesses.add(UserAccess.singular(drivingModuleName, drivingDocumentName));
						}
					}
				}
			}
			
			@Override
			public void visitListRepeater(ListRepeater repeater, boolean parentVisible, boolean parentEnabled) {
				if (determineAccesses) {
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
			}
			
			@Override
			public void visitLookupDescription(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
				if (determineAccesses) {
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
					Relation targetRelation = (Relation) target.getAttribute();
					// This should never happen as the bindings should be validated by now
					if (targetRelation == null) {
						throw new MetaDataException(binding + " does not point anywhere from context document " + document.getName());
					}
					String targetDocumentName = targetRelation.getDocumentName();
	
					// Check lookup query
					String queryName = lookup.getQuery();
					// Maybe the relation is a reference and has a query
					if (queryName == null) {
						if (targetRelation instanceof Reference reference) {
							queryName = reference.getQueryName();
						}
					}
					// Look for the default query for the relation document name
					if (queryName == null) {
						DocumentRef ref = module.getDocumentRefs().get(targetDocumentName);
						queryName = ref.getDefaultQueryName();
					}
	
					// add the query aggregate or a document aggregate
					if (queryName == null) {
						accesses.add(UserAccess.documentAggregate(moduleName, targetDocumentName));
					}
					else {
						accesses.add(UserAccess.queryAggregate(moduleName, queryName));
					}
	
					// If not in a grid and is editable, add the zoom in access
					if ((dataGridBinding == null) && (! Boolean.FALSE.equals(lookup.getEditable()))) {
						Document targetDocument = module.getDocument(customer, targetDocumentName);
						String targetModuleName = targetDocument.getOwningModuleName();
						accesses.add(UserAccess.singular(targetModuleName, targetDocumentName));
					}
				}
			}
			
			@Override
			public void visitMap(MapDisplay map, boolean parentVisible, boolean parentEnabled) {
				if (determineAccesses) {
					String modelName = map.getModelName();
					accesses.add(UserAccess.modelAggregate(moduleName, documentName, modelName));
	
					// NB Can't work out what the map can navigate to - needs to be added to the router manually.
				}
			}
			
			@Override
			public void visitTextField(TextField text, boolean parentVisible, boolean parentEnabled) {
				if (determineAccesses) {
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
			}
			/** {@inheritDoc} */
			
			@Override
			public void visitTreeGrid(TreeGrid grid, boolean parentVisible, boolean parentEnabled) {
				if (determineAccesses) {
					visitListGrid(grid, parentVisible, parentEnabled);
				}
			}
			/** {@inheritDoc} */
			
			@Override
			public void visitZoomIn(ZoomIn zoomIn, boolean parentVisible, boolean parentEnabled) {
				if (determineAccesses) {
					accessThroughBinding(zoomIn.getBinding());
				}
			}

			private void accessThroughBinding(String binding) {
				TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);

				Document relatedDocument = null;
				if (ChildBean.PARENT_NAME.equals(binding) || binding.endsWith(ChildBean.CHILD_PARENT_NAME_SUFFIX)) {
					relatedDocument = target.getDocument().getParentDocument(customer);
				}
				else {
					Relation targetRelation = (Relation) target.getAttribute();
					// This should never happen as the bindings should be validated by now
					if (targetRelation == null) {
						throw new MetaDataException(binding + " does not point anywhere from context document " + document.getName());
					}
					String relatedDocumentName = targetRelation.getDocumentName();
					relatedDocument = module.getDocument(customer, relatedDocumentName);
				}
				String relatedModuleName = relatedDocument.getOwningModuleName();
				accesses.add(UserAccess.singular(relatedModuleName, relatedDocument.getName()));
			}
			
			@Override
			public void visitReportAction(ActionImpl action) {
				String reportName = action.getResourceName();

				String reportModuleName = null;
				String reportDocumentName = null;

				// Determine report module and document names from parameters
				List<Parameter> reportParameters = action.getParameters();
				for (Parameter reportParameter : reportParameters) {
					String reportParameterName = reportParameter.getName();
					if (AbstractWebContext.MODULE_NAME.equals(reportParameterName)) {
						reportModuleName = reportParameter.getValue();
					}
					else if (AbstractWebContext.DOCUMENT_NAME.equals(reportParameterName)) {
						reportDocumentName = reportParameter.getValue();
					}
				}
				
				if ((reportModuleName == null) || (reportDocumentName == null)) {
					throw new MetaDataException("Cannot determine document for report " + reportName + " in document " + 
													document.getOwningModuleName() + "." + document.getName());
				}
				
				if (determineAccesses) {
					accesses.add(UserAccess.report(reportModuleName, reportDocumentName, reportName));
				}
			}
		}.visit();
	}
	
	/**
	 * Reinitialises transient runtime caches after deserialisation.
	 *
	 * @return this instance with transient state restored
	 */
	private Object readResolve() {
		inlineModels = new TreeMap<>();
		fragments = new ComponentFragments(this);
		components = new HashSet<>();

		return this;
	}
	
	/**
	 * Returns the resolved fragment view for a linked component.
	 *
	 * <p>The returned fragment is context-specific to customer, module, document,
	 * and UX/UI, and preserves whether access metadata has already been resolved.
	 *
	 * @param c the customer context
	 * @param m the module context
	 * @param d the document context
	 * @param uxui the active UX/UI profile
	 * @param component the component whose fragment should be resolved
	 * @return the resolved fragment view for the component
	 */
	public ViewImpl getFragment(CustomerImpl c, ModuleImpl m, DocumentImpl d, String uxui, Component component) {
		return fragments.get(c, m, d, uxui, component, (accesses != null));
	}
}
