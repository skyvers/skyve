package org.skyve.impl.metadata.repository.view;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.repository.ConvertibleMetaData;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.repository.view.access.ViewUserAccessesMetaData;
import org.skyve.impl.metadata.repository.view.actions.ActionMetaData;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.view.View.ViewParameter;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB root element for a view descriptor ({@code edit.xml}, {@code list.xml}, etc.),
 * converted to a runtime {@link ViewImpl} during repository bootstrap.
 *
 * <p>A view descriptor holds the full widget tree for one named view, along with
 * the action list, sidebar, access-control entries, auto-refresh settings, and
 * properties.  Extends {@link Container} so that child widgets can be nested
 * using the standard JAXB element-ref polymorphism.
 *
 * <p>Threading: not thread-safe.  Instances are populated during JAXB unmarshalling
 * and are read-only once converted and placed in the repository cache.
 *
 * @see ViewImpl
 * @see Actions
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "view")
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE, 
			name = "view",
			propOrder = {"documentation",
							"sidebar",
							"actions", 
							"name", 
							"title",
							"iconStyleClass",
							"icon32x32RelativeFileName",
							"helpRelativeFileName",
							"helpURL",
							"refreshTimeInSeconds",
							"refreshConditionName", 
							"refreshActionName",
							"parameters",
							"accesses",
							"properties"})
public class ViewMetaData extends Container implements NamedMetaData, ConvertibleMetaData<ViewImpl>, DecoratedMetaData {
	private static final long serialVersionUID = -1831750070396044584L;
	private static final String VIEW_NAME_SEPARATOR = " view ";

	private String name;
	private String title;
	private String iconStyleClass;
	private String icon32x32RelativeFileName;
	private String helpRelativeFileName;
	private String helpURL;
	private Sidebar sidebar = null;
	private Actions actions = null;
	private Integer refreshTimeInSeconds;
	private String refreshConditionName;
	private String refreshActionName;
	private List<ViewParameter> parameters = new ArrayList<>();
	private ViewUserAccessesMetaData accesses = null;
	private String documentation;
	private long lastModifiedMillis = Long.MAX_VALUE;
	
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Returns the logical view name.
	 *
	 * @return view name
	 */
	@Override
	public String getName() {
		return name;
	}

	/**
	 * Sets the logical view name.
	 *
	 * @param name view name
	 */
	@XmlAttribute(required = true)
	public void setName(String name) {
		this.name = UtilImpl.processStringValue(name);
	}

	/**
	 * Returns the display title for this view.
	 *
	 * @return view title
	 */
	public String getTitle() {
		return title;
	}

	/**
	 * Sets the display title for this view.
	 *
	 * @param title view title
	 */
	@XmlAttribute(required = true)
	public void setTitle(String title) {
		this.title = UtilImpl.processStringValue(title);
	}

	/**
	 * Returns the optional 32x32 icon path for this view.
	 *
	 * @return icon path, or {@code null}
	 */
	public String getIcon32x32RelativeFileName() {
		return icon32x32RelativeFileName;
	}

	/**
	 * Sets the optional 32x32 icon path for this view.
	 *
	 * @param icon32x32RelativeFileName icon path
	 */
	@XmlAttribute(name = "icon32x32RelativeFileName")
	public void setIcon32x32RelativeFileName(String icon32x32RelativeFileName) {
		this.icon32x32RelativeFileName = UtilImpl.processStringValue(icon32x32RelativeFileName);
	}

	/**
	 * Returns the optional CSS style class used for the view icon.
	 *
	 * @return icon style class, or {@code null}
	 */
	public String getIconStyleClass() {
		return iconStyleClass;
	}

	/**
	 * Sets the optional CSS style class used for the view icon.
	 *
	 * @param iconStyleClass icon style class
	 */
	@XmlAttribute(name = "iconStyleClass")
	public void setIconStyleClass(String iconStyleClass) {
		this.iconStyleClass = UtilImpl.processStringValue(iconStyleClass);
	}

	/**
	 * Returns the optional help file path for this view.
	 *
	 * @return relative help file path, or {@code null}
	 */
	public String getHelpRelativeFileName() {
		return helpRelativeFileName;
	}

	/**
	 * Sets the optional help file path for this view.
	 *
	 * @param helpRelativeFileName relative help file path
	 */
	@XmlAttribute(name = "helpRelativeFileName")
	public void setHelpRelativeFileName(String helpRelativeFileName) {
		this.helpRelativeFileName = UtilImpl.processStringValue(helpRelativeFileName);
	}

	/**
	 * Returns the optional external help URL for this view.
	 *
	 * @return help URL, or {@code null}
	 */
	public String getHelpURL() {
		return helpURL;
	}

	/**
	 * Sets the optional external help URL for this view.
	 *
	 * @param helpURL help URL
	 */
	@XmlAttribute(name = "helpURL")
	public void setHelpURL(String helpURL) {
		this.helpURL = UtilImpl.processStringValue(helpURL);
	}

	/**
	 * Returns the optional sidebar container.
	 *
	 * @return sidebar definition, or {@code null}
	 */
	public Sidebar getSidebar() {
		return sidebar;
	}

	/**
	 * Sets the optional sidebar container.
	 *
	 * @param sidebar sidebar definition
	 */
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE, required = false, nillable = false)
	public void setSidebar(Sidebar sidebar) {
		this.sidebar = sidebar;
	}

	/**
	 * Returns the optional actions container.
	 *
	 * @return actions definition, or {@code null}
	 */
	public Actions getActions() {
		return actions;
	}

	/**
	 * Sets the optional actions container.
	 *
	 * @param actions actions definition
	 */
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE, required = false, nillable = false)
	public void setActions(Actions actions) {
		this.actions = actions;
	}

	/**
	 * Returns the optional auto-refresh interval in seconds.
	 *
	 * @return refresh interval in seconds, or {@code null}
	 */
	public Integer getRefreshTimeInSeconds() {
		return refreshTimeInSeconds;
	}

	/**
	 * Sets the optional auto-refresh interval in seconds.
	 *
	 * @param refreshTimeInSeconds refresh interval in seconds
	 */
	@XmlAttribute(name = "refreshTimeInSeconds", required = false)
	public void setRefreshTimeInSeconds(Integer refreshTimeInSeconds) {
		this.refreshTimeInSeconds = refreshTimeInSeconds;
	}

	/**
	 * Returns the optional refresh condition expression name.
	 *
	 * @return refresh condition name, or {@code null}
	 */
	public String getRefreshConditionName() {
		return refreshConditionName;
	}

	/**
	 * Sets the optional refresh condition expression name.
	 *
	 * @param refreshConditionName refresh condition name
	 */
	@XmlAttribute(name = "refreshIf", required = false)
	public void setRefreshConditionName(String refreshConditionName) {
		this.refreshConditionName = UtilImpl.processStringValue(refreshConditionName);
	}

	/**
	 * Returns the optional action name invoked during refresh.
	 *
	 * @return refresh action name, or {@code null}
	 */
	public String getRefreshActionName() {
		return refreshActionName;
	}

	/**
	 * Sets the optional action name invoked during refresh.
	 *
	 * @param refreshActionName refresh action name
	 */
	@XmlAttribute(name = "refreshAction", required = false)
	public void setRefreshActionName(String refreshActionName) {
		this.refreshActionName = UtilImpl.processStringValue(refreshActionName);
	}

	/**
	 * These represent parameters that are allowed to be populated when creating a new record.
	 */
	@XmlElementWrapper(namespace = XMLMetaData.VIEW_NAMESPACE, name = "newParameters")
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE,
					name = "parameter",
					type = ViewParameter.class,
					required = false)
	public List<ViewParameter> getParameters() {
		return parameters;
	}
	
	/**
	 * Returns user access entries declared for this view.
	 *
	 * @return access-control metadata, or {@code null}
	 */
	public ViewUserAccessesMetaData getAccesses() {
		return accesses;
	}

	/**
	 * Sets user access entries declared for this view.
	 *
	 * @param accesses access-control metadata
	 */
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE, required = false)
	public void setAccesses(ViewUserAccessesMetaData accesses) {
		this.accesses = accesses ;
	}

	/**
	 * Returns optional human-readable documentation for this view.
	 *
	 * @return documentation text, or {@code null}
	 */
	public String getDocumentation() {
		return documentation;
	}
	
	/**
	 * Sets optional human-readable documentation for this view.
	 *
	 * @param documentation documentation text
	 */
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setDocumentation(String documentation) {
		this.documentation = documentation;
	}

	/**
	 * Returns the source file last-modified timestamp for this descriptor.
	 *
	 * @return last-modified time in milliseconds
	 */
	@Override
	public long getLastModifiedMillis() {
		return lastModifiedMillis;
	}

	/**
	 * Sets the source file last-modified timestamp for this descriptor.
	 *
	 * @param lastModifiedMillis last-modified time in milliseconds
	 */
	@XmlTransient
	public void setLastModifiedMillis(long lastModifiedMillis) {
		this.lastModifiedMillis = lastModifiedMillis;
	}

	/**
	 * Converts this JAXB view descriptor into runtime view metadata.
	 *
	 * <p>Validates required title/name values, converts and validates actions,
	 * enforces refresh configuration consistency, and copies new-record parameter
	 * bindings and properties.
	 *
	 * @param metaDataName metadata path used in validation error messages
	 * @return the populated runtime view metadata
	 * @throws MetaDataException if required metadata is missing or inconsistent
	 */
	@Override
	public ViewImpl convert(String metaDataName) {
		ViewImpl result = new ViewImpl();
		result.setLastModifiedMillis(getLastModifiedMillis());
		
		String value = getTitle();
		if (value == null) {
			throw new MetaDataException(metaDataName + " : The view [title] is required for view " + metaDataName);
		}
		result.setTitle(value);

		result.setIconStyleClass(getIconStyleClass());
		result.setIcon32x32RelativeFileName(getIcon32x32RelativeFileName());

		result.setHelpRelativeFileName(getHelpRelativeFileName());
		result.setHelpURL(getHelpURL());
		
		String theName = getName();
		if (theName == null) {
			throw new MetaDataException(metaDataName + " : The view [name] is required for view " + metaDataName);
		}
		result.setName(theName);

		result.getContained().addAll(getContained());
		result.setSidebar(sidebar);
		if (actions != null) {
			result.setActionsWidgetId(actions.getWidgetId());
			for (ActionMetaData actionMetaData : actions.getActions()) {
				org.skyve.metadata.view.Action action = actionMetaData.toMetaDataAction();
				ImplicitActionName implicitName = action.getImplicitName();
				if (action.getResourceName() == null) {
					if (implicitName == null) { // custom action
						throw new MetaDataException(metaDataName + " : [className] is required for a custom action for " + 
														getName() + VIEW_NAME_SEPARATOR + metaDataName);
					}
					else if (ImplicitActionName.BizExport.equals(implicitName) || 
								ImplicitActionName.BizImport.equals(implicitName)) {
						throw new MetaDataException(metaDataName + " : [className] is required for a BizPort action for " +
														getName() + VIEW_NAME_SEPARATOR + metaDataName);
					}
					else if (ImplicitActionName.Report.equals(implicitName)) {
						throw new MetaDataException(metaDataName + " : [reportName] is required for a report action for " + 
														getName() + VIEW_NAME_SEPARATOR + metaDataName);
					}
				}
				else {
					value = actionMetaData.getDisplayName();
					if (value == null) {
						throw new MetaDataException(metaDataName + " : The view action [displayName] is required for the designer defined action " +
														action.getResourceName() + " for " + getName() + VIEW_NAME_SEPARATOR + metaDataName);
					}
				}

				if (result.getAction(action.getName()) != null) {
					throw new MetaDataException(metaDataName + " : The view action named " + action.getName() + " is defined in the " +
													getName() + " view multiple times");
				}
				result.putAction(action);
			}
		}

		result.setRefreshTimeInSeconds(getRefreshTimeInSeconds());
		value = getRefreshConditionName();
		if (value != null) {
			if (result.getRefreshTimeInSeconds() == null) {
				throw new MetaDataException(metaDataName + " : The view [refreshIf] is defined but no [refreshTimeInSeconds] is defined in " + getName() + VIEW_NAME_SEPARATOR + metaDataName);
			}
			result.setRefreshConditionName(value);
		}
		value = getRefreshActionName();
		if (value != null) {
			if (result.getRefreshTimeInSeconds() == null) {
				throw new MetaDataException(metaDataName + " : The view [refreshAction] is defined but no [refreshTimeInSeconds] is defined in " + getName() + VIEW_NAME_SEPARATOR + metaDataName);
			}
			if (result.getAction(value) == null) {
				throw new MetaDataException(metaDataName + " : The view [refreshAction] is not a valid action in " + getName() + VIEW_NAME_SEPARATOR + metaDataName);
			}
			result.setRefreshActionName(value);
		}

		if ((parameters != null) && (! parameters.isEmpty())) {
			for (ViewParameter parameter : parameters) {
				if (parameter.getFromBinding() == null) {
					throw new MetaDataException(metaDataName + " : The " + getName() + " view newParameter [fromBinding] is required in " + getName() + VIEW_NAME_SEPARATOR + metaDataName);
				}
				if (parameter.getBoundTo() == null) {
					throw new MetaDataException(metaDataName + " : The " + getName() + VIEW_NAME_SEPARATOR + parameter.getFromBinding() + " newParameter [boundTo] is required in " + getName() + VIEW_NAME_SEPARATOR + metaDataName);
				}
			}
			result.getParameters().addAll(parameters);
		}

		result.setDocumentation(documentation);
		result.getProperties().putAll(properties);

		return result;
	}
	
	/**
	 * Returns arbitrary extension properties for this view descriptor.
	 *
	 * @return mutable property map
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
