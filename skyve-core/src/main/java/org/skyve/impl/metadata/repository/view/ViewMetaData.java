package org.skyve.impl.metadata.repository.view;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.repository.ConvertableMetaData;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.repository.ViewLayout;
import org.skyve.impl.metadata.repository.view.access.ViewUserAccessesMetaData;
import org.skyve.impl.metadata.repository.view.actions.ActionMetaData;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.view.View.ViewParameter;

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "view")
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE, 
			name = "view",
			propOrder = {"documentation",
							"actions", 
							"name", 
							"title",
							"iconStyleClass",
							"icon32x32RelativeFileName",
							"helpRelativeFileName",
							"helpURL",
							"layout",
							"refreshTimeInSeconds",
							"refreshConditionName", 
							"refreshActionName",
							"parameters",
							"accesses",
							"properties"})
public class ViewMetaData extends Container implements NamedMetaData, ConvertableMetaData<ViewImpl>, DecoratedMetaData {
	private static final long serialVersionUID = -1831750070396044584L;

	private String name;
	private String title;
	private String iconStyleClass;
	private String icon32x32RelativeFileName;
	private String helpRelativeFileName;
	private String helpURL;
	private ViewLayout layout;
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

	@Override
	public String getName() {
		return name;
	}

	@XmlAttribute(required = true)
	public void setName(String name) {
		this.name = UtilImpl.processStringValue(name);
	}

	public String getTitle() {
		return title;
	}

	@XmlAttribute(required = true)
	public void setTitle(String title) {
		this.title = UtilImpl.processStringValue(title);
	}

	public String getIcon32x32RelativeFileName() {
		return icon32x32RelativeFileName;
	}

	@XmlAttribute(name = "icon32x32RelativeFileName")
	public void setIcon32x32RelativeFileName(String icon32x32RelativeFileName) {
		this.icon32x32RelativeFileName = UtilImpl.processStringValue(icon32x32RelativeFileName);
	}

	public String getIconStyleClass() {
		return iconStyleClass;
	}

	@XmlAttribute(name = "iconStyleClass")
	public void setIconStyleClass(String iconStyleClass) {
		this.iconStyleClass = UtilImpl.processStringValue(iconStyleClass);
	}

	public String getHelpRelativeFileName() {
		return helpRelativeFileName;
	}

	@XmlAttribute(name = "helpRelativeFileName")
	public void setHelpRelativeFileName(String helpRelativeFileName) {
		this.helpRelativeFileName = UtilImpl.processStringValue(helpRelativeFileName);
	}

	public String getHelpURL() {
		return helpURL;
	}

	@XmlAttribute(name = "helpURL")
	public void setHelpURL(String helpURL) {
		this.helpURL = UtilImpl.processStringValue(helpURL);
	}

	public ViewLayout getLayout() {
		return layout;
	}

	@XmlAttribute(name = "layout")
	public void setLayout(ViewLayout layout) {
		this.layout = layout;
	}

	public Actions getActions() {
		return actions;
	}

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE, required = false, nillable = false)
	public void setActions(Actions actions) {
		this.actions = actions;
	}

	public Integer getRefreshTimeInSeconds() {
		return refreshTimeInSeconds;
	}

	@XmlAttribute(name = "refreshTimeInSeconds", required = false)
	public void setRefreshTimeInSeconds(Integer refreshTimeInSeconds) {
		this.refreshTimeInSeconds = refreshTimeInSeconds;
	}

	public String getRefreshConditionName() {
		return refreshConditionName;
	}

	@XmlAttribute(name = "refreshIf", required = false)
	public void setRefreshConditionName(String refreshConditionName) {
		this.refreshConditionName = UtilImpl.processStringValue(refreshConditionName);
	}

	public String getRefreshActionName() {
		return refreshActionName;
	}

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
	
	public ViewUserAccessesMetaData getAccesses() {
		return accesses;
	}

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE, required = false)
	public void setAccesses(ViewUserAccessesMetaData accesses) {
		this.accesses = accesses ;
	}

	public String getDocumentation() {
		return documentation;
	}
	
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setDocumentation(String documentation) {
		this.documentation = documentation;
	}

	@Override
	public long getLastModifiedMillis() {
		return lastModifiedMillis;
	}

	@XmlTransient
	public void setLastModifiedMillis(long lastModifiedMillis) {
		this.lastModifiedMillis = lastModifiedMillis;
	}

	@Override
	public ViewImpl convert(String metaDataName, ProvidedRepository repository) {
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
		
		result.setLayout(getLayout());

		String theName = getName();
		if (theName == null) {
			throw new MetaDataException(metaDataName + " : The view [name] is required for view " + metaDataName);
		}
		result.setName(theName);

		result.getContained().addAll(getContained());

		if (actions != null) {
			result.setActionsWidgetId(actions.getWidgetId());
			for (ActionMetaData actionMetaData : actions.getActions()) {
				org.skyve.metadata.view.Action action = actionMetaData.toMetaDataAction();
				ImplicitActionName implicitName = action.getImplicitName();
				if (action.getResourceName() == null) {
					if (implicitName == null) { // custom action
						throw new MetaDataException(metaDataName + " : [className] is required for a custom action for " + 
														getName() + " view " + metaDataName);
					}
					else if (ImplicitActionName.BizExport.equals(implicitName) || 
								ImplicitActionName.BizImport.equals(implicitName)) {
						throw new MetaDataException(metaDataName + " : [className] is required for a BizPort action for " +
														getName() + " view " + metaDataName);
					}
					else if (ImplicitActionName.Report.equals(implicitName)) {
						throw new MetaDataException(metaDataName + " : [reportName] is required for a report action for " + 
														getName() + " view " + metaDataName);
					}
				}
				else {
					value = actionMetaData.getDisplayName();
					if (value == null) {
						throw new MetaDataException(metaDataName + " : The view action [displayName] is required for the designer defined action " +
														action.getResourceName() + " for " + getName() + " view " + metaDataName);
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
				throw new MetaDataException(metaDataName + " : The view [refreshIf] is defined but no [refreshTimeInSeconds] is defined in " + getName() + " view " + metaDataName);
			}
			result.setRefreshConditionName(value);
		}
		value = getRefreshActionName();
		if (value != null) {
			if (result.getRefreshTimeInSeconds() == null) {
				throw new MetaDataException(metaDataName + " : The view [refreshAction] is defined but no [refreshTimeInSeconds] is defined in " + getName() + " view " + metaDataName);
			}
			if (result.getAction(value) == null) {
				throw new MetaDataException(metaDataName + " : The view [refreshAction] is not a valid action in " + getName() + " view " + metaDataName);
			}
			result.setRefreshActionName(value);
		}

		if ((parameters != null) && (! parameters.isEmpty())) {
			for (ViewParameter parameter : parameters) {
				if (parameter.getFromBinding() == null) {
					throw new MetaDataException(metaDataName + " : The " + getName() + " view newParameter [fromBinding] is required in " + getName() + " view " + metaDataName);
				}
				if (parameter.getBoundTo() == null) {
					throw new MetaDataException(metaDataName + " : The " + getName() + " view " + parameter.getFromBinding() + " newParameter [boundTo] is required in " + getName() + " view " + metaDataName);
				}
			}
			result.getParameters().addAll(parameters);
		}

		result.setDocumentation(documentation);
		result.getProperties().putAll(properties);

		return result;
	}
	
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
