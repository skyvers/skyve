package org.skyve.impl.metadata.view.widget.bound.input;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.metadata.view.event.Addable;
import org.skyve.impl.metadata.view.event.Editable;
import org.skyve.impl.metadata.view.event.EventAction;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.widget.FilterParameterImpl;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Filterable;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.util.Util;
import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"query",
							"disableEditConditionName",
							"enableEditConditionName",
							"disableAddConditionName",
							"enableAddConditionName",
							"disableClearConditionName",
							"enableClearConditionName",
							"disablePickConditionName",
							"enablePickConditionName",
							"pickedActions",
							"clearedActions",
							"editedActions",
							"addedActions",
							"filterParameters",
							"parameters",
							"properties"})
public class Lookup extends InputWidget implements Filterable, Addable, Editable, FormItemWidget {
	private static final long serialVersionUID = 7742358399386599267L;

	private String query;

	private String disableEditConditionName;
	private String disableAddConditionName;
	private String disableClearConditionName;
	private String disablePickConditionName;

	private List<EventAction> pickedActions = new ArrayList<>();
	private List<EventAction> clearedActions = new ArrayList<>();
	private List<EventAction> editedActions = new ArrayList<>();
	private List<EventAction> addedActions = new ArrayList<>();

	private List<FilterParameter> filterParameters = new ArrayList<>();
	private List<Parameter> parameters = new ArrayList<>();
	
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	@Override
	public boolean showsLabelByDefault() {
		return false;
	}
	
	public String getQuery() {
		return query;
	}

	@XmlAttribute(required = false)
	public void setQuery(String query) {
		this.query = UtilImpl.processStringValue(query);
	}

	public String getDisableEditConditionName() {
		return disableEditConditionName;
	}

	@XmlAttribute(name = "disableEdit", required = false)
	public void setDisableEditConditionName(String disableEditConditionName) {
		this.disableEditConditionName = UtilImpl.processStringValue(disableEditConditionName);
	}

	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnableEditConditionName() {
		return null;
	}

	@XmlAttribute(name = "enableEdit", required = false)
	public void setEnableEditConditionName(String enableEditConditionName) {
		this.disableEditConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(enableEditConditionName));
	}

	public String getDisableAddConditionName() {
		return disableAddConditionName;
	}

	@XmlAttribute(name = "disableAdd", required = false)
	public void setDisableAddConditionName(String disableAddConditionName) {
		this.disableAddConditionName = Util.processStringValue(disableAddConditionName);
	}

	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnableAddConditionName() {
		return null;
	}

	@XmlAttribute(name = "enableAdd", required = false)
	public void setEnableAddConditionName(String enableAddConditionName) {
		this.disableAddConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(enableAddConditionName));
	}

	public String getDisableClearConditionName() {
		return disableClearConditionName;
	}

	@XmlAttribute(name = "disableClear", required = false)
	public void setDisableClearConditionName(String disableClearConditionName) {
		this.disableClearConditionName = Util.processStringValue(disableClearConditionName);
	}

	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnableClearConditionName() {
		return null;
	}

	@XmlAttribute(name = "enableClear", required = false)
	public void setEnableClearConditionName(String enableClearConditionName) {
		this.disableClearConditionName = BindUtil.negateCondition(Util.processStringValue(enableClearConditionName));
	}

	public String getDisablePickConditionName() {
		return disablePickConditionName;
	}

	@XmlAttribute(name = "disablePick", required = false)
	public void setDisablePickConditionName(String disablePickConditionName) {
		this.disablePickConditionName = Util.processStringValue(disablePickConditionName);
	}

	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnablePickConditionName() {
		return null;
	}

	@XmlAttribute(name = "enablePick", required = false)
	public void setEnablePickConditionName(String enablePickConditionName) {
		this.disablePickConditionName = BindUtil.negateCondition(Util.processStringValue(enablePickConditionName));
	}

	@Override
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE,
					name = "filterParameter",
					type = FilterParameterImpl.class,
					required = false)
	public List<FilterParameter> getFilterParameters() {
		return filterParameters;
	}

	@Override
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE,
					name = "parameter",
					type = ParameterImpl.class,
					required = false)
	public List<Parameter> getParameters() {
		return parameters;
	}

	@XmlElementWrapper(namespace = XMLMetaData.VIEW_NAMESPACE, name = "onPickedHandlers")
	@XmlElementRefs({@XmlElementRef(type = RerenderEventAction.class), 
						@XmlElementRef(type = ServerSideActionEventAction.class),
						@XmlElementRef(type = SetDisabledEventAction.class),
						@XmlElementRef(type = SetInvisibleEventAction.class)})
	public List<EventAction> getPickedActions() {
		return pickedActions;
	}

	@XmlElementWrapper(namespace = XMLMetaData.VIEW_NAMESPACE, name = "onClearedHandlers")
	@XmlElementRefs({@XmlElementRef(type = RerenderEventAction.class), 
						@XmlElementRef(type = ServerSideActionEventAction.class),
						@XmlElementRef(type = SetDisabledEventAction.class),
						@XmlElementRef(type = SetInvisibleEventAction.class)})
	public List<EventAction> getClearedActions() {
		return clearedActions;
	}

	@Override
	@XmlElementWrapper(namespace = XMLMetaData.VIEW_NAMESPACE, name = "onEditedHandlers")
	@XmlElementRefs({@XmlElementRef(type = RerenderEventAction.class), 
						@XmlElementRef(type = ServerSideActionEventAction.class),
						@XmlElementRef(type = SetDisabledEventAction.class),
						@XmlElementRef(type = SetInvisibleEventAction.class)})
	public List<EventAction> getEditedActions() {
		return editedActions;
	}

	@Override
	@XmlElementWrapper(namespace = XMLMetaData.VIEW_NAMESPACE, name = "onAddedHandlers")
	@XmlElementRefs({@XmlElementRef(type = RerenderEventAction.class), 
						@XmlElementRef(type = ServerSideActionEventAction.class),
						@XmlElementRef(type = SetDisabledEventAction.class),
						@XmlElementRef(type = SetInvisibleEventAction.class)})
	public List<EventAction> getAddedActions() {
		return addedActions;
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
