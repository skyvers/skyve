package org.skyve.wildcat.metadata.view.widget.bound.input;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.view.Filterable;
import org.skyve.metadata.view.widget.bound.FilterParameter;
import org.skyve.wildcat.metadata.view.event.Addable;
import org.skyve.wildcat.metadata.view.event.Editable;
import org.skyve.wildcat.metadata.view.event.EventAction;
import org.skyve.wildcat.metadata.view.event.RerenderEventAction;
import org.skyve.wildcat.metadata.view.event.ServerSideActionEventAction;
import org.skyve.wildcat.metadata.view.event.SetDisabledEventAction;
import org.skyve.wildcat.metadata.view.event.SetInvisibleEventAction;
import org.skyve.wildcat.metadata.view.widget.bound.FilterParameterImpl;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlType(namespace = XMLUtil.VIEW_NAMESPACE,
			propOrder = {"query",
							"disableEditConditionName",
							"disableAddConditionName",
							"disableClearConditionName",
							"disablePickConditionName",
							"pickedActions",
							"clearedActions",
							"editedActions",
							"addedActions",
							"parameters"})
public class Lookup extends InputWidget implements Filterable, Addable, Editable {
	/**
	 * For Serialization
	 */
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

	private List<FilterParameter> parameters = new ArrayList<>();
	
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
		this.disableEditConditionName = disableEditConditionName;
	}

	public String getDisableAddConditionName() {
		return disableAddConditionName;
	}

	@XmlAttribute(name = "disableAdd", required = false)
	public void setDisableAddConditionName(String disableAddConditionName) {
		this.disableAddConditionName = disableAddConditionName;
	}

	public String getDisableClearConditionName() {
		return disableClearConditionName;
	}

	@XmlAttribute(name = "disableClear", required = false)
	public void setDisableClearConditionName(String disableClearConditionName) {
		this.disableClearConditionName = disableClearConditionName;
	}

	public String getDisablePickConditionName() {
		return disablePickConditionName;
	}

	@XmlAttribute(name = "disablePick", required = false)
	public void setDisablePickConditionName(String disablePickConditionName) {
		this.disablePickConditionName = disablePickConditionName;
	}

	@Override
	@XmlElement(namespace = XMLUtil.VIEW_NAMESPACE,
					name = "filterParameter",
					type = FilterParameterImpl.class,
					required = false)
	public List<FilterParameter> getParameters() {
		return parameters;
	}
	
	@XmlElementWrapper(namespace = XMLUtil.VIEW_NAMESPACE, name = "onPickedHandlers")
	@XmlElementRefs({@XmlElementRef(type = RerenderEventAction.class), 
						@XmlElementRef(type = ServerSideActionEventAction.class),
						@XmlElementRef(type = SetDisabledEventAction.class),
						@XmlElementRef(type = SetInvisibleEventAction.class)})
	public List<EventAction> getPickedActions() {
		return pickedActions;
	}

	@XmlElementWrapper(namespace = XMLUtil.VIEW_NAMESPACE, name = "onClearedHandlers")
	@XmlElementRefs({@XmlElementRef(type = RerenderEventAction.class), 
						@XmlElementRef(type = ServerSideActionEventAction.class),
						@XmlElementRef(type = SetDisabledEventAction.class),
						@XmlElementRef(type = SetInvisibleEventAction.class)})
	public List<EventAction> getClearedActions() {
		return clearedActions;
	}

	@Override
	@XmlElementWrapper(namespace = XMLUtil.VIEW_NAMESPACE, name = "onEditedHandlers")
	@XmlElementRefs({@XmlElementRef(type = RerenderEventAction.class), 
						@XmlElementRef(type = ServerSideActionEventAction.class),
						@XmlElementRef(type = SetDisabledEventAction.class),
						@XmlElementRef(type = SetInvisibleEventAction.class)})
	public List<EventAction> getEditedActions() {
		return editedActions;
	}

	@Override
	@XmlElementWrapper(namespace = XMLUtil.VIEW_NAMESPACE, name = "onAddedHandlers")
	@XmlElementRefs({@XmlElementRef(type = RerenderEventAction.class), 
						@XmlElementRef(type = ServerSideActionEventAction.class),
						@XmlElementRef(type = SetDisabledEventAction.class),
						@XmlElementRef(type = SetInvisibleEventAction.class)})
	public List<EventAction> getAddedActions() {
		return addedActions;
	}
}
