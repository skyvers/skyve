package org.skyve.wildcat.metadata.view.widget.bound.tabular;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.MetaData;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Filterable;
import org.skyve.metadata.view.Invisible;
import org.skyve.metadata.view.widget.bound.FilterParameter;
import org.skyve.wildcat.metadata.view.RelativeSize;
import org.skyve.wildcat.metadata.view.event.Addable;
import org.skyve.wildcat.metadata.view.event.Editable;
import org.skyve.wildcat.metadata.view.event.EventAction;
import org.skyve.wildcat.metadata.view.event.Removable;
import org.skyve.wildcat.metadata.view.event.RerenderEventAction;
import org.skyve.wildcat.metadata.view.event.ServerSideActionEventAction;
import org.skyve.wildcat.metadata.view.event.SetDisabledEventAction;
import org.skyve.wildcat.metadata.view.event.SetInvisibleEventAction;
import org.skyve.wildcat.metadata.view.widget.bound.FilterParameterImpl;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlType(namespace = XMLUtil.VIEW_NAMESPACE,
			propOrder = {"title", 
							"pixelWidth",
							"percentageWidth",
							"pixelHeight",
							"percentageHeight",
							"disabledConditionName",
							"invisibleConditionName",
							"disableAddConditionName",
							"disableZoomConditionName",
							"disableEditConditionName",
							"disableRemoveConditionName",
							"queryName", 
							"continueConversation",
							"addedActions",
							"editedActions",
							"removedActions",							
							"parameters"})
public class ListGrid implements MetaData, RelativeSize, Disableable, Invisible, Filterable, DisableableCRUDGrid,
									Addable, Editable, Removable {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 4739299969425100550L;

	private String title;
	
	private Integer pixelWidth;
	private Integer percentageWidth;
	private Integer pixelHeight;
	private Integer percentageHeight;
	
	private String disabledConditionName;
	private String invisibleConditionName;

	private String disableAddConditionName;
	private String disableZoomConditionName;
	private String disableEditConditionName;
	private String disableRemoveConditionName;

	private String queryName;
	private boolean continueConversation;
	
	private List<EventAction> addedActions = new ArrayList<>();
	private List<EventAction> editedActions = new ArrayList<>();
	private List<EventAction> removedActions = new ArrayList<>();

	private List<FilterParameter> parameters = new ArrayList<>();
	
	public String getQueryName() {
		return queryName;
	}

	@XmlAttribute(name = "query", required = true)
	public void setQueryName(String queryName) {
		this.queryName = queryName;
	}

	public boolean getContinueConversation() {
		return continueConversation;
	}

	@XmlAttribute(name = "continueConversation", required = true)
	public void setContinueConversation(boolean continueConversation) {
		this.continueConversation = continueConversation;
	}

	@Override
	@XmlElement(namespace = XMLUtil.VIEW_NAMESPACE, 
					name = "filterParameter",
					type = FilterParameterImpl.class,
					required = false)
	public List<FilterParameter> getParameters() {
		return parameters;
	}
	
	public String getTitle() {
		return title;
	}

	@XmlAttribute(required = false)
	public void setTitle(String title) {
		this.title = UtilImpl.processStringValue(title);
	}

	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	@Override
	@XmlAttribute(required = false)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	@Override
	public Integer getPercentageWidth() {
		return percentageWidth;
	}

	@Override
	@XmlAttribute(required = false)
	public void setPercentageWidth(Integer percentageWidth) {
		this.percentageWidth = percentageWidth;
	}

	@Override
	public Integer getPixelHeight() {
		return pixelHeight;
	}

	@Override
	@XmlAttribute(required = false)
	public void setPixelHeight(Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}

	@Override
	public Integer getPercentageHeight() {
		return percentageHeight;
	}

	@Override
	@XmlAttribute(required = false)
	public void setPercentageHeight(Integer percentageHeight) {
		this.percentageHeight = percentageHeight;
	}

	@Override
	public String getDisabledConditionName() {
		return disabledConditionName;
	}

	@Override
	@XmlAttribute(name = "disabled", required = false)
	public void setDisabledConditionName(String disabledConditionName) {
		this.disabledConditionName = UtilImpl.processStringValue(disabledConditionName);
	}

	@Override
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	@Override
	@XmlAttribute(name = "invisible", required = false)
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = UtilImpl.processStringValue(invisibleConditionName);
	}
	
	@Override
	public String getDisableAddConditionName() {
		return disableAddConditionName;
	}

	@Override
	@XmlAttribute(name = "disableAdd", required = false)
	public void setDisableAddConditionName(String disableAddConditionName) {
		this.disableAddConditionName = UtilImpl.processStringValue(disableAddConditionName);
	}
	
	@Override
	public String getDisableZoomConditionName() {
		return disableZoomConditionName;
	}

	@Override
	@XmlAttribute(name = "disableZoom", required = false)
	public void setDisableZoomConditionName(String disableZoomConditionName) {
		this.disableZoomConditionName = UtilImpl.processStringValue(disableZoomConditionName);
	}

	@Override
	public String getDisableEditConditionName() {
		return disableEditConditionName;
	}

	@Override
	@XmlAttribute(name = "disableEdit", required = false)
	public void setDisableEditConditionName(String disableEditConditionName) {
		this.disableEditConditionName = UtilImpl.processStringValue(disableEditConditionName);
	}

	@Override
	public String getDisableRemoveConditionName() {
		return disableRemoveConditionName;
	}

	@Override
	@XmlAttribute(name = "disableRemove", required = false)
	public void setDisableRemoveConditionName(String disableRemoveConditionName) {
		this.disableRemoveConditionName = UtilImpl.processStringValue(disableRemoveConditionName);
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
	@XmlElementWrapper(namespace = XMLUtil.VIEW_NAMESPACE, name = "onDeletedHandlers")
	@XmlElementRefs({@XmlElementRef(type = RerenderEventAction.class), 
						@XmlElementRef(type = ServerSideActionEventAction.class),
						@XmlElementRef(type = SetDisabledEventAction.class),
						@XmlElementRef(type = SetInvisibleEventAction.class)})
	public List<EventAction> getRemovedActions() {
		return removedActions;
	}
}
