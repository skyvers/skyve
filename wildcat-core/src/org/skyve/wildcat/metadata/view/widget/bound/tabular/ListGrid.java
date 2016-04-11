package org.skyve.wildcat.metadata.view.widget.bound.tabular;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.MetaData;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Filterable;
import org.skyve.metadata.view.Invisible;
import org.skyve.metadata.view.widget.bound.FilterParameter;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.metadata.view.RelativeSize;
import org.skyve.wildcat.metadata.view.event.Editable;
import org.skyve.wildcat.metadata.view.event.EventAction;
import org.skyve.wildcat.metadata.view.event.Removable;
import org.skyve.wildcat.metadata.view.event.RerenderEventAction;
import org.skyve.wildcat.metadata.view.event.Selectable;
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
							"enabledConditionName",
							"invisibleConditionName",
							"visibleConditionName",
							"disableAddConditionName",
							"enableAddConditionName",
							"disableZoomConditionName",
							"enableZoomConditionName",
							"disableEditConditionName",
							"enableEditConditionName",
							"disableRemoveConditionName",
							"enableRemoveConditionName",
							"showAdd",
							"showZoom",
							"showEdit",
							"showRemove",
							"showDeselect",
							"showFilter",
							"showSummary",
							"showExport",
							"showSnap",
							"showTag",
							"queryName",
							"modelName",
							"selectedIdBinding",
							"postRefreshConditionName",
							"continueConversation",
							"editedActions",
							"removedActions",
							"selectedActions",
							"parameters"})
public class ListGrid implements MetaData,
									RelativeSize,
									Disableable,
									Invisible,
									Filterable,
									DisableableCRUDGrid,
									Editable,
									Removable,
									Selectable {
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

	private Boolean showAdd;
	private Boolean showZoom;
	private Boolean showEdit;
	private Boolean showRemove;
	private Boolean showDeselect;
	private Boolean showExport;
	private Boolean showFilter;
	private Boolean showSummary;
	private Boolean showSnap;
	private Boolean showTag;

	private String queryName;
	private String modelName;
	private String selectedIdBinding;
	private boolean continueConversation;
	private String postRefreshConditionName;
	
	private List<EventAction> editedActions = new ArrayList<>();
	private List<EventAction> removedActions = new ArrayList<>();
	private List<EventAction> selectedActions = new ArrayList<>();

	private List<FilterParameter> parameters = new ArrayList<>();
	
	public String getQueryName() {
		return queryName;
	}

	@XmlAttribute(name = "query")
	public void setQueryName(String queryName) {
		this.queryName = UtilImpl.processStringValue(queryName);
	}

	public String getModelName() {
		return modelName;
	}

	@XmlAttribute(name = "model")
	public void setModelName(String modelName) {
		this.modelName = UtilImpl.processStringValue(modelName);
	}

	public boolean getContinueConversation() {
		return continueConversation;
	}

	@XmlAttribute(name = "continueConversation", required = true)
	public void setContinueConversation(boolean continueConversation) {
		this.continueConversation = continueConversation;
	}

	public String getPostRefreshConditionName() {
		return postRefreshConditionName;
	}

	@XmlAttribute(name = "postRefresh")
	public void setPostRefreshConditionName(String refresh) {
		this.postRefreshConditionName = refresh;
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
	@XmlAttribute(name = "enabled", required = false)
	public void setEnabledConditionName(String enabledConditionName) {
		this.disabledConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(enabledConditionName));
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
	@XmlAttribute(name = "visible", required = false)
	public void setVisibleConditionName(String visibleConditionName) {
		this.invisibleConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(visibleConditionName));
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
	@XmlAttribute(name = "enableAdd", required = false)
	public void setEnableAddConditionName(String enableAddConditionName) {
		this.disableAddConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(enableAddConditionName));
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
	@XmlAttribute(name = "enableZoom", required = false)
	public void setEnableZoomConditionName(String enableZoomConditionName) {
		this.disableZoomConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(enableZoomConditionName));
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
	@XmlAttribute(name = "enableEdit", required = false)
	public void setEnableEditConditionName(String enableEditConditionName) {
		this.disableEditConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(enableEditConditionName));
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
	@XmlAttribute(name = "enableRemove", required = false)
	public void setEnableRemoveConditionName(String enableRemoveConditionName) {
		this.disableRemoveConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(enableRemoveConditionName));
	}

	public Boolean getShowAdd() {
		return showAdd;
	}

	@XmlAttribute(name = "showAdd", required = false)
	public void setShowAdd(Boolean showAdd) {
		this.showAdd = showAdd;
	}

	public Boolean getShowZoom() {
		return showZoom;
	}

	@XmlAttribute(name = "showZoom", required = false)
	public void setShowZoom(Boolean showZoom) {
		this.showZoom = showZoom;
	}

	public Boolean getShowEdit() {
		return showEdit;
	}

	@XmlAttribute(name = "showEdit", required = false)
	public void setShowEdit(Boolean showEdit) {
		this.showEdit = showEdit;
	}

	public Boolean getShowRemove() {
		return showRemove;
	}

	@XmlAttribute(name = "showRemove", required = false)
	public void setShowRemove(Boolean showRemove) {
		this.showRemove = showRemove;
	}

	public Boolean getShowDeselect() {
		return showDeselect;
	}

	@XmlAttribute(name = "showDeselect", required = false)
	public void setShowDeselect(Boolean showDeselect) {
		this.showDeselect = showDeselect;
	}

	public Boolean getShowExport() {
		return showExport;
	}

	@XmlAttribute(name = "showExport", required = false)
	public void setShowExport(Boolean showExport) {
		this.showExport = showExport;
	}

	public Boolean getShowFilter() {
		return showFilter;
	}

	@XmlAttribute(name = "showFilter", required = false)
	public void setShowFilter(Boolean showFilter) {
		this.showFilter = showFilter;
	}

	public Boolean getShowSummary() {
		return showSummary;
	}

	@XmlAttribute(name = "showSummary", required = false)
	public void setShowSummary(Boolean showSummary) {
		this.showSummary = showSummary;
	}

	public Boolean getShowSnap() {
		return showSnap;
	}

	@XmlAttribute(name = "showSnap", required = false)
	public void setShowSnap(Boolean showSnap) {
		this.showSnap = showSnap;
	}

	public Boolean getShowTag() {
		return showTag;
	}

	@XmlAttribute(name = "showTag", required = false)
	public void setShowTag(Boolean showTag) {
		this.showTag = showTag;
	}

	@Override
	public String getSelectedIdBinding() {
		return selectedIdBinding;
	}

	@Override
	@XmlAttribute(name = "selectedIdBinding")
	public void setSelectedIdBinding(String selectedIdBinding) {
		this.selectedIdBinding = UtilImpl.processStringValue(selectedIdBinding);
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

	@Override
	@XmlElementWrapper(namespace = XMLUtil.VIEW_NAMESPACE, name = "onSelectedHandlers")
	@XmlElementRefs({@XmlElementRef(type = RerenderEventAction.class), 
						@XmlElementRef(type = ServerSideActionEventAction.class),
						@XmlElementRef(type = SetDisabledEventAction.class),
						@XmlElementRef(type = SetInvisibleEventAction.class)})
	public List<EventAction> getSelectedActions() {
		return selectedActions;
	}
	
	/**
	 * Return the event source string = query name (or model name).
	 */
	@Override
	@XmlTransient
	public String getSource() {
		return (queryName == null) ? modelName : queryName;
	}
}
