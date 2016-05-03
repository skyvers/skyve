package org.skyve.impl.metadata.view.widget.bound.tabular;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.Identifiable;
import org.skyve.impl.metadata.view.event.Addable;
import org.skyve.impl.metadata.view.event.EventAction;
import org.skyve.impl.metadata.view.event.Removable;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.Selectable;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLUtil;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Editable;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DisableableCRUDGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.TabularWidget;

@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlType(namespace = XMLUtil.VIEW_NAMESPACE,
			propOrder = {"widgetId",
							"inline", 
							"editable",
							"wordWrap",
							"disabledConditionName", 
							"enabledConditionName", 
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
							"selectedIdBinding",
							"columns",
							"addedActions",
							"editedActions",
							"removedActions",
							"selectedActions"})
public class DataGrid extends TabularWidget implements Identifiable,
														Disableable,
														Editable,
														DisableableCRUDGrid, 
														Addable,
														org.skyve.impl.metadata.view.event.Editable,
														Removable,
														Selectable {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 5341133860997684429L;

	private String widgetId;
	
	private String disabledConditionName;
	
	private String disableAddConditionName;
	private String disableZoomConditionName;
	private String disableEditConditionName;
	private String disableRemoveConditionName;

	private Boolean showAdd;
	private Boolean showZoom;
	private Boolean showEdit;
	private Boolean showRemove;
	private Boolean showDeselect;

	private String selectedIdBinding;
	
	private Boolean inline;
	
	private Boolean editable;
	
	private Boolean wordWrap;
	
	private List<DataGridColumn> columns = new ArrayList<>();

	private List<EventAction> addedActions = new ArrayList<>();
	private List<EventAction> editedActions = new ArrayList<>();
	private List<EventAction> removedActions = new ArrayList<>();
	private List<EventAction> selectedActions = new ArrayList<>();

	@Override
	public String getWidgetId() {
		return widgetId;
	}

	@XmlAttribute(required = false)
	public void setWidgetId(String widgetId) {
		this.widgetId = UtilImpl.processStringValue(widgetId);
	}

	public Boolean getInline() {
		return inline;
	}

	@XmlAttribute(name = "inline", required = false)
	public void setInline(Boolean inline) {
		this.inline = inline;
	}

	@Override
	public Boolean getEditable() {
		return editable;
	}

	@Override
	@XmlAttribute(name = "editable", required = false)
	public void setEditable(Boolean editable) {
		this.editable = editable;
	}

	public Boolean getWordWrap() {
		return wordWrap;
	}

	@XmlAttribute(name = "wrap", required = false)
	public void setWordWrap(Boolean wordWrap) {
		this.wordWrap = wordWrap;
	}

	@Override
	@XmlElementRefs({@XmlElementRef(type = DataGridBoundColumn.class),
						@XmlElementRef(type = DataGridContainerColumn.class)})
	public List<DataGridColumn> getColumns() {
		return columns;
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
	
	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnabledConditionName() {
		return null;
	}

	@Override
	@XmlAttribute(name = "enabled", required = false)
	public void setEnabledConditionName(String enabledConditionName) {
		this.disabledConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(enabledConditionName));
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
	
	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnableAddConditionName() {
		return null;
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

	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnableZoomConditionName() {
		return null;
	}

	@Override
	@XmlAttribute(name = "enableZoom", required = false)
	public void setEnableZoomConditionName(String enableZoomConditionName) {
		this.disableZoomConditionName =  BindUtil.negateCondition(UtilImpl.processStringValue(enableZoomConditionName));
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

	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnableEditConditionName() {
		return null;
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
	
	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnableRemoveConditionName() {
		return null;
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
	@XmlElementWrapper(namespace = XMLUtil.VIEW_NAMESPACE, name = "onRemovedHandlers")
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
}
