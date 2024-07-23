package org.skyve.impl.metadata.view.widget.bound.tabular;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.event.Addable;
import org.skyve.impl.metadata.view.event.EventAction;
import org.skyve.impl.metadata.view.event.Removable;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.Selectable;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Editable;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlElementRefs;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlElements;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"inline", 
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
							"selectedActions",
							"properties"})
public class DataGrid extends AbstractDataWidget implements DecoratedMetaData,
														Disableable,
														Editable,
														DisableableCRUDGrid, 
														Addable,
														org.skyve.impl.metadata.view.event.Editable,
														Removable,
														Selectable {
	private static final long serialVersionUID = 5341133860997684429L;

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

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

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
	// Ensure that there is at least 1 bound or container column
	@XmlElements({@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "boundColumn", type = DataGridBoundColumn.class, required = true),
						@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "containerColumn", type = DataGridContainerColumn.class, required = true)})
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
	@XmlElementWrapper(namespace = XMLMetaData.VIEW_NAMESPACE, name = "onAddedHandlers")
	@XmlElementRefs({@XmlElementRef(type = RerenderEventAction.class), 
						@XmlElementRef(type = ServerSideActionEventAction.class),
						@XmlElementRef(type = SetDisabledEventAction.class),
						@XmlElementRef(type = SetInvisibleEventAction.class)})
	public List<EventAction> getAddedActions() {
		return addedActions;
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
	@XmlElementWrapper(namespace = XMLMetaData.VIEW_NAMESPACE, name = "onRemovedHandlers")
	@XmlElementRefs({@XmlElementRef(type = RerenderEventAction.class), 
						@XmlElementRef(type = ServerSideActionEventAction.class),
						@XmlElementRef(type = SetDisabledEventAction.class),
						@XmlElementRef(type = SetInvisibleEventAction.class)})
	public List<EventAction> getRemovedActions() {
		return removedActions;
	}
	
	@Override
	@XmlElementWrapper(namespace = XMLMetaData.VIEW_NAMESPACE, name = "onSelectedHandlers")
	@XmlElementRefs({@XmlElementRef(type = RerenderEventAction.class), 
						@XmlElementRef(type = ServerSideActionEventAction.class),
						@XmlElementRef(type = SetDisabledEventAction.class),
						@XmlElementRef(type = SetInvisibleEventAction.class)})
	public List<EventAction> getSelectedActions() {
		return selectedActions;
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
