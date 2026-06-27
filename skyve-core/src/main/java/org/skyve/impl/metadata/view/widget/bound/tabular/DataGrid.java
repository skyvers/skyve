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

/**
 * JAXB-annotated inline-editable grid widget bound to a document collection.
 *
 * <p>Renders rows from the bound collection with per-column display widgets
 * ({@link DataGridBoundColumn}, {@link DataGridContainerColumn}).  Supports
 * add, edit, remove, zoom, and deselect operations with independent
 * disable conditions and event handler lists.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see AbstractDataWidget
 * @see DataGridColumn
 */
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

	/**
	 * Returns whether this grid renders inline.
	 *
	 * @return {@code true} when the grid should render inline
	 */
	public Boolean getInline() {
		return inline;
	}

	/**
	 * Sets whether this grid renders inline.
	 *
	 * @param inline {@code true} to render inline
	 */
	@XmlAttribute(name = "inline", required = false)
	public void setInline(Boolean inline) {
		this.inline = inline;
	}

	/**
	 * Returns whether the grid is editable.
	 *
	 * @return {@code true} when inline editing is enabled
	 */
	@Override
	public Boolean getEditable() {
		return editable;
	}

	/**
	 * Sets whether the grid is editable.
	 *
	 * @param editable {@code true} to enable inline editing
	 */
	@Override
	@XmlAttribute(name = "editable", required = false)
	public void setEditable(Boolean editable) {
		this.editable = editable;
	}

	/**
	 * Returns whether cell text should wrap.
	 *
	 * @return {@code true} when wrapping is enabled
	 */
	public Boolean getWordWrap() {
		return wordWrap;
	}

	/**
	 * Sets whether cell text should wrap.
	 *
	 * @param wordWrap {@code true} to enable wrapping
	 */
	@XmlAttribute(name = "wrap", required = false)
	public void setWordWrap(Boolean wordWrap) {
		this.wordWrap = wordWrap;
	}

	/**
	 * Returns the ordered, mutable column list for this grid.
	 *
	 * <p>At least one bound or container column is expected by metadata validation.
	 *
	 * @return mutable column definitions in display order
	 */
	// Ensure that there is at least 1 bound or container column
	@Override
	@XmlElements({@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "boundColumn", type = DataGridBoundColumn.class, required = true),
						@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "containerColumn", type = DataGridContainerColumn.class, required = true)})
	public List<DataGridColumn> getColumns() {
		return columns;
	}

	/**
	 * Returns the disabled condition expression.
	 *
	 * @return the disabled condition expression, or {@code null} when not set
	 */
	@Override
	public String getDisabledConditionName() {
		return disabledConditionName;
	}

	/**
	 * Sets the disabled condition expression directly.
	 *
	 * @param disabledConditionName the disabled condition expression
	 */
	@Override
	@XmlAttribute(name = "disabled", required = false)
	public void setDisabledConditionName(String disabledConditionName) {
		this.disabledConditionName = UtilImpl.processStringValue(disabledConditionName);
	}
	
	/**
	 * JAXB-only placeholder for {@code enabled}; value is derived via {@link #setEnabledConditionName(String)}.
	 *
	 * @return always {@code null}; JAXB writes via {@code setEnabledConditionName}
	 */
	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnabledConditionName() {
		return null;
	}

	/**
	 * Sets the enabled condition by storing its negation as the disabled condition.
	 *
	 * @param enabledConditionName a condition expression that enables the grid
	 */
	@Override
	@XmlAttribute(name = "enabled", required = false)
	public void setEnabledConditionName(String enabledConditionName) {
		this.disabledConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(enabledConditionName));
	}

	/**
	 * Returns the disable-add condition expression.
	 *
	 * @return the disable-add condition expression, or {@code null} when not set
	 */
	@Override
	public String getDisableAddConditionName() {
		return disableAddConditionName;
	}

	/**
	 * Sets the disable-add condition expression directly.
	 *
	 * @param disableAddConditionName the disable-add condition expression
	 */
	@Override
	@XmlAttribute(name = "disableAdd", required = false)
	public void setDisableAddConditionName(String disableAddConditionName) {
		this.disableAddConditionName = UtilImpl.processStringValue(disableAddConditionName);
	}
	
	/**
	 * JAXB-only placeholder for {@code enableAdd}; value is derived via {@link #setEnableAddConditionName(String)}.
	 *
	 * @return always {@code null}; JAXB writes via {@code setEnableAddConditionName}
	 */
	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnableAddConditionName() {
		return null;
	}

	/**
	 * Sets the add-enabled condition by storing its negation as the add-disabled condition.
	 *
	 * @param enableAddConditionName a condition expression that enables add actions
	 */
	@Override
	@XmlAttribute(name = "enableAdd", required = false)
	public void setEnableAddConditionName(String enableAddConditionName) {
		this.disableAddConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(enableAddConditionName));
	}

	/**
	 * Returns the disable-zoom condition expression.
	 *
	 * @return the disable-zoom condition expression, or {@code null} when not set
	 */
	@Override
	public String getDisableZoomConditionName() {
		return disableZoomConditionName;
	}

	/**
	 * Sets the disable-zoom condition expression directly.
	 *
	 * @param disableZoomConditionName the disable-zoom condition expression
	 */
	@Override
	@XmlAttribute(name = "disableZoom", required = false)
	public void setDisableZoomConditionName(String disableZoomConditionName) {
		this.disableZoomConditionName = UtilImpl.processStringValue(disableZoomConditionName);
	}

	/**
	 * JAXB-only placeholder for {@code enableZoom}; value is derived via {@link #setEnableZoomConditionName(String)}.
	 *
	 * @return always {@code null}; JAXB writes via {@code setEnableZoomConditionName}
	 */
	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnableZoomConditionName() {
		return null;
	}

	/**
	 * Sets the zoom-enabled condition by storing its negation as the zoom-disabled condition.
	 *
	 * @param enableZoomConditionName a condition expression that enables zoom actions
	 */
	@Override
	@XmlAttribute(name = "enableZoom", required = false)
	public void setEnableZoomConditionName(String enableZoomConditionName) {
		this.disableZoomConditionName =  BindUtil.negateCondition(UtilImpl.processStringValue(enableZoomConditionName));
	}

	/**
	 * Returns the disable-edit condition expression.
	 *
	 * @return the disable-edit condition expression, or {@code null} when not set
	 */
	@Override
	public String getDisableEditConditionName() {
		return disableEditConditionName;
	}

	/**
	 * Sets the disable-edit condition expression directly.
	 *
	 * @param disableEditConditionName the disable-edit condition expression
	 */
	@Override
	@XmlAttribute(name = "disableEdit", required = false)
	public void setDisableEditConditionName(String disableEditConditionName) {
		this.disableEditConditionName = UtilImpl.processStringValue(disableEditConditionName);
	}

	/**
	 * JAXB-only placeholder for {@code enableEdit}; value is derived via {@link #setEnableEditConditionName(String)}.
	 *
	 * @return always {@code null}; JAXB writes via {@code setEnableEditConditionName}
	 */
	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnableEditConditionName() {
		return null;
	}

	/**
	 * Sets the edit-enabled condition by storing its negation as the edit-disabled condition.
	 *
	 * @param enableEditConditionName a condition expression that enables edit actions
	 */
	@Override
	@XmlAttribute(name = "enableEdit", required = false)
	public void setEnableEditConditionName(String enableEditConditionName) {
		this.disableEditConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(enableEditConditionName));
	}

	/**
	 * Returns the disable-remove condition expression.
	 *
	 * @return the disable-remove condition expression, or {@code null} when not set
	 */
	@Override
	public String getDisableRemoveConditionName() {
		return disableRemoveConditionName;
	}

	/**
	 * Sets the disable-remove condition expression directly.
	 *
	 * @param disableRemoveConditionName the disable-remove condition expression
	 */
	@Override
	@XmlAttribute(name = "disableRemove", required = false)
	public void setDisableRemoveConditionName(String disableRemoveConditionName) {
		this.disableRemoveConditionName = UtilImpl.processStringValue(disableRemoveConditionName);
	}
	
	/**
	 * JAXB-only placeholder for {@code enableRemove}; value is derived via {@link #setEnableRemoveConditionName(String)}.
	 *
	 * @return always {@code null}; JAXB writes via {@code setEnableRemoveConditionName}
	 */
	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnableRemoveConditionName() {
		return null;
	}

	/**
	 * Sets the remove-enabled condition by storing its negation as the remove-disabled condition.
	 *
	 * @param enableRemoveConditionName a condition expression that enables remove actions
	 */
	@Override
	@XmlAttribute(name = "enableRemove", required = false)
	public void setEnableRemoveConditionName(String enableRemoveConditionName) {
		this.disableRemoveConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(enableRemoveConditionName));
	}

	/**
	 * Returns whether the add action should be shown.
	 *
	 * @return {@code true} when the add action is shown
	 */
	public Boolean getShowAdd() {
		return showAdd;
	}

	/**
	 * Sets whether the add action should be shown.
	 *
	 * @param showAdd {@code true} to show the add action
	 */
	@XmlAttribute(name = "showAdd", required = false)
	public void setShowAdd(Boolean showAdd) {
		this.showAdd = showAdd;
	}

	/**
	 * Returns whether the zoom action should be shown.
	 *
	 * @return {@code true} when the zoom action is shown
	 */
	public Boolean getShowZoom() {
		return showZoom;
	}

	/**
	 * Sets whether the zoom action should be shown.
	 *
	 * @param showZoom {@code true} to show the zoom action
	 */
	@XmlAttribute(name = "showZoom", required = false)
	public void setShowZoom(Boolean showZoom) {
		this.showZoom = showZoom;
	}

	/**
	 * Returns whether the edit action should be shown.
	 *
	 * @return {@code true} when the edit action is shown
	 */
	public Boolean getShowEdit() {
		return showEdit;
	}

	/**
	 * Sets whether the edit action should be shown.
	 *
	 * @param showEdit {@code true} to show the edit action
	 */
	@XmlAttribute(name = "showEdit", required = false)
	public void setShowEdit(Boolean showEdit) {
		this.showEdit = showEdit;
	}

	/**
	 * Returns whether the remove action should be shown.
	 *
	 * @return {@code true} when the remove action is shown
	 */
	public Boolean getShowRemove() {
		return showRemove;
	}

	/**
	 * Sets whether the remove action should be shown.
	 *
	 * @param showRemove {@code true} to show the remove action
	 */
	@XmlAttribute(name = "showRemove", required = false)
	public void setShowRemove(Boolean showRemove) {
		this.showRemove = showRemove;
	}

	/**
	 * Returns whether the deselect action should be shown.
	 *
	 * @return {@code true} when the deselect action is shown
	 */
	public Boolean getShowDeselect() {
		return showDeselect;
	}

	/**
	 * Sets whether the deselect action should be shown.
	 *
	 * @param showDeselect {@code true} to show the deselect action
	 */
	@XmlAttribute(name = "showDeselect", required = false)
	public void setShowDeselect(Boolean showDeselect) {
		this.showDeselect = showDeselect;
	}

	/**
	 * Returns the selected-row identifier binding.
	 *
	 * @return the selected row identifier binding, or {@code null} when not set
	 */
	@Override
	public String getSelectedIdBinding() {
		return selectedIdBinding;
	}

	/**
	 * Sets the selected-row identifier binding after trimming and empty-string normalisation.
	 *
	 * @param selectedIdBinding the selected row identifier binding
	 */
	@Override
	@XmlAttribute(name = "selectedIdBinding")
	public void setSelectedIdBinding(String selectedIdBinding) {
		this.selectedIdBinding = UtilImpl.processStringValue(selectedIdBinding);
	}

	/**
	 * Returns the mutable event handlers triggered after a row is added.
	 *
	 * @return mutable add handlers
	 */
	@Override
	@XmlElementWrapper(namespace = XMLMetaData.VIEW_NAMESPACE, name = "onAddedHandlers")
	@XmlElementRefs({@XmlElementRef(type = RerenderEventAction.class), 
						@XmlElementRef(type = ServerSideActionEventAction.class),
						@XmlElementRef(type = SetDisabledEventAction.class),
						@XmlElementRef(type = SetInvisibleEventAction.class)})
	public List<EventAction> getAddedActions() {
		return addedActions;
	}

	/**
	 * Returns the mutable event handlers triggered after a row is edited.
	 *
	 * @return mutable edit handlers
	 */
	@Override
	@XmlElementWrapper(namespace = XMLMetaData.VIEW_NAMESPACE, name = "onEditedHandlers")
	@XmlElementRefs({@XmlElementRef(type = RerenderEventAction.class), 
						@XmlElementRef(type = ServerSideActionEventAction.class),
						@XmlElementRef(type = SetDisabledEventAction.class),
						@XmlElementRef(type = SetInvisibleEventAction.class)})
	public List<EventAction> getEditedActions() {
		return editedActions;
	}

	/**
	 * Returns the mutable event handlers triggered after a row is removed.
	 *
	 * @return mutable remove handlers
	 */
	@Override
	@XmlElementWrapper(namespace = XMLMetaData.VIEW_NAMESPACE, name = "onRemovedHandlers")
	@XmlElementRefs({@XmlElementRef(type = RerenderEventAction.class), 
						@XmlElementRef(type = ServerSideActionEventAction.class),
						@XmlElementRef(type = SetDisabledEventAction.class),
						@XmlElementRef(type = SetInvisibleEventAction.class)})
	public List<EventAction> getRemovedActions() {
		return removedActions;
	}
	
	/**
	 * Returns the mutable event handlers triggered when a row is selected.
	 *
	 * @return mutable selected handlers
	 */
	@Override
	@XmlElementWrapper(namespace = XMLMetaData.VIEW_NAMESPACE, name = "onSelectedHandlers")
	@XmlElementRefs({@XmlElementRef(type = RerenderEventAction.class), 
						@XmlElementRef(type = ServerSideActionEventAction.class),
						@XmlElementRef(type = SetDisabledEventAction.class),
						@XmlElementRef(type = SetInvisibleEventAction.class)})
	public List<EventAction> getSelectedActions() {
		return selectedActions;
	}

	/**
	 * Returns the mutable decorator property map for this grid.
	 *
	 * @return mutable decorator property map keyed by property name
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
