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

import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlElementRefs;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlElements;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
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
							"xmlDisabledCondition", 
							"xmlEnabledCondition", 
							"xmlDisableAddCondition",
							"xmlEnableAddCondition",
							"xmlDisableZoomCondition",
							"xmlEnableZoomCondition",
							"xmlDisableEditCondition",
							"xmlEnableEditCondition",
							"xmlDisableRemoveCondition",
							"xmlEnableRemoveCondition",
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

	private String disabledCondition;
	private String enabledCondition;
	
	private String disableAddCondition;
	private String enableAddCondition;
	private String disableZoomCondition;
	private String enableZoomCondition;
	private String disableEditCondition;
	private String enableEditCondition;
	private String disableRemoveCondition;
	private String enableRemoveCondition;

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
	 * Returns the derived {@code disabled} condition that renderers and other consumers read.
	 *
	 * @return the {@code disabled} attribute as authored, or else the negation of the authored
	 * 			{@code enabled} attribute; {@code null} when neither was authored
	 */
	@Override
	@XmlTransient
	public @Nullable String getDisabledCondition() {
		return (disabledCondition != null) ? disabledCondition : BindUtil.negateCondition(enabledCondition);
	}

	/**
	 * Sets the {@code disabled} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code enabled} attribute.
	 *
	 * @param disabledCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "disabled", required = false)
	public void setXmlDisabledCondition(@Nullable String disabledCondition) {
		this.disabledCondition = UtilImpl.processStringValue(disabledCondition);
		this.enabledCondition = null;
	}

	/**
	 * Returns the {@code disabled} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code enabled} or neither was authored
	 */
	@Override
	public @Nullable String getXmlDisabledCondition() {
		return disabledCondition;
	}

	
	/**
	 * Returns the {@code enabled} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code disabled} or neither was authored
	 */
	@Override
	public @Nullable String getXmlEnabledCondition() {
		return enabledCondition;
	}

	/**
	 * Sets the {@code enabled} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code disabled} attribute, so that
	 * {@link #getDisabledCondition()} returns the negation of this condition.
	 *
	 * @param enabledCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "enabled", required = false)
	public void setXmlEnabledCondition(@Nullable String enabledCondition) {
		this.enabledCondition = UtilImpl.processStringValue(enabledCondition);
		this.disabledCondition = null;
	}

	/**
	 * Returns the derived {@code disableAdd} condition that renderers and other consumers read.
	 *
	 * @return the {@code disableAdd} attribute as authored, or else the negation of the authored
	 * 			{@code enableAdd} attribute; {@code null} when neither was authored
	 */
	@Override
	@XmlTransient
	public @Nullable String getDisableAddCondition() {
		return (disableAddCondition != null) ? disableAddCondition : BindUtil.negateCondition(enableAddCondition);
	}

	/**
	 * Sets the {@code disableAdd} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code enableAdd} attribute.
	 *
	 * @param disableAddCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "disableAdd", required = false)
	public void setXmlDisableAddCondition(@Nullable String disableAddCondition) {
		this.disableAddCondition = UtilImpl.processStringValue(disableAddCondition);
		this.enableAddCondition = null;
	}

	/**
	 * Returns the {@code disableAdd} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code enableAdd} or neither was authored
	 */
	@Override
	public @Nullable String getXmlDisableAddCondition() {
		return disableAddCondition;
	}

	
	/**
	 * Returns the {@code enableAdd} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code disableAdd} or neither was authored
	 */
	@Override
	public @Nullable String getXmlEnableAddCondition() {
		return enableAddCondition;
	}

	/**
	 * Sets the {@code enableAdd} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code disableAdd} attribute, so that
	 * {@link #getDisableAddCondition()} returns the negation of this condition.
	 *
	 * @param enableAddCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "enableAdd", required = false)
	public void setXmlEnableAddCondition(@Nullable String enableAddCondition) {
		this.enableAddCondition = UtilImpl.processStringValue(enableAddCondition);
		this.disableAddCondition = null;
	}

	/**
	 * Returns the derived {@code disableZoom} condition that renderers and other consumers read.
	 *
	 * @return the {@code disableZoom} attribute as authored, or else the negation of the authored
	 * 			{@code enableZoom} attribute; {@code null} when neither was authored
	 */
	@Override
	@XmlTransient
	public @Nullable String getDisableZoomCondition() {
		return (disableZoomCondition != null) ? disableZoomCondition : BindUtil.negateCondition(enableZoomCondition);
	}

	/**
	 * Sets the {@code disableZoom} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code enableZoom} attribute.
	 *
	 * @param disableZoomCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "disableZoom", required = false)
	public void setXmlDisableZoomCondition(@Nullable String disableZoomCondition) {
		this.disableZoomCondition = UtilImpl.processStringValue(disableZoomCondition);
		this.enableZoomCondition = null;
	}

	/**
	 * Returns the {@code disableZoom} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code enableZoom} or neither was authored
	 */
	@Override
	public @Nullable String getXmlDisableZoomCondition() {
		return disableZoomCondition;
	}

	/**
	 * Returns the {@code enableZoom} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code disableZoom} or neither was authored
	 */
	@Override
	public @Nullable String getXmlEnableZoomCondition() {
		return enableZoomCondition;
	}

	/**
	 * Sets the {@code enableZoom} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code disableZoom} attribute, so that
	 * {@link #getDisableZoomCondition()} returns the negation of this condition.
	 *
	 * @param enableZoomCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "enableZoom", required = false)
	public void setXmlEnableZoomCondition(@Nullable String enableZoomCondition) {
		this.enableZoomCondition = UtilImpl.processStringValue(enableZoomCondition);
		this.disableZoomCondition = null;
	}

	/**
	 * Returns the derived {@code disableEdit} condition that renderers and other consumers read.
	 *
	 * @return the {@code disableEdit} attribute as authored, or else the negation of the authored
	 * 			{@code enableEdit} attribute; {@code null} when neither was authored
	 */
	@Override
	@XmlTransient
	public @Nullable String getDisableEditCondition() {
		return (disableEditCondition != null) ? disableEditCondition : BindUtil.negateCondition(enableEditCondition);
	}

	/**
	 * Sets the {@code disableEdit} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code enableEdit} attribute.
	 *
	 * @param disableEditCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "disableEdit", required = false)
	public void setXmlDisableEditCondition(@Nullable String disableEditCondition) {
		this.disableEditCondition = UtilImpl.processStringValue(disableEditCondition);
		this.enableEditCondition = null;
	}

	/**
	 * Returns the {@code disableEdit} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code enableEdit} or neither was authored
	 */
	@Override
	public @Nullable String getXmlDisableEditCondition() {
		return disableEditCondition;
	}

	/**
	 * Returns the {@code enableEdit} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code disableEdit} or neither was authored
	 */
	@Override
	public @Nullable String getXmlEnableEditCondition() {
		return enableEditCondition;
	}

	/**
	 * Sets the {@code enableEdit} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code disableEdit} attribute, so that
	 * {@link #getDisableEditCondition()} returns the negation of this condition.
	 *
	 * @param enableEditCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "enableEdit", required = false)
	public void setXmlEnableEditCondition(@Nullable String enableEditCondition) {
		this.enableEditCondition = UtilImpl.processStringValue(enableEditCondition);
		this.disableEditCondition = null;
	}

	/**
	 * Returns the derived {@code disableRemove} condition that renderers and other consumers read.
	 *
	 * @return the {@code disableRemove} attribute as authored, or else the negation of the authored
	 * 			{@code enableRemove} attribute; {@code null} when neither was authored
	 */
	@Override
	@XmlTransient
	public @Nullable String getDisableRemoveCondition() {
		return (disableRemoveCondition != null) ? disableRemoveCondition : BindUtil.negateCondition(enableRemoveCondition);
	}

	/**
	 * Sets the {@code disableRemove} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code enableRemove} attribute.
	 *
	 * @param disableRemoveCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "disableRemove", required = false)
	public void setXmlDisableRemoveCondition(@Nullable String disableRemoveCondition) {
		this.disableRemoveCondition = UtilImpl.processStringValue(disableRemoveCondition);
		this.enableRemoveCondition = null;
	}

	/**
	 * Returns the {@code disableRemove} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code enableRemove} or neither was authored
	 */
	@Override
	public @Nullable String getXmlDisableRemoveCondition() {
		return disableRemoveCondition;
	}

	
	/**
	 * Returns the {@code enableRemove} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code disableRemove} or neither was authored
	 */
	@Override
	public @Nullable String getXmlEnableRemoveCondition() {
		return enableRemoveCondition;
	}

	/**
	 * Sets the {@code enableRemove} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code disableRemove} attribute, so that
	 * {@link #getDisableRemoveCondition()} returns the negation of this condition.
	 *
	 * @param enableRemoveCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "enableRemove", required = false)
	public void setXmlEnableRemoveCondition(@Nullable String enableRemoveCondition) {
		this.enableRemoveCondition = UtilImpl.processStringValue(enableRemoveCondition);
		this.disableRemoveCondition = null;
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
