package org.skyve.impl.metadata.view.widget.bound.tabular;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.event.Editable;
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

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlElementRefs;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB-annotated query-driven list grid widget.
 *
 * <p>Renders rows from a named query or model with paging, filtering, sorting,
 * export, charting, and tagging capabilities.  Supports add, edit, remove,
 * zoom, and deselect operations with independent disable conditions and
 * event handler lists.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see AbstractListWidget
 * @see TreeGrid
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"disabledConditionName",
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
							"showFilter",
							"showSummary",
							"showExport",
							"showChart",
							"showSnap",
							"showTag",
							"showFlag",
							"autoPopulate",
							"selectedIdBinding",
							"continueConversation",
							"editedActions",
							"removedActions",
							"selectedActions",
							"properties"})
public class ListGrid extends AbstractListWidget implements DecoratedMetaData,
																Disableable,
																DisableableCRUDGrid,
																Editable,
																Removable,
																Selectable {
	private static final long serialVersionUID = 4739299969425100550L;

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
	private Boolean showExport;
	private Boolean showChart;
	private Boolean showFilter;
	private Boolean showSummary;
	private Boolean showSnap;
	private Boolean showTag;
	private Boolean showFlag;

	private Boolean autoPopulate;
	
	private String selectedIdBinding;
	private boolean continueConversation;
	
	private List<EventAction> editedActions = new ArrayList<>();
	private List<EventAction> removedActions = new ArrayList<>();
	private List<EventAction> selectedActions = new ArrayList<>();
	
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Indicates whether row actions should continue the existing conversation context.
	 *
	 * @return {@code true} when row actions continue the current conversation
	 */
	public boolean getContinueConversation() {
		return continueConversation;
	}

	/**
	 * Sets whether row actions should continue the existing conversation context.
	 *
	 * @param continueConversation {@code true} to keep the current conversation active
	 */
	@XmlAttribute(name = "continueConversation", required = true)
	public void setContinueConversation(boolean continueConversation) {
		this.continueConversation = continueConversation;
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
	 * JAXB placeholder for the enabled condition attribute.
	 *
	 * @return always {@code null}; the enabled value is derived from the disabled condition
	 */
	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnabledConditionName() {
		return null;
	}

	/**
	 * Sets the enabled condition by storing its negation as the disabled condition.
	 *
	 * @param enabledConditionName the enabled condition expression
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
	 * JAXB placeholder for the disable-add attribute's enabled counterpart.
	 *
	 * @return always {@code null}; the enabled value is derived from the disabled condition
	 */
	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnableAddConditionName() {
		return null;
	}

	/**
	 * Sets the add-enabled condition by storing its negation as the add-disabled condition.
	 *
	 * @param enableAddConditionName the add-enabled condition expression
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
	 * JAXB placeholder for the disable-zoom attribute's enabled counterpart.
	 *
	 * @return always {@code null}; the enabled value is derived from the disabled condition
	 */
	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnableZoomConditionName() {
		return null;
	}

	/**
	 * Sets the zoom-enabled condition by storing its negation as the zoom-disabled condition.
	 *
	 * @param enableZoomConditionName the zoom-enabled condition expression
	 */
	@Override
	@XmlAttribute(name = "enableZoom", required = false)
	public void setEnableZoomConditionName(String enableZoomConditionName) {
		this.disableZoomConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(enableZoomConditionName));
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
	 * JAXB placeholder for the disable-edit attribute's enabled counterpart.
	 *
	 * @return always {@code null}; the enabled value is derived from the disabled condition
	 */
	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnableEditConditionName() {
		return null;
	}

	/**
	 * Sets the edit-enabled condition by storing its negation as the edit-disabled condition.
	 *
	 * @param enableEditConditionName the edit-enabled condition expression
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
	 * JAXB placeholder for the disable-remove attribute's enabled counterpart.
	 *
	 * @return always {@code null}; the enabled value is derived from the disabled condition
	 */
	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnableRemoveConditionName() {
		return null;
	}

	/**
	 * Sets the remove-enabled condition by storing its negation as the remove-disabled condition.
	 *
	 * @param enableRemoveConditionName the remove-enabled condition expression
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
	 * Returns whether the export action should be shown.
	 *
	 * @return {@code true} when export is shown
	 */
	public Boolean getShowExport() {
		return showExport;
	}

	/**
	 * Sets whether the export action should be shown.
	 *
	 * @param showExport {@code true} to show export
	 */
	@XmlAttribute(name = "showExport", required = false)
	public void setShowExport(Boolean showExport) {
		this.showExport = showExport;
	}

	/**
	 * Returns whether the chart action should be shown.
	 *
	 * @return {@code true} when charting is shown
	 */
	public Boolean getShowChart() {
		return showChart;
	}

	/**
	 * Sets whether the chart action should be shown.
	 *
	 * @param showChart {@code true} to show charting
	 */
	@XmlAttribute(name = "showChart", required = false)
	public void setShowChart(Boolean showChart) {
		this.showChart = showChart;
	}

	/**
	 * Returns whether the filter UI should be shown.
	 *
	 * @return {@code true} when the filter UI is shown
	 */
	public Boolean getShowFilter() {
		return showFilter;
	}

	/**
	 * Sets whether the filter UI should be shown.
	 *
	 * @param showFilter {@code true} to show the filter UI
	 */
	@XmlAttribute(name = "showFilter", required = false)
	public void setShowFilter(Boolean showFilter) {
		this.showFilter = showFilter;
	}

	/**
	 * Returns whether summary information should be shown.
	 *
	 * @return {@code true} when summary information is shown
	 */
	public Boolean getShowSummary() {
		return showSummary;
	}

	/**
	 * Sets whether summary information should be shown.
	 *
	 * @param showSummary {@code true} to show summary information
	 */
	@XmlAttribute(name = "showSummary", required = false)
	public void setShowSummary(Boolean showSummary) {
		this.showSummary = showSummary;
	}

	/**
	 * Returns whether snap actions should be shown.
	 *
	 * @return {@code true} when snap actions are shown
	 */
	public Boolean getShowSnap() {
		return showSnap;
	}

	/**
	 * Sets whether snap actions should be shown.
	 *
	 * @param showSnap {@code true} to show snap actions
	 */
	@XmlAttribute(name = "showSnap", required = false)
	public void setShowSnap(Boolean showSnap) {
		this.showSnap = showSnap;
	}

	/**
	 * Returns whether tag actions should be shown.
	 *
	 * @return {@code true} when tag actions are shown
	 */
	public Boolean getShowTag() {
		return showTag;
	}

	/**
	 * Sets whether tag actions should be shown.
	 *
	 * @param showTag {@code true} to show tag actions
	 */
	@XmlAttribute(name = "showTag", required = false)
	public void setShowTag(Boolean showTag) {
		this.showTag = showTag;
	}

	/**
	 * Returns whether flag actions should be shown.
	 *
	 * @return {@code true} when flag actions are shown
	 */
	public Boolean getShowFlag() {
		return showFlag;
	}

	/**
	 * Sets whether flag actions should be shown.
	 *
	 * @param showFlag {@code true} to show flag actions
	 */
	@XmlAttribute(name = "showFlag", required = false)
	public void setShowFlag(Boolean showFlag) {
		this.showFlag = showFlag;
	}

	/**
	 * Returns whether auto-population is enabled.
	 *
	 * @return {@code true} when auto-population is enabled
	 */
	public Boolean getAutoPopulate() {
		return autoPopulate;
	}

	/**
	 * Sets whether auto-population is enabled.
	 *
	 * @param autoPopulate {@code true} to enable auto-population
	 */
	@XmlAttribute(name = "autoPopulate", required = false)
	public void setAutoPopulate(Boolean autoPopulate) {
		this.autoPopulate = autoPopulate;
	}

	/**
	 * Returns the selected-row identifier binding.
	 *
	 * @return the selected-row identifier binding, or {@code null} when not set
	 */
	@Override
	public String getSelectedIdBinding() {
		return selectedIdBinding;
	}

	/**
	 * Sets the selected-row identifier binding after trimming and empty-string normalisation.
	 *
	 * @param selectedIdBinding the selected-row identifier binding
	 */
	@Override
	@XmlAttribute(name = "selectedIdBinding")
	public void setSelectedIdBinding(String selectedIdBinding) {
		this.selectedIdBinding = UtilImpl.processStringValue(selectedIdBinding);
	}

	/**
	 * Returns the mutable event handlers triggered after a row is edited.
	 *
	 * @return mutable edited handlers
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
	 * @return mutable removed handlers
	 */
	@Override
	@XmlElementWrapper(namespace = XMLMetaData.VIEW_NAMESPACE, name = "onDeletedHandlers")
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
	 * Returns the event source identifier used by event dispatch = query name (or model name).
	 *
	 * <p>Prefers {@link #getQueryName()} and falls back to {@link #getModelName()} when no query is configured.
	 *
	 * @return the query name if present, otherwise the model name
	 */
	@Override
	@XmlTransient
	public String getSource() {
		String queryName = getQueryName();
		return (queryName == null) ? getModelName() : queryName;
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
