package org.skyve.impl.metadata.view.widget.bound.input;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.AbsoluteWidth;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.metadata.view.event.Addable;
import org.skyve.impl.metadata.view.event.EventAction;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.widget.FilterParameterImpl;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Editable;
import org.skyve.metadata.view.Filterable;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlElementRefs;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB-annotated lookup-description widget that combines a text-input field
 * with a pick-list popup for selecting an associated document instance.
 *
 * <p>Bound to an association attribute.  Renders the description of the
 * selected instance in the text field and provides pick, clear, add, and
 * edit action buttons with independently configurable visibility conditions.
 * Supports filter parameters passed to the backing query.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see LookupDescriptionColumn
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"descriptionBinding",
							"query",
							"disableEditConditionName",
							"enableEditConditionName",
							"disableAddConditionName",
							"enableAddConditionName",
							"disableClearConditionName",
							"enableClearConditionName",
							"disablePickConditionName",
							"enablePickConditionName",
							"editable",
							"pixelWidth",
							"dropDownColumns",
							"pickedActions",
							"clearedActions",
							"editedActions",
							"addedActions",
							"filterParameters",
							"parameters",
							"properties"})
public class LookupDescription extends InputWidget implements FormItemWidget, Editable, AbsoluteWidth, Filterable, Addable, org.skyve.impl.metadata.view.event.Editable {
	private static final long serialVersionUID = -6196902681383778156L;

	private String descriptionBinding;
	private String query;

	private String disableEditConditionName;
	private String disableAddConditionName;
	private String disableClearConditionName;
	private String disablePickConditionName;

	private Boolean editable;
	private Integer pixelWidth;
	private List<LookupDescriptionColumn> dropDownColumns = new ArrayList<>();
	

	private List<EventAction> pickedActions = new ArrayList<>();
	private List<EventAction> clearedActions = new ArrayList<>();
	private List<EventAction> editedActions = new ArrayList<>();
	private List<EventAction> addedActions = new ArrayList<>();

	private List<FilterParameter> filterParameters = new ArrayList<>();
	private List<Parameter> parameters = new ArrayList<>();
	
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Indicates lookup descriptions render a label by default.
	 *
	 * @return {@code true} because this widget renders with a label by default
	 */
	@Override
	public boolean showsLabelByDefault() {
		return true;
	}
	
	/**
	 * Returns the binding used to display the associated document description.
	 *
	 * @return the description binding expression
	 */
	public String getDescriptionBinding() {
		return descriptionBinding;
	}

	/**
	 * Sets the description binding path.
	 *
	 * @param descriptionBinding the description binding expression to apply
	 */
	@XmlAttribute(required = true)
	public void setDescriptionBinding(String descriptionBinding) {
		this.descriptionBinding = descriptionBinding;
	}

	/**
	 * Returns the optional lookup query name.
	 *
	 * @return the configured query name, or {@code null} when default resolution is used
	 */
	public String getQuery() {
		return query;
	}

	/**
	 * Sets the lookup query name after trimming and empty-string normalisation.
	 *
	 * @param query the lookup query name
	 */
	@XmlAttribute(required = false)
	public void setQuery(String query) {
		this.query = UtilImpl.processStringValue(query);
	}

	/**
	 * Returns the disable-edit condition expression.
	 *
	 * @return the disable-edit condition expression
	 */
	public String getDisableEditConditionName() {
		return disableEditConditionName;
	}

	/**
	 * Sets the disable-edit condition expression.
	 *
	 * @param disableEditConditionName the disable-edit condition expression
	 */
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
	 * Sets the enable-edit condition by storing its logical negation as the disable-edit condition.
	 *
	 * <p>Side effects: updates {@code disableEditConditionName}.
	 *
	 * @param enableEditConditionName a condition expression that enables edit actions
	 */
	@XmlAttribute(name = "enableEdit", required = false)
	public void setEnableEditConditionName(String enableEditConditionName) {
		this.disableEditConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(enableEditConditionName));
	}

	/**
	 * Returns the disable-add condition expression.
	 *
	 * @return the disable-add condition expression
	 */
	public String getDisableAddConditionName() {
		return disableAddConditionName;
	}

	/**
	 * Sets the disable-add condition expression.
	 *
	 * @param disableAddConditionName the disable-add condition expression
	 */
	@XmlAttribute(name = "disableAdd", required = false)
	public void setDisableAddConditionName(String disableAddConditionName) {
		this.disableAddConditionName = Util.processStringValue(disableAddConditionName);
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
	 * Sets the enable-add condition by storing its logical negation as the disable-add condition.
	 *
	 * <p>Side effects: updates {@code disableAddConditionName}.
	 *
	 * @param enableAddConditionName a condition expression that enables add actions
	 */
	@XmlAttribute(name = "enableAdd", required = false)
	public void setEnableAddConditionName(String enableAddConditionName) {
		this.disableAddConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(enableAddConditionName));
	}

	/**
	 * Returns the disable-clear condition expression.
	 *
	 * @return the disable-clear condition expression
	 */
	public String getDisableClearConditionName() {
		return disableClearConditionName;
	}

	/**
	 * Sets the disable-clear condition expression.
	 *
	 * @param disableClearConditionName the disable-clear condition expression
	 */
	@XmlAttribute(name = "disableClear", required = false)
	public void setDisableClearConditionName(String disableClearConditionName) {
		this.disableClearConditionName = Util.processStringValue(disableClearConditionName);
	}

	/**
	 * JAXB-only placeholder for {@code enableClear}; value is derived via {@link #setEnableClearConditionName(String)}.
	 *
	 * @return always {@code null}; JAXB writes via {@code setEnableClearConditionName}
	 */
	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnableClearConditionName() {
		return null;
	}

	/**
	 * Sets the enable-clear condition by storing its logical negation as the disable-clear condition.
	 *
	 * <p>Side effects: updates {@code disableClearConditionName}.
	 *
	 * @param enableClearConditionName a condition expression that enables clear actions
	 */
	@XmlAttribute(name = "enableClear", required = false)
	public void setEnableClearConditionName(String enableClearConditionName) {
		this.disableClearConditionName = BindUtil.negateCondition(Util.processStringValue(enableClearConditionName));
	}

	/**
	 * Returns the disable-pick condition expression.
	 *
	 * @return the disable-pick condition expression
	 */
	public String getDisablePickConditionName() {
		return disablePickConditionName;
	}

	/**
	 * Sets the disable-pick condition expression.
	 *
	 * @param disablePickConditionName the disable-pick condition expression
	 */
	@XmlAttribute(name = "disablePick", required = false)
	public void setDisablePickConditionName(String disablePickConditionName) {
		this.disablePickConditionName = Util.processStringValue(disablePickConditionName);
	}

	/**
	 * JAXB-only placeholder for {@code enablePick}; value is derived via {@link #setEnablePickConditionName(String)}.
	 *
	 * @return always {@code null}; JAXB writes via {@code setEnablePickConditionName}
	 */
	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnablePickConditionName() {
		return null;
	}

	/**
	 * Sets the enable-pick condition by storing its logical negation as the disable-pick condition.
	 *
	 * <p>Side effects: updates {@code disablePickConditionName}.
	 *
	 * @param enablePickConditionName a condition expression that enables pick actions
	 */
	@XmlAttribute(name = "enablePick", required = false)
	public void setEnablePickConditionName(String enablePickConditionName) {
		this.disablePickConditionName = BindUtil.negateCondition(Util.processStringValue(enablePickConditionName));
	}

	/**
	 * Returns whether this widget is editable.
	 *
	 * @return {@code true} when editable interactions are enabled; otherwise {@code false}
	 */
	@Override
	public Boolean getEditable() {
		return editable;
	}

	/**
	 * Sets whether this widget is editable.
	 *
	 * @param editable {@code true} to allow edits, {@code false} to prevent edits
	 */
	@Override
	@XmlAttribute(name = "editable", required = false)
	public void setEditable(Boolean editable) {
		this.editable = editable;
	}

	/**
	 * Returns the configured absolute pixel width, or {@code null} for renderer defaults.
	 *
	 * @return the absolute pixel width, or {@code null} to use renderer defaults
	 */
	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	/**
	 * Sets the absolute pixel width for the rendered control.
	 *
	 * @param pixelWidth the absolute pixel width to apply
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}
	
	/**
	 * Returns the ordered columns rendered in the lookup drop-down.
	 *
	 * <p>The returned list is mutable and never {@code null}.
	 *
	 * @return mutable drop-down column metadata in render order
	 */
	@XmlElementWrapper(namespace = XMLMetaData.VIEW_NAMESPACE, name = "dropDown")
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "column", required = false)
	public List<LookupDescriptionColumn> getDropDownColumns() {
		return dropDownColumns;
	}

	/**
	 * Returns mutable filter parameters forwarded to the lookup query.
	 *
	 * @return mutable filter parameters for query evaluation
	 */
	@Override
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE,
					name = "filterParameter",
					type = FilterParameterImpl.class,
					required = false)
	public List<FilterParameter> getFilterParameters() {
		return filterParameters;
	}

	/**
	 * Returns mutable runtime parameters forwarded to lookup execution.
	 *
	 * @return mutable runtime parameters for lookup execution
	 */
	@Override
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE,
					name = "parameter",
					type = ParameterImpl.class,
					required = false)
	public List<Parameter> getParameters() {
		return parameters;
	}

	/**
	 * Returns mutable handlers fired when a value is picked.
	 *
	 * @return mutable picked-action handlers
	 */
	@XmlElementWrapper(namespace = XMLMetaData.VIEW_NAMESPACE, name = "onPickedHandlers")
	@XmlElementRefs({@XmlElementRef(type = RerenderEventAction.class), 
						@XmlElementRef(type = ServerSideActionEventAction.class),
						@XmlElementRef(type = SetDisabledEventAction.class),
						@XmlElementRef(type = SetInvisibleEventAction.class)})
	public List<EventAction> getPickedActions() {
		return pickedActions;
	}

	/**
	 * Returns mutable handlers fired when a value is cleared.
	 *
	 * @return mutable cleared-action handlers
	 */
	@XmlElementWrapper(namespace = XMLMetaData.VIEW_NAMESPACE, name = "onClearedHandlers")
	@XmlElementRefs({@XmlElementRef(type = RerenderEventAction.class), 
						@XmlElementRef(type = ServerSideActionEventAction.class),
						@XmlElementRef(type = SetDisabledEventAction.class),
						@XmlElementRef(type = SetInvisibleEventAction.class)})
	public List<EventAction> getClearedActions() {
		return clearedActions;
	}

	/**
	 * Returns mutable handlers fired when the selected value is edited.
	 *
	 * @return mutable edited-action handlers
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
	 * Returns mutable handlers fired when a new value is added.
	 *
	 * @return mutable added-action handlers
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
	 * Returns the mutable decorator property map for this widget.
	 *
	 * @return mutable decorator property map keyed by property name
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
