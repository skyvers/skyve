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

import jakarta.annotation.Nullable;
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
							"modelName",
							"xmlDisableEditCondition",
							"xmlEnableEditCondition",
							"xmlDisableAddCondition",
							"xmlEnableAddCondition",
							"xmlDisableClearCondition",
							"xmlEnableClearCondition",
							"xmlDisablePickCondition",
							"xmlEnablePickCondition",
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
	private String modelName;

	private String disableEditCondition;
	private String enableEditCondition;
	private String disableAddCondition;
	private String enableAddCondition;
	private String disableClearCondition;
	private String enableClearCondition;
	private String disablePickCondition;
	private String enablePickCondition;

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
	 * Returns the list model owned by the enclosing view's document.
	 *
	 * @return the model name, or {@code null} to use query resolution
	 */
	public String getModelName() {
		return modelName;
	}

	/**
	 * Sets the document-scoped list model used instead of a query.
	 *
	 * @param modelName the model name; blank values are normalised to {@code null}
	 */
	@XmlAttribute(name = "model", required = false)
	public void setModelName(String modelName) {
		this.modelName = UtilImpl.processStringValue(modelName);
	}

	/**
	 * Returns the derived {@code disableEdit} condition that renderers and other consumers read.
	 *
	 * @return the {@code disableEdit} attribute as authored, or else the negation of the authored
	 * 			{@code enableEdit} attribute; {@code null} when neither was authored
	 */
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
	public @Nullable String getXmlDisableEditCondition() {
		return disableEditCondition;
	}

	/**
	 * Returns the {@code enableEdit} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code disableEdit} or neither was authored
	 */
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
	@XmlAttribute(name = "enableEdit", required = false)
	public void setXmlEnableEditCondition(@Nullable String enableEditCondition) {
		this.enableEditCondition = UtilImpl.processStringValue(enableEditCondition);
		this.disableEditCondition = null;
	}

	/**
	 * Returns the derived {@code disableAdd} condition that renderers and other consumers read.
	 *
	 * @return the {@code disableAdd} attribute as authored, or else the negation of the authored
	 * 			{@code enableAdd} attribute; {@code null} when neither was authored
	 */
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
	public @Nullable String getXmlDisableAddCondition() {
		return disableAddCondition;
	}

	/**
	 * Returns the {@code enableAdd} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code disableAdd} or neither was authored
	 */
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
	@XmlAttribute(name = "enableAdd", required = false)
	public void setXmlEnableAddCondition(@Nullable String enableAddCondition) {
		this.enableAddCondition = UtilImpl.processStringValue(enableAddCondition);
		this.disableAddCondition = null;
	}

	/**
	 * Returns the derived {@code disableClear} condition that renderers and other consumers read.
	 *
	 * @return the {@code disableClear} attribute as authored, or else the negation of the authored
	 * 			{@code enableClear} attribute; {@code null} when neither was authored
	 */
	@XmlTransient
	public @Nullable String getDisableClearCondition() {
		return (disableClearCondition != null) ? disableClearCondition : BindUtil.negateCondition(enableClearCondition);
	}

	/**
	 * Sets the {@code disableClear} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code enableClear} attribute.
	 *
	 * @param disableClearCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	@XmlAttribute(name = "disableClear", required = false)
	public void setXmlDisableClearCondition(@Nullable String disableClearCondition) {
		this.disableClearCondition = UtilImpl.processStringValue(disableClearCondition);
		this.enableClearCondition = null;
	}

	/**
	 * Returns the {@code disableClear} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code enableClear} or neither was authored
	 */
	public @Nullable String getXmlDisableClearCondition() {
		return disableClearCondition;
	}

	/**
	 * Returns the {@code enableClear} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code disableClear} or neither was authored
	 */
	public @Nullable String getXmlEnableClearCondition() {
		return enableClearCondition;
	}

	/**
	 * Sets the {@code enableClear} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code disableClear} attribute, so that
	 * {@link #getDisableClearCondition()} returns the negation of this condition.
	 *
	 * @param enableClearCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	@XmlAttribute(name = "enableClear", required = false)
	public void setXmlEnableClearCondition(@Nullable String enableClearCondition) {
		this.enableClearCondition = UtilImpl.processStringValue(enableClearCondition);
		this.disableClearCondition = null;
	}

	/**
	 * Returns the derived {@code disablePick} condition that renderers and other consumers read.
	 *
	 * @return the {@code disablePick} attribute as authored, or else the negation of the authored
	 * 			{@code enablePick} attribute; {@code null} when neither was authored
	 */
	@XmlTransient
	public @Nullable String getDisablePickCondition() {
		return (disablePickCondition != null) ? disablePickCondition : BindUtil.negateCondition(enablePickCondition);
	}

	/**
	 * Sets the {@code disablePick} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code enablePick} attribute.
	 *
	 * @param disablePickCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	@XmlAttribute(name = "disablePick", required = false)
	public void setXmlDisablePickCondition(@Nullable String disablePickCondition) {
		this.disablePickCondition = UtilImpl.processStringValue(disablePickCondition);
		this.enablePickCondition = null;
	}

	/**
	 * Returns the {@code disablePick} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code enablePick} or neither was authored
	 */
	public @Nullable String getXmlDisablePickCondition() {
		return disablePickCondition;
	}

	/**
	 * Returns the {@code enablePick} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code disablePick} or neither was authored
	 */
	public @Nullable String getXmlEnablePickCondition() {
		return enablePickCondition;
	}

	/**
	 * Sets the {@code enablePick} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code disablePick} attribute, so that
	 * {@link #getDisablePickCondition()} returns the negation of this condition.
	 *
	 * @param enablePickCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	@XmlAttribute(name = "enablePick", required = false)
	public void setXmlEnablePickCondition(@Nullable String enablePickCondition) {
		this.enablePickCondition = UtilImpl.processStringValue(enablePickCondition);
		this.disablePickCondition = null;
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
