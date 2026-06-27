package org.skyve.impl.metadata.view.widget.bound.input;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.widget.bound.AbstractBound;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Invisible;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Abstract JAXB base for all data-bound input widgets in a view.
 *
 * <p>Provides common input-widget properties: binding path, disabled/invisible
 * conditions, and decorator settings.  Concrete subclasses represent specific
 * input controls (text field, combo, check box, etc.).
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see FocusableInputWidget
 * @see ChangeableInputWidget
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"invisibleConditionName", "visibleConditionName", "disabledConditionName", "enabledConditionName"})
public abstract class InputWidget extends AbstractBound implements Disableable, Invisible {
	private static final long serialVersionUID = -5332816537102477174L;

	private String disabledConditionName;
	private String invisibleConditionName;

	/**
	 * Returns the disable-condition expression for this widget.
	 *
	 * @return the condition that disables the widget, or {@code null} when always enabled.
	 */
	@Override
	public String getDisabledConditionName() {
		return disabledConditionName;
	}

	/**
	 * Sets the disable-condition expression for this widget.
	 *
	 * @param disabledConditionName a condition expression that disables the widget,
	 *            or {@code null}/blank to clear the condition.
	 */
	@Override
	@XmlAttribute(name = "disabled", required = false)
	public void setDisabledConditionName(String disabledConditionName) {
		this.disabledConditionName = UtilImpl.processStringValue(disabledConditionName);
	}

	/**
	 * JAXB-only placeholder accessor for the {@code enabled} attribute.
	 *
	 * @return always {@code null}; the canonical stored form is the disabled condition.
	 */
	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnabledConditionName() {
		return null;
	}

	/**
	 * Sets the enabled condition by storing its logical negation as the disabled condition.
	 *
	 * <p>Side effects: updates {@code disabledConditionName}. This keeps
	 * {@code enabled="..."} and {@code disabled="..."} metadata mutually exclusive
	 * while preserving one internal representation.
	 *
	 * @param enabledConditionName a condition expression that enables the widget
	 */
	@Override
	@XmlAttribute(name = "enabled", required = false)
	public void setEnabledConditionName(String enabledConditionName) {
		this.disabledConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(enabledConditionName));
	}

	/**
	 * Returns the invisible-condition expression for this widget.
	 *
	 * @return the condition that hides the widget, or {@code null} when always visible.
	 */
	@Override
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	/**
	 * Sets the invisible-condition expression for this widget.
	 *
	 * @param invisibleConditionName a condition expression that hides the widget,
	 *            or {@code null}/blank to clear the condition.
	 */
	@Override
	@XmlAttribute(name = "invisible", required = false)
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = UtilImpl.processStringValue(invisibleConditionName);
	}

	/**
	 * JAXB-only placeholder accessor for the {@code visible} attribute.
	 *
	 * @return always {@code null}; the canonical stored form is the invisible condition.
	 */
	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getVisibleConditionName() {
		return null;
	}

	/**
	 * Sets the visible condition by storing its logical negation as the invisible condition.
	 *
	 * <p>Side effects: updates {@code invisibleConditionName}. This keeps
	 * {@code visible="..."} and {@code invisible="..."} metadata mutually exclusive
	 * while preserving one internal representation.
	 *
	 * @param visibleConditionName a condition expression that makes the widget visible
	 */
	@Override
	@XmlAttribute(name = "visible", required = false)
	public void setVisibleConditionName(String visibleConditionName) {
		this.invisibleConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(visibleConditionName));
	}
}
