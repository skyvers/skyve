package org.skyve.metadata.sail.language.step;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Asserts whether a widget, action button or tab of the current view is visible and enabled,
 * for example {@code <testWidget action="Restore" enabled="false"/>}.
 *
 * <p>Exactly one of {@code binding}, {@code action} and {@code tab} is required; the XSD cannot
 * express this, so {@link #execute(Executor)} checks it. {@code tab} is a tab path as for
 * {@link org.skyve.metadata.sail.language.step.interaction.TabSelect}.
 *
 * <ul>
 *   <li>{@code visible="true"} asserts the target is shown; {@code visible="false"} asserts it is
 *       hidden or not rendered at all.</li>
 *   <li>{@code enabled="true"} or {@code enabled="false"} asserts the target is shown and is
 *       (or is not) enabled.</li>
 *   <li>With neither attribute, the step asserts {@code visible="true"}.</li>
 * </ul>
 * {@code visible="false"} cannot be combined with {@code enabled}.
 *
 * @see org.skyve.metadata.sail.execution.Executor#executeTestWidget
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class TestWidget implements Step {

	private String binding;
	private String actionName;
	private String tabPath;
	private Boolean visible;
	private Boolean enabled;

	/**
	 * Returns the binding of the widget to test.
	 * @return the binding, or {@code null} when an action or tab is tested
	 */
	public @Nullable String getBinding() {
		return binding;
	}

	/**
	 * Sets the binding of the widget to test.
	 * @param binding the binding, or {@code null}
	 */
	@XmlAttribute(name = "binding")
	public void setBinding(@Nullable String binding) {
		this.binding = UtilImpl.processStringValue(binding);
	}

	/**
	 * Returns the name of the action whose button is tested.
	 * @return the action name, or {@code null} when a widget or tab is tested
	 */
	public @Nullable String getActionName() {
		return actionName;
	}

	/**
	 * Sets the name of the action whose button is tested.
	 * @param actionName the action name, or {@code null}
	 */
	@XmlAttribute(name = "action")
	public void setActionName(@Nullable String actionName) {
		this.actionName = UtilImpl.processStringValue(actionName);
	}

	/**
	 * Returns the path of the tab to test.
	 * @return the tab path, or {@code null} when a widget or action is tested
	 */
	public @Nullable String getTabPath() {
		return tabPath;
	}

	/**
	 * Sets the path of the tab to test.
	 * @param tabPath the tab path, or {@code null}
	 */
	@XmlAttribute(name = "tab")
	public void setTabPath(@Nullable String tabPath) {
		this.tabPath = UtilImpl.processStringValue(tabPath);
	}

	/**
	 * Returns whether the target must be visible.
	 * @return the expected visibility, or {@code null}; see {@link #isExpectedVisible()}
	 */
	public @Nullable Boolean getVisible() {
		return visible;
	}

	/**
	 * Sets whether the target must be visible.
	 * @param visible the expected visibility, or {@code null}
	 */
	@XmlAttribute(name = "visible")
	public void setVisible(@Nullable Boolean visible) {
		this.visible = visible;
	}

	/**
	 * Returns whether the target must be enabled.
	 * @return the expected enabled state, or {@code null} to not test it
	 */
	public @Nullable Boolean getEnabled() {
		return enabled;
	}

	/**
	 * Sets whether the target must be enabled.
	 * @param enabled the expected enabled state, or {@code null} to not test it
	 */
	@XmlAttribute(name = "enabled")
	public void setEnabled(@Nullable Boolean enabled) {
		this.enabled = enabled;
	}

	/**
	 * Returns whether the target must be visible after applying the defaults.
	 * @return {@code false} only when {@code visible="false"}
	 */
	public boolean isExpectedVisible() {
		return ! Boolean.FALSE.equals(visible);
	}

	/**
	 * Checks the attribute combination, then dispatches to {@link Executor#executeTestWidget(TestWidget)}.
	 * @param executor the executor
	 * @throws MetaDataException if not exactly one of {@code binding}, {@code action} and {@code tab}
	 * 			is set, or {@code visible="false"} is combined with {@code enabled}
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		int targets = ((binding == null) ? 0 : 1) + ((actionName == null) ? 0 : 1) + ((tabPath == null) ? 0 : 1);
		if (targets != 1) {
			throw new MetaDataException("<TestWidget /> requires exactly one of [binding], [action] or [tab]");
		}
		if (Boolean.FALSE.equals(visible) && (enabled != null)) {
			throw new MetaDataException("<TestWidget /> [visible=\"false\"] cannot be combined with [enabled]");
		}
		executor.executeTestWidget(this);
	}

	/**
	 * Returns the binding, the action name, or the tab path followed by {@code " Tab"},
	 * matching the identifiers of {@link org.skyve.metadata.sail.language.step.interaction.DataEnter},
	 * {@link org.skyve.metadata.sail.language.step.interaction.actions.Action} and
	 * {@link org.skyve.metadata.sail.language.step.interaction.TabSelect}.
	 * @param context the context
	 * @return the identifier
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		if (binding != null) {
			return binding;
		}
		if (actionName != null) {
			return actionName;
		}
		return tabPath + " Tab";
	}
}
