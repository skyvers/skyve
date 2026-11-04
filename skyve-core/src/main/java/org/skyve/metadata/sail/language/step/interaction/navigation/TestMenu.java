package org.skyve.metadata.sail.language.step.interaction.navigation;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Asserts whether a module's menu, or one of its menu items, is shown to the current user,
 * for example {@code <testMenu module="admin" item="Users" present="false"/>}.
 *
 * <p>{@code item} is named as for {@link NavigateMenu}. Without {@code item}, the step tests
 * whether the module itself appears in the menu. {@code present="true"} or absent asserts that
 * it is shown; {@code present="false"} asserts that it is not.
 *
 * @see NavigateMenu
 * @see org.skyve.metadata.sail.execution.Executor#executeTestMenu
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class TestMenu implements Step {

	private String moduleName;
	private String itemName;
	private Boolean present;

	/**
	 * Returns the module whose menu is tested.
	 * @return the module name
	 */
	public @Nonnull String getModuleName() {
		return moduleName;
	}

	/**
	 * Sets the module whose menu is tested.
	 * @param moduleName the module name
	 */
	@XmlAttribute(name = "module", required = true)
	public void setModuleName(@Nonnull String moduleName) {
		this.moduleName = UtilImpl.processStringValue(moduleName);
	}

	/**
	 * Returns the name of the menu item to test.
	 * @return the item name, or {@code null} to test the module itself
	 */
	public @Nullable String getItemName() {
		return itemName;
	}

	/**
	 * Sets the name of the menu item to test.
	 * @param itemName the item name, optionally qualified by its group names separated by {@code /},
	 * 			or {@code null} to test the module itself
	 */
	@XmlAttribute(name = "item")
	public void setItemName(@Nullable String itemName) {
		this.itemName = UtilImpl.processStringValue(itemName);
	}

	/**
	 * Returns whether the menu entry must be shown.
	 * @return {@code true} or {@code null} if it must be shown, {@code false} if it must not
	 */
	public @Nullable Boolean getPresent() {
		return present;
	}

	/**
	 * Sets whether the menu entry must be shown.
	 * @param present {@code true} or {@code null} if it must be shown, {@code false} if it must not
	 */
	@XmlAttribute(name = "present")
	public void setPresent(@Nullable Boolean present) {
		this.present = present;
	}

	/**
	 * Dispatches to {@link Executor#executeTestMenu(TestMenu)}.
	 * @param executor the executor
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		executor.executeTestMenu(this);
	}

	/**
	 * Returns {@code module} or {@code module/item}.
	 * @param context the context
	 * @return the identifier
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		return (itemName == null) ? moduleName : moduleName + '/' + itemName;
	}
}
