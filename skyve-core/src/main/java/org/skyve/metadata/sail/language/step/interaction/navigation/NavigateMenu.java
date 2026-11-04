package org.skyve.metadata.sail.language.step.interaction.navigation;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.annotation.Nonnull;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Navigates by clicking through the rendered menu: opens the menu of {@code module} and clicks
 * its menu item {@code item}, for example {@code <navigateMenu module="admin" item="Contacts"/>}.
 *
 * <p>Unlike {@link NavigateList} and {@link NavigateEdit}, which open a URL directly, this
 * exercises menu rendering and the user's menu permissions. {@code item} is the item's
 * {@code name} as declared in the module's menu metadata. When the name is not unique within
 * the module, qualify it with its enclosing group names separated by {@code /}, for example
 * {@code item="Security Admin/Users"}.
 *
 * <p>Side effects: after the click, the executor sets its context as the equivalent
 * {@code navigateList}, {@code navigateEdit}, {@code navigateTree}, {@code navigateMap},
 * {@code navigateCalendar} or {@code navigateLink} step would for that menu item.
 *
 * @see TestMenu
 * @see org.skyve.metadata.sail.execution.Executor#executeNavigateMenu
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class NavigateMenu implements Step {

	private String moduleName;
	private String itemName;

	/**
	 * Returns the module whose menu is used.
	 * @return the module name
	 */
	public @Nonnull String getModuleName() {
		return moduleName;
	}

	/**
	 * Sets the module whose menu is used.
	 * @param moduleName the module name
	 */
	@XmlAttribute(name = "module", required = true)
	public void setModuleName(@Nonnull String moduleName) {
		this.moduleName = UtilImpl.processStringValue(moduleName);
	}

	/**
	 * Returns the name of the menu item to click.
	 * @return the item name, optionally qualified by its group names separated by {@code /}
	 */
	public @Nonnull String getItemName() {
		return itemName;
	}

	/**
	 * Sets the name of the menu item to click.
	 * @param itemName the item name, optionally qualified by its group names separated by {@code /}
	 */
	@XmlAttribute(name = "item", required = true)
	public void setItemName(@Nonnull String itemName) {
		this.itemName = UtilImpl.processStringValue(itemName);
	}

	/**
	 * Dispatches to {@link Executor#executeNavigateMenu(NavigateMenu)}.
	 * @param executor the executor
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		executor.executeNavigateMenu(this);
	}

	/**
	 * Returns {@code module/item}.
	 * @param context the context
	 * @return the identifier
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		return moduleName + '/' + itemName;
	}
}
