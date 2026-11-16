package org.skyve.impl.sail.execution.sc;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.CORE;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.sail.execution.sc.Locator.Kind;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.sail.language.step.context.PushEditContext;
import org.skyve.metadata.sail.language.step.context.PushListContext;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Automation context implementation for SmartClient client, handling list and edit contexts specific to SmartClient UI components.
 * <p>
 * The locators are SAIL locators (see {@link Locator}), resolved in the browser against the view that
 * is showing, so the context does not need to track window numbers or SmartClient component IDs.
 *
 * @author simeonsolomou
 */
public class SmartClientAutomationContext extends AutomationContext<SmartClientGenerateListContext, SmartClientGenerateEditContext> {
	private static final String ROW_CELL = "/body/row[%d]/col[%d]";

	private Map<String, List<Locator>> locators = new TreeMap<>();

	// The binding of the data grid whose row is being edited inline, if any
	private String inlineGridBinding;

	/**
	 * Generates list-view locators for the supplied SmartClient list context.
	 *
	 * @param listContext the list generation context
	 */
	@Override
	public void generate(@Nonnull SmartClientGenerateListContext listContext) {
		PushListContext push = listContext.pushListContext();

		String listGridIdentifier = push.getIdentifier(this);
		String key = NavigateList.listGridIdentifier(this,
				push.getModuleName(),
				push.getQueryName(),
				push.getDocumentName(),
				push.getModelName());

		// Populate the toolbar locators
		put(key + ".new", Locator.widget(null, Kind.list, null, null, "new", null));
		put(key + ".zoom", Locator.widget(null, Kind.list, null, null, "zoom", null));

		// Results
		put(listGridIdentifier + ".select", Locator.widget(null, Kind.list, null, null, "rows", ROW_CELL));
	}

	/**
	 * Generates edit-view locators for the supplied SmartClient edit context.
	 *
	 * @param editContext the edit generation context
	 */
	@Override
	public void generate(@Nonnull SmartClientGenerateEditContext editContext) {
		PushEditContext push = editContext.pushEditContext();

		User user = CORE.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(push.getModuleName());
		Document document = module.getDocument(customer, push.getDocumentName());

		String viewType = Boolean.TRUE.equals(push.getCreateView())
				? ViewType.create.toString()
				: ViewType.edit.toString();

		String uxui = getUxui();
		if (uxui == null) {
			throw new IllegalStateException("The automation context has no uxui - push it before generating");
		}
		View view = document.getView(uxui, customer, viewType);

		SmartClientSAILViewVisitor visitor = new SmartClientSAILViewVisitor(
				user,
				module,
				document,
				view,
				uxui,
				this);

		visitor.visit();
	}

	/**
	 * Adds a locator for a given identifier, unless an equal locator is already registered for it.
	 *
	 * @param identifier the logical identifier for the UI element
	 * @param locator the locator to associate with the identifier
	 */
	void put(@Nonnull String identifier, @Nonnull Locator locator) {
		List<Locator> locatorList = locators.computeIfAbsent(identifier, key -> new ArrayList<>());
		for (Locator existing : locatorList) {
			if (existing.getLocator().equals(locator.getLocator()) && (existing.getInputType() == locator.getInputType())) {
				return;
			}
		}
		locatorList.add(locator);
	}

	/**
	 * Retrieves all locators associated with a given identifier.
	 *
	 * @param identifier the logical identifier for the UI element
	 * @return the list of locators, or {@code null} if none exist
	 */
	public @Nullable List<Locator> getLocators(@Nonnull String identifier) {
		return locators.get(identifier);
	}

	/**
	 * Returns the binding of the data grid whose row is being edited inline.
	 *
	 * @return the grid binding, or {@code null} when no row is being edited inline
	 */
	public @Nullable String getInlineGridBinding() {
		return inlineGridBinding;
	}

	/**
	 * Records the data grid whose row is being edited inline. While it is set, data entry bindings
	 * are tried as {@code <grid binding>.<column binding>} first.
	 *
	 * @param inlineGridBinding the grid binding, or {@code null} when inline editing ends
	 */
	public void setInlineGridBinding(@Nullable String inlineGridBinding) {
		this.inlineGridBinding = inlineGridBinding;
	}
}
