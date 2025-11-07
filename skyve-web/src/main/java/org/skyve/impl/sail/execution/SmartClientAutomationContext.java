package org.skyve.impl.sail.execution;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.sail.language.step.context.PushEditContext;
import org.skyve.metadata.sail.language.step.context.PushListContext;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;

/**
 * Automation context implementation for SmartClient client, handling list and edit contexts specific to SmartClient UI components.
 * 
 * @author simeonsolomou
 */
public class SmartClientAutomationContext extends AutomationContext<SmartClientGenerateListContext, SmartClientGenerateEditContext> {

	private static int windowNumber = 0;
	
	private Map<String, List<Locator>> locators = new TreeMap<>();

	@Override
	public void generate(SmartClientGenerateListContext listContext) {
		PushListContext push = listContext.pushListContext();

		String listGridIdentifier = push.getIdentifier(this);
		String key = NavigateList.listGridIdentifier(this,
				push.getModuleName(),
				push.getQueryName(),
				push.getDocumentName(),
				push.getModelName());

		// Populate the toolbar locators
		put(String.format("%s.new", key), new Locator("//:VLayout[ID=\"details\"]//ToolStripButton[name=\"new\"]"));
		put(String.format("%s.zoom", key), new Locator("//:VLayout[ID=\"details\"]//ToolStripButton[name=\"zoom\"]"));

		// Results
		put(String.format("%s.select", listGridIdentifier), new Locator(
				"//VLayout[ID=\"details\"]/member[Class=VLayout||classIndex=0]/member[Class=BizListGrid||classIndex=0]/member[Class=ListGrid||classIndex=0]/body/row[%%d]"));

		// Reset as PushListContext cannot be windowed
		resetWindowNumber();
	}

	@Override
	public void generate(SmartClientGenerateEditContext editContext) {
		PushEditContext push = editContext.pushEditContext();
		boolean windowed = editContext.windowed();
		
		User user = CORE.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(push.getModuleName());
		Document document = module.getDocument(customer, push.getDocumentName());

		String viewType = Boolean.TRUE.equals(push.getCreateView())
				? ViewType.create.toString()
				: ViewType.edit.toString();

		View view = document.getView(getUxui(), customer, viewType);

		String windowPrefix;
		if (windowed) {
			windowPrefix = String.format("//:Window[ID=\"Window%d\"]", Integer.valueOf(windowNumber));

			// Increment, handling nested zooms
			incrementWindowNumber();
		} else {
			windowPrefix = "//:VLayout[ID=\"details\"]";

			// Reset, handling edit menu items
			resetWindowNumber();
		}

		SmartClientSAILViewVisitor visitor = new SmartClientSAILViewVisitor(
				user,
				module,
				document,
				view,
				getUxui(),
				this,
				windowPrefix);

		visitor.visit();
	}

	/**
	 * Adds a locator for a given identifier.
	 * 
	 * @param identifier the logical identifier for the UI element
	 * @param locator the locator to associate with the identifier
	 */
	void put(String identifier, Locator locator) {
		List<Locator> locatorList = locators.get(identifier);

		if (locatorList == null) {
			locatorList = new ArrayList<>();
			locators.put(identifier, locatorList);
		}

		locatorList.add(locator);
	}
	
	/**
	 * Retrieves all locators associated with a given identifier.
	 * 
	 * @param identifier the logical identifier for the UI element
	 * @return the list of locators, or {@code null} if none exist
	 */
	public List<Locator> getLocators(String identifier) {
		return locators.get(identifier);
	}

	/**
	 * Increments the global window number.
	 */
	public static void incrementWindowNumber() {
		windowNumber++;
	}

	/**
	 * Decrements the global window number.
	 */
	public static void decrementWindowNumber() {
		windowNumber--;
	}

	/**
	 * Resets the global window number to zero.
	 */
	public static void resetWindowNumber() {
		windowNumber = 0;
	}
}
