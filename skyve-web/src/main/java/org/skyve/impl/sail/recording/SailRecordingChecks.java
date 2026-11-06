package org.skyve.impl.sail.recording;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.skyve.impl.sail.execution.sc.SmartClientAutomationContext;
import org.skyve.impl.sail.execution.sc.SmartClientGenerateEditContext;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.sail.language.step.context.PushEditContext;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.skyve.web.UserAgentType;
import org.slf4j.Logger;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Checks recorded SmartClient steps against the metadata, as the SAIL SmartClient executor will find them: a data
 * entry's (or a testValue's) binding, an action and a tab must be on the edit or create view of the screen the gesture was made on.
 *
 * <p>The views are built as the current user ({@code CORE.getUser()}), the user who recorded, with the same locator
 * generation the executor uses ({@link SmartClientAutomationContext}). A screen that cannot be built is not checked.
 *
 * <p>Threading: not thread-safe; use one instance for one script, in the thread that has the user.
 */
public final class SailRecordingChecks implements SailRecordingBuilder.StepCheck {
	private static final Logger LOGGER = SkyveLoggerFactory.getLogger(SailRecordingChecks.class);

	/** The edit and create view locators of each screen checked, by {@code module.Document}; empty if not buildable. */
	private final Map<String, List<SmartClientAutomationContext>> contexts = new HashMap<>();

	/**
	 * Returns why a data entry, action or tab selection is not on the screen it was recorded on, or {@code null}.
	 */
	@Override
	public @Nullable String unconfirmed(@Nonnull Map<String, String> recorded) {
		String op = recorded.get(SailRecordingBuilder.OP);
		String identifier;
		String what;
		if ("dataEnter".equals(op) || "testValue".equals(op)) {
			identifier = recorded.get("binding");
			what = "a widget for the binding " + identifier;
		}
		else if ("action".equals(op)) {
			identifier = recorded.get("name");
			what = "the action " + identifier;
		}
		else if ("tabSelect".equals(op)) {
			identifier = recorded.get("path") + " Tab";
			what = "the tab " + recorded.get("path");
		}
		else {
			return null;
		}
		String screen = recorded.get(SailRecordingBuilder.VIEW);
		if ((identifier == null) || (screen == null) || (screen.indexOf('.') <= 0)) {
			return null;
		}
		List<SmartClientAutomationContext> views = contexts.computeIfAbsent(screen, SailRecordingChecks::views);
		if (views.isEmpty()) {
			return null;
		}
		for (SmartClientAutomationContext view : views) {
			if (view.getLocators(identifier) != null) {
				return null;
			}
		}
		return "the " + screen + " view has no " + what;
	}

	/**
	 * Builds the SmartClient locators of a document's edit and create views.
	 *
	 * @param screen	{@code module.Document}
	 * @return the contexts; empty if the views cannot be built
	 */
	private static @Nonnull List<SmartClientAutomationContext> views(@Nonnull String screen) {
		int dot = screen.indexOf('.');
		String moduleName = screen.substring(0, dot);
		String documentName = screen.substring(dot + 1);
		List<SmartClientAutomationContext> result = new ArrayList<>(2);
		try {
			for (ViewType viewType : List.of(ViewType.edit, ViewType.create)) {
				SmartClientAutomationContext context = new SmartClientAutomationContext();
				context.setModuleName(moduleName);
				context.setDocumentName(documentName);
				context.setViewType(viewType);
				context.setUxui(UxUi.DESKTOP_NAME);
				context.setUserAgentType(UserAgentType.desktop);
				PushEditContext push = new PushEditContext();
				push.setModuleName(moduleName);
				push.setDocumentName(documentName);
				push.setCreateView(Boolean.valueOf(viewType == ViewType.create));
				context.generate(new SmartClientGenerateEditContext(push));
				result.add(context);
			}
		}
		catch (Exception e) {
			LOGGER.warn("SAIL recording could not check the steps on {}: {}", screen, e.toString());
			result.clear();
		}
		return result;
	}
}
