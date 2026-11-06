package org.skyve.impl.sail.recording;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import java.util.function.Predicate;

import org.skyve.impl.sail.execution.pf.PrimeFacesAutomationContext;
import org.skyve.impl.sail.execution.pf.PrimeFacesGenerateEditContext;
import org.skyve.impl.sail.execution.sc.SmartClientAutomationContext;
import org.skyve.impl.sail.execution.sc.SmartClientGenerateEditContext;
import org.skyve.impl.sail.mock.MockFacesContext;
import org.skyve.impl.web.faces.pipeline.component.SkyveComponentBuilderChain;
import org.skyve.impl.web.faces.pipeline.layout.ResponsiveLayoutBuilder;
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
 * entry's (or a testValue's or a lookup's) binding, an action and a tab must be on the edit or create view of the screen the
 * gesture was made on. A data entry into an inline data grid's cell names a column, not a widget, and is not checked.
 *
 * <p>The views are built as the current user ({@code CORE.getUser()}), the user who recorded, for the UX/UI and user
 * agent type the recording targets, with the same generation that renderer's executor uses: SmartClient's locators
 * ({@link SmartClientAutomationContext}) for desktop, otherwise the PrimeFaces components
 * ({@link PrimeFacesAutomationContext}), so that a step on a UX/UI's own view (such as the Control Panel's external
 * view) is checked against that view. A screen that cannot be built is not checked.
 *
 * <p>Threading: not thread-safe; use one instance for one script, in the thread that has the user.
 */
public final class SailRecordingChecks implements SailRecordingBuilder.StepCheck {
	private static final Logger LOGGER = SkyveLoggerFactory.getLogger(SailRecordingChecks.class);

	/**
	 * For each screen checked, by {@code module.Document}, a test of whether its edit or create view has an identifier;
	 * always true (not checked) if the views cannot be built.
	 */
	private final Map<String, Predicate<String>> contexts = new HashMap<>();
	/** The UX/UI the recording targets. */
	private final String uxui;
	/** The user agent type the recording targets. */
	private final UserAgentType userAgentType;

	/**
	 * Checks against SmartClient's desktop views.
	 */
	public SailRecordingChecks() {
		this(null, null);
	}

	/**
	 * Checks against the views of the UX/UI and user agent type a recording targets.
	 *
	 * @param uxui	the UX/UI, or {@code null} for desktop
	 * @param userAgentType	the user agent type, or {@code null} for desktop
	 */
	public SailRecordingChecks(@Nullable String uxui, @Nullable UserAgentType userAgentType) {
		boolean desktop = (uxui == null) || (userAgentType == null) || UxUi.DESKTOP_NAME.equals(uxui);
		this.uxui = desktop ? UxUi.DESKTOP_NAME : uxui;
		this.userAgentType = desktop ? UserAgentType.desktop : userAgentType;
	}

	/**
	 * Returns why a data entry, action or tab selection is not on the screen it was recorded on, or {@code null}.
	 */
	@Override
	public @Nullable String unconfirmed(@Nonnull Map<String, String> recorded) {
		String op = recorded.get(SailRecordingBuilder.OP);
		String identifier;
		String what;
		if (recorded.get(SailRecordingBuilder.GRID) != null) {
			return null; // a column of an inline data grid, not a widget of the view
		}
		if ("dataEnter".equals(op) || "testValue".equals(op) || ((op != null) && op.startsWith("lookupDescription"))) {
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
		Predicate<String> views = contexts.computeIfAbsent(screen, this::views);
		if (views.test(identifier)) {
			return null;
		}
		return "the " + screen + " view has no " + what;
	}

	/**
	 * Builds a document's edit and create views for the recording's UX/UI and user agent type.
	 *
	 * @param screen	{@code module.Document}
	 * @return whether the views have an identifier; always true if the views cannot be built
	 */
	private @Nonnull Predicate<String> views(@Nonnull String screen) {
		int dot = screen.indexOf('.');
		String moduleName = screen.substring(0, dot);
		String documentName = screen.substring(dot + 1);
		try {
			if (UxUi.DESKTOP_NAME.equals(uxui)) {
				List<SmartClientAutomationContext> result = new ArrayList<>(2);
				for (ViewType viewType : List.of(ViewType.edit, ViewType.create)) {
					SmartClientAutomationContext context = new SmartClientAutomationContext();
					context.setModuleName(moduleName);
					context.setDocumentName(documentName);
					context.setViewType(viewType);
					context.setUxui(uxui);
					context.setUserAgentType(userAgentType);
					context.generate(new SmartClientGenerateEditContext(push(moduleName, documentName, viewType)));
					result.add(context);
				}
				return identifier -> result.stream().anyMatch(context -> context.getLocators(identifier) != null);
			}
			List<PrimeFacesAutomationContext> result = new ArrayList<>(2);
			try (MockFacesContext ignored = MockFacesContext.replacingCurrent()) {
				for (ViewType viewType : List.of(ViewType.edit, ViewType.create)) {
					PrimeFacesAutomationContext context = new PrimeFacesAutomationContext();
					context.setModuleName(moduleName);
					context.setDocumentName(documentName);
					context.setViewType(viewType);
					context.setUxui(uxui);
					context.setUserAgentType(userAgentType);
					context.generate(new PrimeFacesGenerateEditContext(push(moduleName, documentName, viewType),
																		new SkyveComponentBuilderChain(),
																		new ResponsiveLayoutBuilder()));
					result.add(context);
				}
			}
			return identifier -> result.stream().anyMatch(context -> context.getFacesComponents(identifier) != null);
		}
		catch (Exception e) {
			LOGGER.warn("SAIL recording could not check the steps on {}: {}", screen, e.toString());
			return identifier -> true;
		}
	}

	private static @Nonnull PushEditContext push(@Nonnull String moduleName, @Nonnull String documentName, @Nonnull ViewType viewType) {
		PushEditContext result = new PushEditContext();
		result.setModuleName(moduleName);
		result.setDocumentName(documentName);
		result.setCreateView(Boolean.valueOf(viewType == ViewType.create));
		return result;
	}
}
