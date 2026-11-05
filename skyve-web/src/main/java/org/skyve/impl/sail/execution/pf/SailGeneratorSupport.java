package org.skyve.impl.sail.execution.pf;

import java.util.function.Consumer;

import org.skyve.CORE;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.interaction.DataEnter;
import org.skyve.metadata.sail.language.step.interaction.DataEnter.Match;
import org.skyve.metadata.sail.language.step.interaction.grids.GridRowStep;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionAutoComplete;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * What the PrimeFaces SAIL code generators ({@link PrimeFacesInlineWebDriverExecutor}, {@link PrimeFacesSeleneseExecutor})
 * share about the parts of the language they cannot generate: a grid or lookup row found by its text
 * ({@code rowContaining}), a combo or radio option named by its code or label ({@code match}), a lookup suggestion
 * found by its text ({@code pick}), and declining a confirmation ({@code confirm="false"}). The generated code drives
 * the page with fixed row and option numbers and can only accept a client-side confirmation.
 */
final class SailGeneratorSupport {
	private SailGeneratorSupport() {
		// static only
	}

	/**
	 * Indicates if a grid step finds its row by text, which the generators cannot express, and if so emits a comment
	 * saying that the step's gesture is not generated.
	 *
	 * @param step	the step
	 * @param comment	emits a comment into the generated code
	 * @return true if the gesture must be skipped
	 */
	static boolean rowContainingNotSupported(@Nonnull Step step, @Nonnull Consumer<String> comment) {
		if ((step instanceof GridRowStep rowStep) && (rowStep.getRowContaining() != null)) {
			comment.accept(String.format("<%s rowContaining=\"%s\" /> is not supported by this generator (it needs a row number): not generated",
											step.getClass().getSimpleName(), rowStep.getRowContaining()));
			return true;
		}
		return false;
	}

	/**
	 * Indicates if a combo or radio data entry names its option by code or label ({@code match}), which the generators
	 * cannot express, and if so emits a comment saying that the entry is not generated.
	 *
	 * @param dataEnter	the step
	 * @param comment	emits a comment into the generated code
	 * @return true if the entry must be skipped
	 */
	static boolean optionMatchNotSupported(@Nonnull DataEnter dataEnter, @Nonnull Consumer<String> comment) {
		Match match = dataEnter.getEffectiveMatch();
		if (match != Match.index) {
			comment.accept(String.format("<dataEnter binding=\"%s\" match=\"%s\" /> is not supported by this generator (it needs an option number): not generated",
											dataEnter.getBinding(), match));
			return true;
		}
		return false;
	}

	/**
	 * Indicates if an auto complete picks a suggestion by its text ({@code pick}), which the generators cannot express,
	 * and if so emits a comment saying that the step is not generated.
	 *
	 * @param complete	the step
	 * @param comment	emits a comment into the generated code
	 * @return true if the step must be skipped
	 */
	static boolean pickNotSupported(@Nonnull LookupDescriptionAutoComplete complete, @Nonnull Consumer<String> comment) {
		String pick = complete.getPick();
		if (pick != null) {
			comment.accept(String.format("<lookupDescriptionAutoComplete binding=\"%s\" pick=\"%s\" /> is not supported by this generator (it picks the first suggestion): not generated",
											complete.getBinding(), pick));
			return true;
		}
		return false;
	}

	/**
	 * Indicates if a step declines its confirmation ({@code confirm="false"}), which the generated code cannot do, and if
	 * so emits a comment saying that the step is not generated.
	 *
	 * @param confirm	the step's confirm attribute
	 * @param tagName	the step's name, for the comment
	 * @param comment	emits a comment into the generated code
	 * @return true if the step must be skipped
	 */
	static boolean declineNotSupported(@Nullable Boolean confirm, @Nonnull String tagName, @Nonnull Consumer<String> comment) {
		if (Boolean.FALSE.equals(confirm)) {
			comment.accept(String.format("<%s confirm=\"false\" /> (declining) is not supported by this generator: not generated", tagName));
			return true;
		}
		return false;
	}

	/**
	 * Indicates if an action of the context's view asks the user to confirm it (it has confirmation text), so that the
	 * generated code must answer the confirmation (positively: {@code confirm} true or absent).
	 *
	 * @param context	the automation context (module, document, view type and UX/UI)
	 * @param actionName	the action's name
	 * @return true if the action has a confirmation
	 */
	static boolean actionConfirms(@Nonnull AutomationContext<?, ?> context, @Nonnull String actionName) {
		String moduleName = context.getModuleName();
		String documentName = context.getDocumentName();
		if ((moduleName == null) || (documentName == null)) {
			return false;
		}
		Customer c = CORE.getUser().getCustomer();
		Module m = c.getModule(moduleName);
		Document d = m.getDocument(c, documentName);
		ViewType viewType = context.getViewType();
		View view = d.getView(context.getUxui(), c, ((viewType == null) ? ViewType.edit : viewType).toString());
		Action action = view.getAction(actionName);
		return (action != null) && (action.getConfirmationText() != null);
	}
}
