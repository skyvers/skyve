package org.skyve.impl.sail.execution.sc;

import org.skyve.impl.sail.execution.GenerateEditContext;
import org.skyve.metadata.sail.language.step.context.PushEditContext;

import jakarta.annotation.Nonnull;

/**
 * Edit context generation in a SmartClient UI.
 * <p>
 * Used by the SmartClient-specific implementation of the test framework.
 *
 * @param pushEditContext the edit context to push
 */
public record SmartClientGenerateEditContext(
		@Nonnull PushEditContext pushEditContext) implements GenerateEditContext {
}
