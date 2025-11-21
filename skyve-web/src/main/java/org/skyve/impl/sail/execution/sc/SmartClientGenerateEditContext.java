package org.skyve.impl.sail.execution.sc;

import org.skyve.impl.sail.execution.GenerateEditContext;
import org.skyve.metadata.sail.language.step.context.PushEditContext;

/**
 * Edit context generation in a SmartClient UI.
 * <p>
 * Used by the SmartClient-specific implementation of the test framework.
 *
 * @param pushEditContext the edit context to push
 */
public record SmartClientGenerateEditContext(
		PushEditContext pushEditContext) implements GenerateEditContext {
}
