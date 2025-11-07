package org.skyve.impl.sail.execution;

import org.skyve.metadata.sail.language.step.context.PushEditContext;

/**
 * Edit context generation in a SmartClient UI.
 * <p>
 * Used by the SmartClient-specific implementation of the test framework.
 *
 * @param pushEditContext the edit context to push
 * @param windowed if this edit context is windowed
 */
public record SmartClientGenerateEditContext(
		PushEditContext pushEditContext,
		boolean windowed) implements GenerateEditContext {
}
