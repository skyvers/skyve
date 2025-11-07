package org.skyve.impl.sail.execution;

import org.skyve.metadata.sail.language.step.context.PushListContext;

/**
 * List context generation in a SmartClient UI.
 * <p>
 * Used by the SmartClient-specific implementation of the test framework.
 *
 * @param pushListContext the list context to push
 */
public record SmartClientGenerateListContext(
		PushListContext pushListContext) implements GenerateListContext {
}
