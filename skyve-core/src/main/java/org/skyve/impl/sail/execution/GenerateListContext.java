package org.skyve.impl.sail.execution;

import org.skyve.metadata.sail.language.step.context.PushListContext;

/**
 * Marker interface for list context generation in the UI test framework.
 * <p>
 * Implementations encapsulate parameters required to perform generation on different UI technologies.
 */
public interface GenerateListContext {

	PushListContext pushListContext();
}
