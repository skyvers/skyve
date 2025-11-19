package org.skyve.impl.sail.execution.pf;

import org.skyve.impl.sail.execution.GenerateListContext;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.metadata.sail.language.step.context.PushListContext;

/**
 * List context generation in a PrimeFaces UI.
 * <p>
 * Used by the PrimeFaces-specific implementation of the test framework.
 *
 * @param pushListContext the list context to push
 * @param componentBuilder the component builder for this view
 */
public record PrimeFacesGenerateListContext(
		PushListContext pushListContext,
		ComponentBuilder componentBuilder) implements GenerateListContext {
}
