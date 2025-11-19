package org.skyve.impl.sail.execution.pf;

import org.skyve.impl.sail.execution.GenerateEditContext;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.layout.LayoutBuilder;
import org.skyve.metadata.sail.language.step.context.PushEditContext;

/**
 * Edit context generation in a PrimeFaces UI.
 * <p>
 * Used by the PrimeFaces-specific implementation of the test framework.
 *
 * @param pushEditContext the edit context to push
 * @param componentBuilder the component builder for this view
 * @param layoutBuilder the layout builder for this view
 */
public record PrimeFacesGenerateEditContext(
		PushEditContext pushEditContext,
		ComponentBuilder componentBuilder,
		LayoutBuilder layoutBuilder) implements GenerateEditContext {
}
