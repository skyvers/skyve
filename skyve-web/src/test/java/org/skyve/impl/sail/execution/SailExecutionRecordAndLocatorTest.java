package org.skyve.impl.sail.execution;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.pf.PrimeFacesGenerateEditContext;
import org.skyve.impl.sail.execution.pf.PrimeFacesGenerateListContext;
import org.skyve.impl.sail.execution.sc.Locator;
import org.skyve.impl.sail.execution.sc.SmartClientGenerateEditContext;
import org.skyve.impl.sail.execution.sc.SmartClientGenerateListContext;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.layout.LayoutBuilder;
import org.skyve.metadata.sail.language.step.context.PushEditContext;
import org.skyve.metadata.sail.language.step.context.PushListContext;

@SuppressWarnings("static-method")
class SailExecutionRecordAndLocatorTest {

	@Test
	void locatorSupportsOptionalInputTypes() {
		Locator plain = new Locator("#search");
		Locator typed = new Locator("#search", Locator.InputType.COMBO);

		assertEquals("#search", plain.getLocator());
		assertNull(plain.getInputType());
		assertEquals("#search", typed.getLocator());
		assertSame(Locator.InputType.COMBO, typed.getInputType());
	}

	@Test
	void smartClientGenerateContextsExposeTheirRecordComponents() {
		PushListContext pushListContext = mock(PushListContext.class);
		PushEditContext pushEditContext = mock(PushEditContext.class);

		SmartClientGenerateListContext listContext = new SmartClientGenerateListContext(pushListContext);
		SmartClientGenerateEditContext editContext = new SmartClientGenerateEditContext(pushEditContext);

		assertSame(pushListContext, listContext.pushListContext());
		assertSame(pushEditContext, editContext.pushEditContext());
	}

	@Test
	void primeFacesGenerateContextsExposeTheirRecordComponents() {
		PushListContext pushListContext = mock(PushListContext.class);
		PushEditContext pushEditContext = mock(PushEditContext.class);
		ComponentBuilder componentBuilder = mock(ComponentBuilder.class);
		LayoutBuilder layoutBuilder = mock(LayoutBuilder.class);

		PrimeFacesGenerateListContext listContext = new PrimeFacesGenerateListContext(pushListContext, componentBuilder);
		PrimeFacesGenerateEditContext editContext = new PrimeFacesGenerateEditContext(pushEditContext, componentBuilder, layoutBuilder);

		assertSame(pushListContext, listContext.pushListContext());
		assertSame(componentBuilder, listContext.componentBuilder());
		assertSame(pushEditContext, editContext.pushEditContext());
		assertSame(componentBuilder, editContext.componentBuilder());
		assertSame(layoutBuilder, editContext.layoutBuilder());
	}
}