package org.skyve.impl.web.faces.components;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;

import org.junit.jupiter.api.Test;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;

import jakarta.faces.component.UIComponent;
import jakarta.faces.context.FacesContext;

@SuppressWarnings("static-method")
class MapComponentTest {
	@Test
	void generateUsesModelBranchWhenModelNameIsProvided() {
		ComponentBuilder componentBuilder = mock(ComponentBuilder.class);
		UIComponent component = mock(UIComponent.class);
		when(componentBuilder.map(eq(null), any(org.skyve.impl.metadata.view.widget.MapDisplay.class), eq("savedModel")))
				.thenReturn(component);

		UIComponent result = Map.generate("admin", "qAll", "location", "savedModel", componentBuilder);

		assertSame(component, result);
		verify(componentBuilder).map(eq(null), any(org.skyve.impl.metadata.view.widget.MapDisplay.class), eq("savedModel"));
	}

	@Test
	void generateUsesQueryBranchWhenModelNameIsNull() {
		ComponentBuilder componentBuilder = mock(ComponentBuilder.class);
		UIComponent component = mock(UIComponent.class);
		when(componentBuilder.map(eq(null),
								any(org.skyve.impl.metadata.view.widget.MapDisplay.class),
								eq("admin"),
								eq("qAll"),
								eq("location")))
				.thenReturn(component);

		UIComponent result = Map.generate("admin", "qAll", "location", null, componentBuilder);

		assertSame(component, result);
		verify(componentBuilder).map(eq(null),
								any(org.skyve.impl.metadata.view.widget.MapDisplay.class),
								eq("admin"),
								eq("qAll"),
								eq("location"));
	}

	@Test
	void encodeBeginThrowsIOExceptionWhenComponentBuilderClassCannotLoad() {
		Map component = new Map();
		component.getAttributes().put("module", "admin");
		component.getAttributes().put("query", "qAll");
		component.getAttributes().put("managedBean", "view");
		component.getAttributes().put(ComponentBuilder.COMPONENT_BUILDER_CLASS_KEY, "no.such.Builder");
		FacesContext context = mock(FacesContext.class);

		IOException thrown = assertThrows(IOException.class, () -> component.encodeBegin(context));
		assertEquals("Cannot instantiate the component builder no.such.Builder", thrown.getMessage());
	}
}
