package org.skyve.impl.web.faces;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.primefaces.component.message.Message;

import jakarta.el.ValueExpression;
import jakarta.faces.application.FacesMessage;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIViewRoot;
import jakarta.faces.component.html.HtmlInputHidden;
import jakarta.faces.component.html.HtmlInputText;
import jakarta.faces.context.FacesContext;
import jakarta.faces.context.PartialViewContext;

@SuppressWarnings("static-method")
class FacesActionTest {

	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext context) {
			setCurrentInstance(context);
		}
	}

	@AfterEach
	void clearFacesContext() {
		FacesContextBridge.setCurrent(null);
	}

	@Test
	void findComponentByIdReturnsNestedComponentAndNullForMissingId() {
		UIComponent root = mock(UIComponent.class);
		UIComponent parent = mock(UIComponent.class);
		UIComponent child = mock(UIComponent.class);
		when(root.getId()).thenReturn("root");
		when(parent.getId()).thenReturn("parent");
		when(child.getId()).thenReturn("child");
		when(root.getFacetsAndChildren()).thenAnswer(i -> iterator(parent));
		when(parent.getFacetsAndChildren()).thenAnswer(i -> iterator(child));
		when(child.getFacetsAndChildren()).thenAnswer(i -> iterator());

		UIComponent found = FacesAction.findComponentById(root, "child");
		UIComponent missing = FacesAction.findComponentById(root, "missing");

		assertNotNull(found);
		assertEquals("child", found.getId());
		assertNull(missing);
	}

	@Test
	void processFacesMessagesAddsFieldAndDistinctGlobalMessageOnce() {
		UIViewRoot root = mock(UIViewRoot.class);
		org.mockito.Mockito.doReturn(Boolean.TRUE).when(root).isRendered();
		HtmlInputText input = mock(HtmlInputText.class);
		ValueExpression ve = mock(ValueExpression.class);
		when(ve.getExpressionString()).thenReturn("#{beanMapAdapter['name']}");
		when(input.getValueExpression("value")).thenReturn(ve);
		when(input.getFacetsAndChildren()).thenAnswer(i -> iterator());
		org.mockito.Mockito.doReturn(Boolean.TRUE).when(input).isRendered();
		when(input.getClientId()).thenReturn("form:nameInput");
		when(root.getFacetsAndChildren()).thenAnswer(i -> iterator(input));

		FacesContext context = mock(FacesContext.class);
		when(context.getViewRoot()).thenReturn(root);
		FacesContextBridge.setCurrent(context);

		List<String> messageTargets = new ArrayList<>();
		doAnswer(invocation -> {
			messageTargets.add((String) invocation.getArgument(0));
			return null;
		}).when(context).addMessage(any(), any(FacesMessage.class));

		Set<String> globalSet = new java.util.TreeSet<>();
		org.skyve.domain.messages.Message message = new org.skyve.domain.messages.Message("name", "Name is required");

		FacesAction.processFacesMessages(context, FacesMessage.SEVERITY_ERROR, message, globalSet);
		FacesAction.processFacesMessages(context, FacesMessage.SEVERITY_ERROR, message, globalSet);

		assertTrue(messageTargets.stream().anyMatch(t -> (t != null) && t.contains("nameInput")));
		assertEquals(1L, messageTargets.stream().filter(t -> t == null).count());
	}

	@Test
	void validateRequiredFieldsReturnsFalseAndPopulatesRenderIds() {
		UIViewRoot root = mock(UIViewRoot.class);
		org.mockito.Mockito.doReturn(Boolean.TRUE).when(root).isRendered();
		HtmlInputText requiredInput = mock(HtmlInputText.class);
		org.mockito.Mockito.doReturn(Boolean.TRUE).when(requiredInput).isRendered();
		when(requiredInput.getRequiredMessage()).thenReturn("Required");
		when(requiredInput.getValue()).thenReturn(null);
		when(requiredInput.getStyle()).thenReturn("");
		when(requiredInput.getClientId()).thenReturn("form:requiredField");
		when(requiredInput.getFacetsAndChildren()).thenAnswer(i -> iterator());

		Message fieldMessage = mock(Message.class);
		org.mockito.Mockito.doReturn(Boolean.TRUE).when(fieldMessage).isRendered();
		when(fieldMessage.getClientId()).thenReturn("form:fieldMessage");
		when(fieldMessage.getFacetsAndChildren()).thenAnswer(i -> iterator());
		when(root.getFacetsAndChildren()).thenAnswer(i -> iterator(requiredInput, fieldMessage));

		Set<String> renderIds = new LinkedHashSet<>();
		PartialViewContext partial = mock(PartialViewContext.class);
		when(partial.getRenderIds()).thenReturn(renderIds);

		FacesContext context = mock(FacesContext.class);
		when(context.getViewRoot()).thenReturn(root);
		when(context.getPartialViewContext()).thenReturn(partial);
		FacesContextBridge.setCurrent(context);

		boolean valid = FacesAction.validateRequiredFields();

		assertFalse(valid);
		assertTrue(renderIds.contains("form:fieldMessage"));
	}

	@Test
	void validateRequiredFieldsForHiddenInputUsesParentClientId() {
		UIViewRoot root = mock(UIViewRoot.class);
		org.mockito.Mockito.doReturn(Boolean.TRUE).when(root).isRendered();
		UIComponent parent = mock(UIComponent.class);
		org.mockito.Mockito.doReturn(Boolean.TRUE).when(parent).isRendered();
		when(parent.getClientId()).thenReturn("form:parent");

		HtmlInputHidden hidden = mock(HtmlInputHidden.class);
		org.mockito.Mockito.doReturn(Boolean.TRUE).when(hidden).isRendered();
		when(hidden.getRequiredMessage()).thenReturn("Hidden required");
		when(hidden.getValue()).thenReturn(null);
		when(hidden.getParent()).thenReturn(parent);
		when(hidden.getFacetsAndChildren()).thenAnswer(i -> iterator());

		when(parent.getFacetsAndChildren()).thenAnswer(i -> iterator(hidden));
		when(root.getFacetsAndChildren()).thenAnswer(i -> iterator(parent));

		Set<String> renderIds = new LinkedHashSet<>();
		PartialViewContext partial = mock(PartialViewContext.class);
		when(partial.getRenderIds()).thenReturn(renderIds);

		FacesContext context = mock(FacesContext.class);
		when(context.getViewRoot()).thenReturn(root);
		when(context.getPartialViewContext()).thenReturn(partial);
		FacesContextBridge.setCurrent(context);

		List<String> messageTargets = new ArrayList<>();
		doAnswer(invocation -> {
			messageTargets.add((String) invocation.getArgument(0));
			return null;
		}).when(context).addMessage(any(), any(FacesMessage.class));

		boolean valid = FacesAction.validateRequiredFields();

		assertFalse(valid);
		assertTrue(messageTargets.contains("form:parent"));
	}

	@Test
	void validateRequiredFieldsForDisplayNoneTextUsesParentClientId() {
		UIViewRoot root = mock(UIViewRoot.class);
		org.mockito.Mockito.doReturn(Boolean.TRUE).when(root).isRendered();
		UIComponent parent = mock(UIComponent.class);
		org.mockito.Mockito.doReturn(Boolean.TRUE).when(parent).isRendered();
		when(parent.getClientId()).thenReturn("form:parent");

		HtmlInputText hiddenText = mock(HtmlInputText.class);
		org.mockito.Mockito.doReturn(Boolean.TRUE).when(hiddenText).isRendered();
		when(hiddenText.getRequiredMessage()).thenReturn("Hidden text required");
		when(hiddenText.getValue()).thenReturn(null);
		when(hiddenText.getStyle()).thenReturn("display:none");
		when(hiddenText.getParent()).thenReturn(parent);
		when(hiddenText.getFacetsAndChildren()).thenAnswer(i -> iterator());

		when(parent.getFacetsAndChildren()).thenAnswer(i -> iterator(hiddenText));
		when(root.getFacetsAndChildren()).thenAnswer(i -> iterator(parent));

		Set<String> renderIds = new LinkedHashSet<>();
		PartialViewContext partial = mock(PartialViewContext.class);
		when(partial.getRenderIds()).thenReturn(renderIds);

		FacesContext context = mock(FacesContext.class);
		when(context.getViewRoot()).thenReturn(root);
		when(context.getPartialViewContext()).thenReturn(partial);
		FacesContextBridge.setCurrent(context);

		List<String> messageTargets = new ArrayList<>();
		doAnswer(invocation -> {
			messageTargets.add((String) invocation.getArgument(0));
			return null;
		}).when(context).addMessage(any(), any(FacesMessage.class));

		boolean valid = FacesAction.validateRequiredFields();

		assertFalse(valid);
		assertTrue(messageTargets.contains("form:parent"));
	}

	@SafeVarargs
	private static <T> Iterator<T> iterator(T... values) {
		return java.util.Arrays.asList(values).iterator();
	}
}