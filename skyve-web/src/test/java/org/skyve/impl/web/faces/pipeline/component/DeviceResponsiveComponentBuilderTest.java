package org.skyve.impl.web.faces.pipeline.component;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.mockito.ArgumentCaptor;
import org.primefaces.behavior.confirm.ConfirmBehavior;
import org.primefaces.component.commandbutton.CommandButton;
import org.primefaces.component.dialog.Dialog;
import org.primefaces.component.inputtext.InputText;
import org.primefaces.component.menubutton.MenuButton;
import org.primefaces.component.menuitem.UIMenuItem;
import org.primefaces.component.overlaypanel.OverlayPanel;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.bound.input.ContentCapture;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.widget.bound.input.ContentDisplay;
import org.skyve.impl.metadata.view.widget.bound.input.ContentUpload;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder.EventSourceComponent;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.view.Action;
import org.skyve.web.UserAgentType;

import jakarta.el.ELContext;
import jakarta.el.ExpressionFactory;
import jakarta.el.MethodExpression;
import jakarta.el.ValueExpression;
import jakarta.faces.application.Application;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIOutput;
import jakarta.faces.component.html.HtmlInputHidden;
import jakarta.faces.component.html.HtmlOutputLink;
import jakarta.faces.component.html.HtmlOutputText;
import jakarta.faces.component.html.HtmlPanelGrid;
import jakarta.faces.component.html.HtmlPanelGroup;
import jakarta.faces.context.FacesContext;

class DeviceResponsiveComponentBuilderTest {

	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext facesContext) {
			setCurrentInstance(facesContext);
		}
	}

	private static Application mockApplication;
	private static ExpressionFactory mockExpressionFactory;

	@BeforeAll
	
	static void setUpFacesContext() {
		FacesContext facesContext = mock(FacesContext.class);
		mockApplication = mock(Application.class);
		mockExpressionFactory = mock(ExpressionFactory.class);
		ELContext elContext = mock(ELContext.class);
		when(facesContext.getApplication()).thenReturn(mockApplication);
		when(facesContext.getELContext()).thenReturn(elContext);
		when(mockApplication.getExpressionFactory()).thenReturn(mockExpressionFactory);
		FacesContextBridge.setCurrent(facesContext);
	}

	@AfterAll
	static void tearDownFacesContext() {
		FacesContextBridge.setCurrent(null);
	}

	// ---- spacer ----

	@Test
	@SuppressWarnings("static-method")
	void spacerReturnsExistingComponentWhenNotNull() {
		DeviceResponsiveComponentBuilder builder = new DeviceResponsiveComponentBuilder();
		builder.setUserAgentType(UserAgentType.phone);
		UIComponent existing = mock(UIComponent.class);
		assertSame(existing, builder.spacer(existing, new Spacer()));
	}

	@Test
	@SuppressWarnings("static-method")
	void spacerReturnsNullForPhone() {
		DeviceResponsiveComponentBuilder builder = new DeviceResponsiveComponentBuilder();
		builder.setUserAgentType(UserAgentType.phone);
		assertNull(builder.spacer(null, new Spacer()));
	}

	@Test
	@SuppressWarnings({"static-method"})
	void spacerDelegatesToSuperForNonPhone() {
		DeviceResponsiveComponentBuilder builder = new DeviceResponsiveComponentBuilder();
		builder.setUserAgentType(UserAgentType.desktop);
		// Super call creates a Spacer component — mock the Application.createComponent call
		org.primefaces.component.spacer.Spacer mockSpacer = mock(org.primefaces.component.spacer.Spacer.class);
		when(mockApplication.createComponent(anyString())).thenReturn(mockSpacer);
		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), eq(String.class)))
			.thenReturn(mock(jakarta.el.ValueExpression.class));		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("spacer1");
		builder.setSAILManagedBean(managedBean);		UIComponent result = builder.spacer(null, new Spacer());
		assertSame(mockSpacer, result);
	}

	// ---- upload short-circuit ----

	@Test
	@SuppressWarnings("static-method")
	void uploadReturnsExistingComponentWhenNotNull() {
		DeviceResponsiveComponentBuilder builder = new DeviceResponsiveComponentBuilder();
		builder.setUserAgentType(UserAgentType.phone);
		UIComponent existing = mock(UIComponent.class);
		Action action = mock(Action.class);
		assertSame(existing, builder.upload(existing, "label", "icon", "tip", "confirm", action));
	}

	// ---- uploadButton short-circuit ----

	@Test
	@SuppressWarnings("static-method")
	void uploadButtonReturnsExistingComponentWhenNotNull() {
		DeviceResponsiveComponentBuilder builder = new DeviceResponsiveComponentBuilder();
		builder.setUserAgentType(UserAgentType.phone);
		UIComponent existing = mock(UIComponent.class);
		Button button = new Button();
		Action action = mock(Action.class);
		assertSame(existing, builder.uploadButton(existing, "label", "icon", "tip", "confirm", button, null, action));
	}

	// ---- upload delegates with phone flag ----

	@Test
	@SuppressWarnings({"static-method"})
	void uploadCallsUploadButtonWithPhoneFlagTrue() {
		// Use a capturing subclass to verify the useDialog flag
		final boolean[] capturedUseDialog = {false};
		DeviceResponsiveComponentBuilder builder = new DeviceResponsiveComponentBuilder() {
			@Override
			protected UIComponent uploadButton(EscapableText title,
												String iconStyleClass,
												String tooltip,
												String actionName,
												Integer pixelWidth,
												Integer pixelHeight,
												Boolean clientValidation,
												EscapableText confirmationText,
												String disabled,
												String formDisabled,
												String invisible,
												ContentCapture capture,
												boolean useDialog) {
				capturedUseDialog[0] = useDialog;
				return mock(UIComponent.class);
			}
		};
		builder.setUserAgentType(UserAgentType.phone);
		Action action = mock(Action.class);
		when(action.getName()).thenReturn("save");
		when(action.getClientValidation()).thenReturn(Boolean.TRUE);
		when(action.getDisabledConditionName()).thenReturn(null);
		when(action.getInvisibleConditionName()).thenReturn(null);
		builder.upload(null, "Upload", "icon", "tip", null, action);
		assertTrue(capturedUseDialog[0], "Expected useDialog=true for phone");
	}

	@Test
	@SuppressWarnings({"static-method"})
	void uploadCallsUploadButtonWithPhoneFlagFalse() {
		final boolean[] capturedUseDialog = {true};
		DeviceResponsiveComponentBuilder builder = new DeviceResponsiveComponentBuilder() {
			@Override
			protected UIComponent uploadButton(EscapableText title,
												String iconStyleClass,
												String tooltip,
												String actionName,
												Integer pixelWidth,
												Integer pixelHeight,
												Boolean clientValidation,
												EscapableText confirmationText,
												String disabled,
												String formDisabled,
												String invisible,
												ContentCapture capture,
												boolean useDialog) {
				capturedUseDialog[0] = useDialog;
				return mock(UIComponent.class);
			}
		};
		builder.setUserAgentType(UserAgentType.desktop);
		Action action = mock(Action.class);
		when(action.getName()).thenReturn("save");
		when(action.getClientValidation()).thenReturn(Boolean.FALSE);
		when(action.getDisabledConditionName()).thenReturn(null);
		when(action.getInvisibleConditionName()).thenReturn(null);
		builder.upload(null, "Upload", "icon", "tip", null, action);
		assertFalse(capturedUseDialog[0], "Expected useDialog=false for desktop");
	}

	// ---- uploadButton delegates with phone flag ----

	@Test
	@SuppressWarnings({"static-method"})
	void uploadButtonCallsUploadButtonWithPhoneFlagTrue() {
		final boolean[] capturedUseDialog = {false};
		DeviceResponsiveComponentBuilder builder = new DeviceResponsiveComponentBuilder() {
			@Override
			protected UIComponent uploadButton(EscapableText title,
												String iconStyleClass,
												String tooltip,
												String actionName,
												Integer pixelWidth,
												Integer pixelHeight,
												Boolean clientValidation,
												EscapableText confirmationText,
												String disabled,
												String formDisabled,
												String invisible,
												ContentCapture capture,
												boolean useDialog) {
				capturedUseDialog[0] = useDialog;
				return mock(UIComponent.class);
			}
		};
		builder.setUserAgentType(UserAgentType.phone);
		Button button = new Button();
		Action action = mock(Action.class);
		when(action.getName()).thenReturn("save");
		when(action.getClientValidation()).thenReturn(null);
		when(action.getDisabledConditionName()).thenReturn(null);
		when(action.getInvisibleConditionName()).thenReturn(null);
		builder.uploadButton(null, "Upload", "icon", "tip", null, button, null, action);
		assertTrue(capturedUseDialog[0], "Expected useDialog=true for phone");
	}

	@Test
	@SuppressWarnings({"static-method"})
	void uploadButtonCallsUploadButtonWithPhoneFlagFalse() {
		final boolean[] capturedUseDialog = {true};
		DeviceResponsiveComponentBuilder builder = new DeviceResponsiveComponentBuilder() {
			@Override
			protected UIComponent uploadButton(EscapableText title,
												String iconStyleClass,
												String tooltip,
												String actionName,
												Integer pixelWidth,
												Integer pixelHeight,
												Boolean clientValidation,
												EscapableText confirmationText,
												String disabled,
												String formDisabled,
												String invisible,
												ContentCapture capture,
												boolean useDialog) {
				capturedUseDialog[0] = useDialog;
				return mock(UIComponent.class);
			}
		};
		builder.setUserAgentType(UserAgentType.desktop);
		Button button = new Button();
		Action action = mock(Action.class);
		when(action.getName()).thenReturn("save");
		when(action.getClientValidation()).thenReturn(null);
		when(action.getDisabledConditionName()).thenReturn(null);
		when(action.getInvisibleConditionName()).thenReturn(null);
		builder.uploadButton(null, "Upload", "icon", "tip", null, button, null, action);
		assertFalse(capturedUseDialog[0], "Expected useDialog=false for desktop");
	}

	@Test
	@SuppressWarnings("static-method")
	void geometryUsesFullScreenDialogForPhone() {
		DeviceResponsiveComponentBuilder builder = new DeviceResponsiveComponentBuilder();
		builder.setUserAgentType(UserAgentType.phone);

		HtmlPanelGrid panelGrid = mock(HtmlPanelGrid.class);
		List<UIComponent> gridChildren = new ArrayList<>();
		when(panelGrid.getId()).thenReturn("geoGrid");
		when(panelGrid.getChildren()).thenReturn(gridChildren);

		InputText inputText = mock(InputText.class);

		CommandButton mapButton = mock(CommandButton.class);
		when(mapButton.getId()).thenReturn("geoMapButton");

		Dialog dialog = mock(Dialog.class);
		when(dialog.getId()).thenReturn("geoDialog");
		List<UIComponent> dialogChildren = new ArrayList<>();
		when(dialog.getChildren()).thenReturn(dialogChildren);

		HtmlPanelGroup outerGroup = mock(HtmlPanelGroup.class);
		List<UIComponent> outerChildren = new ArrayList<>();
		when(outerGroup.getChildren()).thenReturn(outerChildren);
		HtmlPanelGroup innerGroup = mock(HtmlPanelGroup.class);

		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("geoGrid",
												"geoGridPanel",
												"geoMapButton",
												"geoDialog",
												"geoMapOuter",
												"geoMapInner");
		builder.setSAILManagedBean(managedBean);

		ValueExpression valueExpression = mock(ValueExpression.class);
		when(mockApplication.createComponent(HtmlPanelGrid.COMPONENT_TYPE)).thenReturn(panelGrid);
		when(mockApplication.createComponent(InputText.COMPONENT_TYPE)).thenReturn(inputText);
		when(mockApplication.createComponent(CommandButton.COMPONENT_TYPE)).thenReturn(mapButton);
		when(mockApplication.createComponent(Dialog.COMPONENT_TYPE)).thenReturn(dialog);
		when(mockApplication.createComponent(HtmlPanelGroup.COMPONENT_TYPE)).thenReturn(outerGroup, innerGroup);
		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), eq(Object.class))).thenReturn(valueExpression);
		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), eq(String.class))).thenReturn(valueExpression);

		Geometry geometry = new Geometry();
		geometry.setBinding("geometry");

		EventSourceComponent result = builder.geometry(null, "row", geometry, null, "Geometry", null, HorizontalAlignment.left);

		assertNotNull(result);
		assertSame(panelGrid, result.getComponent());
		assertSame(inputText, result.getEventSource());
		assertEquals(3, gridChildren.size());
		assertSame(inputText, gridChildren.get(0));
		assertSame(mapButton, gridChildren.get(1));
		assertSame(dialog, gridChildren.get(2));
		assertSame(outerGroup, dialogChildren.get(0));
		verify(dialog).setModal(true);
		verify(dialog).setResponsive(true);
		verify(dialog).setFitViewport(true);
		verify(dialog).setResizable(false);
		verify(dialog).setCloseOnEscape(true);
		verify(dialog).setWidth("96vw");
		verify(dialog).setHeight("96vh");
		verify(dialog).setStyle("max-width:100vw;max-height:100vh;");
		verify(dialog).setOnHide("SKYVE.PF.unlockPageScroll()");
		verify(mapButton).setOnclick("PF('geoDialogDialog').show();return false");
		ArgumentCaptor<String> expressions = ArgumentCaptor.forClass(String.class);
		verify(mockExpressionFactory, atLeastOnce()).createValueExpression(any(ELContext.class), expressions.capture(), eq(String.class));
		assertTrue(expressions.getAllValues().stream().anyMatch(value -> value.contains("'SKYVE.PF.lockPageScroll();'.concat(") &&
																			value.contains(".getMapScript(")));
		verify(mockApplication, never()).createComponent(OverlayPanel.COMPONENT_TYPE);
	}

	@Test
	@SuppressWarnings("static-method")
	void contentUploadUsesFullScreenDialogForPhone() {
		DeviceResponsiveComponentBuilder builder = new DeviceResponsiveComponentBuilder();
		builder.setUserAgentType(UserAgentType.phone);

		HtmlPanelGrid panelGrid = mock(HtmlPanelGrid.class);
		List<UIComponent> panelChildren = new ArrayList<>();
		when(panelGrid.getId()).thenReturn("contentGrid");
		when(panelGrid.getChildren()).thenReturn(panelChildren);

		HtmlOutputLink outputLink = mock(HtmlOutputLink.class);
		List<UIComponent> linkChildren = new ArrayList<>();
		when(outputLink.getChildren()).thenReturn(linkChildren);
		UIOutput outputText = mock(UIOutput.class);

		HtmlPanelGroup actionGroup = mock(HtmlPanelGroup.class);
		List<UIComponent> actionChildren = new ArrayList<>();
		when(actionGroup.getChildren()).thenReturn(actionChildren);

		HtmlInputHidden hidden = mock(HtmlInputHidden.class);
		MenuButton actionButton = mock(MenuButton.class);
		List<UIComponent> menuChildren = new ArrayList<>();
		when(actionButton.getId()).thenReturn("actionButton");
		when(actionButton.getChildren()).thenReturn(menuChildren);

		UIMenuItem uploadItem = mock(UIMenuItem.class);
		UIMenuItem clearItem = mock(UIMenuItem.class);

		Dialog dialog = mock(Dialog.class);
		List<UIComponent> dialogChildren = new ArrayList<>();
		when(dialog.getChildren()).thenReturn(dialogChildren);

		HtmlOutputText iframe = mock(HtmlOutputText.class);
		ValueExpression valueExpression = mock(ValueExpression.class);

		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("contentGrid",
												"contentLinkInner",
												"actionGroup",
												"hiddenId",
												"actionButton",
												"uploadItem",
												"contentDialog",
												"iframe",
												"clearItem");
		builder.setSAILManagedBean(managedBean);

		when(mockApplication.createComponent(HtmlPanelGrid.COMPONENT_TYPE)).thenReturn(panelGrid);
		when(mockApplication.createComponent(HtmlOutputLink.COMPONENT_TYPE)).thenReturn(outputLink);
		when(mockApplication.createComponent(UIOutput.COMPONENT_TYPE)).thenReturn(outputText);
		when(mockApplication.createComponent(HtmlPanelGroup.COMPONENT_TYPE)).thenReturn(actionGroup);
		when(mockApplication.createComponent(HtmlInputHidden.COMPONENT_TYPE)).thenReturn(hidden);
		when(mockApplication.createComponent(MenuButton.COMPONENT_TYPE)).thenReturn(actionButton);
		when(mockApplication.createComponent(UIMenuItem.COMPONENT_TYPE)).thenReturn(uploadItem, clearItem);
		when(mockApplication.createComponent(Dialog.COMPONENT_TYPE)).thenReturn(dialog);
		when(mockApplication.createComponent(HtmlOutputText.COMPONENT_TYPE)).thenReturn(iframe);
		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), eq(Object.class))).thenReturn(valueExpression);
		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), eq(String.class))).thenReturn(valueExpression);

		ContentUpload content = new ContentUpload();
		content.setBinding("doc.attachment");
		content.setDisplay(ContentDisplay.link);
		content.setCapture(ContentCapture.none);
		content.setEditable(Boolean.TRUE);
		content.setShowMarkup(Boolean.FALSE);

		UIComponent result = builder.content(null, "row", content, null, "Attachment", null, HorizontalAlignment.left, true, false);

		assertSame(panelGrid, result);
		assertEquals(2, panelChildren.size());
		assertSame(outputLink, panelChildren.get(0));
		assertSame(actionGroup, panelChildren.get(1));
		assertSame(hidden, actionChildren.get(0));
		assertSame(actionButton, actionChildren.get(1));
		assertSame(dialog, actionChildren.get(2));
		assertSame(iframe, dialogChildren.get(0));
		verify(dialog).setModal(true);
		verify(dialog).setResponsive(true);
		verify(dialog).setFitViewport(true);
		verify(dialog).setResizable(false);
		verify(dialog).setCloseOnEscape(true);
		verify(dialog).setWidth("96vw");
		verify(dialog).setHeight("96vh");
		verify(dialog).setStyle("max-width:100vw;max-height:100vh;");
		verify(dialog).setOnHide("SKYVE.PF.contentOverlayOnHide('contentGrid',true,true)");
		verify(uploadItem).setValueExpression(eq("onclick"), any(ValueExpression.class));
		ArgumentCaptor<String> expressions = ArgumentCaptor.forClass(String.class);
		verify(mockExpressionFactory, atLeastOnce()).createValueExpression(any(ELContext.class), expressions.capture(), eq(String.class));
		assertTrue(expressions.getAllValues().stream().anyMatch(value -> value.contains("contentOverlayOnShow(\\'contentGrid\\'") &&
																			value.contains(".concat('\\',true);PF(\\'contentGrid_doc_attachmentOverlay\\').show();")));
		verify(mockApplication, never()).createComponent(OverlayPanel.COMPONENT_TYPE);
	}

	// ---- actionButton phone branch ----

	@Test
	@SuppressWarnings("static-method")
	void actionButtonForPhoneCallsThrough() {
		DeviceResponsiveComponentBuilder builder = new DeviceResponsiveComponentBuilder();
		builder.setUserAgentType(UserAgentType.phone);
		Button button = new Button();
		button.setPixelWidth(Integer.valueOf(200));
		button.setPixelHeight(Integer.valueOf(50));
		Action action = mock(Action.class);
		when(action.getName()).thenReturn("Save");
		when(action.getImplicitName()).thenReturn(ImplicitActionName.Save);
		when(action.getDisabledConditionName()).thenReturn(null);
		when(action.getInvisibleConditionName()).thenReturn(null);
		CommandButton btn = mock(CommandButton.class);
		when(mockApplication.createComponent(CommandButton.COMPONENT_TYPE)).thenReturn(btn);
		ConfirmBehavior confirmBehavior = mock(ConfirmBehavior.class);
		when(mockApplication.createBehavior(ConfirmBehavior.BEHAVIOR_ID)).thenReturn(confirmBehavior);
		MethodExpression me = mock(MethodExpression.class);
		ValueExpression ve = mock(ValueExpression.class);
		when(mockExpressionFactory.createMethodExpression(any(ELContext.class), anyString(), any(), any()))
			.thenReturn(me);
		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), any(Class.class)))
			.thenReturn(ve);
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("btnId");
		builder.setSAILManagedBean(managedBean);
		UIComponent result = builder.actionButton(null, null, null, "Save", null, null, null, button, null, action);
		Assertions.assertNotNull(result);
	}

	@Test
	@SuppressWarnings("static-method")
	void actionButtonForDesktopCallsThrough() {
		DeviceResponsiveComponentBuilder builder = new DeviceResponsiveComponentBuilder();
		builder.setUserAgentType(UserAgentType.desktop);
		Button button = new Button();
		button.setPixelWidth(Integer.valueOf(200));
		button.setPixelHeight(Integer.valueOf(50));
		Action action = mock(Action.class);
		when(action.getName()).thenReturn("Save");
		when(action.getImplicitName()).thenReturn(ImplicitActionName.Save);
		when(action.getDisabledConditionName()).thenReturn(null);
		when(action.getInvisibleConditionName()).thenReturn(null);
		CommandButton btn = mock(CommandButton.class);
		when(mockApplication.createComponent(CommandButton.COMPONENT_TYPE)).thenReturn(btn);
		ConfirmBehavior confirmBehavior = mock(ConfirmBehavior.class);
		when(mockApplication.createBehavior(ConfirmBehavior.BEHAVIOR_ID)).thenReturn(confirmBehavior);
		MethodExpression me = mock(MethodExpression.class);
		ValueExpression ve = mock(ValueExpression.class);
		when(mockExpressionFactory.createMethodExpression(any(ELContext.class), anyString(), any(), any()))
			.thenReturn(me);
		when(mockExpressionFactory.createValueExpression(any(ELContext.class), anyString(), any(Class.class)))
			.thenReturn(ve);
		FacesView managedBean = mock(FacesView.class);
		when(managedBean.nextId()).thenReturn("btnId");
		builder.setSAILManagedBean(managedBean);
		UIComponent result = builder.actionButton(null, null, null, "Save", null, null, null, button, null, action);
		Assertions.assertNotNull(result);
	}
}
